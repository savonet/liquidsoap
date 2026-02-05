(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

open Mm

let ffmpeg_decode =
  Lang.add_module ~base:Builtins_ffmpeg_base.track_ffmpeg "decode"

let ffmpeg_raw_decode =
  Lang.add_module ~base:Builtins_ffmpeg_base.track_ffmpeg_raw "decode"

module InternalResampler =
  Swresample.Make (Swresample.Frame) (Swresample.PlanarFloatArray)

module InternalScaler = Swscale.Make (Swscale.Frame) (Swscale.BigArray)

let log = Log.make ["ffmpeg"; "internal"; "decoder"]

let decode_audio_frame ~field ~mode generator =
  let internal_channel_layout =
    Avutil.Channel_layout.get_default (Lazy.force Frame.audio_channels)
  in
  let internal_samplerate = Lazy.force Frame.audio_rate in

  let mk_converter ~in_sample_format ~channel_layout ~samplerate =
    let converter =
      InternalResampler.create ~in_sample_format channel_layout samplerate
        internal_channel_layout internal_samplerate
    in

    fun data ->
      let data =
        match data with
          | `Frame data -> InternalResampler.convert converter data
          | `Flush -> InternalResampler.flush converter
      in
      let data = Content.Audio.lift_data data in
      Generator.put generator field data
  in

  let mk_copy_decoder () =
    let current_converter = ref None in
    let current_stream = ref None in
    let current_params = ref None in

    let mk_decoder ~time_base ~stream_idx params =
      let channels = Avcodec.Audio.get_nb_channels params in
      let channel_layout = Avutil.Channel_layout.get_default channels in
      let samplerate = Avcodec.Audio.get_sample_rate params in

      let codec_id = Avcodec.Audio.get_params_id params in
      let codec = Avcodec.Audio.find_decoder codec_id in
      let decoder = Avcodec.Audio.create_decoder ~params codec in

      let in_sample_format = Avcodec.Audio.sample_format decoder in
      let converter =
        mk_converter ~channel_layout ~samplerate ~in_sample_format
      in

      current_converter := Some (converter, decoder);
      current_stream := Some (stream_idx, time_base);
      current_params := Some params;
      (converter, decoder)
    in

    let get_converter ~time_base ~stream_idx params =
      match !current_converter with
        | None -> mk_decoder ~time_base ~stream_idx params
        | Some (converter, decoder)
          when !current_stream <> Some (stream_idx, time_base) ->
            Avcodec.flush_decoder decoder (fun frame ->
                converter (`Frame frame));
            converter `Flush;
            mk_decoder ~time_base ~stream_idx params
        | Some c -> c
    in

    function
    | `Frame frame ->
        let content = Ffmpeg_copy_content.get_data frame in
        let params = Ffmpeg_content_base.params content in
        let audio_params =
          match params with Some (`Audio p) -> p | _ -> assert false
        in
        List.iter
          (fun chunk_data ->
            let { Ffmpeg_content_base.data; stream_idx; time_base; _ } =
              chunk_data
            in
            let data =
              List.sort (fun (pos, _) (pos', _) -> compare pos pos') data
            in
            List.iter
              (function
                | _, `Audio packet ->
                    let converter, decoder =
                      get_converter ~time_base ~stream_idx audio_params
                    in
                    Avcodec.decode decoder
                      (fun frame -> converter (`Frame frame))
                      packet
                | _ -> assert false)
              data)
          content.chunks
    | `Flush ->
        ignore
          (Option.map
             (fun (stream_idx, time_base) ->
               let converter, decoder =
                 get_converter ~time_base ~stream_idx
                   (Option.get !current_params)
               in
               Avcodec.flush_decoder decoder (fun frame ->
                   converter (`Frame frame));
               converter `Flush)
             !current_stream)
  in

  let mk_raw_decoder () =
    let current_converter = ref None in

    let mk_converter ~time_base ~stream_idx
        {
          Ffmpeg_raw_content.AudioSpecs.channel_layout;
          sample_rate;
          sample_format;
        } =
      let channel_layout = Option.get channel_layout in
      let samplerate = Option.get sample_rate in
      let in_sample_format = Option.get sample_format in
      let converter =
        mk_converter ~channel_layout ~samplerate ~in_sample_format
      in
      current_converter := Some (converter, time_base, stream_idx);
      converter
    in

    let get_converter ~time_base ~stream_idx params =
      match !current_converter with
        | None -> mk_converter ~time_base ~stream_idx params
        | Some (c, t, i) when (t, i) <> (time_base, stream_idx) ->
            c `Flush;
            mk_converter ~time_base ~stream_idx params
        | Some (c, _, _) -> c
    in

    function
    | `Frame frame ->
        let content = Ffmpeg_raw_content.Audio.get_data frame in
        let params = Ffmpeg_content_base.params content in
        List.iter
          (fun chunk_data ->
            let { Ffmpeg_content_base.data; time_base; stream_idx; _ } =
              chunk_data
            in
            let data =
              List.sort (fun (pos, _) (pos', _) -> compare pos pos') data
            in
            List.iter
              (fun (_, frame) ->
                (get_converter ~time_base ~stream_idx params) (`Frame frame))
              data)
          content.chunks
    | `Flush -> (
        match !current_converter with None -> () | Some (c, _, _) -> c `Flush)
  in

  let convert ~decoder = function
    | `Frame frame -> decoder (`Frame (Frame.get frame field))
    | `Flush -> decoder `Flush
  in

  match mode with
    | `Decode -> convert ~decoder:(mk_copy_decoder ())
    | `Raw -> convert ~decoder:(mk_raw_decoder ())

let decode_video_frame ~field ~mode generator =
  let internal_width = Lazy.force Frame.video_width in
  let internal_height = Lazy.force Frame.video_height in
  let target_fps = Lazy.force Frame.video_rate in

  let mk_converter () =
    let converter = ref None in
    let current_format = ref None in

    let mk_converter ~width ~height ~pixel_format ~time_base ?pixel_aspect
        ~stream_idx () =
      current_format :=
        Some (width, height, pixel_format, time_base, pixel_aspect, stream_idx);
      let scaler =
        InternalScaler.create [] width height pixel_format internal_width
          internal_height Ffmpeg_utils.liq_frame_pixel_format
      in
      let fps_converter =
        Ffmpeg_avfilter_utils.Fps.init ~width ~height ~pixel_format ~time_base
          ?pixel_aspect ~target_fps ()
      in
      converter := Some (scaler, fps_converter);
      (scaler, fps_converter)
    in

    let get_converter ?pixel_aspect ~pixel_format ~time_base ~width ~height
        ~stream_idx () =
      match !converter with
        | None ->
            mk_converter ~width ~height ~pixel_format ~time_base ?pixel_aspect
              ~stream_idx ()
        | Some _
          when !current_format
               <> Some
                    ( width,
                      height,
                      pixel_format,
                      time_base,
                      pixel_aspect,
                      stream_idx ) ->
            log#info "Video frame format change detected..";
            mk_converter ~width ~height ~pixel_format ~time_base ?pixel_aspect
              ~stream_idx ()
        | Some v -> v
    in

    let put ~scaler data =
      let img =
        Ffmpeg_utils.unpack_image ~width:internal_width ~height:internal_height
          (InternalScaler.convert scaler data)
      in
      let data = Content.Video.lift_image (Video.Canvas.Image.make img) in
      Generator.put generator field data
    in

    fun ~time_base ~stream_idx -> function
      | `Frame frame ->
          let width = Avutil.Video.frame_get_width frame in
          let height = Avutil.Video.frame_get_height frame in
          let pixel_format = Avutil.Video.frame_get_pixel_format frame in
          let pixel_aspect = Avutil.Video.frame_get_pixel_aspect frame in
          let scaler, fps_converter =
            get_converter ?pixel_aspect ~pixel_format ~time_base ~width ~height
              ~stream_idx ()
          in
          Ffmpeg_avfilter_utils.Fps.convert fps_converter frame (put ~scaler)
      | `Flush ->
          ignore
            (Option.map
               (fun (scaler, fps_converter) ->
                 Ffmpeg_avfilter_utils.Fps.eof fps_converter (put ~scaler))
               !converter)
  in

  let mk_copy_decoder () =
    let convert = mk_converter () in

    let current_stream_idx = ref None in
    let current_time_base = ref None in
    let current_params = ref None in
    let current_decoder = ref None in

    let mk_decoder ~(params : Ffmpeg_copy_content.video_params) ~stream_idx
        ~time_base =
      let codec_id =
        Avcodec.Video.get_params_id params.Ffmpeg_copy_content.codec_params
      in
      let codec = Avcodec.Video.find_decoder codec_id in
      let decoder =
        Avcodec.Video.create_decoder
          ~params:params.Ffmpeg_copy_content.codec_params codec
      in
      current_decoder := Some decoder;
      current_stream_idx := Some stream_idx;
      current_params := Some params;
      current_time_base := Some time_base;
      decoder
    in

    let get_decoder ~params ~stream_idx ~time_base =
      match !current_decoder with
        | None -> mk_decoder ~params ~stream_idx ~time_base
        | Some decoder
          when !current_stream_idx <> Some stream_idx
               || !current_time_base <> Some time_base ->
            log#info "Video frame format change detected..";
            ignore
              (Option.map
                 (fun stream_idx ->
                   Avcodec.flush_decoder decoder (fun frame ->
                       convert
                         ~time_base:(Option.get !current_time_base)
                         ~stream_idx (`Frame frame)))
                 !current_stream_idx);
            mk_decoder ~params ~stream_idx ~time_base
        | Some d -> d
    in
    function
    | `Frame frame ->
        let content = Ffmpeg_copy_content.get_data frame in
        let params = Ffmpeg_content_base.params content in
        let video_params =
          match params with Some (`Video p) -> p | _ -> assert false
        in
        List.iter
          (fun chunk_data ->
            let { Ffmpeg_content_base.data; stream_idx; time_base; _ } =
              chunk_data
            in
            let data =
              List.sort (fun (pos, _) (pos', _) -> compare pos pos') data
            in
            List.iter
              (function
                | _, `Video packet ->
                    let decoder =
                      get_decoder ~params:video_params ~time_base ~stream_idx
                    in
                    Avcodec.decode decoder
                      (fun frame ->
                        convert ~time_base ~stream_idx (`Frame frame))
                      packet
                | _ -> assert false)
              data)
          content.chunks
    | `Flush ->
        ignore
          (Option.map
             (fun stream_idx ->
               let decoder =
                 get_decoder
                   ~params:(Option.get !current_params)
                   ~time_base:(Option.get !current_time_base)
                   ~stream_idx:(Option.get !current_stream_idx)
               in
               Avcodec.flush_decoder decoder (fun frame ->
                   convert
                     ~time_base:(Option.get !current_time_base)
                     ~stream_idx (`Frame frame));
               convert
                 ~time_base:(Option.get !current_time_base)
                 ~stream_idx `Flush)
             !current_stream_idx)
  in

  let mk_raw_decoder () =
    let convert = mk_converter () in
    let last_params = ref None in
    function
    | `Frame frame ->
        let content = Ffmpeg_raw_content.Video.get_data frame in
        List.iter
          (fun chunk_data ->
            let { Ffmpeg_content_base.data; stream_idx; time_base; _ } =
              chunk_data
            in
            let data =
              List.sort (fun (pos, _) (pos', _) -> compare pos pos') data
            in
            List.iter
              (fun (_, frame) ->
                last_params := Some (time_base, stream_idx);
                convert ~time_base ~stream_idx (`Frame frame))
              data)
          content.chunks
    | `Flush ->
        ignore
          (Option.map
             (fun (time_base, stream_idx) ->
               convert ~time_base ~stream_idx `Flush)
             !last_params)
  in

  let convert ~decoder = function
    | `Frame frame -> decoder (`Frame (Frame.get frame field))
    | `Flush -> decoder `Flush
  in

  match mode with
    | `Decode -> convert ~decoder:(mk_copy_decoder ())
    | `Raw -> convert ~decoder:(mk_raw_decoder ())

let mk_decoder mode =
  let input_frame_t =
    match mode with
      | `Audio_encoded ->
          Type.make (Format_type.descr (`Kind Ffmpeg_copy_content.kind))
      | `Audio_raw ->
          Type.make (Format_type.descr (`Kind Ffmpeg_raw_content.Audio.kind))
      | `Video_encoded ->
          Type.make (Format_type.descr (`Kind Ffmpeg_copy_content.kind))
      | `Video_raw ->
          Type.make (Format_type.descr (`Kind Ffmpeg_raw_content.Video.kind))
  in
  let output_frame_t =
    match mode with
      | `Audio_encoded | `Audio_raw -> Format_type.audio ()
      | `Video_encoded | `Video_raw -> Format_type.video ()
  in
  let base, name, decode_mode =
    match mode with
      | `Audio_encoded -> (ffmpeg_decode, "audio", `Decode)
      | `Audio_raw -> (ffmpeg_raw_decode, "audio", `Raw)
      | `Video_encoded -> (ffmpeg_decode, "video", `Decode)
      | `Video_raw -> (ffmpeg_raw_decode, "video", `Raw)
  in
  let proto = [("", input_frame_t, None, None)] in
  ignore
    (Lang.add_track_operator name proto ~base ~return_t:output_frame_t
       ~category:`Conversion ~descr:"Decode a track content" (fun p ->
         let id =
           Lang.to_default_option ~default:name Lang.to_string
             (List.assoc "id" p)
         in
         let field, source = Lang.to_track (List.assoc "" p) in

         let mk_decode_frame generator =
           let decode_frame =
             match mode with
               | `Audio_encoded | `Audio_raw ->
                   decode_audio_frame ~field ~mode:decode_mode generator
               | `Video_encoded | `Video_raw ->
                   decode_video_frame ~field ~mode:decode_mode generator
           in
           let size = Lazy.force Frame.size in
           let decode_frame = function
             | `Frame frame ->
                 List.iter
                   (fun (pos, m) -> Generator.add_metadata ~pos generator m)
                   (Frame.get_all_metadata frame);
                 List.iter
                   (fun pos -> Generator.add_track_mark ~pos generator)
                   (List.filter (fun x -> x < size) (Frame.track_marks frame));
                 decode_frame (`Frame frame)
             | `Flush -> decode_frame `Flush
           in

           decode_frame
         in

         let decode_frame_ref = ref None in

         let get_decode_frame generator =
           match !decode_frame_ref with
             | None ->
                 let fn = mk_decode_frame generator in
                 decode_frame_ref := Some fn;
                 fn
             | Some fn -> fn
         in

         let decode_frame generator frame =
           let decode_frame = get_decode_frame generator in
           match frame with
             | `Frame frame -> decode_frame (`Frame frame)
             | `Flush ->
                 decode_frame `Flush;
                 decode_frame_ref := None
         in

         let consumer =
           new Producer_consumer.consumer
             ~always_enabled:true ~write_frame:decode_frame
             ~name:(id ^ ".consumer") ~source:(Lang.source source) ()
         in
         let stack = Liquidsoap_lang.Lang_core.pos p in
         consumer#set_stack stack;

         let input_frame_t = Type.fresh input_frame_t in
         Typing.(
           consumer#frame_type
           <: Lang.frame_t (Lang.univ_t ())
                (Frame.Fields.add field input_frame_t Frame.Fields.empty));

         ( field,
           new Producer_consumer.producer
           (* We are expecting real-rate with a couple of hickups.. *)
             ~stack ~check_self_sync:false ~consumers:[consumer]
             ~name:(id ^ ".producer") () )))

let () =
  List.iter mk_decoder [`Audio_encoded; `Audio_raw; `Video_encoded; `Video_raw]
