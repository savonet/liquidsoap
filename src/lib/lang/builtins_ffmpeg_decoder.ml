(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

module InternalResampler =
  Swresample.Make (Swresample.Frame) (Swresample.FltPlanarBigArray)

module InternalScaler = Swscale.Make (Swscale.Frame) (Swscale.BigArray)

let log = Log.make ["ffmpeg"; "internal"; "decoder"]

let decode_audio_frame ~mode c =
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
      let data = InternalResampler.convert converter data in
      let len = Audio.length data in
      let data = Frame_content.Audio.lift_data data in
      Producer_consumer.(
        Generator.put_audio c.generator ?pts:None data 0
          (Frame.main_of_audio len))
  in

  let mk_copy_decoder () =
    let current_converter = ref None in
    let current_stream_idx = ref None in
    let current_params = ref None in

    let mk_decoder ~stream_idx params =
      let params = Option.get params in
      let channels = Avcodec.Audio.get_nb_channels params in
      let channel_layout = Avutil.Channel_layout.get_default channels in
      let samplerate = Avcodec.Audio.get_sample_rate params in
      let in_sample_format = Avcodec.Audio.get_sample_format params in

      let converter =
        mk_converter ~channel_layout ~samplerate ~in_sample_format
      in

      let codec_id = Avcodec.Audio.get_params_id params in
      let codec = Avcodec.Audio.find_decoder codec_id in
      let decoder = Avcodec.Audio.create_decoder ~params codec in
      current_converter := Some (converter, decoder);
      current_stream_idx := Some stream_idx;
      current_params := Some params;
      (converter, decoder)
    in

    let get_converter ~stream_idx params =
      match !current_converter with
        | None -> mk_decoder ~stream_idx params
        | Some (converter, decoder) when !current_stream_idx <> Some stream_idx
          ->
            Avcodec.flush_decoder decoder converter;
            mk_decoder ~stream_idx params
        | Some c -> c
    in

    function
    | `Frame frame ->
        let { Ffmpeg_content_base.data; params } =
          Ffmpeg_copy_content.Audio.get_data frame
        in
        let data =
          List.sort (fun (pos, _) (pos', _) -> compare pos pos') data
        in
        List.iter
          (fun (_, { Ffmpeg_copy_content.packet; stream_idx }) ->
            let converter, decoder = get_converter ~stream_idx params in
            Avcodec.decode decoder converter packet)
          data
    | `Flush ->
        ignore
          (Option.map
             (fun stream_idx ->
               let converter, decoder =
                 get_converter ~stream_idx !current_params
               in
               Avcodec.flush_decoder decoder converter)
             !current_stream_idx)
  in

  let mk_raw_decoder () =
    let current_converter = ref None in

    let mk_decoder
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
      current_converter := Some converter;
      converter
    in

    let get_converter params =
      match !current_converter with None -> mk_decoder params | Some c -> c
    in

    function
    | `Frame frame ->
        let { Ffmpeg_content_base.data; params } =
          Ffmpeg_raw_content.Audio.get_data frame
        in
        let data =
          List.sort (fun (pos, _) (pos', _) -> compare pos pos') data
        in
        let converter = get_converter params in
        List.iter
          (fun (_, { Ffmpeg_raw_content.frame; _ }) -> converter frame)
          data
    | `Flush -> ()
  in

  let convert
        : 'a 'b.
          get_data:(Frame_content.data -> ('a, 'b) Ffmpeg_content_base.content) ->
          decoder:([ `Frame of Frame_content.data | `Flush ] -> unit) ->
          [ `Frame of Frame.t | `Flush ] -> unit =
   fun ~get_data ~decoder -> function
    | `Frame frame ->
        let frame = Frame.(frame.content.audio) in
        let { Ffmpeg_content_base.data; _ } = get_data frame in
        if data = [] then () else decoder (`Frame frame)
    | `Flush -> decoder `Flush
  in

  match mode with
    | `Decode ->
        convert ~get_data:Ffmpeg_copy_content.Audio.get_data
          ~decoder:(mk_copy_decoder ())
    | `Raw ->
        convert ~get_data:Ffmpeg_raw_content.Audio.get_data
          ~decoder:(mk_raw_decoder ())

let decode_video_frame ~mode c =
  let internal_width = Lazy.force Frame.video_width in
  let internal_height = Lazy.force Frame.video_height in
  let target_fps = Lazy.force Frame.video_rate in

  let mk_converter () =
    let converter = ref None in
    let current_format = ref None in

    let mk_converter ~width ~height ~pixel_format ~time_base ?pixel_aspect
        ~stream_idx =
      current_format :=
        Some (width, height, pixel_format, time_base, pixel_aspect, stream_idx);
      let scaler =
        InternalScaler.create [] width height pixel_format internal_width
          internal_height
          (Ffmpeg_utils.liq_frame_pixel_format ())
      in
      let fps_converter =
        Ffmpeg_utils.Fps.init ~width ~height ~pixel_format ~time_base
          ?pixel_aspect ~target_fps ()
      in
      converter := Some (scaler, fps_converter);
      (scaler, fps_converter)
    in

    let get_converter ?pixel_aspect ~pixel_format ~time_base ~width ~height
        ~stream_idx =
      match !converter with
        | None ->
            mk_converter ~width ~height ~pixel_format ~time_base ?pixel_aspect
              ~stream_idx
        | Some _
          when !current_format
               <> Some
                    ( width,
                      height,
                      pixel_format,
                      time_base,
                      pixel_aspect,
                      stream_idx ) ->
            log#info "Audio frame format change detected..";
            mk_converter ~width ~height ~pixel_format ~time_base ?pixel_aspect
              ~stream_idx
        | Some v -> v
    in

    fun ~time_base ~stream_idx frame ->
      let width = Avutil.Video.frame_get_width frame in
      let height = Avutil.Video.frame_get_height frame in
      let pixel_format = Avutil.Video.frame_get_pixel_format frame in
      let pixel_aspect = Avutil.Video.frame_get_pixel_aspect frame in
      let scaler, fps_converter =
        get_converter ?pixel_aspect ~pixel_format ~time_base ~width ~height
          ~stream_idx
      in
      Ffmpeg_utils.Fps.convert fps_converter frame (fun data ->
          let img =
            Ffmpeg_utils.unpack_image ~width:internal_width
              ~height:internal_height
              (InternalScaler.convert scaler data)
          in
          let data = Video.single img in
          let data = Frame_content.Video.lift_data data in
          Producer_consumer.(
            Generator.put_video c.generator ?pts:None data 0
              (Frame.main_of_video 1)))
  in

  let mk_copy_decoder () =
    let convert = mk_converter () in

    let current_stream_idx = ref None in
    let current_time_base = ref None in
    let current_params = ref None in
    let current_decoder = ref None in

    let mk_decoder ~params ~stream_idx ~time_base =
      let params = Option.get params in
      let codec_id = Avcodec.Video.get_params_id params in
      let codec = Avcodec.Video.find_decoder codec_id in
      let decoder = Avcodec.Video.create_decoder ~params codec in
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
                   Avcodec.flush_decoder decoder
                     (convert
                        ~time_base:(Option.get !current_time_base)
                        ~stream_idx))
                 !current_stream_idx);
            mk_decoder ~params ~stream_idx ~time_base
        | Some d -> d
    in
    function
    | `Frame frame ->
        let { Ffmpeg_content_base.data; params } =
          Ffmpeg_copy_content.Video.get_data frame
        in
        let data =
          List.sort (fun (pos, _) (pos', _) -> compare pos pos') data
        in
        List.iter
          (fun (_, { Ffmpeg_copy_content.packet; stream_idx; time_base }) ->
            let decoder = get_decoder ~params ~time_base ~stream_idx in
            Avcodec.decode decoder (convert ~time_base ~stream_idx) packet)
          data
    | `Flush ->
        ignore
          (Option.map
             (fun stream_idx ->
               let decoder =
                 get_decoder ~params:!current_params
                   ~time_base:(Option.get !current_time_base)
                   ~stream_idx:(Option.get !current_stream_idx)
               in
               Avcodec.flush_decoder decoder
                 (convert
                    ~time_base:(Option.get !current_time_base)
                    ~stream_idx))
             !current_stream_idx)
  in

  let mk_raw_decoder () =
    let convert = mk_converter () in
    function
    | `Frame frame ->
        let { Ffmpeg_content_base.data; _ } =
          Ffmpeg_raw_content.Video.get_data frame
        in
        let data =
          List.sort (fun (pos, _) (pos', _) -> compare pos pos') data
        in
        List.iter
          (fun (_, { Ffmpeg_raw_content.frame; stream_idx; time_base }) ->
            convert ~time_base ~stream_idx frame)
          data
    | `Flush -> ()
  in

  let convert
        : 'a 'b.
          get_data:(Frame_content.data -> ('a, 'b) Ffmpeg_content_base.content) ->
          decoder:([ `Frame of Frame_content.data | `Flush ] -> unit) ->
          [ `Frame of Frame.t | `Flush ] -> unit =
   fun ~get_data ~decoder -> function
    | `Frame frame ->
        let frame = Frame.(frame.content.video) in
        let { Ffmpeg_content_base.data; _ } = get_data frame in
        if data = [] then () else decoder (`Frame frame)
    | `Flush -> decoder `Flush
  in

  match mode with
    | `Decode ->
        convert ~get_data:Ffmpeg_copy_content.Video.get_data
          ~decoder:(mk_copy_decoder ())
    | `Raw ->
        convert ~get_data:Ffmpeg_raw_content.Video.get_data
          ~decoder:(mk_raw_decoder ())

let () =
  Lang.add_module "ffmpeg.decode";
  Lang.add_module "ffmpeg.raw.decode"

let mk_encoder mode =
  let has_audio =
    List.mem mode [`Audio_encoded; `Audio_raw; `Both_encoded; `Both_raw]
  in
  let has_video =
    List.mem mode [`Video_encoded; `Video_raw; `Both_encoded; `Both_raw]
  in
  let has_encoded_audio = List.mem mode [`Audio_encoded; `Both_encoded] in
  let has_encoded_video = List.mem mode [`Video_encoded; `Both_encoded] in
  let source_kind =
    Frame.
      {
        audio =
          ( match mode with
            | `Audio_encoded | `Both_encoded ->
                `Kind Ffmpeg_copy_content.Audio.kind
            | `Audio_raw | `Both_raw -> `Kind Ffmpeg_raw_content.Audio.kind
            | _ -> none );
        video =
          ( match mode with
            | `Video_encoded | `Both_encoded ->
                `Kind Ffmpeg_copy_content.Video.kind
            | `Video_raw | `Both_raw -> `Kind Ffmpeg_raw_content.Video.kind
            | _ -> none );
        midi = none;
      }
  in
  let source_t = Lang.kind_type_of_kind_format source_kind in
  let return_kind =
    Frame.
      {
        audio = (if has_audio then audio_pcm else none);
        video = (if has_video then video_yuva420p else none);
        midi = none;
      }
  in
  let return_t = Lang.kind_type_of_kind_format return_kind in
  let proto =
    [
      ("", Lang.source_t source_t, None, None);
      ( "buffer",
        Lang.float_t,
        Some (Lang.float 1.),
        Some "Amount of data to pre-buffer, in seconds." );
      ( "max",
        Lang.float_t,
        Some (Lang.float 10.),
        Some "Maximum amount of buffered data, in seconds." );
    ]
  in
  let extension =
    match mode with
      | `Audio_encoded -> "decode.audio"
      | `Audio_raw -> "raw.decode.audio"
      | `Video_encoded -> "decode.video"
      | `Video_raw -> "raw.decode.video"
      | `Both_encoded -> "decode.audio_video"
      | `Both_raw -> "raw.decode.audio_video"
  in
  Lang.add_operator ("ffmpeg." ^ extension) proto ~return_t
    ~category:Lang.Conversions ~descr:"Convert a source's content" (fun p ->
      let pre_buffer = Lang.to_float (List.assoc "buffer" p) in
      let max_buffer = Lang.to_float (List.assoc "max" p) in
      let max_buffer = max max_buffer (pre_buffer *. 1.1) in
      let source = List.assoc "" p in
      let generator =
        Producer_consumer.Generator.create
          ( match mode with
            | `Audio_raw | `Audio_encoded -> `Audio
            | `Video_raw | `Video_encoded -> `Video
            | `Both_raw | `Both_encoded -> `Both )
      in
      let control =
        Producer_consumer.
          { generator; lock = Mutex.create (); buffering = true; abort = false }
      in
      let producer =
        new Producer_consumer.producer
          ~kind:return_kind
          ~name:("ffmpeg." ^ extension ^ ".producer")
          control
      in

      let mk_decode_frame () =
        let decode_audio_frame =
          if has_audio then (
            let mode = if has_encoded_audio then `Decode else `Raw in
            Some (decode_audio_frame ~mode control) )
          else None
        in
        let decode_video_frame =
          if has_video then (
            let mode = if has_encoded_video then `Decode else `Raw in
            Some (decode_video_frame ~mode control) )
          else None
        in
        let size = Lazy.force Frame.size in

        let decode_frame = function
          | `Frame frame ->
              List.iter
                (fun (pos, m) ->
                  Producer_consumer.Generator.add_metadata ~pos generator m)
                (Frame.get_all_metadata frame);
              List.iter
                (fun pos ->
                  Producer_consumer.Generator.add_break ~pos generator)
                (List.filter (fun x -> x < size) (Frame.breaks frame));
              ignore
                (Option.map (fun fn -> fn (`Frame frame)) decode_video_frame);
              ignore
                (Option.map (fun fn -> fn (`Frame frame)) decode_audio_frame)
          | `Flush ->
              ignore (Option.map (fun fn -> fn `Flush) decode_video_frame);
              ignore (Option.map (fun fn -> fn `Flush) decode_audio_frame)
        in

        decode_frame
      in

      let decode_frame_ref = ref (mk_decode_frame ()) in

      let decode_frame = function
        | `Frame frame -> !decode_frame_ref (`Frame frame)
        | `Flush ->
            !decode_frame_ref `Flush;
            decode_frame_ref := mk_decode_frame ()
      in

      let _ =
        new Producer_consumer.consumer
          ~write_frame:decode_frame ~producer
          ~output_kind:("ffmpeg." ^ extension ^ ".consumer")
          ~kind:source_kind ~content:`Audio ~max_buffer ~pre_buffer ~source
          control
      in
      producer)

let () =
  List.iter mk_encoder
    [
      `Audio_encoded;
      `Audio_raw;
      `Video_encoded;
      `Video_raw;
      `Both_encoded;
      `Both_raw;
    ]
