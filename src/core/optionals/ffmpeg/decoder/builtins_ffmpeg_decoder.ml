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
    Avutil.Channel_layout.get_default (Lazy.Mutexed.force Frame.audio_channels)
  in
  let internal_samplerate = Lazy.Mutexed.force Frame.audio_rate in

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

type video_converter = {
  format : Avutil.Video.frame_format;
  time_base : Avutil.rational;
  stream_idx : int64;
  scale : Avutil.video Avutil.frame -> Video.Canvas.Image.t;
  fps : Ffmpeg_avfilter_utils.Fps.t;
}

(* What a video decoder is built from. *)
type video_codec = {
  codec_id : Avcodec.Video.id;
  width : int;
  height : int;
  pixel_format : Avutil.Pixel_format.t option;
}

(* The stream being decoded. The display stage is built from the side data of
   the stream, which can change while the codec stays: it is replaced alone. *)
type video_stream = {
  stream_idx : int64;
  time_base : Avutil.rational;
  codec : video_codec;
  decoder : Avutil.video Avcodec.decoder;
  mutable params : Ffmpeg_copy_content.video_params;
  mutable display : Ffmpeg_avfilter_utils.Display.t;
}

(* The display stage of raw frames, which come with their own side data. *)
type raw_video_stream = {
  raw_stream_idx : int64;
  raw_time_base : Avutil.rational;
  raw_display : Ffmpeg_avfilter_utils.Display.t;
}

let decode_video_frame ~field ~mode generator =
  let video_width, video_height = Frame.video_dimensions () in
  let internal_width = Lazy.Mutexed.force video_width in
  let internal_height = Lazy.Mutexed.force video_height in
  let target_fps = Lazy.Mutexed.force Frame.video_rate in

  let mk_converter () =
    let converter = ref None in

    let mk_converter ~time_base ~stream_idx (format : Avutil.Video.frame_format)
        =
      let created =
        {
          format;
          time_base;
          stream_idx;
          scale =
            Ffmpeg_decoder_common.internal_scaler ~width:internal_width
              ~height:internal_height
              ~pixel_format:
                (Ffmpeg_utils.liq_frame_pixel_format_for format.pixel_format)
              format;
          fps =
            Ffmpeg_avfilter_utils.Fps.of_frame_format ~format ~time_base
              ~target_fps ();
        }
      in
      converter := Some created;
      created
    in

    let get_converter ~time_base ~stream_idx format =
      match !converter with
        | Some built
          when built.time_base = time_base
               && built.stream_idx = stream_idx
               && Ffmpeg_utils.same_video_format built.format format ->
            built
        | previous ->
            if previous <> None then
              log#info "Video frame format change detected..";
            mk_converter ~time_base ~stream_idx format
    in

    let put ~scale frame =
      Generator.put generator field (Content.Video.lift_image (scale frame))
    in

    fun ~time_base ~stream_idx -> function
      | `Frame frame ->
          let { scale; fps; _ } =
            get_converter ~time_base ~stream_idx
              (Avutil.Video.frame_format frame)
          in
          Ffmpeg_avfilter_utils.Fps.convert fps frame (put ~scale)
      | `Flush ->
          Option.iter
            (fun { scale; fps; _ } ->
              Ffmpeg_avfilter_utils.Fps.eof fps (put ~scale))
            !converter
  in

  let mk_copy_decoder () =
    let convert = mk_converter () in
    let current = ref None in

    let codec_of (params : Ffmpeg_copy_content.video_params) =
      let codec_params = params.Ffmpeg_copy_content.codec_params in
      {
        codec_id = Avcodec.Video.get_params_id codec_params;
        width = Avcodec.Video.get_width codec_params;
        height = Avcodec.Video.get_height codec_params;
        pixel_format = Avcodec.Video.get_pixel_format codec_params;
      }
    in
    let side_data (params : Ffmpeg_copy_content.video_params) =
      Avcodec.params_side_data params.Ffmpeg_copy_content.codec_params
    in
    let mk_display ~time_base (params : Ffmpeg_copy_content.video_params) =
      Ffmpeg_avfilter_utils.Display.init
        ~params:params.Ffmpeg_copy_content.codec_params ~time_base ()
    in

    let deliver { stream_idx; time_base; _ } frame =
      convert ~time_base ~stream_idx (`Frame frame)
    in
    let display stream frame =
      Ffmpeg_avfilter_utils.Display.convert stream.display frame
        (deliver stream)
    in
    let flush_display stream =
      Ffmpeg_avfilter_utils.Display.eof stream.display (deliver stream)
    in
    let flush stream =
      Avcodec.flush_decoder stream.decoder (display stream);
      flush_display stream
    in

    let mk_stream ~stream_idx ~time_base
        (params : Ffmpeg_copy_content.video_params) =
      let codec = codec_of params in
      let stream =
        {
          stream_idx;
          time_base;
          codec;
          decoder =
            Avcodec.Video.create_decoder
              ~params:params.Ffmpeg_copy_content.codec_params
              (Avcodec.Video.find_decoder codec.codec_id);
          params;
          display = mk_display ~time_base params;
        }
      in
      current := Some stream;
      stream
    in

    (* A decoder restarted in the middle of a group of pictures has no
       reference to decode from, so a change of side data alone keeps it. *)
    let follow_side_data stream (params : Ffmpeg_copy_content.video_params) =
      if
        stream.params.Ffmpeg_copy_content.codec_params
        != params.Ffmpeg_copy_content.codec_params
      then (
        if side_data stream.params <> side_data params then (
          flush_display stream;
          stream.display <- mk_display ~time_base:stream.time_base params);
        stream.params <- params)
    in

    let get_stream ~stream_idx ~time_base
        (params : Ffmpeg_copy_content.video_params) =
      match !current with
        | Some stream
          when stream.stream_idx = stream_idx
               && stream.time_base = time_base
               && (stream.params.Ffmpeg_copy_content.codec_params
                   == params.Ffmpeg_copy_content.codec_params
                  || stream.codec = codec_of params) ->
            follow_side_data stream params;
            stream
        | previous ->
            Option.iter
              (fun stream ->
                log#info "Video frame format change detected..";
                flush stream)
              previous;
            mk_stream ~stream_idx ~time_base params
    in
    function
    | `Frame frame ->
        let content = Ffmpeg_copy_content.get_data frame in
        let params =
          match Ffmpeg_content_base.params content with
            | Some (`Video p) -> p
            | _ -> assert false
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
                    let stream = get_stream ~stream_idx ~time_base params in
                    Avcodec.decode stream.decoder (display stream) packet
                | _ -> assert false)
              data)
          content.chunks
    | `Flush ->
        Option.iter
          (fun stream ->
            flush stream;
            convert ~time_base:stream.time_base ~stream_idx:stream.stream_idx
              `Flush)
          !current
  in

  let mk_raw_decoder () =
    let convert = mk_converter () in
    let current = ref None in
    let deliver { raw_stream_idx; raw_time_base; _ } frame =
      convert ~time_base:raw_time_base ~stream_idx:raw_stream_idx (`Frame frame)
    in
    let flush_display stream =
      Ffmpeg_avfilter_utils.Display.eof stream.raw_display (deliver stream)
    in
    let get_stream ~stream_idx ~time_base =
      match !current with
        | Some stream
          when stream.raw_stream_idx = stream_idx
               && stream.raw_time_base = time_base ->
            stream
        | previous ->
            Option.iter flush_display previous;
            let stream =
              {
                raw_stream_idx = stream_idx;
                raw_time_base = time_base;
                raw_display = Ffmpeg_avfilter_utils.Display.init ~time_base ();
              }
            in
            current := Some stream;
            stream
    in
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
            let stream = get_stream ~stream_idx ~time_base in
            List.iter
              (fun (_, frame) ->
                Ffmpeg_avfilter_utils.Display.convert stream.raw_display frame
                  (deliver stream))
              data)
          content.chunks
    | `Flush ->
        Option.iter
          (fun stream ->
            flush_display stream;
            convert ~time_base:stream.raw_time_base
              ~stream_idx:stream.raw_stream_idx `Flush)
          !current
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
           decode_frame
         in

         let producer =
           Ffmpeg_inline.mk_producer ~stack:(Lang.pos p)
             ~name:(id ^ ".producer") ~field ~input_frame_t
             ~mk_process_frame:mk_decode_frame (Lang.source source)
         in

         (field, producer)))

let () =
  List.iter mk_decoder [`Audio_encoded; `Audio_raw; `Video_encoded; `Video_raw]
