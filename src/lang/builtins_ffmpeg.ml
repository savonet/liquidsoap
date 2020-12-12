(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2020 Savonet team

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

module InternalResampler =
  Swresample.Make (Swresample.FltPlanarBigArray) (Swresample.Frame)

module InternalScaler = Swscale.Make (Swscale.BigArray) (Swscale.Frame)

let write_audio_frame ~mode ~opts ?codec ~format c =
  let src_channel_layout =
    Avutil.Channel_layout.get_default (Lazy.force Frame.audio_channels)
  in
  let src_sample_rate = Lazy.force Frame.audio_rate in
  let channels = format.Ffmpeg_format.channels in
  let channel_layout = Avutil.Channel_layout.get_default channels in
  let sample_rate = Lazy.force format.Ffmpeg_format.samplerate in
  let time_base = { Avutil.num = 1; den = sample_rate } in

  let last_pts = ref None in

  let pts_duration ~get_duration pts =
    let d =
      match (!last_pts, pts) with
        | Some old_pts, Some new_pts ->
            get_duration (Some (Int64.sub new_pts old_pts))
        | _ -> 0
    in
    last_pts := pts;
    d
  in

  let sample_format, frame_size, write_frame =
    match mode with
      | `Encoded ->
          let codec = Option.get codec in
          let sample_format =
            Avcodec.Audio.find_best_sample_format codec `Dbl
          in
          let encoder =
            Avcodec.Audio.create_encoder ~opts ~channel_layout ~channels
              ~sample_format ~sample_rate ~time_base codec
          in
          let encoder_time_base = Avcodec.time_base encoder in
          let get_duration =
            Ffmpeg_decoder_common.convert_duration ~src:encoder_time_base
          in
          ( sample_format,
            ( if List.mem `Variable_frame_size (Avcodec.Audio.capabilities codec)
            then None
            else Some (Avcodec.Audio.frame_size encoder) ),
            Avcodec.encode encoder (fun packet ->
                let duration =
                  try get_duration (Avcodec.Packet.get_duration packet)
                  with _ ->
                    pts_duration ~get_duration (Avcodec.Packet.get_pts packet)
                in
                let packet =
                  { Ffmpeg_copy_content.packet; time_base = encoder_time_base }
                in
                let data =
                  {
                    Ffmpeg_content_base.params = Some (Avcodec.params encoder);
                    data = [(0, packet)];
                  }
                in
                let data = Ffmpeg_copy_content.Audio.lift_data data in
                Producer_consumer.(
                  Generator.put_audio c.generator data 0 duration)) )
      | `Raw ->
          let sample_format = `Dbl in
          let params =
            {
              Ffmpeg_raw_content.AudioSpecs.channel_layout = Some channel_layout;
              sample_format = Some sample_format;
              sample_rate = Some sample_rate;
            }
          in
          let get_duration =
            Ffmpeg_decoder_common.convert_duration ~src:time_base
          in
          ( sample_format,
            None,
            fun frame ->
              let duration =
                pts_duration ~get_duration (Avutil.frame_pts frame)
              in
              let frame = { Ffmpeg_raw_content.time_base; frame } in
              let data = { Ffmpeg_content_base.params; data = [(0, frame)] } in
              let data = Ffmpeg_raw_content.Audio.lift_data data in
              Producer_consumer.(
                Generator.put_audio c.generator data 0 duration) )
  in

  let resampler =
    InternalResampler.create ~out_sample_format:sample_format src_channel_layout
      src_sample_rate channel_layout sample_rate
  in

  let write_ffmpeg_frame =
    Ffmpeg_internal_encoder.write_audio_frame ~src_time_base:time_base
      ~dst_time_base:time_base ~target_samplerate:sample_rate
      ~target_channel_layout:channel_layout ~target_sample_format:sample_format
      ~frame_size write_frame
  in
  Printf.printf "here..\n%!";

  fun frame ->
    let frame = InternalResampler.convert resampler (AFrame.pcm frame) in
    write_ffmpeg_frame frame

let write_video_frame ~mode ~opts ?codec ~format c =
  let pixel_aspect = { Avutil.num = 1; den = 1 } in

  let pixel_format =
    Avutil.Pixel_format.of_string format.Ffmpeg_format.pixel_format
  in
  let target_fps = Lazy.force format.Ffmpeg_format.framerate in
  let frame_rate = { Avutil.num = target_fps; den = 1 } in
  let time_base = { Avutil.num = 1; den = target_fps } in
  let width = Lazy.force format.Ffmpeg_format.width in
  let height = Lazy.force format.Ffmpeg_format.height in

  let flag =
    match Ffmpeg_utils.conf_scaling_algorithm#get with
      | "fast_bilinear" -> Swscale.Fast_bilinear
      | "bilinear" -> Swscale.Bilinear
      | "bicubic" -> Swscale.Bicubic
      | _ -> failwith "Invalid value set for ffmpeg scaling algorithm!"
  in

  let src_width = Lazy.force Frame.video_width in
  let src_height = Lazy.force Frame.video_height in

  let scaler =
    InternalScaler.create [flag] src_width src_height `Yuv420p width height
      pixel_format
  in

  let fps_converter =
    Ffmpeg_utils.Fps.init ~width ~height ~pixel_format ~time_base ~pixel_aspect
      ~target_fps ()
  in

  let last_pts = ref None in

  let pts_duration ~get_duration pts =
    let d =
      match (!last_pts, pts) with
        | Some old_pts, Some new_pts ->
            get_duration (Some (Int64.sub new_pts old_pts))
        | _ -> 0
    in
    last_pts := pts;
    d
  in

  let write_frame =
    match mode with
      | `Encoded ->
          let encoder =
            Avcodec.Video.create_encoder ~opts ~frame_rate ~pixel_format ~width
              ~height ~time_base (Option.get codec)
          in

          let encoder_time_base = Avcodec.time_base encoder in

          let get_duration =
            Ffmpeg_decoder_common.convert_duration ~src:encoder_time_base
          in
          Avcodec.encode encoder (fun packet ->
              let duration =
                try get_duration (Avcodec.Packet.get_duration packet)
                with _ ->
                  pts_duration ~get_duration (Avcodec.Packet.get_pts packet)
              in
              Avcodec.Packet.set_duration packet (Some (Int64.of_int duration));
              let packet =
                { Ffmpeg_copy_content.packet; time_base = encoder_time_base }
              in
              let data =
                {
                  Ffmpeg_content_base.params = Some (Avcodec.params encoder);
                  data = [(0, packet)];
                }
              in
              let data = Ffmpeg_copy_content.Video.lift_data data in
              Producer_consumer.(
                Generator.put_video c.generator data 0 duration))
      | `Raw ->
          let params =
            {
              Ffmpeg_raw_content.VideoSpecs.width = Some width;
              height = Some height;
              pixel_format = Some pixel_format;
            }
          in
          let get_duration =
            Ffmpeg_decoder_common.convert_duration ~src:time_base
          in
          fun frame ->
            let duration =
              pts_duration ~get_duration (Avutil.frame_pts frame)
            in
            let frame = { Ffmpeg_raw_content.time_base; frame } in
            let data = { Ffmpeg_content_base.params; data = [(0, frame)] } in
            let data = Ffmpeg_raw_content.Video.lift_data data in
            Producer_consumer.(Generator.put_video c.generator data 0 duration)
  in

  (* We don't know packet duration in advance so we have to infer
     it from the next packet. *)
  let write_ffmpeg_frame frame =
    Ffmpeg_utils.Fps.convert fps_converter frame (fun ~time_base:_ frame ->
        write_frame frame)
  in

  let nb_frames = ref 0L in

  fun frame ->
    let vstart = 0 in
    let vstop = Frame.video_of_master (Lazy.force Frame.size) in
    let vbuf = VFrame.yuv420p frame in
    for i = vstart to vstop - 1 do
      let f = Video.get vbuf i in
      let y, u, v = Image.YUV420.data f in
      let sy = Image.YUV420.y_stride f in
      let s = Image.YUV420.uv_stride f in
      let vdata = [| (y, sy); (u, s); (v, s) |] in
      let frame = InternalScaler.convert scaler vdata in
      Avutil.frame_set_pts frame (Some !nb_frames);
      nb_frames := Int64.succ !nb_frames;
      write_ffmpeg_frame frame
    done

let () =
  Lang.add_module "ffmpeg";
  Lang.add_module "ffmpeg.encode";
  Lang.add_module "ffmpeg.raw"

let mk_encoder mode =
  let has_audio =
    List.mem mode [`Audio_encoded; `Audio_raw; `Both_encoded; `Both_raw]
  in
  let has_video =
    List.mem mode [`Video_encoded; `Video_raw; `Both_encoded; `Both_raw]
  in
  let has_encoded_audio = List.mem mode [`Audio_encoded; `Both_encoded] in
  let has_raw_audio = List.mem mode [`Audio_raw; `Both_raw] in
  let has_encoded_video = List.mem mode [`Video_encoded; `Both_encoded] in
  let has_raw_video = List.mem mode [`Video_raw; `Both_raw] in
  let source_kind =
    Frame.
      {
        audio = (if has_audio then audio_pcm else none);
        video = (if has_video then video_yuv420p else none);
        midi = none;
      }
  in
  let source_t = Lang.kind_type_of_kind_format source_kind in
  let source_kind_t = Lang.of_frame_kind_t source_t in
  let format_kind =
    Frame.
      {
        audio =
          ( match mode with
            | `Audio_encoded | `Both_encoded -> source_kind.Frame.audio
            | `Audio_raw | `Both_raw -> `Kind Ffmpeg_raw_content.Audio.kind
            | _ -> none );
        video =
          ( match mode with
            | `Video_encoded | `Both_encoded -> source_kind.Frame.video
            | `Video_raw | `Both_raw -> `Kind Ffmpeg_raw_content.Video.kind
            | _ -> none );
        midi = none;
      }
  in
  let format_t =
    Lang.frame_kind_t
      ~audio:
        ( match mode with
          | `Audio_encoded | `Both_encoded -> source_kind_t.Frame.audio
          | `Audio_raw | `Both_raw ->
              Lang.kind_t (`Kind Ffmpeg_raw_content.Audio.kind)
          | _ -> Lang.kind_none_t )
      ~video:
        ( match mode with
          | `Video_encoded | `Both_encoded -> source_kind_t.Frame.video
          | `Video_raw | `Both_raw ->
              Lang.kind_t (`Kind Ffmpeg_raw_content.Video.kind)
          | _ -> Lang.kind_none_t )
      ~midi:Lang.kind_none_t
  in
  let format_kind_t = Lang.of_frame_kind_t format_t in
  let return_kind =
    Frame.
      {
        audio =
          ( match mode with
            | `Audio_encoded | `Both_encoded ->
                `Kind Ffmpeg_copy_content.Audio.kind
            | `Audio_raw | `Both_raw -> format_kind.Frame.audio
            | _ -> none );
        video =
          ( match mode with
            | `Video_encoded | `Both_encoded ->
                `Kind Ffmpeg_copy_content.Video.kind
            | `Video_raw | `Both_raw -> format_kind.Frame.video
            | _ -> none );
        midi = none;
      }
  in
  let return_t =
    Lang.frame_kind_t
      ~audio:
        ( match mode with
          | `Audio_encoded | `Both_encoded ->
              Lang.kind_t (`Kind Ffmpeg_copy_content.Audio.kind)
          | `Audio_raw | `Both_raw -> format_kind_t.Frame.audio
          | _ -> Lang.kind_none_t )
      ~video:
        ( match mode with
          | `Video_encoded | `Both_encoded ->
              Lang.kind_t (`Kind Ffmpeg_copy_content.Video.kind)
          | `Video_raw | `Both_raw -> format_kind_t.Frame.video
          | _ -> Lang.kind_none_t )
      ~midi:Lang.kind_none_t
  in
  let proto =
    [
      ("", Lang.format_t format_t, None, Some "Encoding format.");
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
      | `Audio_encoded -> "encode.audio"
      | `Audio_raw -> "raw.audio"
      | `Video_encoded -> "encode.video"
      | `Video_raw -> "raw.video"
      | `Both_encoded -> "encode.audio_video"
      | `Both_raw -> "raw.audio_video"
  in
  Lang.add_operator ("ffmpeg." ^ extension) proto ~return_t
    ~category:Lang.Conversions ~descr:"Convert a source's content" (fun p ->
      let pre_buffer = Lang.to_float (List.assoc "buffer" p) in
      let max_buffer = Lang.to_float (List.assoc "max" p) in
      let max_buffer = max max_buffer (pre_buffer *. 1.1) in
      let format_val = Lang.assoc "" 1 p in
      let source = Lang.assoc "" 2 p in
      let format =
        match Lang.to_format format_val with
          | Encoder.Ffmpeg ffmpeg -> ffmpeg
          | _ ->
              raise
                (Lang_errors.Invalid_value
                   (format_val, "Only %ffmpeg encoder is currently supported!"))
      in
      let control =
        Producer_consumer.
          {
            generator =
              Generator.create
                ( match mode with
                  | `Audio_raw | `Audio_encoded -> `Audio
                  | `Video_raw | `Video_encoded -> `Video
                  | `Both_raw | `Both_encoded -> `Both );
            lock = Mutex.create ();
            buffering = true;
            abort = false;
          }
      in
      let producer =
        new Producer_consumer.producer
          ~kind:return_kind
          ~name:("ffmpeg." ^ extension ^ ".producer")
          control
      in

      if Hashtbl.length format.Ffmpeg_format.other_opts > 0 then
        raise
          (Lang_errors.Invalid_value
             ( format_val,
               Printf.sprintf
                 "Muxer options are not supported for inline encoders: %s"
                 (Ffmpeg_format.string_of_options
                    format.Ffmpeg_format.other_opts) ));

      if format.Ffmpeg_format.format <> None then
        raise
          (Lang_errors.Invalid_value
             (format_val, "Format option is not supported inline encoders"));

      let audio_opts = Hashtbl.copy format.Ffmpeg_format.audio_opts in

      let video_opts = Hashtbl.copy format.Ffmpeg_format.video_opts in

      let original_opts = Hashtbl.create 10 in

      if has_audio then
        Hashtbl.iter
          (fun name value -> Hashtbl.add original_opts ("audio: " ^ name) value)
          format.Ffmpeg_format.audio_opts;

      if has_video then
        Hashtbl.iter
          (fun name value -> Hashtbl.add original_opts ("video: " ^ name) value)
          format.Ffmpeg_format.video_opts;

      let write_frame =
        let write_audio_frame =
          if has_audio then (
            match format.Ffmpeg_format.audio_codec with
              | Some (`Raw None) when has_raw_audio ->
                  Some
                    (write_audio_frame ~mode:`Raw ~opts:audio_opts ~format
                       control)
              | Some (`Internal (Some codec)) when has_encoded_audio ->
                  let codec = Avcodec.Audio.find_encoder codec in
                  Some
                    (write_audio_frame ~mode:`Encoded ~opts:audio_opts ~codec
                       ~format control)
              | _ ->
                  let encoder =
                    if has_encoded_audio then "%audio(codec=..., ...)"
                    else "%audio.raw"
                  in
                  raise
                    (Lang_errors.Invalid_value
                       ( format_val,
                         "Operator expects an encoder of the form: " ^ encoder
                       )) )
          else None
        in
        let write_video_frame =
          if has_video then (
            match format.Ffmpeg_format.video_codec with
              | Some (`Raw None) when has_raw_video ->
                  Some
                    (write_video_frame ~mode:`Raw ~opts:video_opts ~format
                       control)
              | Some (`Internal (Some codec)) when has_encoded_video ->
                  let codec = Avcodec.Video.find_encoder codec in
                  Some
                    (write_video_frame ~mode:`Encoded ~opts:video_opts ~codec
                       ~format control)
              | _ ->
                  let encoder =
                    if has_encoded_video then "%video" else "%video.raw"
                  in
                  raise
                    (Lang_errors.Invalid_value
                       ( format_val,
                         "Operator expects an encoder of the form: " ^ encoder
                       )) )
          else None
        in
        fun frame ->
          ignore (Option.map (fun fn -> fn frame) write_video_frame);
          ignore (Option.map (fun fn -> fn frame) write_audio_frame)
      in

      let () =
        let left_over_opts = Hashtbl.create 10 in
        if has_audio then
          Hashtbl.iter
            (fun name value ->
              Hashtbl.add left_over_opts ("audio: " ^ name) value)
            audio_opts;

        if has_video then
          Hashtbl.iter
            (fun name value ->
              Hashtbl.add left_over_opts ("video: " ^ name) value)
            video_opts;

        Hashtbl.filter_map_inplace
          (fun l v -> if Hashtbl.mem left_over_opts l then Some v else None)
          original_opts;

        if Hashtbl.length original_opts > 0 then
          raise
            (Lang_errors.Invalid_value
               ( format_val,
                 Printf.sprintf "Unrecognized options: %s"
                   (Ffmpeg_format.string_of_options original_opts) ))
      in

      let _ =
        new Producer_consumer.consumer
          ~write_frame ~producer
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
