(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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
open Builtins_ffmpeg_base

module InternalResampler =
  Swresample.Make (Swresample.PlanarFloatArray) (Swresample.Frame)

module InternalScaler = Swscale.Make (Swscale.BigArray) (Swscale.Frame)

let encode_audio_frame ~stream_idx ~type_t ~mode ~opts ?codec ~format generator
    =
  let internal_channel_layout =
    Avutil.Channel_layout.get_default (Lazy.force Frame.audio_channels)
  in
  let internal_samplerate = Lazy.force Frame.audio_rate in
  let target_channels = format.Ffmpeg_format.channels in
  let target_channel_layout =
    Avutil.Channel_layout.get_default target_channels
  in
  let target_samplerate = Lazy.force format.Ffmpeg_format.samplerate in
  let target_time_base = { Avutil.num = 1; den = target_samplerate } in

  let target_sample_format =
    match format.Ffmpeg_format.sample_format with
      | Some format -> Avutil.Sample_format.find format
      | None -> `Dbl
  in

  let target_sample_format, frame_size, encode_frame =
    match mode with
      | `Encoded -> (
          let codec = Option.get codec in

          let target_sample_format =
            Avcodec.Audio.find_best_sample_format codec target_sample_format
          in

          let encoder =
            Avcodec.Audio.create_encoder ~opts
              ~channel_layout:target_channel_layout ~channels:target_channels
              ~sample_format:target_sample_format ~sample_rate:target_samplerate
              ~time_base:target_time_base codec
          in
          let encoder_time_base = Avcodec.time_base encoder in

          let duration_converter =
            Ffmpeg_utils.Duration.init ~src:encoder_time_base
              ~get_ts:Avcodec.Packet.get_dts
          in

          let params = Some (Avcodec.params encoder) in
          let effective_t =
            Type.make
              (Format_type.descr
                 (`Format (Ffmpeg_copy_content.Audio.lift_params params)))
          in
          Typing.(effective_t <: type_t);

          let write_packet packet =
            match Ffmpeg_utils.Duration.push duration_converter packet with
              | Some (length, packets) ->
                  let data =
                    List.map
                      (fun (pos, packet) ->
                        ( pos,
                          {
                            Ffmpeg_copy_content.packet;
                            time_base = encoder_time_base;
                            stream_idx;
                          } ))
                      packets
                  in
                  let data = { Ffmpeg_content_base.params; data; length } in
                  let data = Ffmpeg_copy_content.Audio.lift_data data in
                  Producer_consumer.(
                    Generator.put_audio generator data 0 length)
              | None -> ()
          in

          ( target_sample_format,
            (if List.mem `Variable_frame_size (Avcodec.capabilities codec) then
             None
            else Some (Avcodec.Audio.frame_size encoder)),
            function
            | `Frame frame -> Avcodec.encode encoder write_packet frame
            | `Flush -> Avcodec.flush_encoder encoder write_packet ))
      | `Raw -> (
          let params =
            {
              Ffmpeg_raw_content.AudioSpecs.channel_layout =
                Some target_channel_layout;
              sample_format = Some target_sample_format;
              sample_rate = Some target_samplerate;
            }
          in
          let effective_t =
            Type.make
              (Format_type.descr
                 (`Format (Ffmpeg_raw_content.Audio.lift_params params)))
          in
          Typing.(effective_t <: type_t);
          let duration_converter =
            Ffmpeg_utils.Duration.init ~src:target_time_base
              ~get_ts:Ffmpeg_utils.best_pts
          in
          ( target_sample_format,
            None,
            function
            | `Frame frame -> (
                match Ffmpeg_utils.Duration.push duration_converter frame with
                  | Some (length, frames) ->
                      let data =
                        List.map
                          (fun (pos, frame) ->
                            ( pos,
                              {
                                Ffmpeg_raw_content.time_base = target_time_base;
                                stream_idx;
                                frame;
                              } ))
                          frames
                      in
                      let data = { Ffmpeg_content_base.params; data; length } in
                      let data = Ffmpeg_raw_content.Audio.lift_data data in
                      Producer_consumer.(
                        Generator.put_audio generator data 0 length)
                  | None -> ())
            | `Flush -> () ))
  in

  let resampler =
    InternalResampler.create ~out_sample_format:target_sample_format
      internal_channel_layout internal_samplerate target_channel_layout
      target_samplerate
  in

  let encode_ffmpeg_frame =
    Ffmpeg_internal_encoder.write_audio_frame ~time_base:target_time_base
      ~sample_rate:target_samplerate ~channel_layout:target_channel_layout
      ~sample_format:target_sample_format ~frame_size (fun frame ->
        encode_frame (`Frame frame))
  in

  function
  | `Frame frame ->
      let frame =
        InternalResampler.convert ~length:(AFrame.position frame) resampler
          (AFrame.pcm frame)
      in
      encode_ffmpeg_frame frame
  | `Flush -> encode_frame `Flush

let encode_video_frame ~stream_idx ~type_t ~mode ~opts ?codec ~format generator
    =
  let internal_fps = Lazy.force Frame.video_rate in
  let internal_time_base = { Avutil.num = 1; den = internal_fps } in
  let internal_width = Lazy.force Frame.video_width in
  let internal_height = Lazy.force Frame.video_height in

  let target_fps = Lazy.force format.Ffmpeg_format.framerate in
  let target_frame_rate = { Avutil.num = target_fps; den = 1 } in
  let target_width = Lazy.force format.Ffmpeg_format.width in
  let target_height = Lazy.force format.Ffmpeg_format.height in
  let target_pixel_aspect = { Avutil.num = 1; den = 1 } in

  let flag =
    match Ffmpeg_utils.conf_scaling_algorithm#get with
      | "fast_bilinear" -> Swscale.Fast_bilinear
      | "bilinear" -> Swscale.Bilinear
      | "bicubic" -> Swscale.Bicubic
      | _ -> failwith "Invalid value set for ffmpeg scaling algorithm!"
  in

  let scaler = ref None in
  let mk_scaler ~target_pixel_format =
    scaler :=
      Some
        (InternalScaler.create [flag] internal_width internal_height
           (Ffmpeg_utils.liq_frame_pixel_format ())
           target_width target_height target_pixel_format)
  in

  let fps_converter = ref None in
  let mk_fps_converter ~target_pixel_format =
    fps_converter :=
      Some
        (Ffmpeg_avfilter_utils.Fps.init ~width:target_width
           ~height:target_height ~pixel_format:target_pixel_format
           ~time_base:internal_time_base ~pixel_aspect:target_pixel_aspect
           ~target_fps ())
  in

  let encode_frame =
    match mode with
      | `Encoded -> (
          let codec = Option.get codec in

          let target_pixel_format =
            Ffmpeg_utils.pixel_format codec format.Ffmpeg_format.pixel_format
          in

          mk_scaler ~target_pixel_format;
          mk_fps_converter ~target_pixel_format;

          let time_base =
            Ffmpeg_avfilter_utils.Fps.time_base (Option.get !fps_converter)
          in

          let hwaccel = format.Ffmpeg_format.hwaccel in
          let hwaccel_device = format.Ffmpeg_format.hwaccel_device in

          let hardware_context, target_pixel_format =
            Ffmpeg_utils.mk_hardware_context ~hwaccel ~hwaccel_device ~opts
              ~target_pixel_format ~target_width ~target_height codec
          in

          let encoder =
            Avcodec.Video.create_encoder ?hardware_context ~opts
              ~frame_rate:target_frame_rate ~pixel_format:target_pixel_format
              ~width:target_width ~height:target_height ~time_base codec
          in

          let params = Some (Avcodec.params encoder) in
          let effective_t =
            Type.make
              (Format_type.descr
                 (`Format (Ffmpeg_copy_content.Video.lift_params params)))
          in
          Typing.(effective_t <: type_t);

          let encoder_time_base = Avcodec.time_base encoder in

          let duration_converter =
            Ffmpeg_utils.Duration.init ~src:encoder_time_base
              ~get_ts:Avcodec.Packet.get_dts
          in

          let write_packet packet =
            match Ffmpeg_utils.Duration.push duration_converter packet with
              | Some (length, packets) ->
                  let data =
                    List.map
                      (fun (pos, packet) ->
                        ( pos,
                          {
                            Ffmpeg_copy_content.packet;
                            time_base = encoder_time_base;
                            stream_idx;
                          } ))
                      packets
                  in
                  let data = { Ffmpeg_content_base.params; data; length } in
                  let data = Ffmpeg_copy_content.Video.lift_data data in
                  Producer_consumer.(
                    Generator.put_video generator data 0 length)
              | None -> ()
          in

          function
          | `Frame frame -> Avcodec.encode encoder write_packet frame
          | `Flush -> Avcodec.flush_encoder encoder write_packet)
      | `Raw -> (
          let target_pixel_format = Ffmpeg_utils.liq_frame_pixel_format () in

          mk_scaler ~target_pixel_format;
          mk_fps_converter ~target_pixel_format;

          let time_base =
            Ffmpeg_avfilter_utils.Fps.time_base (Option.get !fps_converter)
          in

          let params =
            {
              Ffmpeg_raw_content.VideoSpecs.width = Some target_width;
              height = Some target_height;
              pixel_format = Some target_pixel_format;
              pixel_aspect = Some target_pixel_aspect;
            }
          in

          let effective_t =
            Type.make
              (Format_type.descr
                 (`Format (Ffmpeg_raw_content.Video.lift_params params)))
          in
          Typing.(effective_t <: type_t);

          let duration_converter =
            Ffmpeg_utils.Duration.init ~src:time_base
              ~get_ts:Ffmpeg_utils.best_pts
          in

          function
          | `Frame frame -> (
              match Ffmpeg_utils.Duration.push duration_converter frame with
                | Some (length, frames) ->
                    let data =
                      List.map
                        (fun (pos, frame) ->
                          ( pos,
                            { Ffmpeg_raw_content.time_base; stream_idx; frame }
                          ))
                        frames
                    in
                    let data = { Ffmpeg_content_base.params; data; length } in
                    let data = Ffmpeg_raw_content.Video.lift_data data in
                    Producer_consumer.(
                      Generator.put_video generator data 0 length)
                | None -> ())
          | `Flush -> ())
  in

  (* We don't know packet duration in advance so we have to infer
     it from the next packet. *)
  let encode_ffmpeg_frame frame =
    Ffmpeg_avfilter_utils.Fps.convert (Option.get !fps_converter) frame
      (fun frame -> encode_frame (`Frame frame))
  in

  let nb_frames = ref 0L in

  function
  | `Frame frame ->
      let vstart = 0 in
      let vstop = VFrame.position frame in
      let vbuf = VFrame.data frame in
      for i = vstart to vstop - 1 do
        let f = Video.Canvas.render vbuf i in
        let vdata = Ffmpeg_utils.pack_image f in
        let frame = InternalScaler.convert (Option.get !scaler) vdata in
        Avutil.Frame.set_pts frame (Some !nb_frames);
        nb_frames := Int64.succ !nb_frames;
        encode_ffmpeg_frame frame
      done
  | `Flush -> encode_frame `Flush

let () =
  Lang.add_module "ffmpeg.encode";
  Lang.add_module "ffmpeg.raw.encode"

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
  let audio_field_t = if has_audio then Some (Format_type.audio ()) else None in
  let video_field_t = if has_video then Some (Format_type.video ()) else None in
  let source_base_t = Lang.univ_t () in
  let source_frame_t =
    Lang.frame_t source_base_t
      (Frame.mk_fields ?audio:audio_field_t ?video:video_field_t ())
  in
  let format_audio_field_t, return_audio_field_t =
    match mode with
      | `Audio_encoded | `Both_encoded ->
          ( audio_field_t,
            Some
              (Type.make
                 (Format_type.descr (`Kind Ffmpeg_copy_content.Audio.kind))) )
      | `Audio_raw | `Both_raw ->
          let t =
            Some
              (Type.make
                 (Format_type.descr (`Kind Ffmpeg_raw_content.Audio.kind)))
          in
          (t, t)
      | _ -> (None, None)
  in
  let format_video_field_t, return_video_field_t =
    match mode with
      | `Video_encoded | `Both_encoded ->
          ( video_field_t,
            Some
              (Type.make
                 (Format_type.descr (`Kind Ffmpeg_copy_content.Video.kind))) )
      | `Video_raw | `Both_raw ->
          let t =
            Some
              (Type.make
                 (Format_type.descr (`Kind Ffmpeg_raw_content.Video.kind)))
          in
          (t, t)
      | _ -> (None, None)
  in
  let format_frame_t =
    Lang.frame_t Lang.unit_t
      (Frame.mk_fields ?audio:format_audio_field_t ?video:format_video_field_t
         ())
  in
  let return_t =
    Lang.frame_t source_base_t
      (Frame.mk_fields ?audio:return_audio_field_t ?video:return_video_field_t
         ())
  in
  let extension =
    match mode with
      | `Audio_encoded -> "encode.audio"
      | `Audio_raw -> "raw.encode.audio"
      | `Video_encoded -> "encode.video"
      | `Video_raw -> "raw.encode.video"
      | `Both_encoded -> "encode.audio_video"
      | `Both_raw -> "raw.encode.audio_video"
  in
  let name = "ffmpeg." ^ extension in
  let proto =
    [
      ("", Lang.format_t format_frame_t, None, Some "Encoding format.");
      ("", Lang.source_t source_frame_t, None, None);
    ]
  in
  Lang.add_operator name proto ~return_t ~category:`Conversion
    ~descr:"Convert a source's content" (fun p ->
      let id =
        Lang.to_default_option ~default:name Lang.to_string (List.assoc "id" p)
      in
      let format_val = Lang.assoc "" 1 p in
      let source = Lang.assoc "" 2 p in
      let format =
        match Lang.to_format format_val with
          | Encoder.Ffmpeg ffmpeg -> ffmpeg
          | _ ->
              raise
                (Error.Invalid_value
                   (format_val, "Only %ffmpeg encoder is currently supported!"))
      in
      let content =
        match mode with
          | `Audio_raw | `Audio_encoded -> `Audio
          | `Video_raw | `Video_encoded -> `Video
          | `Both_raw | `Both_encoded -> `Both
      in
      let generator = Producer_consumer.Generator.create content in

      if Hashtbl.length format.Ffmpeg_format.other_opts > 0 then
        raise
          (Error.Invalid_value
             ( format_val,
               Printf.sprintf
                 "Muxer options are not supported for inline encoders: %s"
                 (Ffmpeg_format.string_of_options
                    format.Ffmpeg_format.other_opts) ));

      if format.Ffmpeg_format.format <> None then
        raise
          (Error.Invalid_value
             (format_val, "Format option is not supported inline encoders"));

      let mk_encode_frame () =
        let audio_t =
          Option.map
            (fun t ->
              let s = Typing.generalize ~level:(-1) t in
              Typing.instantiate ~level:(-1) s)
            return_audio_field_t
        in

        let video_t =
          Option.map
            (fun t ->
              let s = Typing.generalize ~level:(-1) t in
              Typing.instantiate ~level:(-1) s)
            return_video_field_t
        in

        let audio_opts = Hashtbl.copy format.Ffmpeg_format.audio_opts in

        let video_opts = Hashtbl.copy format.Ffmpeg_format.video_opts in

        let original_opts = Hashtbl.create 10 in

        let stream_idx = Ffmpeg_content_base.new_stream_idx () in

        if has_audio then
          Hashtbl.iter
            (fun name value ->
              Hashtbl.add original_opts ("audio: " ^ name) value)
            format.Ffmpeg_format.audio_opts;

        if has_video then
          Hashtbl.iter
            (fun name value ->
              Hashtbl.add original_opts ("video: " ^ name) value)
            format.Ffmpeg_format.video_opts;

        let encode_audio_frame =
          if has_audio then (
            match format.Ffmpeg_format.audio_codec with
              | Some (`Raw None) when has_raw_audio ->
                  Some
                    (encode_audio_frame ~stream_idx ~type_t:(Option.get audio_t)
                       ~mode:`Raw ~opts:audio_opts ~format generator)
              | Some (`Internal (Some codec)) when has_encoded_audio ->
                  let codec = Avcodec.Audio.find_encoder_by_name codec in
                  Some
                    (encode_audio_frame ~stream_idx ~type_t:(Option.get audio_t)
                       ~mode:`Encoded ~opts:audio_opts ~codec ~format generator)
              | _ ->
                  let encoder =
                    if has_encoded_audio then "%audio(codec=..., ...)"
                    else "%audio.raw"
                  in
                  raise
                    (Error.Invalid_value
                       ( format_val,
                         "Operator expects an encoder of the form: " ^ encoder
                       )))
          else None
        in
        let encode_video_frame =
          if has_video then (
            match format.Ffmpeg_format.video_codec with
              | Some (`Raw None) when has_raw_video ->
                  Some
                    (encode_video_frame ~stream_idx ~type_t:(Option.get video_t)
                       ~mode:`Raw ~opts:video_opts ~format generator)
              | Some (`Internal (Some codec)) when has_encoded_video ->
                  let codec = Avcodec.Video.find_encoder_by_name codec in
                  Some
                    (encode_video_frame ~stream_idx ~type_t:(Option.get video_t)
                       ~mode:`Encoded ~opts:video_opts ~codec ~format generator)
              | _ ->
                  let encoder =
                    if has_encoded_video then "%video" else "%video.raw"
                  in
                  raise
                    (Error.Invalid_value
                       ( format_val,
                         "Operator expects an encoder of the form: " ^ encoder
                       )))
          else None
        in
        let size = Lazy.force Frame.size in

        let encode_frame = function
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
                (Option.map (fun fn -> fn (`Frame frame)) encode_video_frame);
              ignore
                (Option.map (fun fn -> fn (`Frame frame)) encode_audio_frame)
          | `Flush ->
              ignore (Option.map (fun fn -> fn `Flush) encode_video_frame);
              ignore (Option.map (fun fn -> fn `Flush) encode_audio_frame)
        in

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
            (Error.Invalid_value
               ( format_val,
                 Printf.sprintf "Unrecognized options: %s"
                   (Ffmpeg_format.string_of_options original_opts) ));

        encode_frame
      in

      let encode_frame_ref = ref (mk_encode_frame ()) in

      let encode_frame = function
        | `Frame frame -> !encode_frame_ref (`Frame frame)
        | `Flush ->
            !encode_frame_ref `Flush;
            encode_frame_ref := mk_encode_frame ()
      in

      let consumer =
        new Producer_consumer.consumer
          ~write_frame:encode_frame ~name:(id ^ ".consumer") ~source ()
      in
      let source_frame_t =
        Typing.instantiate ~level:(-1)
          (Typing.generalize ~level:(-1) source_frame_t)
      in
      Typing.(consumer#frame_type <: source_frame_t);

      new Producer_consumer.producer
      (* We are expecting real-rate with a couple of hickups.. *)
        ~check_self_sync:false ~consumers:[consumer] ~name:(id ^ ".producer")
        generator)

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
