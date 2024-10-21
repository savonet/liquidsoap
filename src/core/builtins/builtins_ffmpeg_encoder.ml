(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

let ffmpeg_encode =
  Lang.add_module ~base:Builtins_ffmpeg_base.track_ffmpeg "encode"

let ffmpeg_raw_encode =
  Lang.add_module ~base:Builtins_ffmpeg_base.track_ffmpeg_raw "encode"

module InternalResampler =
  Swresample.Make (Swresample.PlanarFloatArray) (Swresample.Frame)

module InternalScaler = Swscale.Make (Swscale.PackedBigArray) (Swscale.Frame)

type source_idx = { source : Source.source; idx : int64 }

module SourceIdx = Weak.Make (struct
  type t = source_idx

  let equal x y = x.source == y.source
  let hash x = Obj.magic x.source
end)

let source_idx_map = SourceIdx.create 0

let encode_audio_frame ~source_idx ~type_t ~mode ~opts ?codec ~format
    ~content_type ~field generator =
  let internal_channel_layout =
    Avutil.Channel_layout.get_default
      (Content.Audio.channels_of_format
         (Frame.Fields.find field (content_type ())))
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
              ~channel_layout:target_channel_layout
              ~sample_format:target_sample_format ~sample_rate:target_samplerate
              ~time_base:target_time_base codec
          in
          let encoder_time_base = Avcodec.time_base encoder in

          let duration_converter =
            Ffmpeg_utils.Duration.init ~mode:`DTS ~src:encoder_time_base
              ~convert_ts:false ~get_ts:Avcodec.Packet.get_dts
              ~set_ts:Avcodec.Packet.set_dts ()
          in

          let params = Some (`Audio (Avcodec.params encoder)) in
          let effective_t =
            Type.make
              (Format_type.descr
                 (`Format (Ffmpeg_copy_content.lift_params params)))
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
                            Ffmpeg_copy_content.packet = `Audio packet;
                            time_base = encoder_time_base;
                            stream_idx = source_idx.idx;
                          } ))
                      packets
                  in
                  let data = { Content.Video.params; data; length } in
                  let data = Ffmpeg_copy_content.lift_data data in
                  Generator.put generator field data
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
            Ffmpeg_utils.Duration.init ~mode:`PTS ~src:target_time_base
              ~convert_ts:false ~get_ts:Avutil.Frame.pts
              ~set_ts:Avutil.Frame.set_pts ()
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
                                stream_idx = source_idx.idx;
                                frame;
                              } ))
                          frames
                      in
                      let data = { Content.Video.params; data; length } in
                      let data = Ffmpeg_raw_content.Audio.lift_data data in
                      Generator.put generator field data
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

let encode_video_frame ~source_idx ~type_t ~mode ~opts ?codec ~format ~field
    generator =
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
          let hwaccel_pixel_format =
            Option.map Avutil.Pixel_format.of_string
              format.Ffmpeg_format.hwaccel_pixel_format
          in

          let hardware_context, stream_pixel_format =
            Ffmpeg_utils.mk_hardware_context ~hwaccel ~hwaccel_pixel_format
              ~hwaccel_device ~opts ~target_pixel_format ~target_width
              ~target_height codec
          in

          let encoder =
            Avcodec.Video.create_encoder ?hardware_context ~opts
              ~frame_rate:target_frame_rate ~pixel_format:stream_pixel_format
              ~width:target_width ~height:target_height ~time_base codec
          in

          let params =
            Some
              (`Video
                {
                  Ffmpeg_copy_content.avg_frame_rate = Some target_frame_rate;
                  params = Avcodec.params encoder;
                })
          in
          let effective_t =
            Type.make
              (Format_type.descr
                 (`Format (Ffmpeg_copy_content.lift_params params)))
          in
          Typing.(effective_t <: type_t);

          let encoder_time_base = Avcodec.time_base encoder in

          let duration_converter =
            Ffmpeg_utils.Duration.init ~mode:`DTS ~src:encoder_time_base
              ~convert_ts:false ~get_ts:Avcodec.Packet.get_dts
              ~set_ts:Avcodec.Packet.set_dts ()
          in

          let write_packet packet =
            match Ffmpeg_utils.Duration.push duration_converter packet with
              | Some (length, packets) ->
                  let data =
                    List.map
                      (fun (pos, packet) ->
                        ( pos,
                          {
                            Ffmpeg_copy_content.packet = `Video packet;
                            time_base = encoder_time_base;
                            stream_idx = source_idx.idx;
                          } ))
                      packets
                  in
                  let data = { Content.Video.params; data; length } in
                  let data = Ffmpeg_copy_content.lift_data data in
                  Generator.put generator field data
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
            Ffmpeg_utils.Duration.init ~mode:`PTS ~src:time_base
              ~convert_ts:false ~get_ts:Avutil.Frame.pts
              ~set_ts:Avutil.Frame.set_pts ()
          in

          function
          | `Frame frame -> (
              match Ffmpeg_utils.Duration.push duration_converter frame with
                | Some (length, frames) ->
                    let data =
                      List.map
                        (fun (pos, frame) ->
                          ( pos,
                            {
                              Ffmpeg_raw_content.time_base;
                              stream_idx = source_idx.idx;
                              frame;
                            } ))
                        frames
                    in
                    let data = { Content.Video.params; data; length } in
                    let data = Ffmpeg_raw_content.Video.lift_data data in
                    Generator.put generator field data
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
      let vbuf = VFrame.data frame in
      List.iter
        (fun (_, img) ->
          let f = Video.Canvas.Image.render img in
          let vdata = Ffmpeg_utils.pack_image f in
          let frame = InternalScaler.convert (Option.get !scaler) vdata in
          Avutil.Frame.set_pts frame (Some !nb_frames);
          nb_frames := Int64.succ !nb_frames;
          encode_ffmpeg_frame frame)
        vbuf.Content.Video.data
  | `Flush -> encode_frame `Flush

let mk_encoder mode =
  let format_field, input_frame_t =
    match mode with
      | `Audio_encoded | `Audio_raw -> (Frame.Fields.audio, Format_type.audio ())
      | `Video_encoded | `Video_raw -> (Frame.Fields.video, Format_type.video ())
  in
  let output_frame_t =
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
  let format_frame_t =
    match mode with
      | `Audio_encoded | `Video_encoded -> input_frame_t
      | `Audio_raw | `Video_raw -> output_frame_t
  in
  let proto =
    [
      ( "",
        Lang.format_t
          (Lang.frame_t (Lang.univ_t ())
             (Frame.Fields.add format_field format_frame_t Frame.Fields.empty)),
        None,
        Some "Encoding format." );
      ("", input_frame_t, None, None);
    ]
  in
  let base, name =
    match mode with
      | `Audio_encoded -> (ffmpeg_encode, "audio")
      | `Audio_raw -> (ffmpeg_raw_encode, "audio")
      | `Video_encoded -> (ffmpeg_encode, "video")
      | `Video_raw -> (ffmpeg_raw_encode, "video")
  in
  ignore
    (Lang.add_track_operator name proto ~base ~return_t:output_frame_t
       ~category:`Conversion ~descr:"Convert a track's content" (fun p ->
         let id =
           Lang.to_default_option ~default:name Lang.to_string
             (List.assoc "id" p)
         in
         let format_val = Lang.assoc "" 1 p in
         let format =
           match Lang.to_format format_val with
             | Encoder.Ffmpeg ffmpeg -> ffmpeg
             | _ -> assert false
         in
         let field, source = Lang.to_track (Lang.assoc "" 2 p) in
         let content_type () = source#content_type in

         if Hashtbl.length format.Ffmpeg_format.opts > 0 then
           raise
             (Error.Invalid_value
                ( format_val,
                  Printf.sprintf
                    "Muxer options are not supported for inline encoders: %s"
                    (Ffmpeg_format.string_of_options format.Ffmpeg_format.opts)
                ));

         if format.Ffmpeg_format.format <> None then
           raise
             (Error.Invalid_value
                (format_val, "Format option is not supported inline encoders"));

         let mk_encode_frame generator =
           let output_frame_t = Type.fresh output_frame_t in

           let stream = List.assoc format_field format.Ffmpeg_format.streams in

           let opts =
             match stream with
               | `Encode { Ffmpeg_format.opts } -> opts
               | _ -> assert false
           in

           let original_opts = Hashtbl.create 10 in

           let source_idx =
             SourceIdx.merge source_idx_map
               { source; idx = Ffmpeg_content_base.new_stream_idx () }
           in

           let encode_frame =
             match stream with
               | `Encode { mode = `Raw; options = `Audio format } ->
                   encode_audio_frame ~source_idx ~type_t:output_frame_t
                     ~mode:`Raw ~opts ~format ~content_type ~field generator
               | `Encode
                   {
                     mode = `Internal;
                     codec = Some codec;
                     options = `Audio format;
                   } ->
                   let codec = Avcodec.Audio.find_encoder_by_name codec in
                   encode_audio_frame ~source_idx ~type_t:output_frame_t
                     ~mode:`Encoded ~opts ~codec ~format ~content_type ~field
                     generator
               | `Encode { mode = `Raw; options = `Video format } ->
                   encode_video_frame ~source_idx ~type_t:output_frame_t
                     ~mode:`Raw ~opts ~format ~field generator
               | `Encode
                   {
                     mode = `Internal;
                     codec = Some codec;
                     options = `Video format;
                   } ->
                   let codec = Avcodec.Video.find_encoder_by_name codec in
                   encode_video_frame ~source_idx ~type_t:output_frame_t
                     ~mode:`Encoded ~opts ~codec ~format ~field generator
               | _ -> assert false
           in
           let size = Lazy.force Frame.size in

           let encode_frame = function
             | `Frame frame ->
                 List.iter
                   (fun (pos, m) -> Generator.add_metadata ~pos generator m)
                   (Frame.get_all_metadata frame);
                 List.iter
                   (fun pos -> Generator.add_track_mark ~pos generator)
                   (List.filter (fun x -> x < size) (Frame.track_marks frame));
                 encode_frame (`Frame frame)
             | `Flush -> encode_frame `Flush
           in

           let left_over_opts = Hashtbl.create 10 in

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

         let encode_frame_ref = ref None in

         let get_encode_frame generator =
           match !encode_frame_ref with
             | None ->
                 let fn = mk_encode_frame generator in
                 encode_frame_ref := Some fn;
                 fn
             | Some fn -> fn
         in

         let encode_frame generator frame =
           let encode_frame = get_encode_frame generator in
           match frame with
             | `Frame frame -> encode_frame (`Frame frame)
             | `Flush ->
                 encode_frame `Flush;
                 encode_frame_ref := None
         in

         let consumer =
           new Producer_consumer.consumer
             ~write_frame:encode_frame ~name:(id ^ ".consumer")
             ~source:(Lang.source source) ()
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
  List.iter mk_encoder [`Audio_encoded; `Audio_raw; `Video_encoded; `Video_raw]
