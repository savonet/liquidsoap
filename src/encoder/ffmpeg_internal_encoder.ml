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

(** FFMPEG internal encoder *)

module InternalResampler =
  Swresample.Make (Swresample.FltPlanarBigArray) (Swresample.Frame)

module RawResampler = Swresample.Make (Swresample.Frame) (Swresample.Frame)
module InternalScaler = Swscale.Make (Swscale.BigArray) (Swscale.Frame)
module RawScaler = Swscale.Make (Swscale.Frame) (Swscale.Frame)

let log = Log.make ["ffmpeg"; "decoder"; "internal"]

(* mk_stream is used for the copy encoder, where stream creation has to be
   delayed until the first packet is passed. This is not needed here. *)
let mk_stream _ = ()

let get_channel_layout channels =
  try Avutil.Channel_layout.get_default channels
  with Not_found ->
    failwith
      (Printf.sprintf
         "%%ffmpeg encoder: could not find a default channel configuration for \
          %d channels.."
         channels)

let mk_audio ~ffmpeg ~options output =
  let codec =
    match ffmpeg.Ffmpeg_format.audio_codec with
      | Some (`Raw codec) | Some (`Internal codec) ->
          Avcodec.Audio.find_encoder codec
      | _ -> assert false
  in

  let target_samplerate = Lazy.force ffmpeg.Ffmpeg_format.samplerate in
  let target_liq_audio_sample_time_base =
    { Avutil.num = 1; den = target_samplerate }
  in
  let target_channels = ffmpeg.Ffmpeg_format.channels in
  let target_channel_layout = get_channel_layout target_channels in
  let target_sample_format = Avcodec.Audio.find_best_sample_format codec `Dbl in

  let opts = Hashtbl.create 10 in
  Hashtbl.iter (Hashtbl.add opts) ffmpeg.Ffmpeg_format.audio_opts;
  Hashtbl.iter (Hashtbl.add opts) options;

  let internal_converter () =
    let src_samplerate = Lazy.force Frame.audio_rate in
    let src_channels = ffmpeg.Ffmpeg_format.channels in
    let src_channel_layout = get_channel_layout src_channels in

    let resampler =
      InternalResampler.create ~out_sample_format:target_sample_format
        src_channel_layout src_samplerate target_channel_layout
        target_samplerate
    in
    fun frame start len ->
      let astart = Frame.audio_of_master start in
      let alen = Frame.audio_of_master len in
      let pcm = Audio.sub (AFrame.pcm frame) astart alen in
      [InternalResampler.convert resampler pcm]
  in

  let raw_converter =
    let resampler = ref None in
    let resample frame =
      let src_samplerate = Avutil.Audio.frame_get_sample_rate frame in
      let resampler =
        match !resampler with
          | Some f -> f
          | None ->
              let src_channel_layout =
                Avutil.Audio.frame_get_channel_layout frame
              in
              let src_sample_format =
                Avutil.Audio.frame_get_sample_format frame
              in
              let f =
                if
                  src_samplerate <> target_samplerate
                  || src_channel_layout <> target_channel_layout
                  || src_sample_format <> target_sample_format
                then (
                  let fn =
                    RawResampler.create ~in_sample_format:src_sample_format
                      ~out_sample_format:target_sample_format src_channel_layout
                      src_samplerate target_channel_layout target_samplerate
                  in
                  RawResampler.convert fn )
                else fun f -> f
              in
              resampler := Some f;
              f
      in
      resampler frame
    in
    fun frame start len ->
      let frames =
        Ffmpeg_raw_content.Audio.(get_data Frame.(frame.content.audio))
          .Ffmpeg_content_base.data
      in
      let frames =
        List.filter (fun (pos, _) -> start <= pos && pos < start + len) frames
      in
      List.map (fun (_, { Ffmpeg_raw_content.frame }) -> resample frame) frames
  in

  let converter =
    match ffmpeg.Ffmpeg_format.audio_codec with
      | Some (`Internal _) -> internal_converter ()
      | Some (`Raw _) -> raw_converter
      | _ -> assert false
  in

  let stream =
    Av.new_audio_stream ~sample_rate:target_samplerate
      ~time_base:target_liq_audio_sample_time_base
      ~channel_layout:target_channel_layout ~sample_format:target_sample_format
      ~opts ~codec output
  in

  let codec_attr () = Av.codec_attr stream in

  let bitrate () = Av.bitrate stream in

  let video_size () = None in

  let audio_opts = Hashtbl.copy ffmpeg.Ffmpeg_format.audio_opts in

  Hashtbl.filter_map_inplace
    (fun l v -> if Hashtbl.mem opts l then Some v else None)
    audio_opts;

  if Hashtbl.length audio_opts > 0 then
    failwith
      (Printf.sprintf "Unrecognized options: %s"
         (Ffmpeg_format.string_of_options audio_opts));

  Hashtbl.filter_map_inplace
    (fun l v -> if Hashtbl.mem opts l then Some v else None)
    options;

  let write_frame =
    let variable_frame_size =
      List.mem `Variable_frame_size (Avcodec.Audio.capabilities codec)
    in

    let stream_time_base = Av.get_time_base stream in

    let nb_samples = ref 0L in
    let write_frame frame =
      let frame_pts =
        Ffmpeg_utils.convert_time_base ~src:target_liq_audio_sample_time_base
          ~dst:stream_time_base !nb_samples
      in
      nb_samples :=
        Int64.add !nb_samples
          (Int64.of_int (Avutil.Audio.frame_nb_samples frame));
      Avutil.frame_set_pts frame (Some frame_pts);
      Av.write_frame stream frame
    in

    if variable_frame_size then write_frame
    else (
      let out_frame_size = Av.get_frame_size stream in

      let in_params =
        {
          Avfilter.Utils.sample_rate = target_samplerate;
          channel_layout = target_channel_layout;
          sample_format = target_sample_format;
        }
      in
      let filter_in, filter_out =
        Avfilter.Utils.convert_audio ~in_params
          ~in_time_base:target_liq_audio_sample_time_base ~out_frame_size ()
      in
      let rec flush () =
        try
          write_frame (filter_out ());
          flush ()
        with Avutil.Error `Eagain -> ()
      in
      fun frame ->
        filter_in frame;
        flush () )
  in

  let was_keyframe () = false in

  let encode frame start len =
    List.iter write_frame (converter frame start len)
  in

  {
    Ffmpeg_encoder_common.mk_stream;
    was_keyframe;
    encode;
    codec_attr;
    bitrate;
    video_size;
  }

let mk_video ~ffmpeg ~options output =
  let codec =
    match ffmpeg.Ffmpeg_format.video_codec with
      | Some (`Raw codec) | Some (`Internal codec) ->
          Avcodec.Video.find_encoder codec
      | _ -> assert false
  in
  let pixel_aspect = { Avutil.num = 1; den = 1 } in

  let target_fps = Lazy.force ffmpeg.Ffmpeg_format.framerate in
  let target_video_frame_time_base = { Avutil.num = 1; den = target_fps } in
  let target_width = Lazy.force ffmpeg.Ffmpeg_format.width in
  let target_height = Lazy.force ffmpeg.Ffmpeg_format.height in

  let flag =
    match Ffmpeg_utils.conf_scaling_algorithm#get with
      | "fast_bilinear" -> Swscale.Fast_bilinear
      | "bilinear" -> Swscale.Bilinear
      | "bicubic" -> Swscale.Bicubic
      | _ -> failwith "Invalid value set for ffmpeg scaling algorithm!"
  in

  let opts = Hashtbl.create 10 in
  Hashtbl.iter (Hashtbl.add opts) ffmpeg.Ffmpeg_format.video_opts;
  Hashtbl.iter (Hashtbl.add opts) options;

  let stream =
    Av.new_video_stream ~time_base:target_video_frame_time_base
      ~pixel_format:
        (Avutil.Pixel_format.of_string ffmpeg.Ffmpeg_format.pixel_format)
      ~frame_rate:{ Avutil.num = target_fps; den = 1 }
      ~width:target_width ~height:target_height ~opts ~codec output
  in

  let codec_attr () = Av.codec_attr stream in

  let bitrate () = Av.bitrate stream in

  let video_size () =
    let p = Av.get_codec_params stream in
    Some (Avcodec.Video.get_width p, Avcodec.Video.get_height p)
  in

  let video_opts = Hashtbl.copy ffmpeg.Ffmpeg_format.video_opts in
  Hashtbl.filter_map_inplace
    (fun l v -> if Hashtbl.mem opts l then Some v else None)
    video_opts;

  if Hashtbl.length video_opts > 0 then
    failwith
      (Printf.sprintf "Unrecognized options: %s"
         (Ffmpeg_format.string_of_options video_opts));

  Hashtbl.filter_map_inplace
    (fun l v -> if Hashtbl.mem opts l then Some v else None)
    options;

  let converter = ref None in

  let mk_converter ~pixel_format ~time_base () =
    let c =
      Ffmpeg_utils.Fps.init ~width:target_width ~height:target_height
        ~pixel_format ~time_base ~pixel_aspect ~target_fps ()
    in
    converter := Some (pixel_format, time_base, c);
    c
  in

  let get_converter ~pixel_format ~time_base () =
    match !converter with
      | None -> mk_converter ~pixel_format ~time_base ()
      | Some (p, t, _) when (p, t) <> (pixel_format, time_base) ->
          log#important "Frame format change detected!";
          mk_converter ~pixel_format ~time_base ()
      | Some (_, _, c) -> c
  in

  let stream_time_base = Av.get_time_base stream in

  let was_keyframe = ref false in

  let fps_converter ~time_base frame =
    let converter =
      get_converter ~time_base
        ~pixel_format:(Avutil.Video.frame_get_pixel_format frame)
        ()
    in
    Ffmpeg_utils.Fps.convert converter frame (fun ~time_base frame ->
        let frame_pts =
          Option.map
            (fun pts ->
              Ffmpeg_utils.convert_time_base ~src:time_base
                ~dst:stream_time_base pts)
            (Avutil.frame_pts frame)
        in
        Avutil.frame_set_pts frame frame_pts;
        Av.write_frame stream frame;
        if Av.was_keyframe stream then was_keyframe := true)
  in

  let internal_converter cb =
    let src_width = Lazy.force Frame.video_width in
    let src_height = Lazy.force Frame.video_height in
    let scaler =
      InternalScaler.create [flag] src_width src_height `Yuv420p target_width
        target_height
        (Avutil.Pixel_format.of_string ffmpeg.Ffmpeg_format.pixel_format)
    in
    let nb_frames = ref 0L in
    let time_base = Ffmpeg_utils.liq_video_sample_time_base () in
    fun frame start len ->
      let vstart = Frame.video_of_master start in
      let vstop = Frame.video_of_master (start + len) in
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
        cb ~time_base frame
      done
  in

  let raw_converter cb =
    let scaler = ref None in
    let scale frame =
      let scaler =
        match !scaler with
          | Some f -> f
          | None ->
              let src_width = Avutil.Video.frame_get_width frame in
              let src_height = Avutil.Video.frame_get_height frame in
              let src_pixel_format =
                Avutil.Video.frame_get_pixel_format frame
              in
              let f =
                if src_width <> target_width || src_height <> target_height then (
                  let scaler =
                    RawScaler.create [flag] src_width src_height
                      src_pixel_format target_width target_height
                      src_pixel_format
                  in
                  fun frame ->
                    let scaled = RawScaler.convert scaler frame in
                    Avutil.frame_set_pts scaled (Avutil.frame_pts frame);
                    scaled )
                else fun f -> f
              in
              scaler := Some f;
              f
      in
      scaler frame
    in
    fun frame start len ->
      let stop = start + len in
      let { Ffmpeg_raw_content.VideoSpecs.data } =
        Ffmpeg_raw_content.Video.get_data Frame.(frame.content.video)
      in
      List.iter
        (fun (pos, { Ffmpeg_raw_content.time_base; frame }) ->
          if start <= pos && pos < stop then cb ~time_base (scale frame))
        data
  in

  let converter =
    match ffmpeg.Ffmpeg_format.video_codec with
      | Some (`Internal _) -> internal_converter
      | Some (`Raw _) -> raw_converter
      | _ -> assert false
  in

  let encode =
    was_keyframe := false;
    converter fps_converter
  in

  let was_keyframe () = !was_keyframe in

  {
    Ffmpeg_encoder_common.mk_stream;
    was_keyframe;
    encode;
    codec_attr;
    bitrate;
    video_size;
  }
