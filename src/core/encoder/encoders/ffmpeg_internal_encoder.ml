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

(** FFMPEG internal encoder *)

module type InternalResampler_type = sig
  type t

  module Content : sig
    type data

    val get_data : Content.data -> data
  end

  val create :
    ?options:Swresample.options list ->
    Avutil.Channel_layout.t ->
    ?in_sample_format:Avutil__Sample_format.t ->
    int ->
    Avutil.Channel_layout.t ->
    ?out_sample_format:Avutil__Sample_format.t ->
    int ->
    t

  val convert :
    ?offset:int -> ?length:int -> t -> Content.data -> Swresample.Frame.t
end

module InternalResampler = struct
  module Content = Content_audio
  include Swresample.Make (Swresample.PlanarFloatArray) (Swresample.Frame)
end

module InternalResampler_pcm_s16 = struct
  module Content = Content_pcm_s16
  include Swresample.Make (Swresample.S16PlanarBigArray) (Swresample.Frame)
end

module InternalResampler_pcm_f32 = struct
  module Content = Content_pcm_f32
  include Swresample.Make (Swresample.FltPlanarBigArray) (Swresample.Frame)
end

module RawResampler = Swresample.Make (Swresample.Frame) (Swresample.Frame)
module InternalScaler = Swscale.Make (Swscale.BigArray) (Swscale.Frame)
module RawScaler = Swscale.Make (Swscale.Frame) (Swscale.Frame)

let log = Log.make ["ffmpeg"; "encoder"; "internal"]

(* mk_stream is used for the copy encoder, where stream creation has to be
   delayed until the first packet is passed. This is not needed here. *)
let mk_stream _ = ()

let get_channel_layout ~pos channels =
  try Avutil.Channel_layout.get_default channels
  with Not_found ->
    Lang_encoder.raise_error ~pos
      (Printf.sprintf
         "%%ffmpeg encoder: could not find a default channel configuration for \
          %d channels.."
         channels)

(* This function optionally splits frames into [frame_size]
   and also adds PTS based on targeted [time_base], [sample_rate]
   and number of channel. *)
let write_audio_frame ~time_base ~sample_rate ~channel_layout ~sample_format
    ~frame_size write_frame =
  let src_time_base = { Avutil.num = 1; den = sample_rate } in
  let convert_pts =
    Ffmpeg_utils.convert_time_base ~src:src_time_base ~dst:time_base
  in

  let add_frame_pts () =
    let nb_samples = ref 0L in
    fun frame ->
      let frame_pts = convert_pts !nb_samples in
      nb_samples :=
        Int64.add !nb_samples
          (Int64.of_int (Avutil.Audio.frame_nb_samples frame));
      Avutil.Frame.set_pts frame (Some frame_pts)
  in

  let add_final_frame_pts = add_frame_pts () in
  let write_frame frame =
    add_final_frame_pts frame;
    write_frame frame
  in

  match frame_size with
    | None -> write_frame
    | Some out_frame_size ->
        let in_params =
          { Avfilter.Utils.sample_rate; channel_layout; sample_format }
        in
        let converter =
          Avfilter.Utils.init_audio_converter ~in_params ~in_time_base:time_base
            ~out_frame_size ()
        in
        let add_filter_frame_pts = add_frame_pts () in
        fun frame ->
          add_filter_frame_pts frame;
          Avfilter.Utils.convert_audio converter write_frame (`Frame frame)

let mk_audio ~pos ~on_keyframe ~mode ~codec ~params ~options ~field output =
  let internal_resampler =
    match params.Ffmpeg_format.pcm_kind with
      | pcm_kind when Content_audio.is_kind pcm_kind ->
          (module InternalResampler : InternalResampler_type)
      | pcm_kind when Content_pcm_s16.is_kind pcm_kind ->
          (module InternalResampler_pcm_s16 : InternalResampler_type)
      | pcm_kind when Content_pcm_f32.is_kind pcm_kind ->
          (module InternalResampler_pcm_f32 : InternalResampler_type)
      | _ -> raise Content_base.Invalid
  in
  let module InternalResampler =
    (val internal_resampler : InternalResampler_type)
  in
  let codec =
    try Avcodec.Audio.find_encoder_by_name codec
    with e ->
      log#severe "Cannot find encoder %s: %s." codec (Printexc.to_string e);
      raise e
  in

  let target_samplerate = Lazy.force params.Ffmpeg_format.samplerate in
  let target_liq_audio_sample_time_base =
    { Avutil.num = 1; den = target_samplerate }
  in
  let target_channels = params.Ffmpeg_format.channels in
  let target_channel_layout = get_channel_layout ~pos target_channels in
  let target_sample_format =
    match params.Ffmpeg_format.sample_format with
      | Some format -> Avutil.Sample_format.find format
      | None -> `Dbl
  in
  let target_sample_format =
    Avcodec.Audio.find_best_sample_format codec target_sample_format
  in

  let internal_converter () =
    let src_samplerate = Lazy.force Frame.audio_rate in
    (* The typing system ensures that this is the number of channels in the frame. *)
    let src_channels = params.Ffmpeg_format.channels in
    let src_channel_layout = get_channel_layout ~pos src_channels in

    let resampler =
      InternalResampler.create ~out_sample_format:target_sample_format
        src_channel_layout src_samplerate target_channel_layout
        target_samplerate
    in
    fun frame ->
      let alen = AFrame.position frame in
      let content = Frame.get frame field in
      let pcm = InternalResampler.Content.get_data content in
      [InternalResampler.convert ~length:alen ~offset:0 resampler pcm]
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
                  || (not
                        (Avutil.Channel_layout.compare src_channel_layout
                           target_channel_layout))
                  || src_sample_format <> target_sample_format
                then (
                  let fn =
                    RawResampler.create ~in_sample_format:src_sample_format
                      ~out_sample_format:target_sample_format src_channel_layout
                      src_samplerate target_channel_layout target_samplerate
                  in
                  RawResampler.convert fn)
                else fun f -> f
              in
              resampler := Some f;
              f
      in
      resampler frame
    in
    fun frame ->
      let frames =
        Ffmpeg_raw_content.Audio.(get_data (Frame.get frame field))
          .Content.Video.data
      in
      let len = Frame.position frame in
      let frames = List.filter (fun (pos, _) -> 0 <= pos && pos < len) frames in
      List.map (fun (_, { Ffmpeg_raw_content.frame }) -> resample frame) frames
  in

  let converter =
    match mode with
      | `Internal -> internal_converter ()
      | `Raw -> raw_converter
      | _ -> assert false
  in

  let opts = Hashtbl.copy options in

  let stream =
    try
      Av.new_audio_stream ~sample_rate:target_samplerate
        ~time_base:target_liq_audio_sample_time_base
        ~channel_layout:target_channel_layout
        ~sample_format:target_sample_format ~opts ~codec output
    with e ->
      log#severe
        "Cannot create audio stream (samplerate: %d, time_base: %s, channel \
         layout: %s, sample format: %s, options: %s): %s."
        target_samplerate
        (Avutil.string_of_rational target_liq_audio_sample_time_base)
        (Avutil.Channel_layout.get_description target_channel_layout)
        (Option.value ~default:""
           (Avutil.Sample_format.get_name target_sample_format))
        (Avutil.string_of_opts opts)
        (Printexc.to_string e);
      raise e
  in

  let options = Hashtbl.copy options in

  Hashtbl.filter_map_inplace
    (fun l v -> if Hashtbl.mem opts l then Some v else None)
    options;

  if Hashtbl.length options > 0 then
    Lang_encoder.raise_error ~pos
      (Printf.sprintf "Unrecognized options: %s"
         (Ffmpeg_format.string_of_options options));

  let intra_only =
    let params = Av.get_codec_params stream in
    match Avcodec.descriptor params with
      | None -> true
      | Some { Avcodec.properties } -> List.mem `Intra_only properties
  in

  let on_keyframe =
    Option.map
      (fun on_keyframe () ->
        if not intra_only then Av.flush output;
        on_keyframe ())
      on_keyframe
  in

  let codec_attr () = Av.codec_attr stream in

  let bitrate () = Av.bitrate stream in

  let video_size () = None in

  let frame_size =
    if List.mem `Variable_frame_size (Avcodec.capabilities codec) then None
    else Some (Av.get_frame_size stream)
  in

  let write_frame =
    try
      write_audio_frame ~time_base:(Av.get_time_base stream)
        ~sample_rate:target_samplerate ~channel_layout:target_channel_layout
        ~sample_format:target_sample_format ~frame_size
        (Av.write_frame ?on_keyframe stream)
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      log#severe "Error writing audio frame: %s." (Printexc.to_string e);
      Printexc.raise_with_backtrace e bt
  in

  let encode frame = List.iter write_frame (converter frame) in

  {
    Ffmpeg_encoder_common.mk_stream;
    encode;
    flush = (fun () -> ());
    codec_attr;
    bitrate;
    video_size;
  }

let mk_video ~pos ~on_keyframe ~mode ~codec ~params ~options ~field output =
  let codec =
    try Avcodec.Video.find_encoder_by_name codec
    with e ->
      log#severe "Cannot find encoder %s: %s." codec (Printexc.to_string e);
      raise e
  in
  let pixel_aspect = { Avutil.num = 1; den = 1 } in

  let target_fps = Lazy.force params.Ffmpeg_format.framerate in
  let target_video_frame_time_base = { Avutil.num = 1; den = target_fps } in
  let target_width = Lazy.force params.Ffmpeg_format.width in
  let target_height = Lazy.force params.Ffmpeg_format.height in
  let target_pixel_format =
    Ffmpeg_utils.pixel_format codec params.Ffmpeg_format.pixel_format
  in

  let flag =
    match Ffmpeg_utils.conf_scaling_algorithm#get with
      | "fast_bilinear" -> Swscale.Fast_bilinear
      | "bilinear" -> Swscale.Bilinear
      | "bicubic" -> Swscale.Bicubic
      | _ ->
          Lang_encoder.raise_error ~pos
            "Invalid value set for ffmpeg scaling algorithm!"
  in

  let opts = Hashtbl.copy options in

  let hwaccel = params.Ffmpeg_format.hwaccel in
  let hwaccel_device = params.Ffmpeg_format.hwaccel_device in
  let hwaccel_pixel_format =
    Option.map Avutil.Pixel_format.of_string
      params.Ffmpeg_format.hwaccel_pixel_format
  in

  let hardware_context, stream_pixel_format =
    Ffmpeg_utils.mk_hardware_context ~hwaccel ~hwaccel_pixel_format
      ~hwaccel_device ~opts ~target_pixel_format ~target_width ~target_height
      codec
  in

  let stream =
    Av.new_video_stream ~time_base:target_video_frame_time_base
      ~pixel_format:stream_pixel_format ?hardware_context
      ~frame_rate:{ Avutil.num = target_fps; den = 1 }
      ~width:target_width ~height:target_height ~opts ~codec output
  in

  let options = Hashtbl.copy options in
  Hashtbl.filter_map_inplace
    (fun l v -> if Hashtbl.mem opts l then Some v else None)
    options;

  if Hashtbl.length options > 0 then
    Lang_encoder.raise_error ~pos
      (Printf.sprintf "Unrecognized options: %s"
         (Ffmpeg_format.string_of_options options));

  let intra_only =
    let params = Av.get_codec_params stream in
    match Avcodec.descriptor params with
      | None -> true
      | Some { Avcodec.properties } -> List.mem `Intra_only properties
  in

  let on_keyframe =
    Option.map
      (fun on_keyframe () ->
        if not intra_only then Av.flush output;
        on_keyframe ())
      on_keyframe
  in

  let codec_attr () = Av.codec_attr stream in

  let bitrate () = Av.bitrate stream in

  let video_size () =
    let p = Av.get_codec_params stream in
    Some (Avcodec.Video.get_width p, Avcodec.Video.get_height p)
  in

  let converter = ref None in

  let start_pts = ref 0L in

  let mk_converter ~pixel_format ~time_base ~stream_idx () =
    let c =
      Ffmpeg_avfilter_utils.Fps.init ~start_pts:!start_pts ~width:target_width
        ~height:target_height ~pixel_format ~time_base ~pixel_aspect ~target_fps
        ()
    in
    converter := Some (pixel_format, time_base, stream_idx, c);
    c
  in

  let get_converter ~pixel_format ~time_base ~stream_idx () =
    match !converter with
      | None -> mk_converter ~stream_idx ~pixel_format ~time_base ()
      | Some (p, t, i, _) when (p, t, i) <> (pixel_format, time_base, stream_idx)
        ->
          mk_converter ~stream_idx ~pixel_format ~time_base ()
      | Some (_, _, _, c) -> c
  in

  let stream_time_base = Av.get_time_base stream in

  let write_frame ~time_base frame =
    let frame_pts =
      Option.map
        (fun pts ->
          Ffmpeg_utils.convert_time_base ~src:time_base ~dst:stream_time_base
            pts)
        (Avutil.Frame.pts frame)
    in
    Avutil.Frame.set_pts frame frame_pts;
    start_pts := Int64.succ !start_pts;
    Av.write_frame ?on_keyframe stream frame
  in

  let fps_converter ~stream_idx ~time_base frame =
    let converter =
      get_converter ~time_base ~stream_idx
        ~pixel_format:(Avutil.Video.frame_get_pixel_format frame)
        ()
    in
    let time_base = Ffmpeg_avfilter_utils.Fps.time_base converter in
    Ffmpeg_avfilter_utils.Fps.convert converter frame (write_frame ~time_base)
  in

  let flush () =
    match !converter with
      | None -> ()
      | Some (_, _, _, converter) ->
          let time_base = Ffmpeg_avfilter_utils.Fps.time_base converter in
          Ffmpeg_avfilter_utils.Fps.eof converter (write_frame ~time_base)
  in

  let internal_converter cb =
    let src_width = Lazy.force Frame.video_width in
    let src_height = Lazy.force Frame.video_height in
    let scaler =
      InternalScaler.create [flag] src_width src_height
        (Ffmpeg_utils.liq_frame_pixel_format ())
        target_width target_height target_pixel_format
    in
    let nb_frames = ref 0L in
    let time_base = Ffmpeg_utils.liq_video_sample_time_base () in
    let stream_idx = 1L in

    fun frame ->
      let content = Frame.get frame field in
      let buf = Content.Video.get_data content in
      List.iter
        (fun (_, img) ->
          let f =
            img
            (* TODO: we could scale instead of aggressively changing the viewport *)
            |> Video.Canvas.Image.viewport src_width src_height
            |> Video.Canvas.Image.render ~transparent:false
          in
          let vdata = Ffmpeg_utils.pack_image f in
          let frame = InternalScaler.convert scaler vdata in
          Avutil.Frame.set_pts frame (Some !nb_frames);
          nb_frames := Int64.succ !nb_frames;
          cb ~stream_idx ~time_base frame)
        buf.Content.Video.data
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
                    Avutil.Frame.set_pts scaled (Avutil.Frame.pts frame);
                    scaled)
                else fun f -> f
              in
              scaler := Some f;
              f
      in
      scaler frame
    in
    fun frame ->
      let len = Frame.position frame in
      let { Ffmpeg_raw_content.VideoSpecs.data } =
        Ffmpeg_raw_content.Video.get_data (Frame.get frame field)
      in
      List.iter
        (fun (pos, { Ffmpeg_raw_content.time_base; frame; stream_idx }) ->
          if 0 <= pos && pos < len then cb ~stream_idx ~time_base (scale frame))
        data
  in

  let converter =
    match mode with `Internal -> internal_converter | `Raw -> raw_converter
  in

  let encode = converter fps_converter in

  {
    Ffmpeg_encoder_common.mk_stream;
    encode;
    flush;
    codec_attr;
    bitrate;
    video_size;
  }
