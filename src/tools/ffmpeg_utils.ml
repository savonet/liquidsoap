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

let log = Log.make ["ffmpeg"]

let () =
  Printexc.register_printer (function
    | Avutil.Error `Encoder_not_found ->
        Some
          "The requested ffmpeg encoder was not found, please make sure that \
           ffmpeg was compiled with support for it"
    | _ -> None)

let conf_ffmpeg =
  Dtools.Conf.void ~p:(Configure.conf#plug "ffmpeg") "FFMPEG configuration"

let conf_log = Dtools.Conf.void ~p:(conf_ffmpeg#plug "log") "Log configuration"

let conf_verbosity =
  Dtools.Conf.string
    ~p:(conf_log#plug "verbosity")
    "Verbosity" ~d:"warning"
    ~comments:
      [
        "Set FFMPEG log level, one of: \"quiet\", \"panic\", \"fatal\"";
        "\"error\", \"warning\", \"info\", \"verbose\" or \"debug\"";
      ]

let conf_level = Dtools.Conf.int ~p:(conf_log#plug "level") "Level" ~d:3

let conf_scaling_algorithm =
  Dtools.Conf.string
    ~p:(conf_ffmpeg#plug "scaling_algorithm")
    "Scaling algorithm" ~d:"bicubic"
    ~comments:
      [
        "Set FFMPEG scaling algorithm. One of: \"fast_bilinear\",";
        "\"bilinear\" or \"bicubic\".";
      ]

let () =
  Lifecycle.before_start (fun () ->
      let verbosity =
        match conf_verbosity#get with
          | "quiet" -> `Quiet
          | "panic" -> `Panic
          | "fatal" -> `Fatal
          | "error" -> `Error
          | "warning" -> `Warning
          | "info" -> `Info
          | "verbose" -> `Verbose
          | "debug" -> `Debug
          | _ ->
              log#severe "Invalid value for \"ffmpeg.log.verbosity\"!";
              `Quiet
      in
      let level = conf_level#get in
      Avutil.Log.set_level verbosity;
      Avutil.Log.set_callback (fun s -> log#f level "%s" (String.trim s)))

module Fps = struct
  type filter = {
    time_base : Avutil.rational;
    input : [ `Video ] Avfilter.input;
    output : [ `Video ] Avfilter.output;
  }

  type t = [ `Filter of filter | `Pass_through of Avutil.rational ]

  let time_base = function
    | `Filter { time_base } -> time_base
    | `Pass_through time_base -> time_base

  let init ~width ~height ~pixel_format ~time_base ~pixel_aspect ?source_fps
      ~target_fps () =
    let config = Avfilter.init () in
    let _buffer =
      let args =
        [
          `Pair ("video_size", `String (Printf.sprintf "%dx%d" width height));
          `Pair ("pix_fmt", `Int (Avutil.Pixel_format.get_id pixel_format));
          `Pair ("time_base", `Rational time_base);
          `Pair ("pixel_aspect", `Rational pixel_aspect);
        ]
      in
      let args =
        match source_fps with
          | None -> args
          | Some fps ->
              `Pair ("frame_rate", `Rational { Avutil.num = fps; den = 1 })
              :: args
      in
      Avfilter.attach ~name:"buffer" ~args Avfilter.buffer config
    in
    let fps =
      match
        List.find_opt (fun { Avfilter.name } -> name = "fps") Avfilter.filters
      with
        | Some fps -> fps
        | None -> failwith "Could not find fps ffmpeg filter!"
    in
    let fps =
      let args =
        [`Pair ("fps", `Rational { Avutil.num = target_fps; den = 1 })]
      in
      Avfilter.attach ~name:"fps" ~args fps config
    in
    let _buffersink =
      Avfilter.attach ~name:"buffersink" Avfilter.buffersink config
    in
    Avfilter.link
      (List.hd Avfilter.(_buffer.io.outputs.video))
      (List.hd Avfilter.(fps.io.inputs.video));
    Avfilter.link
      (List.hd Avfilter.(fps.io.outputs.video))
      (List.hd Avfilter.(_buffersink.io.inputs.video));
    let graph = Avfilter.launch config in
    let _, input = List.hd Avfilter.(graph.inputs.video) in
    let _, output = List.hd Avfilter.(graph.outputs.video) in
    let time_base = Avfilter.(time_base output.context) in
    { input; output; time_base }

  (* Source fps is not always known so it is optional here. *)
  let init ~width ~height ~pixel_format ~time_base ~pixel_aspect ?source_fps
      ~target_fps () =
    match source_fps with
      | Some f when f = target_fps -> `Pass_through time_base
      | _ ->
          `Filter
            (init ~width ~height ~pixel_format ~time_base ~pixel_aspect
               ?source_fps ~target_fps ())

  let convert converter frame cb =
    match converter with
      | `Pass_through _ -> cb frame
      | `Filter { input; output } ->
          input frame;
          let rec flush () =
            try
              cb (output.Avfilter.handler ());
              flush ()
            with Avutil.Error `Eagain -> ()
          in
          flush ()
end

let liq_master_ticks_time_base () =
  { Avutil.num = 1; den = Lazy.force Frame.master_rate }

let liq_audio_sample_time_base () =
  { Avutil.num = 1; den = Lazy.force Frame.audio_rate }

let liq_video_sample_time_base () =
  { Avutil.num = 1; den = Lazy.force Frame.video_rate }

let liq_frame_time_base () =
  { Avutil.num = Lazy.force Frame.size; den = Lazy.force Frame.master_rate }

let convert_time_base ~src ~dst pts =
  let num = src.Avutil.num * dst.Avutil.den in
  let den = src.Avutil.den * dst.Avutil.num in
  Int64.div (Int64.mul pts (Int64.of_int num)) (Int64.of_int den)

exception
  Found of (Avcodec.Video.hardware_context option * Avutil.Pixel_format.t)

let mk_hardware_context ~hwaccel ~hwaccel_device ~opts ~target_pixel_format
    ~target_width ~target_height codec =
  let codec_name = Avcodec.name codec in
  let no_hardware_context = (None, target_pixel_format) in
  try
    if hwaccel = `None then raise (Found no_hardware_context);
    let hw_configs = Avcodec.hw_configs codec in
    let find hw_method cb =
      ignore
        (Option.map cb
           (List.find_opt
              (fun { Avcodec.methods; _ } -> List.mem hw_method methods)
              hw_configs))
    in
    find `Internal (fun _ ->
        (* Setting a hwaccel_device explicitely disables this method. *)
        if hwaccel_device = None && hwaccel <> `None then (
          log#info
            "Codec %s has internal hardware capabilities that should work \
             without specific settings."
            codec_name;
          raise (Found (None, target_pixel_format)) ));
    find `Hw_device_ctx (fun { Avcodec.device_type; _ } ->
        log#info
          "Codec %s has device context-based hardware capabilities. Enabling \
           it.."
          codec_name;
        let device_context =
          Avutil.HwContext.create_device_context ?device:hwaccel_device ~opts
            device_type
        in
        raise
          (Found (Some (`Device_context device_context), target_pixel_format)));
    find `Hw_frames_ctx (fun { Avcodec.device_type; pixel_format; _ } ->
        log#info
          "Codec %s has frame context-based hardware cabilities. Enabling it.."
          codec_name;
        let device_context =
          Avutil.HwContext.create_device_context ?device:hwaccel_device ~opts
            device_type
        in
        let frame_context =
          Avutil.HwContext.create_frame_context ~width:target_width
            ~height:target_height ~src_pixel_format:target_pixel_format
            ~dst_pixel_format:pixel_format device_context
        in
        raise (Found (Some (`Frame_context frame_context), pixel_format)));
    no_hardware_context
  with Found v -> v

let find_pixel_format codec =
  match
    List.filter
      (fun f ->
        not (List.mem `Hwaccel Avutil.Pixel_format.((descriptor f).flags)))
      (Avcodec.Video.get_supported_pixel_formats codec)
  with
    | p :: _ -> p
    | [] ->
        failwith
          (Printf.sprintf "No suitable pixel format for codec %s!"
             (Avcodec.name codec))

let pixel_format codec = function
  | Some p -> Avutil.Pixel_format.of_string p
  | None -> find_pixel_format codec
