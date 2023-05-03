(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

let log = Log.make ["ffmpeg"]
let () = Avdevice.init ()

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
        "Set FFMPEG log level, one of: \"quiet\", \"panic\", \"fatal\", ";
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
      (* let level = conf_level#get in *)
      Avutil.Log.set_level verbosity)

(*
      Avutil.Log.set_callback (fun s -> log#f level "%s" (String.trim s)))
*)

let best_pts frame =
  match Avutil.Frame.pts frame with
    | Some pts -> Some pts
    | None -> Avutil.Frame.best_effort_timestamp frame

let liq_main_ticks_time_base () =
  { Avutil.num = 1; den = SyncLazy.force Frame.main_rate }

let liq_audio_sample_time_base () =
  { Avutil.num = 1; den = SyncLazy.force Frame.audio_rate }

let liq_video_sample_time_base () =
  { Avutil.num = 1; den = SyncLazy.force Frame.video_rate }

let liq_frame_time_base () =
  {
    Avutil.num = SyncLazy.force Frame.size;
    den = SyncLazy.force Frame.main_rate;
  }

let liq_frame_pixel_format () = `Yuv420p

let pack_image f =
  let y, u, v = Image.YUV420.data f in
  let sy = Image.YUV420.y_stride f in
  let s = Image.YUV420.uv_stride f in
  [| (y, sy); (u, s); (v, s) |]

let unpack_image ~width ~height = function
  | [| (y, sy); (u, su); (v, sv) |] ->
      assert (su = sv);
      Image.YUV420.make width height y sy u v su
  | [| (y, sy); (u, su); (v, sv); (alpha, sa) |] ->
      assert (su = sv);
      assert (sa = sy);
      Image.YUV420.make width height ~alpha y sy u v su
  | _ -> assert false

let convert_time_base ~src ~dst pts =
  let num = src.Avutil.num * dst.Avutil.den in
  let den = src.Avutil.den * dst.Avutil.num in
  if den = 0 then Int64.max_int
  else Int64.div (Int64.mul pts (Int64.of_int num)) (Int64.of_int den)

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
        (* Setting a hwaccel_device explicitly disables this method. *)
        if hwaccel_device = None && hwaccel <> `None then (
          log#info
            "Codec %s has internal hardware capabilities that should work \
             without specific settings."
            codec_name;
          raise (Found (None, target_pixel_format))));
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

module Duration = struct
  type 'a t = {
    get_ts : 'a -> Int64.t option;
    src : Avutil.rational;
    dst : Avutil.rational;
    mutable last_packet : 'a option;
    mutable packets : (int * 'a) list;
  }

  let init ~src ~get_ts =
    {
      get_ts;
      src;
      dst = liq_main_ticks_time_base ();
      last_packet = None;
      packets = [];
    }

  let push t packet =
    let { get_ts; last_packet; packets; src; dst } = t in
    t.last_packet <- Some packet;
    let last_ts =
      Option.join (Option.map (fun packet -> get_ts packet) last_packet)
    in
    let duration =
      match (last_ts, get_ts packet) with
        | None, Some _ -> 0
        | Some old_pts, Some pts ->
            let d = Int64.sub pts old_pts in
            Int64.to_int (convert_time_base ~src ~dst d)
        | _, None -> 0
    in
    if duration > 0 then (
      t.packets <- [(0, packet)];
      Some (duration, packets))
    else (
      t.packets <- packets @ [(0, packet)];
      None)

  let flush { packets } = packets
end

let find_pixel_format codec pixel_format =
  let formats = Avcodec.Video.get_supported_pixel_formats codec in
  if List.mem pixel_format formats then pixel_format
  else (
    match
      List.filter
        (fun f ->
          not (List.mem `Hwaccel Avutil.Pixel_format.((descriptor f).flags)))
        formats
    with
      | p :: _ -> p
      | [] ->
          failwith
            (Printf.sprintf "No suitable pixel format for codec %s!"
               (Avcodec.name codec)))

let pixel_format codec = function
  | Some p -> Avutil.Pixel_format.of_string p
  | None -> find_pixel_format codec (liq_frame_pixel_format ())
