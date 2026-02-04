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
        "\"error\", \"warning\", \"info\", \"verbose\", \"debug\" or \"trace\".";
      ]

let conf_level = Dtools.Conf.int ~p:(conf_log#plug "level") "Level" ~d:3

let conf_capture =
  Dtools.Conf.bool ~p:(conf_log#plug "capture")
    "Process logs through the main liquidsoap log facilities." ~d:false

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
  Lifecycle.on_start ~name:"ffmpeg utils initialization" (fun () ->
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
          | "trace" -> `Trace
          | _ ->
              log#severe "Invalid value for \"ffmpeg.log.verbosity\"!";
              `Quiet
      in
      (* let level = conf_level#get in *)
      Avutil.Log.set_level verbosity;
      if conf_capture#get then
        Avutil.Log.set_callback (fun s ->
            log#f conf_level#get "%s" (String.trim s)))

let liq_main_ticks_time_base () =
  { Avutil.num = 1; den = Lazy.force Frame.main_rate }

let liq_audio_sample_time_base () =
  { Avutil.num = 1; den = Lazy.force Frame.audio_rate }

let liq_video_sample_time_base () =
  { Avutil.num = 1; den = Lazy.force Frame.video_rate }

let liq_frame_time_base () =
  { Avutil.num = Lazy.force Frame.size; den = Lazy.force Frame.main_rate }

let liq_frame_pixel_format = `Yuv420p
let liq_frame_pixel_format_with_alpha = `Yuva420p

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

let mk_hardware_context ~hwaccel ~hwaccel_pixel_format ~hwaccel_device ~opts
    ~target_pixel_format ~target_width ~target_height codec =
  let codec_name = Avcodec.name codec in
  let no_hardware_context = (None, target_pixel_format) in
  let string_of_pixel_format p =
    match (p, Avutil.Pixel_format.to_string p) with
      | _, Some s -> s
      | `None, _ -> "all pixel formats supported by the codec"
      | _, None -> "N/A"
  in
  try
    if hwaccel = `None then raise (Found no_hardware_context);
    let suitable_pixel_format = function
      | _, None | `None, _ -> true
      | p, Some p' -> p = p'
    in
    let suitable_method = function `Auto, _ -> true | m, m' -> m = m' in
    let default_pixel_format = function
      | `None -> target_pixel_format
      | p -> p
    in
    List.iter
      (function
        (* Setting a hwaccel_device or pixel_format explicitly disables this method. *)
        | { Avcodec.methods; pixel_format; _ }
          when List.mem `Internal methods
               && suitable_method (hwaccel, `Internal)
               && suitable_pixel_format (pixel_format, hwaccel_pixel_format)
               && hwaccel_device = None ->
            log#important
              "Codec %s has internal hardware capabilities that should work \
               without specific settings."
              codec_name;
            raise
              (Found
                 ( None,
                   Option.value
                     ~default:(default_pixel_format pixel_format)
                     hwaccel_pixel_format ))
        | { Avcodec.methods; _ }
          when List.mem `Internal methods
               && not (suitable_method (hwaccel, `Internal)) ->
            log#important
              "Codec %s has internal hardware capabilities but hwaccel %S is \
               selected"
              codec_name
              (Ffmpeg_format.string_of_hwaccel hwaccel)
        | { Avcodec.methods; _ }
          when List.mem `Internal methods && hwaccel_device <> None ->
            log#important
              "Codec %s has internal hardware capabilities that should work \
               without specific settings but hwaccel_device %S\n\
              \             is selected."
              codec_name
              (Option.get hwaccel_device)
        | { Avcodec.methods; _ }
          when List.mem `Internal methods && hwaccel_pixel_format <> None ->
            log#important
              "Codec %s has internal hardware capabilities that should work \
               without specific settings for but hwaccel_pixel_format %s is \
               selected."
              codec_name
              (string_of_pixel_format (Option.get hwaccel_pixel_format))
        | { Avcodec.methods; device_type; pixel_format; _ }
          when List.mem `Hw_device_ctx methods
               && suitable_method (hwaccel, `Device)
               && suitable_pixel_format (pixel_format, hwaccel_pixel_format) ->
            log#important
              "Codec %s has device context hardware capabilities. Enabling it.."
              codec_name;
            let device_context =
              Avutil.HwContext.create_device_context ?device:hwaccel_device
                ~opts device_type
            in
            raise
              (Found
                 ( Some (`Device_context device_context),
                   Option.value
                     ~default:(default_pixel_format pixel_format)
                     hwaccel_pixel_format ))
        | { Avcodec.methods; _ }
          when List.mem `Hw_device_ctx methods
               && not (suitable_method (hwaccel, `Device)) ->
            log#important
              "Codec %s has device context hardware capabilities but hwaccel \
               %S is selected"
              codec_name
              (Ffmpeg_format.string_of_hwaccel hwaccel)
        | { Avcodec.methods; _ }
          when List.mem `Hw_device_ctx methods && hwaccel_pixel_format <> None
          ->
            log#important
              "Codec %s has device context hardware capabilities that should \
               work without specific pixel format settings but \
               hwaccel_pixel_format %s is selected."
              codec_name
              (string_of_pixel_format (Option.get hwaccel_pixel_format))
        | { Avcodec.methods; device_type; pixel_format; _ }
          when List.mem `Hw_frames_ctx methods
               && suitable_method (hwaccel, `Frame)
               && suitable_pixel_format (pixel_format, hwaccel_pixel_format) ->
            log#important
              "Codec %s has frame context hardware capabilities for \
               hwaccel_pixel_format %s. Enabling it.."
              codec_name
              Avutil.Pixel_format.((descriptor pixel_format).name);
            let device_context =
              Avutil.HwContext.create_device_context ?device:hwaccel_device
                ~opts device_type
            in
            let frame_context =
              Avutil.HwContext.create_frame_context ~width:target_width
                ~height:target_height ~src_pixel_format:target_pixel_format
                ~dst_pixel_format:pixel_format device_context
            in
            let pixel_format =
              match pixel_format with
                | `None -> Option.get hwaccel_pixel_format
                | p -> Option.value ~default:p hwaccel_pixel_format
            in
            raise (Found (Some (`Frame_context frame_context), pixel_format))
        | { Avcodec.methods; pixel_format; _ }
          when List.mem `Hw_frames_ctx methods && hwaccel_pixel_format <> None
          ->
            log#important
              "Codec %s has frame context hardware capabilities for \
               hwaccel_pixel_format %s but hwaccel_pixel_format %s is \
               selected."
              codec_name
              (string_of_pixel_format pixel_format)
              (string_of_pixel_format (Option.get hwaccel_pixel_format))
        | { Avcodec.methods; _ } when List.mem `Ad_hoc methods ->
            log#important
              "Codec %s has unsupported ad-hoc hardware acceleration \
               capabilities."
              codec_name
        | _ -> ())
      (Avcodec.hw_configs codec);
    if hwaccel <> `Auto && hwaccel <> `Auto then
      Lang_encoder.raise_error ~pos:None
        (Printf.sprintf
           "No suitable hardware acceleration method %S found for codec %s!"
           (Ffmpeg_format.string_of_hwaccel hwaccel)
           codec_name);
    no_hardware_context
  with Found v -> v

module Duration = struct
  type 'a t = {
    mode : [ `PTS | `DTS ];
    convert_ts : bool;
    get_ts : 'a -> int64 option;
    set_ts : 'a -> int64 option -> unit;
    get_duration : 'a -> int64 option;
    src : Avutil.rational;
    dst : Avutil.rational;
    offset : int64;
    mutable last_ts : int64 option;
    mutable packets : (int * 'a) list;
  }

  let init ?(offset = 0L) ?last_ts ~mode ~src ~convert_ts ~get_ts ~set_ts
      ~get_duration () =
    {
      mode;
      convert_ts;
      get_ts;
      set_ts;
      get_duration;
      src;
      dst = liq_main_ticks_time_base ();
      offset;
      last_ts;
      packets = [];
    }

  let last_ts { last_ts } = last_ts

  let push t packet =
    let { mode; offset; convert_ts; set_ts; get_ts; last_ts; packets; src; dst }
        =
      t
    in
    let duration =
      match (last_ts, get_ts packet) with
        | None, Some ts ->
            let ts = Int64.add ts offset in
            set_ts packet (Some ts);
            t.last_ts <- Some ts;
            0
        | Some old_ts, Some ts ->
            let ts = Int64.add ts offset in
            if ts < old_ts then (
              log#important
                "Invalid ffmpeg content: non-monotonic %s: %Ld < %Ld"
                (match mode with `DTS -> "DTS" | `PTS -> "PTS")
                ts old_ts;
              raise Content.Invalid);
            set_ts packet
              (Some (if convert_ts then convert_time_base ~src ~dst ts else ts));
            t.last_ts <- Some ts;
            let d = Int64.sub ts old_ts in
            Int64.to_int (convert_time_base ~src ~dst d)
        | _ -> 0
    in
    if duration > 0 then (
      t.packets <- [(0, packet)];
      Some (duration, packets))
    else (
      t.packets <- packets @ [(0, packet)];
      None)

  let flush { packets; get_duration; src; dst } =
    let max_duration =
      List.fold_left
        (fun acc (_, packet) ->
          match get_duration packet with
            | Some d ->
                let d = Int64.to_int (convert_time_base ~src ~dst d) in
                max acc d
            | None -> acc)
        1 packets
    in
    (max_duration, packets)
end

let find_pixel_format codec =
  let formats = Avcodec.Video.get_supported_pixel_formats codec in
  if List.mem liq_frame_pixel_format formats then liq_frame_pixel_format
  else (
    match
      List.filter
        (fun f ->
          not (List.mem `Hwaccel Avutil.Pixel_format.((descriptor f).flags)))
        formats
    with
      | p :: _ -> p
      (* Hardware accelerated codecs list hardware-specific pixel_formats
         as supported and then accept regular format. So, last resort here,
         we use the internal pixel_format. *)
      | [] -> liq_frame_pixel_format)

let pixel_format codec = function
  | Some p -> Avutil.Pixel_format.of_string p
  | None -> find_pixel_format codec
