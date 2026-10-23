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

let method_name : Liquidsoap_ffmpeg_formats.Encoder_format.hwaccel -> string =
  function
  | `Auto -> "auto"
  | `None -> "none"
  | `Internal -> "internal"
  | `Device -> "device"
  | `Frame -> "frame"

let pixel_format_name = function
  | `None -> "none"
  | p -> Option.value ~default:"none" (Avutil.Pixel_format.to_string p)

let candidates ~hwaccel configs =
  List.concat_map
    (fun (config : Avcodec.hw_config) ->
      List.filter_map
        (fun (m, flag) ->
          if
            List.mem flag config.methods
            && (hwaccel = `Auto
               || hwaccel
                  = (m :> Liquidsoap_ffmpeg_formats.Encoder_format.hwaccel))
          then Some (config, m)
          else None)
        [
          (`Internal, `Internal);
          (`Device, `Hw_device_ctx);
          (`Frame, `Hw_frames_ctx);
        ])
    configs

let named = function `None -> None | p -> Some p

let select ~(log : Log.t) ~hwaccel ?hardware_pixel_format ?device ~opts ~target
    ~width ~height codec =
  let codec_name = Avcodec.name codec in
  match hwaccel with
    | `None -> (None, target)
    | _ -> (
        let configs = Avcodec.hw_configs codec in
        List.iter
          (fun (c : Avcodec.hw_config) ->
            if c.methods = [`Ad_hoc] then
              log#info "Codec %s: configuration %s is ad hoc only" codec_name
                (pixel_format_name c.pixel_format))
          configs;
        let stream_format p =
          match (hardware_pixel_format, named p) with
            | Some hpf, _ -> hpf
            | None, Some p -> p
            | None, None -> target
        in
        let fits ((c : Avcodec.hw_config), m) =
          let format_fits =
            named c.pixel_format = None
            || hardware_pixel_format = None
            || Some c.pixel_format = hardware_pixel_format
          in
          let reason =
            if not format_fits then Some "pixel format differs"
            else (
              match m with
                | `Internal when device <> None -> Some "a device is given"
                | `Frame
                  when hardware_pixel_format = None
                       && named c.pixel_format = None ->
                    Some "no named hardware pixel format"
                | _ -> None)
          in
          match reason with
            | None -> true
            | Some reason ->
                log#info "Codec %s: %s candidate %s does not fit: %s" codec_name
                  (method_name
                     (m :> Liquidsoap_ffmpeg_formats.Encoder_format.hwaccel))
                  (pixel_format_name c.pixel_format)
                  reason;
                false
        in
        let last_failure = ref None in
        let try_candidate ((c : Avcodec.hw_config), m) =
          try
            match m with
              | `Internal -> Some (None, stream_format c.pixel_format)
              | `Device ->
                  let context =
                    Avutil.HwContext.create_device_context ?device ~opts
                      c.device_type
                  in
                  Some
                    ( Some (`Device_context context),
                      stream_format c.pixel_format )
              | `Frame ->
                  let hw = stream_format c.pixel_format in
                  let context =
                    Avutil.HwContext.create_device_context ?device ~opts
                      c.device_type
                  in
                  let frames =
                    Avutil.HwContext.create_frame_context ~width ~height
                      ~src_pixel_format:target ~dst_pixel_format:hw context
                  in
                  Some (Some (`Frame_context frames), hw)
          with Avutil.Error e ->
            let message = Avutil.string_of_error e in
            log#important "Codec %s: %s hardware context failed: %s" codec_name
              (method_name
                 (m :> Liquidsoap_ffmpeg_formats.Encoder_format.hwaccel))
              message;
            last_failure := Some message;
            None
        in
        match
          List.find_map try_candidate
            (List.filter fits (candidates ~hwaccel configs))
        with
          | Some result -> result
          | None when hwaccel = `Auto ->
              if configs <> [] then
                log#important "Codec %s encodes in software" codec_name;
              (None, target)
          | None ->
              failwith
                (Printf.sprintf "hwaccel=%s cannot be met by codec %s%s"
                   (method_name hwaccel) codec_name
                   (match !last_failure with Some m -> ": " ^ m | None -> "")))
