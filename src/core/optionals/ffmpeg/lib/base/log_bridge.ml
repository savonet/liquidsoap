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

let log = Log.make ["ffmpeg"]

let mapped_level : Avutil.Log.level -> int = function
  | `Quiet | `Panic | `Fatal -> 1
  | `Error -> 2
  | `Warning -> 3
  | `Info | `Verbose | `Debug | `Trace -> 4

let liquidsoap_level ~level severity = min level (mapped_level severity)

let verbosity_of_string : string -> Avutil.Log.level option = function
  | "quiet" -> Some `Quiet
  | "panic" -> Some `Panic
  | "fatal" -> Some `Fatal
  | "error" -> Some `Error
  | "warning" -> Some `Warning
  | "info" -> Some `Info
  | "verbose" -> Some `Verbose
  | "debug" -> Some `Debug
  | "trace" -> Some `Trace
  | _ -> None

let capture severity message =
  log#f
    (liquidsoap_level ~level:Settings.log_level#get severity)
    "%s" (String.trim message)

let () =
  Avdevice.init ();
  Avutil.Log.set_level `Warning;
  Avutil.Log.set_callback_with_level capture

let () =
  Lifecycle.on_init ~name:"ffmpeg versions" (fun () ->
      let v name { Avutil.major; minor; micro } =
        Printf.sprintf "%s %d.%d.%d" name major minor micro
      in
      log#important "%s"
        (String.concat ", "
           [
             v "libavutil" Avutil.version;
             v "libavformat" Av.avformat_version;
             v "libavcodec" Avcodec.version;
             v "libswresample" Swresample.version;
             v "libswscale" Swscale.version;
           ]));
  Lifecycle.on_start ~name:"ffmpeg log" (fun () ->
      let verbosity = Settings.log_verbosity#get in
      match verbosity_of_string verbosity with
        | None ->
            failwith
              (Printf.sprintf
                 "Invalid settings.ffmpeg.log.verbosity %S: expected one of %s"
                 verbosity
                 (String.concat ", " Settings.verbosities))
        | Some level ->
            Avutil.Log.set_level level;
            if not Settings.log_capture#get then Avutil.Log.clear_callback ());
  Lifecycle.before_final_cleanup ~name:"ffmpeg log" Avutil.Log.clear_callback
