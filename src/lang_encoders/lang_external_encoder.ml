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

open Value
open Ground

let kind_of_encoder p = Encoder.audio_kind (Lang_encoder.channels_of_params p)

let make params =
  let defaults =
    {
      External_encoder_format.channels
      (* We use a hardcoded value in order not to force the evaluation of the
           number of channels too early, see #933. *) = 2;
      samplerate = Frame.audio_rate;
      video = None;
      header = true;
      restart_on_crash = false;
      restart = External_encoder_format.No_condition;
      process = "";
    }
  in
  let ext =
    List.fold_left
      (fun f -> function
        | "stereo", `Value { value = Ground (Bool b); _ } ->
            { f with External_encoder_format.channels = (if b then 2 else 1) }
        | "mono", `Value { value = Ground (Bool b); _ } ->
            { f with External_encoder_format.channels = (if b then 1 else 2) }
        | "", `Value { value = Ground (String s); _ }
          when String.lowercase_ascii s = "mono" ->
            { f with External_encoder_format.channels = 1 }
        | "", `Value { value = Ground (String s); _ }
          when String.lowercase_ascii s = "stereo" ->
            { f with External_encoder_format.channels = 2 }
        | "channels", `Value { value = Ground (Int c); _ } ->
            { f with External_encoder_format.channels = c }
        | "samplerate", `Value { value = Ground (Int i); _ } ->
            { f with External_encoder_format.samplerate = Lazy.from_val i }
        | "video", `Value { value = Ground (Bool b); _ } ->
            let w, h =
              match f.External_encoder_format.video with
                | None -> (Frame.video_width, Frame.video_height)
                | Some (w, h) -> (w, h)
            in
            {
              f with
              External_encoder_format.video = (if b then Some (w, h) else None);
            }
        | "width", `Value { value = Ground (Int w); _ } ->
            let _, h =
              match f.External_encoder_format.video with
                | None -> (Frame.video_width, Frame.video_height)
                | Some (w, h) -> (w, h)
            in
            let w = Lazy.from_val w in
            { f with External_encoder_format.video = Some (w, h) }
        | "height", `Value { value = Ground (Int h); _ } ->
            let w, _ =
              match f.External_encoder_format.video with
                | None -> (Frame.video_width, Frame.video_height)
                | Some (w, h) -> (w, h)
            in
            let h = Lazy.from_val h in
            { f with External_encoder_format.video = Some (w, h) }
        | "header", `Value { value = Ground (Bool h); _ } ->
            { f with External_encoder_format.header = h }
        | "restart_on_crash", `Value { value = Ground (Bool h); _ } ->
            { f with External_encoder_format.restart_on_crash = h }
        | "", `Value { value = Ground (String s); _ }
          when String.lowercase_ascii s = "restart_on_metadata" ->
            {
              f with
              External_encoder_format.restart = External_encoder_format.Metadata;
            }
        | "restart_after_delay", `Value { value = Ground (Int i); _ } ->
            {
              f with
              External_encoder_format.restart = External_encoder_format.Delay i;
            }
        | "process", `Value { value = Ground (String s); _ } ->
            { f with External_encoder_format.process = s }
        | "", `Value { value = Ground (String s); _ } ->
            { f with External_encoder_format.process = s }
        | t -> Lang_encoder.raise_generic_error t)
      defaults params
  in
  if ext.External_encoder_format.process = "" then
    raise External_encoder_format.No_process;
  Encoder.External ext

let () = Lang_encoder.register "external" kind_of_encoder make
