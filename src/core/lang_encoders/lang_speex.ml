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

open Value
open Ground

let type_of_encoder p = Encoder.audio_type (Lang_encoder.channels_of_params p)

let make params =
  let defaults =
    {
      Speex_format.stereo = true;
      fill = None;
      samplerate = Frame.audio_rate;
      bitrate_control = Speex_format.Quality 7;
      mode = Speex_format.Narrowband;
      frames_per_packet = 1;
      complexity = None;
      dtx = false;
      vad = false;
    }
  in
  let speex =
    List.fold_left
      (fun f -> function
        | "stereo", `Value { value = Ground (Bool b); _ } ->
            { f with Speex_format.stereo = b }
        | "mono", `Value { value = Ground (Bool b); _ } ->
            { f with Speex_format.stereo = not b }
        | "samplerate", `Value { value = Ground (Int i); _ } ->
            { f with Speex_format.samplerate = SyncLazy.from_val i }
        | "abr", `Value { value = Ground (Int i); _ } ->
            { f with Speex_format.bitrate_control = Speex_format.Abr i }
        | "quality", `Value { value = Ground (Int q); pos } ->
            (* Doc say this should be from 0 to 10. *)
            if q < 0 || q > 10 then
              Lang_encoder.raise_error ~pos "Speex quality should be in 0..10";
            { f with Speex_format.bitrate_control = Speex_format.Quality q }
        | "vbr", `Value { value = Ground (Int q); _ } ->
            { f with Speex_format.bitrate_control = Speex_format.Vbr q }
        | "mode", `Value { value = Ground (String s); _ }
          when String.lowercase_ascii s = "wideband" ->
            { f with Speex_format.mode = Speex_format.Wideband }
        | "mode", `Value { value = Ground (String s); _ }
          when String.lowercase_ascii s = "narrowband" ->
            { f with Speex_format.mode = Speex_format.Narrowband }
        | "mode", `Value { value = Ground (String s); _ }
          when String.lowercase_ascii s = "ultra-wideband" ->
            { f with Speex_format.mode = Speex_format.Ultra_wideband }
        | "frames_per_packet", `Value { value = Ground (Int i); _ } ->
            { f with Speex_format.frames_per_packet = i }
        | "complexity", `Value { value = Ground (Int i); pos } ->
            (* Doc says this should be between 1 and 10. *)
            if i < 1 || i > 10 then
              Lang_encoder.raise_error ~pos
                "Speex complexity should be in 1..10";
            { f with Speex_format.complexity = Some i }
        | "bytes_per_page", `Value { value = Ground (Int i); _ } ->
            { f with Speex_format.fill = Some i }
        | "dtx", `Value { value = Ground (Bool b); _ } ->
            { f with Speex_format.dtx = b }
        | "vad", `Value { value = Ground (Bool b); _ } ->
            { f with Speex_format.vad = b }
        | "", `Value { value = Ground (String s); _ }
          when String.lowercase_ascii s = "mono" ->
            { f with Speex_format.stereo = false }
        | "", `Value { value = Ground (String s); _ }
          when String.lowercase_ascii s = "stereo" ->
            { f with Speex_format.stereo = true }
        | t -> Lang_encoder.raise_generic_error t)
      defaults params
  in
  Ogg_format.Speex speex

let () =
  let make p = Encoder.Ogg { Ogg_format.audio = Some (make p); video = None } in
  Lang_encoder.register "speex" type_of_encoder make
