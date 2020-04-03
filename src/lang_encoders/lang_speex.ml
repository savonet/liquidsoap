(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

open Lang_values
open Lang_encoders

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
        | "stereo", { term = Bool b; _ } -> { f with Speex_format.stereo = b }
        | "mono", { term = Bool b; _ } -> { f with Speex_format.stereo = not b }
        | "samplerate", { term = Int i; _ } ->
            { f with Speex_format.samplerate = Lazy.from_val i }
        | "abr", { term = Int i; _ } ->
            { f with Speex_format.bitrate_control = Speex_format.Abr i }
        | "quality", ({ term = Int q; _ } as t) ->
            (* Doc say this should be from 0 to 10. *)
            if q < 0 || q > 10 then
              raise (Error (t, "Speex quality should be in 0..10"));
            { f with Speex_format.bitrate_control = Speex_format.Quality q }
        | "vbr", { term = Int q; _ } ->
            { f with Speex_format.bitrate_control = Speex_format.Vbr q }
        | "mode", { term = Var s; _ } when String.lowercase_ascii s = "wideband"
          ->
            { f with Speex_format.mode = Speex_format.Wideband }
        | "mode", { term = Var s; _ }
          when String.lowercase_ascii s = "narrowband" ->
            { f with Speex_format.mode = Speex_format.Narrowband }
        | "mode", { term = Var s; _ }
          when String.lowercase_ascii s = "ultra-wideband" ->
            { f with Speex_format.mode = Speex_format.Ultra_wideband }
        | "frames_per_packet", { term = Int i; _ } ->
            { f with Speex_format.frames_per_packet = i }
        | "complexity", ({ term = Int i; _ } as t) ->
            (* Doc says this should be between 1 and 10. *)
            if i < 1 || i > 10 then
              raise (Error (t, "Speex complexity should be in 1..10"));
            { f with Speex_format.complexity = Some i }
        | "bytes_per_page", { term = Int i; _ } ->
            { f with Speex_format.fill = Some i }
        | "dtx", { term = Bool b; _ } -> { f with Speex_format.dtx = b }
        | "vad", { term = Bool b; _ } -> { f with Speex_format.vad = b }
        | "", { term = Var s; _ } when String.lowercase_ascii s = "mono" ->
            { f with Speex_format.stereo = false }
        | "", { term = Var s; _ } when String.lowercase_ascii s = "stereo" ->
            { f with Speex_format.stereo = true }
        | _, t -> raise (generic_error t))
      defaults params
  in
  Ogg_format.Speex speex
