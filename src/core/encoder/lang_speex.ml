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

open Value

let type_of_encoder p =
  Encoder.audio_type ~pcm_kind:Content.Audio.kind
    (Lang_encoder.channels_of_params p)

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
        | `Labelled ("stereo", Value.Bool { value = b; _ }) ->
            { f with Speex_format.stereo = b }
        | `Labelled ("mono", Value.Bool { value = b; _ }) ->
            { f with Speex_format.stereo = not b }
        | `Labelled ("samplerate", Value.Int { value = i; _ }) ->
            { f with Speex_format.samplerate = Lazy.from_val i }
        | `Labelled ("abr", Value.Int { value = i; _ }) ->
            { f with Speex_format.bitrate_control = Speex_format.Abr i }
        | `Labelled ("quality", Int { value = q; pos }) ->
            (* Doc say this should be from 0 to 10. *)
            if q < 0 || q > 10 then
              Lang_encoder.raise_error ~pos "Speex quality should be in 0..10";
            { f with Speex_format.bitrate_control = Speex_format.Quality q }
        | `Labelled ("vbr", Int { value = q; _ }) ->
            { f with Speex_format.bitrate_control = Speex_format.Vbr q }
        | `Labelled ("mode", String { value = s; _ })
          when String.lowercase_ascii s = "wideband" ->
            { f with Speex_format.mode = Speex_format.Wideband }
        | `Labelled ("mode", String { value = s; _ })
          when String.lowercase_ascii s = "narrowband" ->
            { f with Speex_format.mode = Speex_format.Narrowband }
        | `Labelled ("mode", String { value = s; _ })
          when String.lowercase_ascii s = "ultra-wideband" ->
            { f with Speex_format.mode = Speex_format.Ultra_wideband }
        | `Labelled ("frames_per_packet", Value.Int { value = i; _ }) ->
            { f with Speex_format.frames_per_packet = i }
        | `Labelled ("complexity", Value.Int { value = i; pos }) ->
            (* Doc says this should be between 1 and 10. *)
            if i < 1 || i > 10 then
              Lang_encoder.raise_error ~pos
                "Speex complexity should be in 1..10";
            { f with Speex_format.complexity = Some i }
        | `Labelled ("bytes_per_page", Value.Int { value = i; _ }) ->
            { f with Speex_format.fill = Some i }
        | `Labelled ("dtx", Value.Bool { value = b; _ }) ->
            { f with Speex_format.dtx = b }
        | `Labelled ("vad", Value.Bool { value = b; _ }) ->
            { f with Speex_format.vad = b }
        | `Anonymous s when String.lowercase_ascii s = "mono" ->
            { f with Speex_format.stereo = false }
        | `Anonymous s when String.lowercase_ascii s = "stereo" ->
            { f with Speex_format.stereo = true }
        | t -> Lang_encoder.raise_generic_error t)
      defaults params
  in
  Ogg_format.Speex speex

let () =
  let make p = Encoder.Ogg { Ogg_format.audio = Some (make p); video = None } in
  Lang_encoder.register "speex" type_of_encoder make
