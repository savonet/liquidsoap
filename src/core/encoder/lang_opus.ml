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
      Opus_format.application = None;
      complexity = None;
      max_bandwidth = None;
      mode = Opus_format.VBR true;
      bitrate = `Auto;
      fill = None;
      channels = 2;
      samplerate = 48000;
      signal = None;
      frame_size = 20.;
      dtx = false;
      phase_inversion = true;
    }
  in
  let opus =
    List.fold_left
      (fun f -> function
        | `Labelled ("application", String { value = "voip"; _ }) ->
            { f with Opus_format.application = Some `Voip }
        | `Labelled ("application", String { value = "audio" }) ->
            { f with Opus_format.application = Some `Audio }
        | `Labelled ("application", String { value = "restricted_lowdelay" }) ->
            { f with Opus_format.application = Some `Restricted_lowdelay }
        | `Labelled ("complexity", Int { value = c; pos }) ->
            (* Doc say this should be from 0 to 10. *)
            if c < 0 || c > 10 then
              Lang_encoder.raise_error ~pos "Opus complexity should be in 0..10";
            { f with Opus_format.complexity = Some c }
        | `Labelled ("max_bandwidth", String { value = "narrow_band" }) ->
            { f with Opus_format.max_bandwidth = Some `Narrow_band }
        | `Labelled ("max_bandwidth", String { value = "medium_band" }) ->
            { f with Opus_format.max_bandwidth = Some `Medium_band }
        | `Labelled ("max_bandwidth", String { value = "wide_band" }) ->
            { f with Opus_format.max_bandwidth = Some `Wide_band }
        | `Labelled ("max_bandwidth", String { value = "super_wide_band" }) ->
            { f with Opus_format.max_bandwidth = Some `Super_wide_band }
        | `Labelled ("max_bandwidth", String { value = "full_band" }) ->
            { f with Opus_format.max_bandwidth = Some `Full_band }
        | `Labelled ("frame_size", Float { value = size; pos }) ->
            let frame_sizes = [2.5; 5.; 10.; 20.; 40.; 60.] in
            if not (List.mem size frame_sizes) then
              Lang_encoder.raise_error ~pos
                "Opus frame size should be one of 2.5, 5., 10., 20., 40. or 60.";
            { f with Opus_format.frame_size = size }
        | `Labelled ("samplerate", Value.Int { value = i; pos }) ->
            let samplerates = [8000; 12000; 16000; 24000; 48000] in
            if not (List.mem i samplerates) then
              Lang_encoder.raise_error ~pos
                "Opus samplerate should be one of 8000, 12000, 16000, 24000 or \
                 48000";
            { f with Opus_format.samplerate = i }
        | `Labelled ("bitrate", Value.Int { value = i; pos }) ->
            let i = i * 1000 in
            (* Doc say this should be from 500 to 512000. *)
            if i < 500 || i > 512000 then
              Lang_encoder.raise_error ~pos "Opus bitrate should be in 5..512";
            { f with Opus_format.bitrate = `Bitrate i }
        | `Labelled ("bitrate", String { value = "auto" }) ->
            { f with Opus_format.bitrate = `Auto }
        | `Labelled ("bitrate", String { value = "max" }) ->
            { f with Opus_format.bitrate = `Bitrate_max }
        | `Labelled ("stereo", Value.Bool { value = b; _ }) ->
            { f with Opus_format.channels = (if b then 2 else 1) }
        | `Labelled ("mono", Value.Bool { value = b; _ }) ->
            { f with Opus_format.channels = (if b then 1 else 2) }
        | `Anonymous s when String.lowercase_ascii s = "mono" ->
            { f with Opus_format.channels = 1 }
        | `Anonymous s when String.lowercase_ascii s = "stereo" ->
            { f with Opus_format.channels = 2 }
        | `Labelled ("channels", Value.Int { value = i; pos }) ->
            if i < 1 || i > 2 then
              Lang_encoder.raise_error ~pos
                "only mono and stereo streams are supported for now";
            { f with Opus_format.channels = i }
        | `Labelled ("vbr", String { value = "none"; _ }) ->
            { f with Opus_format.mode = Opus_format.CBR }
        | `Labelled ("vbr", String { value = "constrained" }) ->
            { f with Opus_format.mode = Opus_format.VBR true }
        | `Labelled ("vbr", String { value = "unconstrained" }) ->
            { f with Opus_format.mode = Opus_format.VBR false }
        | `Labelled ("signal", String { value = "voice" }) ->
            { f with Opus_format.signal = Some `Voice }
        | `Labelled ("signal", String { value = "music" }) ->
            { f with Opus_format.signal = Some `Music }
        | `Labelled ("bytes_per_page", Value.Int { value = i; _ }) ->
            { f with Opus_format.fill = Some i }
        | `Labelled ("dtx", Value.Bool { value = b; _ }) ->
            { f with Opus_format.dtx = b }
        | `Labelled ("phase_inversion", Value.Bool { value = b; _ }) ->
            { f with Opus_format.phase_inversion = b }
        | `Anonymous s when String.lowercase_ascii s = "mono" ->
            { f with Opus_format.channels = 1 }
        | `Anonymous s when String.lowercase_ascii s = "stereo" ->
            { f with Opus_format.channels = 2 }
        | t -> Lang_encoder.raise_generic_error t)
      defaults params
  in
  Ogg_format.Opus opus

let () =
  let make p = Encoder.Ogg { Ogg_format.audio = Some (make p); video = None } in
  Lang_encoder.register "opus" type_of_encoder make
