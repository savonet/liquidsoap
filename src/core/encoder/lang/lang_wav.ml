(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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
      Wav_format.samplesize = 16;
      header = true;
      duration = None;
      (* We use a hardcoded value in order not to force the evaluation of the
         number of channels too early, see #933. *)
      channels = 2;
      samplerate = Frame.audio_rate;
    }
  in
  let wav =
    List.fold_left
      (fun f -> function
        | `Labelled ("stereo", Bool { value = b; _ }) ->
            { f with Wav_format.channels = (if b then 2 else 1) }
        | `Labelled ("mono", Bool { value = b; _ }) ->
            { f with Wav_format.channels = (if b then 1 else 2) }
        | `Anonymous s when String.lowercase_ascii s = "stereo" ->
            { f with Wav_format.channels = 2 }
        | `Anonymous s when String.lowercase_ascii s = "mono" ->
            { f with Wav_format.channels = 1 }
        | `Labelled ("channels", Int { value = c }) ->
            { f with Wav_format.channels = c }
        | `Labelled ("duration", Float { value = d }) ->
            { f with Wav_format.duration = Some d }
        | `Labelled ("samplerate", Int { value = i; _ }) ->
            { f with Wav_format.samplerate = Lazy.from_val i }
        | `Labelled ("samplesize", Int { value = i; pos }) ->
            if i <> 8 && i <> 16 && i <> 24 && i <> 32 then
              Lang_encoder.raise_error ~pos "invalid sample size";
            { f with Wav_format.samplesize = i }
        | `Labelled ("header", Bool { value = b; _ }) ->
            { f with Wav_format.header = b }
        | t -> Lang_encoder.raise_generic_error t)
      defaults params
  in
  Encoder.WAV wav

let () = Lang_encoder.register "wav" type_of_encoder make
