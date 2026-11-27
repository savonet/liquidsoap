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

let accepted_bits_per_sample = [8; 16; 24; 32]

let flac_gen ~ogg params =
  let defaults =
    {
      Flac_format.fill = None;
      (* We use a hardcoded value in order not to force the evaluation of the
         number of channels too early, see #933. *)
      channels = 2;
      samplerate = `Setting `Audio_rate;
      bits_per_sample = 16;
      compression = 5;
    }
  in
  let flac =
    List.fold_left
      (fun f -> function
        | `Labelled ("stereo", Bool { value = b; _ }) ->
            { f with Flac_format.channels = (if b then 2 else 1) }
        | `Labelled ("mono", Bool { value = b; _ }) ->
            { f with Flac_format.channels = (if b then 1 else 2) }
        | `Labelled ("channels", Int { value = i; _ }) ->
            { f with Flac_format.channels = i }
        | `Labelled ("samplerate", Int { value = i; _ }) ->
            { f with Flac_format.samplerate = `Value i }
        | `Labelled ("compression", Int { value = i; pos }) ->
            if i < 0 || i > 8 then
              Format_builders.raise_error ~pos "invalid compression value";
            { f with Flac_format.compression = i }
        | `Labelled ("bits_per_sample", Int { value = i; pos }) ->
            if not (List.mem i accepted_bits_per_sample) then
              Format_builders.raise_error ~pos "invalid bits_per_sample value";
            { f with Flac_format.bits_per_sample = i }
        | `Labelled ("bytes_per_page", Int { value = i; pos }) ->
            if not ogg then
              Format_builders.raise_error ~pos
                "native FLAC has no pages: use %ogg(%flac(bytes_per_page=...))";
            { f with Flac_format.fill = Some i }
        | `Anonymous s when String.lowercase_ascii s = "mono" ->
            { f with Flac_format.channels = 1 }
        | `Anonymous s when String.lowercase_ascii s = "stereo" ->
            { f with Flac_format.channels = 2 }
        | t -> Format_builders.raise_generic_error t)
      defaults params
  in
  if flac.Flac_format.channels < 1 || flac.Flac_format.channels > 8 then
    Format_builders.raise_error ~pos:None "FLAC takes 1 to 8 channels";
  (match flac.Flac_format.samplerate with
    | `Value samplerate when samplerate < 1 || samplerate > 655350 ->
        Format_builders.raise_error ~pos:None
          "FLAC sample rates are 1 to 655350 Hz"
    | _ -> ());
  flac

let make_ogg params = Ogg_format.Flac (flac_gen ~ogg:true params)
let make params = Format_value.Flac (flac_gen ~ogg:false params)
let () = Format_builders.register "flac" make
