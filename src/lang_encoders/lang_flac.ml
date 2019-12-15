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

let flac_gen params =
  let defaults =
    {
      Flac_format.fill = None;
      (* We use a hardcoded value in order not to force the evaluation of the
           number of channels too early, see #933. *)
      channels = 2;
      samplerate = Frame.audio_rate;
      bits_per_sample = 16;
      compression = 5;
    }
  in
  List.fold_left
    (fun f -> function
      | "channels", { term = Int i; _ } -> { f with Flac_format.channels = i }
      | "samplerate", { term = Int i; _ } ->
          { f with Flac_format.samplerate = Lazy.from_val i }
      | "compression", ({ term = Int i; _ } as t) ->
          if i < 0 || i > 8 then raise (Error (t, "invalid compression value"));
          { f with Flac_format.compression = i }
      | "bits_per_sample", ({ term = Int i; _ } as t) ->
          if i <> 8 && i <> 16 && i <> 32 then
            raise (Error (t, "invalid bits_per_sample value"));
          { f with Flac_format.bits_per_sample = i }
      | "bytes_per_page", { term = Int i; _ } ->
          { f with Flac_format.fill = Some i }
      | "", { term = Var s; _ } when String.lowercase_ascii s = "mono" ->
          { f with Flac_format.channels = 1 }
      | "", { term = Var s; _ } when String.lowercase_ascii s = "stereo" ->
          { f with Flac_format.channels = 2 } | _, t -> raise (generic_error t))
    defaults params

let make_ogg params = Ogg_format.Flac (flac_gen params)
let make params = Encoder.Flac (flac_gen params)
