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
      Shine_format.channels
      (* We use a hardcoded value in order not to force the evaluation of the
           number of channels too early, see #933. *)=
        2;
      samplerate= Frame.audio_rate;
      bitrate= 128;
    }
  in
  let shine =
    List.fold_left
      (fun f -> function "channels", {term= Int i; _} ->
            {f with Shine_format.channels= i}
        | "samplerate", {term= Int i; _} ->
            {f with Shine_format.samplerate= Lazy.from_val i}
        | "bitrate", {term= Int i; _} -> {f with Shine_format.bitrate= i}
        | "", {term= Var s; _} when String.lowercase_ascii s = "mono" ->
            {f with Shine_format.channels= 1}
        | "", {term= Var s; _} when String.lowercase_ascii s = "stereo" ->
            {f with Shine_format.channels= 2} | _, t ->
            raise (generic_error t))
      defaults params
  in
  Encoder.Shine shine
