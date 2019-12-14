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
      Avi_format.channels
      (* We use a hardcoded value in order not to force the evaluation of the
         number of channels too early, see #933. *)=
        2;
      samplerate= Frame.audio_rate;
    }
  in
  let avi =
    List.fold_left
      (fun f -> function "channels", {term= Int c; _} ->
            {f with Avi_format.channels= c} | "samplerate", {term= Int i; _} ->
            {f with Avi_format.samplerate= Lazy.from_val i} | _, t ->
            raise (generic_error t))
      defaults params
  in
  Encoder.AVI avi
