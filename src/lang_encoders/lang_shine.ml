(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2017 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Lang_values
open Lang_encoders

let make params =
  let defaults =
    { Shine_format.
        channels = 2 ;
        samplerate = 44100 ;
        bitrate = 128 }
  in
  let shine =
    List.fold_left
      (fun f ->
        function
          | ("channels",{ term = Int i; _}) ->
              { f with Shine_format.channels = i }
          | ("samplerate",{ term = Int i; _}) ->
              { f with Shine_format.samplerate = i }
          | ("bitrate",{ term = Int i; _}) ->
              { f with Shine_format.bitrate = i }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "mono" ->
              { f with Shine_format.channels = 1 }
          | ("",{ term = Var s; _}) when Utils.StringCompat.lowercase_ascii s = "stereo" ->
              { f with Shine_format.channels = 2 }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
    Encoder.Shine shine
