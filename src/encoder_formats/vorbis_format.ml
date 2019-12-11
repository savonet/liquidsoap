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

type quality = float

type bitrate = int

type mode =
  | VBR of quality (* Variable bitrate. *)
  | CBR of bitrate (* Constant bitrate. *)
  | ABR of bitrate option * bitrate option * bitrate option

(* Average: min,avg,max. *)

type t = {channels: int; mode: mode; samplerate: int Lazy.t; fill: int option}

let string_of_mode = function
  | ABR (min, avg, max) ->
      let f v x =
        match x with Some x -> Printf.sprintf "%s=%d," v x | None -> ""
      in
      Printf.sprintf ".abr(%s%s%s" (f "min_bitrate" min) (f "bitrate" avg)
        (f "max_bitrate" max)
  | CBR bitrate ->
      Printf.sprintf ".cbr(bitrate=%d" bitrate
  | VBR q ->
      Printf.sprintf "(quality=%.2f" q

let to_string v =
  Printf.sprintf "%%vorbis%s,channels=%d,samplerate=%d)"
    (string_of_mode v.mode) v.channels (Lazy.force v.samplerate)
