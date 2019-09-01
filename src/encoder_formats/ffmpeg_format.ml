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

type t = {
  format     : string;
  codec      : string;
  bitrate    : int option;
  channels   : int;
  samplerate : int Lazy.t ;
}

let bitrate {bitrate} =
  match bitrate with
    | Some br -> br
    | None -> raise Not_found

let to_string m =
  Printf.sprintf
    "%%fmpeg(format=%S,codec=%S%s,channels=%d,samplerate=%d)"
    m.format m.codec (match m.bitrate with None -> "" | Some br -> Printf.sprintf ",bitrate=%d" br)
    m.channels (Lazy.force m.samplerate)
