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

type t = { string : string; offset : int; length : int }

let of_string s = { string = s; offset = 0; length = String.length s }

let of_substring s o l =
  assert (0 <= o && 0 <= l && o + l <= String.length s);
  { string = s; offset = o; length = l }

let to_string s = String.sub s.string s.offset s.length
let to_substring s = (s.string, s.offset, s.length)
let length s = s.length
let is_empty s = s.length = 0
let sub s o l = of_substring s.string (s.offset + o) l
let blit s b o = String.blit s.string s.offset b o s.length
