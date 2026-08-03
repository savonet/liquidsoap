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

open Mm

(* Colour conversions shared by the video operators and the video sources. *)

let log = Log.make ["video"; "color"]

let rgb_of_int c =
  let c =
    if c < 0 || c > 0xffffff then (
      log#important
        "color 0x%x is greater than maximum assignable value 0xffffff" c;
      c land 0xffffff)
    else c
  in
  Image.RGB8.Color.of_int c

let yuv_of_int c = Image.Pixel.yuv_of_rgb (rgb_of_int c)
