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

open Mm
open Extralib

let log = Log.make ["video"; "text"; "native"]

let render_text ~font ~size text =
  if font <> Configure.conf_default_font#get then
    Fun.once (fun () ->
        log#important "video.text.native does not support custom fonts yet!");
  let () = ignore font in
  let font = Image.Bitmap.Font.native in
  let bmp = Image.Bitmap.Font.render text in
  let w = Image.Bitmap.width bmp in
  let h = Image.Bitmap.height bmp in
  let char_height = Image.Bitmap.Font.height font in
  let get_pixel x y =
    let x = x * char_height / size in
    let y = y * char_height / size in
    if 0 <= y && y < h && 0 <= x && x < w then
      if Mm.Image.Bitmap.get_pixel bmp x y then 0xff else 0x00
    else 0x00
  in
  let h = h * size / char_height in
  let w = w * size / char_height in
  (w, h, get_pixel)

let () = Video_text.register "native" (fun () -> ()) render_text
