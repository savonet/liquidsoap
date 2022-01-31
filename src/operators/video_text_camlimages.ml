(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

open Color

let init () = ()

let render_text ~font ~size text =
  let csize = float size *. 7.2 in
  let face = new OFreetype.face font 0 in
  face#set_char_size csize csize 72 72;
  face#set_charmap (List.hd face#charmaps);
  let encoded = Fttext.unicode_of_latin text in
  let x1, y1, x2, y2 = face#size encoded in
  let plus = 8 in
  let h = truncate (ceil y2) - truncate y1 + 1 + plus in
  let bg = { r = 0; g = 0; b = 0 } in
  let fg = { r = 255; g = 255; b = 255 } in
  let rgb = new OImages.rgb24_filled (truncate (x2 -. x1) + plus) h bg in
  OFreetype.draw_text face
    (fun org level ->
      let level' = 255 - level in
      {
        r = ((org.r * level') + (fg.r * level)) / 255;
        g = ((org.g * level') + (fg.g * level)) / 255;
        b = ((org.b * level') + (fg.b * level)) / 255;
      })
    (rgb :> Images.rgb OImages.map)
    ((plus / 2) - truncate x1)
    (truncate y2 + (plus / 2))
    encoded;
  let w = rgb#width in
  let h = rgb#height in
  let get_pixel x y =
    let c = rgb#get x y in
    (c.r + c.g + c.b) / (255 * 255)
  in
  (w, h, get_pixel)

let () = Video_text.register "camlimages" init render_text
