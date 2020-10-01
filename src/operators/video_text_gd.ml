(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2020 Savonet team

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

let init () = ()

let render_text ~font ~size text =
  let size = float size in
  let fname = font in
  let angle = 0. in
  let bounds = Gd.ft_bbox ~fname ~size ~angle ~x:0 ~y:0 text in
  let x1, y1 = (bounds.(6), bounds.(7)) in
  let x2, y2 = (bounds.(2), bounds.(3)) in
  let w, h = (x2 - x1, y2 - y1) in
  (* Double height in order to account for text below the baseline. *)
  let h = 2 * h in
  (* Double the size of the image in order to perform anti-aliasing. *)
  let w, h = (2 * w, 2 * h) in
  let size = 2. *. size in
  let img = Gd.create ~x:w ~y:h in
  let ca = img#colors in
  img#filled_rectangle ~x1:0 ~y1:0 ~x2:(w - 1) ~y2:(h - 1) ca#black;
  ignore
    (img#string_ft ~fname ~size ~angle:0. ~x:0 ~y:(h / 2) ~fg:ca#white text);
  let get_pixel x y =
    let c = img#get_pixel ~x ~y in
    if c = ca#white then 0xff else 0
  in
  (* Anti-aliasing. *)
  let get_pixel x y =
    ( get_pixel (2 * x) (2 * y)
    + get_pixel ((2 * x) + 1) (2 * y)
    + get_pixel (2 * x) ((2 * y) + 1)
    + get_pixel ((2 * x) + 1) ((2 * y) + 1) )
    / 4
  in
  (w, h, get_pixel)

let () = Video_text.register "gd" init render_text
