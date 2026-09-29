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

let init () = ()

let render_text ~font ~size text =
  if text = "" then (0, 0, fun _ _ -> 0)
  else (
    let h = size in
    (* Scale because gd is using 96 DPI instead of the default 72 DPI. *)
    let size = float size *. 72. /. 96. in
    let fname = font in
    let angle = 0. in
    let bounds = Gd.ft_bbox ~fname ~size ~angle ~x:0 ~y:0 text in
    let lowline_reference = Gd.ft_bbox ~fname ~size ~angle ~x:0 ~y:0 "j" in
    let y = h - lowline_reference.(1) in
    let w = bounds.(2) - bounds.(0) in

    (* Anti-aliasing. *)
    let h = h * 2 in
    let size = size *. 2. in
    let y = y * 2 in
    let w = w * 2 in

    let img = Gd.create ~x:w ~y:h in
    let ca = img#colors in
    img#filled_rectangle ~x1:0 ~y1:0 ~x2:w ~y2:h ca#black;
    ignore (img#string_ft ~fname ~size ~angle:0. ~x:0 ~y ~fg:ca#white text);
    let get_pixel x y =
      let c = img#get_pixel ~x ~y in
      if c = ca#white then 0xff else 0
    in

    (* Anti-aliasing. *)
    let get_pixel x y =
      (get_pixel (2 * x) (2 * y)
      + get_pixel ((2 * x) + 1) (2 * y)
      + get_pixel (2 * x) ((2 * y) + 1)
      + get_pixel ((2 * x) + 1) ((2 * y) + 1))
      / 4
    in
    (w, h, get_pixel))

let () = Video_text.register "gd" init render_text
