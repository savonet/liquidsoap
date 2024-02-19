(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
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
module GU = Gstreamer_utils

let init () = ()

let render_text ~font ~size text =
  (* Override default value... *)
  let font = if font.[0] = '/' then "Helvetica" else font in
  let font = Printf.sprintf "%s %d" font size in
  let pipeline =
    Printf.sprintf
      "videotestsrc pattern=black ! textoverlay font-desc=\"%s\" \
       valignment=top halignment=left text=\"%s\" ypad=0"
      font text
  in
  let img = GU.render_image pipeline in
  let width = Lazy.force Frame.video_width in
  let height = Lazy.force Frame.video_height in
  let get_pixel i j =
    let z, _, _, _ = Image.YUV420.get_pixel_rgba img i j in
    z
  in
  (* Normalize the pixel values, we get shade otherwise, see #1190. *)
  let pmin = ref 0xff in
  let pmax = ref 0x00 in
  for j = 0 to height - 1 do
    for i = 0 to width - 1 do
      let z = get_pixel i j in
      pmin := min !pmin z;
      pmax := max !pmax z
    done
  done;
  if !pmin = !pmax then pmax := 0xff;
  let pmin = !pmin in
  let pmax = !pmax in
  let get_pixel i j =
    let z = get_pixel i j in
    (z - pmin) * 0xff / (pmax - pmin)
  in
  (width, height, get_pixel)

let () = Video_text.register "gstreamer" init render_text
