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
  let get_pixel x y =
    let z, _, _, _ = Video.Image.get_pixel_rgba img x y in
    z
  in
  (width, height, get_pixel)

let () = Video_text.register "gstreamer" init render_text
