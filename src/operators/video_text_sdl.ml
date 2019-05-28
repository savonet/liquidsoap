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

let init () =
  Sdl_utils.init [];
  Sdl_utils.ttf_init ()

let get_font font size =
  try
    Sdlttf.open_font font size
  with
    | Sdlttf.SDLttf_exception s ->
      raise (Lang_errors.Invalid_value (Lang.string font, s))
    | e ->
      raise (Lang_errors.Invalid_value (Lang.string font, Printexc.to_string e))

let render_text ~font ~size text =
  let text = if text = "" then " " else text in
  let font = get_font font size in
  let ts = Sdlttf.render_utf8_shaded font text ~bg:Sdlvideo.black ~fg:Sdlvideo.white in
  let w, h =
    let si = Sdlvideo.surface_info ts in
    si.Sdlvideo.w, si.Sdlvideo.h
  in
  let get_pixel x y =
    let r, _, _ = Sdlvideo.get_pixel_color ts ~x ~y in
    r
  in
  w, h, get_pixel

let () =
  Video_text.register "sdl" init render_text
