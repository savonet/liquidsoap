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

open Tsdl
open Tsdl_ttf

let init () =
  Sdl_utils.init [];
  ignore (Dtools.Init.at_start (fun () -> Sdl_utils.check Tsdl_ttf.Ttf.init ()))

let get_font font size =
  try Sdl_utils.check (Ttf.open_font font) size
  with e ->
    raise (Lang_errors.Invalid_value (Lang.string font, Printexc.to_string e))

let render_text ~font ~size text =
  let text = if text = "" then " " else text in
  let font = get_font font size in
  let white = Sdl.Color.create ~r:0xff ~g:0xff ~b:0xff ~a:0xff in
  let black = Sdl.Color.create ~r:0x00 ~g:0x00 ~b:0x00 ~a:0xff in
  let ts =
    Sdl_utils.check (fun () -> Ttf.render_utf8_shaded font text white black) ()
  in
  let img = Sdl_utils.Surface.to_img ts in
  let w = Video.Image.width img in
  let h = Video.Image.height img in
  Sdl.free_surface ts;
  (* TODO: improve performance *)
  let get_pixel x y =
    assert (0 <= x && x < w);
    assert (0 <= y && y < h);
    let r, _, _, _ = Video.Image.get_pixel_rgba img x y in
    r
  in
  (w, h, get_pixel)

let () = Video_text.register "sdl" init render_text
