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
open Tsdl
open Tsdl_ttf

let init () =
  Sdl_utils.init [];
  Sdl_utils.check Tsdl_ttf.Ttf.init ()

let get_font font size =
  let font = if font = "" then Configure.conf_default_font#get else font in
  try Sdl_utils.check (Ttf.open_font font) size
  with e ->
    raise
      (Error.Invalid_value
         (Lang.string font, Printexc.to_string e ^ "(font: " ^ font ^ ")"))

let render_text ~font ~size text =
  let text = if text = "" then " " else text in
  let font = get_font font size in
  let ts =
    Fun.protect
      (fun () ->
        let white = Sdl.Color.create ~r:0xff ~g:0xff ~b:0xff ~a:0xff in
        Sdl_utils.check
          (fun () -> Ttf.render_utf8_blended_wrapped font text white Int32.zero)
          ())
      ~finally:(fun () -> Ttf.close_font font)
  in
  let img =
    Fun.protect
      (fun () -> Sdl_utils.Surface.to_img ts)
      ~finally:(fun () -> Sdl.free_surface ts)
  in
  let w = Video.Image.width img in
  let h = Video.Image.height img in

  (* TODO: improve performance *)
  let get_pixel x y =
    assert (0 <= x && x < w);
    assert (0 <= y && y < h);
    Image.YUV420.get_pixel_a img x y
  in
  (w, h, get_pixel)

let () = Video_text.register "sdl" init render_text
