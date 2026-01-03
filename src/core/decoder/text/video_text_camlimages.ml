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

(* Based on https://gitlab.com/camlspotter/camlimages/-/blob/master/examples/ttfimg/ttfimg.ml *)

let log = Log.make ["video"; "add_text"; "camlimages"]
let init () = ()

let render_text ~font ~size text =
  if text = "" then (0, 0, fun _ _ -> 0)
  else (
    let csize = float size in
    let face = new OFreetype.face font 0 in
    face#set_char_size csize csize 72 72;
    List.iter
      (fun cmap ->
        log#debug "available charmap: platform %d / encoding %d"
          cmap.Freetype.platform_id cmap.Freetype.encoding_id)
      face#charmaps;
    (try face#set_charmap { Freetype.platform_id = 3; encoding_id = 1 }
     with _ -> (
       try face#set_charmap { Freetype.platform_id = 3; encoding_id = 0 }
       with _ -> face#set_charmap (List.hd face#charmaps)));
    let encoded = Fttext.unicode_of_latin text in
    let x1, y1, x2, y2 = face#size encoded in
    let plus = 8 in
    let w = truncate (x2 -. x1) + plus in
    let h = truncate (ceil y2) - truncate y1 + 1 + plus in
    let img = Array.init h (fun _ -> Array.make w 0) in
    OFreetype.draw_gen Freetype.Render_Normal Freetype.render_char 0.
      (fun x y l -> try img.(y).(x) <- l with _ -> ())
      face
      ((plus / 2) - truncate x1)
      (truncate y2 + (plus / 2))
      encoded;
    let get_pixel x y = img.(y).(x) in
    (w, h, get_pixel))

let () = Video_text.register "camlimages" init render_text
