(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Source

class text dx dy speed cycle text =
object (self)
  inherit source

  method stype = Infallible
  method is_ready = true

  val mutable remaining = 0

  val mutable must_fail = false
  method abort_track =
    must_fail <- true;
    remaining <- 0

  val mutable text_frame = None
  method remaining = 0

  val mutable pos_x = dx
  val pos_y = dy

  initializer
    Sdl.init [`VIDEO];
    Sdlttf.init ();
    let font =
      try
        Sdlttf.open_font "/usr/share/fonts/truetype/msttcorefonts/Arial.ttf" 40
      with
        | e ->
            Printf.printf "Sdlttf error: %s\n" (Printexc.to_string e);
            exit (-1)
    in
    let ts = Sdlttf.render_text_solid font text ~fg:Sdlvideo.white in
    let w, h =
      let si = Sdlvideo.surface_info ts in
        si.Sdlvideo.w, si.Sdlvideo.h
    in
    let tf = RGB.create w h in
      for y = 0 to h - 1 do
        for x = 0 to w - 1 do
          let r, g, b = Sdlvideo.get_pixel_color ts ~x ~y in
            RGB.set_pixel tf x y (r, g, b, r)
        done
      done;
      text_frame <- Some tf

  method get_frame ab =
    let b = VFrame.get_rgb ab in
    let off = VFrame.position ab in
    let size = VFrame.size ab in
    let tf = Utils.get_some text_frame in
    let tfw = RGB.get_width tf in
      for c = 0 to Array.length b - 1 do
        let buf_c = b.(c) in
          for i = off to size - 1 do
            RGB.blit_off tf buf_c.(i) pos_x pos_y;
            pos_x <- pos_x - speed;
            if pos_x < -tfw then
              if cycle then
                pos_x <- Fmt.video_width ()
              else
                pos_x <- -tfw (* avoid overflows *)
          done;
      done;
      AFrame.add_break ab (AFrame.size ab)
end

let () =
  Lang.add_operator "video.text"
    [
      "x", Lang.int_t, Some (Lang.int (Fmt.video_width ())), Some "x offset.";
      "y", Lang.int_t, Some (Lang.int 0), Some "y offset.";
      "speed", Lang.int_t, Some (Lang.int 100), Some "Speed in pixels per second.";
      "cycle", Lang.bool_t, Some (Lang.bool true), Some "Cyle text";
      "", Lang.string_t, None, Some "Text.";
    ]
    ~category:Lang.Input
    ~descr:"Display a text."
    (fun p ->
       let f v = List.assoc v p in
       let x, y, speed, cycle, txt =
         Lang.to_int (f "x"),
         Lang.to_int (f "y"),
         Lang.to_int (f "speed"),
         Lang.to_bool (f "cycle"),
         Lang.to_string (f "")
       in
         ((new text x y (speed / Fmt.video_frames_per_second ()) cycle txt):>source))
