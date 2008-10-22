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

class text ttf ttf_size color dx dy speed cycle text =
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
  val mutable pos_y = dy

  val mutable font = None

  val mutable cur_text = text ()

  method render_text text =
    let font = Utils.get_some font in
    let ts = Sdlttf.render_text_shaded font text ~bg:Sdlvideo.black ~fg:Sdlvideo.white in
    let w, h =
      let si = Sdlvideo.surface_info ts in
        si.Sdlvideo.w, si.Sdlvideo.h
    in
    let tf = RGB.create w h in
    let tr, tg, tb = RGB.rgb_of_int color in
      if dy < 0 then
        pos_y <- Fmt.video_height () + dy - h;
      for y = 0 to h - 1 do
        for x = 0 to w - 1 do
          let r, g, b = Sdlvideo.get_pixel_color ts ~x ~y in
            RGB.set_pixel tf x y (tr, tg, tb, r)
        done
      done;
      text_frame <- Some tf

  initializer
    Sdl.init [];
    Sdlttf.init ();
    font <- Some
    (
      try
        Sdlttf.open_font ttf ttf_size
      with
        | e ->
            raise (Lang.Invalid_value ((Lang.string ttf), Printf.sprintf "Could not open font: %s" (Printexc.to_string e)));
    );
    self#render_text cur_text

  method get_frame ab =
    let b = VFrame.get_rgb ab in
    let off = VFrame.position ab in
    let size = VFrame.size ab in
    let tf = Utils.get_some text_frame in
    let tfw = RGB.get_width tf in
      if cur_text <> text () then
        (
          cur_text <- text ();
          self#render_text cur_text;
          if pos_x = -tfw then pos_x <- Fmt.video_width ()
        );
      for c = 0 to Array.length b - 1 do
        let buf_c = b.(c) in
          for i = off to size - 1 do
            if pos_x <> -tfw then
              RGB.blit tf buf_c.(i) ~x:pos_x ~y:pos_y;
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
      "font", Lang.string_t, Some (Lang.string "/usr/share/fonts/truetype/msttcorefonts/Arial.ttf"), Some "Path to ttf font file.";
      "size", Lang.int_t, Some (Lang.int 18), Some "Font size.";
      "color", Lang.int_t, Some (Lang.int 0xffffff), Some "Text color (in 0xRRGGBB format).";
      "x", Lang.int_t, Some (Lang.int (Fmt.video_width ())), Some "x offset.";
      "y", Lang.int_t, Some (Lang.int (-5)), Some "y offset (negative means from bottom).";
      "speed", Lang.int_t, Some (Lang.int 70), Some "Speed in pixels per second.";
      "cycle", Lang.bool_t, Some (Lang.bool true), Some "Cyle text";
      "", Lang.string_getter_t 1, Some (Lang.string ""), Some "Text.";
    ]
    ~category:Lang.Input
    ~descr:"Display a text."
    (fun p ->
       let f v = List.assoc v p in
       let ttf, ttf_size, color, x, y, speed, cycle, txt =
         Lang.to_string (f "font"),
         Lang.to_int (f "size"),
         Lang.to_int (f "color"),
         Lang.to_int (f "x"),
         Lang.to_int (f "y"),
         Lang.to_int (f "speed"),
         Lang.to_bool (f "cycle"),
         Lang.to_string_getter (f "")
       in
         ((new text ttf ttf_size color x y (speed / Fmt.video_frames_per_second ()) cycle txt):>source))
