(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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

module Img = Image.RGBA8

class text ~kind
  ttf ttf_size color dx dy speed cycle meta text (source:source) =
let video_height = Lazy.force Frame.video_height in
let video_width = Lazy.force Frame.video_width in
object (self)
  inherit operator kind [source] as super

  method stype = source#stype

  method remaining = source#remaining
  method is_ready = source#is_ready
  method abort_track = source#abort_track

  val mutable text_frame = None

  val mutable pos_x = dx
  val mutable pos_y = dy

  val mutable font = None

  val mutable cur_text = text ()

  method private render_text text =
    let font = Utils.get_some font in
    let text = if text = "" then " " else text in
    let ts =
      Sdlttf.render_text_shaded font text ~bg:Sdlvideo.black ~fg:Sdlvideo.white
    in
    let w, h =
      let si = Sdlvideo.surface_info ts in
        si.Sdlvideo.w, si.Sdlvideo.h
    in
    let tf = Img.create w h in
    let tr, tg, tb = Image.RGB8.Color.of_int color in
      if dy < 0 then
        pos_y <- video_height + dy - h;
      for y = 0 to h - 1 do
        for x = 0 to w - 1 do
          let r, g, b = Sdlvideo.get_pixel_color ts ~x ~y in
            Img.set_pixel tf x y (tr, tg, tb, r)
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
            (* TODO it is forbidden to raise Invalid_value on a made-up value
             *   because it needs a position information to report the error *)
            raise (Lang.Invalid_value
                     ((Lang.string ttf),
                      Printf.sprintf
                        "Could not open font: %s"
                        (Utils.error_message e)));
    );
    self#render_text cur_text

  method private get_frame ab =
    let off = VFrame.position ab in
    source#get ab;
    let rgb = (VFrame.content ab off).(0) in
    let size = VFrame.size ab in
    let tf = Utils.get_some text_frame in
    let tfw = Img.width tf in
    let text =
      match meta with
        | None -> text ()
        | Some meta ->
            let ans = ref cur_text in
              List.iter
                (fun (t,m) ->
                   try
                     ans := Hashtbl.find m meta
                   with
                     | Not_found -> ()
                ) (Frame.get_all_metadata ab);
              !ans
    in
      if cur_text <> text then
        (
          cur_text <- text;
          self#render_text cur_text;
          if pos_x = -tfw then pos_x <- video_width
        );
      for i = off to size - 1 do
        if pos_x <> -tfw then
          Img.add tf rgb.(i) ~x:pos_x ~y:pos_y;
        pos_x <- pos_x - speed;
        if pos_x < -tfw then
          if cycle then
            pos_x <- video_width
          else
            pos_x <- -tfw (* avoid overflows *)
      done

end

let () =
  let k =
    Lang.kind_type_of_kind_format ~fresh:2 (Lang.any_fixed_with ~video:1 ())
  in
  Lang.add_operator "video.add_text"
    [
      "font", Lang.string_t,
      (* TODO Autodetect *)
      Some (Lang.string "/usr/share/fonts/truetype/msttcorefonts/Arial.ttf"),
      Some "Path to ttf font file.";

      "size", Lang.int_t, Some (Lang.int 18), Some "Font size.";

      (* TODO background color, possibly transparent *)
      "color", Lang.int_t, Some (Lang.int 0xffffff),
      Some "Text color (in 0xRRGGBB format).";

      "x", Lang.int_t, Some (Lang.int (Lazy.force Frame.video_width)),
      Some "x offset.";
      "y", Lang.int_t, Some (Lang.int (-5)),
      Some "y offset (negative means from bottom).";

      "speed", Lang.int_t, Some (Lang.int 70),
      Some "Speed in pixels per second (0 means no scrolling).";

      "cycle", Lang.bool_t, Some (Lang.bool true), Some "Cycle text.";
      "metadata", Lang.string_t, Some (Lang.string ""),
      Some "Change text on a particular metadata \
            (empty string means disabled).";

      "", Lang.string_getter_t 1, None, Some "Text to display.";
      "", Lang.source_t k, None, None
    ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Input
    ~descr:"Display a text."
    (fun p kind ->
       let f v = List.assoc v p in
       let ttf, ttf_size, color, x, y, speed, cycle, meta, txt, source =
         Lang.to_string (f "font"),
         Lang.to_int (f "size"),
         Lang.to_int (f "color"),
         Lang.to_int (f "x"),
         Lang.to_int (f "y"),
         Lang.to_int (f "speed"),
         Lang.to_bool (f "cycle"),
         Lang.to_string (f "metadata"),
         Lang.to_string_getter (Lang.assoc "" 1 p),
         Lang.to_source (Lang.assoc "" 2 p)
       in
       let speed = speed / (Lazy.force Frame.video_rate) in
       let meta = if meta = "" then None else Some meta in
         new text ~kind ttf ttf_size color x y speed cycle meta txt source)
