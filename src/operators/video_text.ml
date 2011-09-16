(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2011 Savonet team

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

module Img = Image.RGBA32

class text ~kind
  init render_text ttf ttf_size color dx dy speed cycle meta text (source:source) =
  let () = init () in
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
    let w, h, get_pixel = render_text ~font:ttf ~size:ttf_size text in
    let tf = Img.create w h in
    let tr, tg, tb = Image.RGB8.Color.of_int color in
      if dy < 0 then
        pos_y <- video_height + dy - h;
      if dx < 0 then
        pos_x <- video_width + dx - w;
      for y = 0 to h - 1 do
        for x = 0 to w - 1 do
          let a = get_pixel x y in
            Img.set_pixel tf x y (tr, tg, tb, a)
        done
      done;
      text_frame <- Some tf

  method get_text_frame =
    match text_frame with
      | Some tf -> tf
      | None ->
         self#render_text cur_text;
         Utils.get_some text_frame

  method private get_frame ab =
    match VFrame.get_content ab source with
      | None -> ()
      | Some (rgb,off,len) ->
          let rgb = rgb.(0) in
          let tf = self#get_text_frame in
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
                           | Not_found -> ())
                      (Frame.get_all_metadata ab) ;
                    !ans
          in
            if cur_text <> text then begin
              cur_text <- text ;
              self#render_text cur_text ;
              if pos_x = -tfw then pos_x <- video_width
            end ;
            for i = off to off + len - 1 do
              if pos_x <> -tfw then
                Img.add tf rgb.(i) ~x:pos_x ~y:pos_y ;
                pos_x <- pos_x - speed ;
                if pos_x < -tfw then
                  if cycle then
                    pos_x <- video_width
                  else
                    pos_x <- -tfw (* avoid overflows *)
            done

end

let register name init render_text =
  let k =
    Lang.kind_type_of_kind_format ~fresh:2 (Lang.any_fixed_with ~video:1 ())
  in
  Lang.add_operator ("video.add_text."^name)
    [
      "font", Lang.string_t,
      Some (Lang.string "/usr/share/fonts/truetype/ttf-dejavu/DejaVuSans.ttf"),
      Some "Path to ttf font file.";

      "size", Lang.int_t, Some (Lang.int 18), Some "Font size.";

      (* TODO background color, possibly transparent *)
      "color", Lang.int_t, Some (Lang.int 0xffffff),
      Some "Text color (in 0xRRGGBB format).";

      "x", Lang.int_t, Some (Lang.int (-1)),
      Some "x offset (negative means from right)";
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
    ~category:Lang.VideoProcessing
    ~descr:"Display a text (using the SDL library)."
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
         ((new text ~kind init render_text ttf ttf_size color x y speed
                    cycle meta txt source):>Source.source))
