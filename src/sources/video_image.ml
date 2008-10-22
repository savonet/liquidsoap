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

class image fname duration width height x y alpha =
  let nb_frames = Fmt.video_frames_of_seconds duration in
object
  inherit source

  method stype = Infallible
  method is_ready = true

  val mutable remaining = nb_frames
  method remaining = Fmt.ticks_of_video_frames remaining

  val mutable must_fail = false
  method abort_track =
    must_fail <- true;
    remaining <- 0

  val mutable img = None

  val mutable pos_x = x
  val mutable pos_y = y

  initializer
    (* TODO: handle more formats... *)
    let f = RGB.read_ppm ?alpha:(if alpha < 0 then None else Some (RGB.rgb_of_int alpha)) fname in
    let w = if width < 0 then RGB.get_width f else width in
    let h = if height < 0 then RGB.get_height f else height in
    let f =
      if w = RGB.get_width f && h = RGB.get_height f then
        f
      else
        RGB.scale_to f w h
    in
      if x < 0 then pos_x <- Fmt.video_width () - w + x;
      if y < 0 then pos_y <- Fmt.video_height () - h + y;
      img <- Some f

  method get_frame ab =
    if must_fail then begin
      VFrame.add_break ab (VFrame.position ab);
      remaining <- nb_frames;
      must_fail <- false
    end else
      let b = VFrame.get_rgb ab in
      let off = VFrame.position ab in
      let size = VFrame.size ab in
      let img = Utils.get_some img in
        for c = 0 to Array.length b - 1 do
          let buf_c = b.(c) in
            for i = off to size - 1 do
              RGB.blit img buf_c.(i) ~x:pos_x ~y:pos_y
            done;
        done;
        AFrame.add_break ab (AFrame.size ab) ;
        remaining <- remaining - (AFrame.size ab) - off ;
        if remaining <= 0 then must_fail <- true

end

let () =
  Lang.add_operator "video.image"
    ~category:Lang.Input
    ~descr:"Display a static image."
    [
      "width", Lang.int_t, Some (Lang.int (-1)), Some "Scale to width (negative means original width).";
      "height", Lang.int_t, Some (Lang.int (-1)), Some "Scale to width (negative means original height).";
      "x", Lang.int_t, Some (Lang.int 0), Some "x position (negative means from right).";
      "y", Lang.int_t, Some (Lang.int 0), Some "y position (negative means from bottom).";
      "alpha", Lang.int_t, Some (Lang.int (-1)), Some "Color to convert to alpha (in 0xRRGGBB format, negative means no alpha).";
      "duration", Lang.float_t, Some (Lang.float 0.), None;
      "", Lang.string_t, None, Some "Path to image file.";
    ]
    (fun p ->
       let fname, duration, w, h, x, y, alpha =
         let f v = List.assoc v p in
           Lang.to_string (f ""),
           Lang.to_float (f "duration"),
           Lang.to_int (f "width"),
           Lang.to_int (f "height"),
           Lang.to_int (f "x"),
           Lang.to_int (f "y"),
           Lang.to_int (f "alpha")
       in
         (new image fname duration w h x y alpha :> source))
