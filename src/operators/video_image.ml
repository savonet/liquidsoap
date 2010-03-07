(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

(** The content kind should allow for pure video,
  * we handle any number of channels. *)
class image kind fname duration width height x y alpha meta source =
object (self)
  inherit operator kind [source] as super

  method stype = source#stype

  method remaining = source#remaining
  method is_ready = source#is_ready
  method abort_track = source#abort_track

  val mutable img = None

  val mutable pos_x = x
  val mutable pos_y = y

  method private load fname =
    try
      let f =
        (* TODO: Handle more formats. *)
        RGB.read_ppm
          ?alpha:(if alpha < 0 then None else Some (RGB.rgb_of_int alpha))
          fname
      in
      let w = if width < 0 then f.RGB.width else width in
      let h = if height < 0 then f.RGB.height else height in
      let f =
        if w = f.RGB.width && h = f.RGB.height then
          f
        else
          RGB.scale_to f w h
      in
        if x < 0 then pos_x <- (Lazy.force Frame.video_width) - w + x;
        if y < 0 then pos_y <- (Lazy.force Frame.video_height) - h + y;
        img <- Some f
    with
      | _ ->
          self#log#f 3 "Could not open file %s." fname;
          img <- None

  initializer
    self#load fname

  method private get_frame ab =
    let off = VFrame.position ab in
    source#get ab;
    let rgb = (VFrame.content ab off).(0) in
    let size = VFrame.size ab in
    let read_from_meta () =
      match meta with
        | Some meta ->
            List.iter
              (fun (t,m) ->
                 try
                   self#load (Hashtbl.find m meta)
                 with
                   | Not_found -> ()
              ) (Frame.get_all_metadata ab)
        | None -> ()
    in
      (* TODO: be able to load an image in the middle of a frame? *)
      read_from_meta ();
      match img with
        | Some img ->
            (* TODO: Handle other channels? *)
            for i = off to size - 1 do
              RGB.add img rgb.(i) ~x:pos_x ~y:pos_y
            done
        | None -> ()
end

let () =
  let k =
    Lang.kind_type_of_kind_format ~fresh:2 (Lang.any_fixed_with ~video:1 ())
  in
  Lang.add_operator "video.add_image"
    ~category:Lang.Input
    ~descr:"Add a static image on the first video channel. \
            The image can be changed based on metadata."
    [
      "width", Lang.int_t, Some (Lang.int (-1)),
      Some "Scale to width (negative means original width).";

      "height", Lang.int_t, Some (Lang.int (-1)),
      Some "Scale to width (negative means original height).";

      "x", Lang.int_t, Some (Lang.int 0),
      Some "x position (negative means from right).";

      "y", Lang.int_t, Some (Lang.int 0),
      Some "y position (negative means from bottom).";

      "alpha", Lang.int_t, Some (Lang.int (-1)),
      Some "Color to convert to alpha \
            (in 0xRRGGBB format, negative means no alpha).";

      "duration", Lang.float_t, Some (Lang.float 0.), None;

      "file", Lang.string_t, Some (Lang.string ""),
      Some "Path to image file.";

      "metadata", Lang.string_t, Some (Lang.string ""),
      Some "Metadata on which file name should be read (empty means disabled).";

      "", Lang.source_t k, None, None;
    ]
    ~kind:(Lang.Unconstrained k)
    (fun p kind ->
       let fname, duration, w, h, x, y, alpha, meta, source =
         let f v = List.assoc v p in
           Lang.to_string (f "file"),
           Lang.to_float (f "duration"),
           Lang.to_int (f "width"),
           Lang.to_int (f "height"),
           Lang.to_int (f "x"),
           Lang.to_int (f "y"),
           Lang.to_int (f "alpha"),
           Lang.to_string (f "metadata"),
           Lang.to_source (f "")
       in
       let meta = if meta = "" then None else Some meta in
         new image kind fname duration w h x y alpha meta source)
