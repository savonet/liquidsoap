(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2013 Savonet team

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

let read_PPM ?alpha fname =
  let ic = open_in_bin fname in
  let len = in_channel_length ic in
  let data = String.create len in
  really_input ic data 0 len;
  close_in ic;
  Img.of_PPM ?alpha data

(** The content kind should allow for pure video,
  * we handle any number of channels. *)
class image kind fname duration width height x y alpha meta source =
object (self)
  inherit operator ~name:"video.add_image" kind [source] as super

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
        read_PPM
          ?alpha:(if alpha < 0 then
                    None
                  else
                    Some (Image.RGB8.Color.of_int alpha))
          fname
      in
      let fw, fh = Img.dimensions f in
      let w = if width < 0 then fw else width in
      let h = if height < 0 then fh else height in
      let f =
        if w = fw && h = fh then
          f
        else
          (* TODO: optionally proportional + option for scaling kind. *)
          Img.Scale.create f w h
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
    let master_offset = Frame.position ab in
    let content = VFrame.get_content ab source in
      begin match meta, Frame.get_metadata ab master_offset with
        | Some meta, Some m ->
            begin try
              self#load (Hashtbl.find m meta)
            with
              | Not_found -> ()
            end
        | _ -> ()
      end ;
      match content with
        | Some (rgb,off,len) ->
            let rgb = rgb.(0) in
              begin match img with
                | Some img ->
                    for i = off to off+len-1 do
                      Img.add img rgb.(i) ~x:pos_x ~y:pos_y
                    done
                | None -> ()
              end
        | None -> ()

end

let () =
  let k =
    Lang.kind_type_of_kind_format ~fresh:2 (Lang.any_fixed_with ~video:1 ())
  in
  Lang.add_operator "video.add_image"
    ~category:Lang.VideoProcessing
    ~descr:"Add a static image on the first video channel. \
            The image can be changed based on metadata \
            found at the beginning of a track."
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
