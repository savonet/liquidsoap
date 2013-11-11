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

module Img = Image.RGBA32
module Gen = Image.Generic

(** Converter used to resize images. *)
let converter =
  Lazy.lazy_from_fun
    (fun () ->
      Video_converter.find_converter
        (Gen.Pixel.RGB Gen.Pixel.RGBA32)
        (Gen.Pixel.RGB Gen.Pixel.RGBA32))

(** Function to retrieve width an height from parameters. *)
(* TODO: put this in some library as it can be used in many other places... *)
let wh iw ih w h =
  let frame_w = Lazy.force Frame.video_width in
  let frame_h = Lazy.force Frame.video_height in
  match w, h with
  | None, None ->
    (* By default resize anamorphically to the maximum size wrt the frame *)
    let w = frame_w in
    let h = ih * frame_w / iw in
    let w, h =
      if h <= frame_h then w, h else
        let h = frame_h in
        let w = iw * frame_h / ih in
        w, h
    in
    w, h
  | Some w, None -> w, ih*w/iw
  | None, Some h -> iw*h/ih, h
  | Some w, Some h -> w, h

let wh_string iw ih w h =
  let frame_w = Lazy.force Frame.video_width in
  let frame_h = Lazy.force Frame.video_height in
  let f d i l =
    if l = "" then None
    else if l.[String.length l - 1] = '%' then
      let a = float_of_string (String.sub l 0 (String.length l - 1)) /. 100. in
      let d = float_of_int d in
      let l = int_of_float (a *. d +. 0.5) in
      Some l
    else
      let l = int_of_string l in
      let l = if l < 0 then i else l in
      Some l
  in
  wh iw ih (f frame_w iw w) (f frame_h ih h)

(* TODO: negative used to mean from right but I don't think it's a good idea
   anymore (for instance to have a scrolling image). *)
let off_string iw ih ox oy =
  let frame_w = Lazy.force Frame.video_width in
  let frame_h = Lazy.force Frame.video_height in
  let f d frame l =
    if l = "" then d
    else if l.[String.length l - 1] = '%' then
      let a = float_of_string (String.sub l 0 (String.length l - 1)) /. 100. in
      let frame = float_of_int frame in
      let o = int_of_float (frame *. a +. 0.5) in
      o
    else
      int_of_string l
  in
  let ox = f ((frame_w-iw)/2) frame_w ox in
  let oy = f ((frame_h-ih)/2) frame_h oy in
  ox, oy

let create_decoder metadata img =
  (* Dimensions. *)
  let img_w, img_h = Img.dimensions img in
  let width = try Hashtbl.find metadata "width" with Not_found -> "" in
  let height = try Hashtbl.find metadata "height" with Not_found -> "" in
  let width, height = wh_string img_w img_h width height in
  (* Offset. *)
  let off_x = try Hashtbl.find metadata "x" with Not_found -> "" in
  let off_y = try Hashtbl.find metadata "y" with Not_found -> "" in
  let off_x, off_y = off_string width height off_x off_y in
  let img =
    let img =
      if (width,height) = (img_w,img_h) then img else
        (* Img.Scale.create img width height *)
        let img' = Img.create width height in
        let converter = Lazy.force converter in
        converter (Gen.of_RGBA32 img) (Gen.of_RGBA32 img');
        img'
    in
    let img =
      if (off_x,off_y) = (0,0) then img else
        let img' = Img.create (width+off_x) (height+off_y) in
        Img.blank_all img';
        Img.add img img' ~x:off_x ~y:off_y;
        img'
    in
    img
  in
  let duration =
    try
      let seconds = float_of_string (Hashtbl.find metadata "duration") in
      if seconds < 0. then -1 else Frame.video_of_seconds seconds
    with
    | Not_found -> -1
  in
  let duration = ref duration in
  let close () = () in
  let fill frame =
    let video = (VFrame.content_of_type ~channels:1 frame).(0) in
    let start = VFrame.next_sample_position frame in
    let stop =
      if !duration = -1 then VFrame.size frame else
        min (VFrame.size frame) (start + !duration)
    in
    VFrame.add_break frame stop;
    for i = start to stop-1 do
      (* TODO: One could think of avoiding the creation of a blank video layer
       * that will be overwritten immediately. However, in most cases an old
       * layer will be re-used.  In fact, we might even need to explicitly
       * blankify because our image might be transparent and the current frame
       * might contain random stuff. *)
      Img.blit img video.(i)
    done ;
    if !duration = -1 then -1 else begin
      duration := !duration - (stop-start);
      Frame.master_of_video !duration
    end
  in
  { Decoder.
    fill = fill;
    fseek = (fun _ -> 0);
    close = close }

let () =
  Decoder.file_decoders#register "Image"
    ~sdoc:"Decoder for static images."
    (fun ~metadata filename kind ->
      let ctype = { Frame. video = 1; audio = 0; midi = 0 } in
      try
        if not (Frame.type_has_kind ctype kind) then raise Exit;
        let img =
          match Decoder.get_image_file_decoder filename with
          | Some img -> img
          | None -> failwith "Could not decode image file."
        in
        Some (fun () -> create_decoder metadata img)
      with
      | _ -> None
    )

