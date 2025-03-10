(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

let log = Log.make ["image"; "decoder"]

(* TODO: put this in some library as it can be used in many other places... *)

(** Function to retrieve width an height from parameters. *)
let wh iw ih w h =
  let frame_w = Lazy.force Frame.video_width in
  let frame_h = Lazy.force Frame.video_height in
  match (w, h) with
    | None, None ->
        (* By default resize anamorphically to the maximum size wrt the frame *)
        let w = frame_w in
        let h = ih * frame_w / iw in
        let w, h =
          if h <= frame_h then (w, h)
          else (
            let h = frame_h in
            let w = iw * frame_h / ih in
            (w, h))
        in
        (w, h)
    | Some w, None -> (w, ih * w / iw)
    | None, Some h -> (iw * h / ih, h)
    | Some w, Some h -> (w, h)

let wh_string iw ih w h =
  let frame_w = Lazy.force Frame.video_width in
  let frame_h = Lazy.force Frame.video_height in
  let f d i l =
    if l = "" then None
    else if l.[String.length l - 1] = '%' then (
      let a = float_of_string (String.sub l 0 (String.length l - 1)) /. 100. in
      let d = float_of_int d in
      let l = int_of_float ((a *. d) +. 0.5) in
      Some l)
    else (
      let l = int_of_string l in
      let l = if l < 0 then i else l in
      Some l)
  in
  wh iw ih (f frame_w iw w) (f frame_h ih h)

(* TODO: negative used to mean from right but I don't think it's a good idea
   anymore (for instance to have a scrolling image). *)
let off_string iw ih ox oy =
  let frame_w = Lazy.force Frame.video_width in
  let frame_h = Lazy.force Frame.video_height in
  let f d frame l =
    if l = "" then d
    else if l.[String.length l - 1] = '%' then (
      let a = float_of_string (String.sub l 0 (String.length l - 1)) /. 100. in
      let frame = float_of_int frame in
      let o = int_of_float ((frame *. a) +. 0.5) in
      o)
    else int_of_string l
  in
  let ox = f ((frame_w - iw) / 2) frame_w ox in
  let oy = f ((frame_h - ih) / 2) frame_h oy in
  (ox, oy)

let create_decoder ~ctype ~width ~height ~metadata img =
  let frame_width = width in
  let frame_height = height in
  (* Dimensions. *)
  let img_w = Image.YUV420.width img in
  let img_h = Image.YUV420.height img in
  let width = try Frame.Metadata.find "width" metadata with Not_found -> "" in
  let height =
    try Frame.Metadata.find "height" metadata with Not_found -> ""
  in
  let width, height = wh_string img_w img_h width height in
  (* Offset. *)
  let off_x = try Frame.Metadata.find "x" metadata with Not_found -> "" in
  let off_y = try Frame.Metadata.find "y" metadata with Not_found -> "" in
  let off_x, off_y = off_string width height off_x off_y in
  log#debug "Decoding to %dx%d at %dx%d" width height off_x off_y;
  (* We are likely to have no α channel, optimize it. *)
  Image.YUV420.optimize_alpha img;
  let img =
    Video.Canvas.Image.make ~width:frame_width ~height:frame_height img
  in
  let scaler = Video_converter.scaler () in
  let img =
    Video.Canvas.Image.scale ~scaler (width, img_w) (height, img_h) img
  in
  let img = Video.Canvas.Image.translate off_x off_y img in
  let duration =
    try
      let seconds = float_of_string (Frame.Metadata.find "duration" metadata) in
      if seconds < 0. then -1 else Frame.main_of_seconds seconds
    with Not_found -> -1
  in
  let duration = Atomic.make duration in
  let fclose () = () in
  let remaining () = Atomic.get duration in
  let generator =
    Content.Video.make_generator
      (Content.Video.get_params (Frame.Fields.find Frame.Fields.video ctype))
  in
  let fread length =
    let length =
      match Atomic.get duration with
        | -1 -> length
        | 0 -> 0
        | d ->
            let length = min d length in
            Atomic.set duration (d - length);
            length
    in
    let frame = Frame.create ~length ctype in
    match length with
      | 0 -> frame
      | length -> (
          let video =
            Content.Video.generate
              ~create:(fun ~pos:_ ~width:_ ~height:_ () -> img)
              generator length
          in
          let frame =
            Frame.set_data frame Frame.Fields.video Content.Video.lift_data
              video
          in
          match Frame.Fields.find_opt Frame.Fields.audio ctype with
            | None -> frame
            | Some format ->
                let pcm =
                  Content.Audio.get_data (Content.make ~length format)
                in
                Audio.clear pcm 0 (Frame.audio_of_main length);
                Frame.set_data frame Frame.Fields.audio Content.Audio.lift_data
                  pcm)
  in
  { Decoder.fread; remaining; fseek = (fun len -> len); fclose }

let is_audio_compatible ctype =
  match Frame.Fields.find_opt Frame.Fields.audio ctype with
    | None -> true
    | Some f -> Content.Audio.is_format f

let is_video_compatible ctype =
  match Frame.Fields.find_opt Frame.Fields.video ctype with
    | None -> false
    | Some f -> Content.Video.is_format f

let () =
  Plug.register Decoder.decoders "image" ~doc:"Decoder for static images."
    {
      Decoder.priority = (fun () -> 1);
      file_extensions = (fun () -> None);
      mime_types = (fun () -> None);
      file_type =
        (fun ~metadata:_ ~ctype filename ->
          if
            Decoder.check_image_file_decoder filename
            && is_audio_compatible ctype && is_video_compatible ctype
          then
            Some
              (Frame.Fields.make
                 ?audio:(Frame.Fields.find_opt Frame.Fields.audio ctype)
                 ~video:Content.(default_format Video.kind)
                 ())
          else None);
      file_decoder =
        Some
          (fun ~metadata ~ctype filename ->
            let img = Decoder.get_image_file_decoder filename in
            let width, height =
              Content.Video.dimensions_of_format
                (Option.get (Frame.Fields.find_opt Frame.Fields.video ctype))
            in
            create_decoder ~ctype ~width ~height ~metadata img);
      stream_decoder = None;
    }
