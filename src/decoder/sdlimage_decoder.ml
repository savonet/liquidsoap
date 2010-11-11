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

module Img = Image.RGBA32
module P = Image.Generic.Pixel

let log = Dtools.Log.make ["decoder";"sdlimage"]

let load_image filename =
  let surface = Sdlloader.load_image filename in
  let image =
    log#f 4 "SDL loaded %S as %dbpp." filename (Sdlvideo.surface_bpp surface) ;
    match Sdlvideo.surface_bpp surface with
      | 8 -> Sdl_utils.from_8 surface
      | 24 -> Sdl_utils.from_24 surface
      | 32 -> Sdl_utils.from_32 surface
      | _ -> failwith "unsupported pixel format"
  in
  let convert =
    Video_converter.find_converter
      (P.RGB P.RGBA32)
      (P.RGB P.RGBA32)
  in
  let frame = Img.create (Lazy.force Frame.video_width)
                         (Lazy.force Frame.video_height)
  in
  convert (Image.Generic.of_RGBA32 image)
          (Image.Generic.of_RGBA32 frame) ;
  frame

let create_decoder metadata img =
  let duration =
    ref (try
           let seconds =
             float_of_string (Hashtbl.find metadata "duration")
           in
             Frame.video_of_seconds seconds
         with
           | Not_found -> -1)
  in
  let close () = () in
  let fill frame =
    let video = (VFrame.content_of_type ~channels:1 frame).(0) in
    let start = VFrame.next_sample_position frame in
    let stop =
      if !duration = -1 then VFrame.size frame else
        min (VFrame.size frame) (start + !duration)
    in
      VFrame.add_break frame stop ;
      for i = start to stop-1 do
        (* One could think of avoiding the creation of a blank
         * video layer that will be overwritten immediately.
         * However, in most cases an old layer will be re-used.
         * In fact, we might even need to explicitly blankify
         * because our image might be transparent and the
         * current frame might contain random stuff. TODO *)
        Img.blit img video.(i)
      done ;
      if !duration = -1 then -1 else begin
        duration := !duration - (stop-start) ;
        Frame.master_of_video !duration
      end
  in
    { Decoder. fill = fill ; close = close }

let () =
  Decoder.file_decoders#register "SDL/image"
    ~sdoc:"Use SDL to display static images."
    (fun ~metadata filename kind ->
       let ctype = { Frame. video = 1 ; audio = 0 ; midi = 0 } in
         if Frame.type_has_kind ctype kind then
           let img = load_image filename in
             Some (fun () -> create_decoder metadata img)
         else
           None)
