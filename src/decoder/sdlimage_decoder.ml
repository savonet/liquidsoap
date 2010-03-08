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

let from_8 surface =
  let width,height,pitch = Sdlvideo.surface_dims surface in
  let image = Sdlvideo.pixel_data_8 surface in
  let a = RGB.create width height in
    for i = 0 to width-1 do
      for j = 0 to height-1 do
        let r,g,b = Sdlvideo.get_palette_color surface image.{i+j*pitch} in
          RGB.set_pixel a i j (r,g,b,0xff)
      done
    done ;
    a

let from_24 surface =
  let width,height,pitch = Sdlvideo.surface_dims surface in
  let rgb = Sdlvideo.pixel_data_24 surface in
  let a = RGB.create width height in
  let rgba = a.RGB.data in
    for i = 0 to width-1 do
      for j = 0 to height-1 do
        for c = 0 to 2 do
          rgba.{c+i*4+j*a.RGB.stride} <- rgb.{c+i*3+j*pitch}
        done ;
        rgba.{3+i*4+j*a.RGB.stride} <- 0xff
      done
    done ;
    a

let from_32 surface =
  let img = Sdlvideo.pixel_data_32 surface in
  let width,height,pitch = Sdlvideo.surface_dims surface in
  let pitch = pitch/4 in (* pitch is in bytes, convert for int32 array *)
  let a = RGB.create width height in
  let rgba = a.RGB.data in
  let (&&) = Int32.logand in
  let (>>) = Int32.shift_right in
    for i = 0 to width-1 do
      for j = 0 to height-1 do
        rgba.{0+i*4+j*a.RGB.stride} <-
                Int32.to_int ((img.{i+j*pitch} && 0x000000ffl) >> 0) ;
        rgba.{1+i*4+j*a.RGB.stride} <-
                Int32.to_int ((img.{i+j*pitch} && 0x0000ff00l) >> 8) ;
        rgba.{2+i*4+j*a.RGB.stride} <-
                Int32.to_int ((img.{i+j*pitch} && 0x00ff0000l) >> 16) ;
        rgba.{3+i*4+j*a.RGB.stride} <-
                Int32.to_int ((img.{i+j*pitch} && 0xff000000l) >> 24)
      done
    done ;
    a

let load_image filename =
  let surface = Sdlloader.load_image filename in
  let image =
    match (Sdlvideo.surface_format surface).Sdlvideo.bytes_pp with
      | 1 -> from_8 surface
      | 3 -> from_24 surface
      | 4 -> from_32 surface
      | _ -> failwith "unsupported pixel format"
  in
    RGB.proportional_scale_to
      image
      (Lazy.force Frame.video_width)
      (Lazy.force Frame.video_height)

let () =
  Decoder.file_decoders#register "SDL/image"
    ~sdoc:"Use SDL to display static images."
    (fun filename kind ->
       let ctype = { Frame. video = 1 ; audio = 0 ; midi = 0 } in
         if Frame.type_has_kind ctype kind then
           let img = load_image filename in
           let close () = () in
           let fill frame =
             let pos = VFrame.position frame in
             let video = (VFrame.content_of_type ~channels:1 frame pos).(0) in
               for i = pos to VFrame.size frame - 1 do
                 (* If we had [Frame.set_content] (not the unsafe version
                  * but one that blits only if necessary) we could avoid
                  * the creation of empty frames that we're going to
                  * replace anyway. Anyway, in most cases we're only changing
                  * an existing content layer. *)
                 RGB.blit img video.(i)
               done ;
               VFrame.add_break frame (VFrame.size frame) ;
               -1
           in
             Some (fun () -> { Decoder. fill = fill ; close = close })
         else begin
           None
       end)
