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
  (* This seems to avoid _some_ segfaults, and it's a better practice than
   * not initializing, but what we really need is a clean central module
   * for Sdl, that is thread-safe. Most Sdl stuff is in a streaming
   * thread but there are now several, and image loading is done from
   * duppy tasks. *)
  if Sdl.was_init () = [] then Sdl.init [`VIDEO] ;
  let surface = Sdlloader.load_image filename in
  let image =
    match Sdlvideo.surface_bpp surface with
      | 8 -> from_8 surface
      | 24 -> from_24 surface
      | 32 -> from_32 surface
      | _ -> failwith "unsupported pixel format"
  in
    RGB.proportional_scale_to
      image
      (Lazy.force Frame.video_width)
      (Lazy.force Frame.video_height)

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
    let start = VFrame.position frame in
    let video = (VFrame.content_of_type ~channels:1 frame start).(0) in
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
        RGB.blit img video.(i)
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
