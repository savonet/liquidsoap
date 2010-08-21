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

let () = Sdl.init [`VIDEO]

(** 8bit surfaces always use a palette *)
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

(** 16bits surfaces contain specially packed RGB *)
let to_16 rgb surface =
  let s = Sdlvideo.pixel_data_16 surface in
  let width,height,pitch = Sdlvideo.surface_dims surface in
  let pitch = pitch/2 in (* initial pitch was in bytes *)
  let fmt = Sdlvideo.surface_format surface in
  let stride = rgb.RGB.stride in
    assert (width = rgb.RGB.width && height = rgb.RGB.height) ;
    assert (fmt.Sdlvideo.amask = 0l && not fmt.Sdlvideo.palette) ;
    let rgb = rgb.RGB.data in
    for i = 0 to width-1 do
      for j = 0 to height-1 do
        let pos = i*4+j*stride in
        let r = rgb.{pos} in
        let g = rgb.{1+pos} in
        let b = rgb.{2+pos} in
        let color =
          ((r lsr fmt.Sdlvideo.rloss) lsl fmt.Sdlvideo.rshift) lor
          ((g lsr fmt.Sdlvideo.gloss) lsl fmt.Sdlvideo.gshift) lor
          ((b lsr fmt.Sdlvideo.bloss) lsl fmt.Sdlvideo.bshift)
        in
        (* let color = Int32.to_int (Sdlvideo.map_RGB surface (r,g,b)) in *)
          s.{i+j*pitch} <- color
      done
    done

(** 24bits surfaces are standard RGB stored in three different bytes,
  * but the order might vary. *)
let from_24 surface =
  let width,height,pitch = Sdlvideo.surface_dims surface in
  let fmt = Sdlvideo.surface_format surface in
  let rgb = Sdlvideo.pixel_data_24 surface in
  let a = RGB.create width height in
  let rgba = a.RGB.data in
    for i = 0 to width-1 do
      for j = 0 to height-1 do
        for c = 0 to 2 do
          let c' = if fmt.Sdlvideo.rshift = 0 then c else 2-c in
            rgba.{c+i*4+j*a.RGB.stride} <- rgb.{c'+i*3+j*pitch}
        done ;
        rgba.{3+i*4+j*a.RGB.stride} <- 0xff
      done
    done ;
    a

(** 32bits surfaces are standard RGBA
  * However, the RGB components are (at least sometimes) packed in
  * a different order as in liquidsoap: 0xAARRGGBB.
  *
  * An alternative implementation, which is surprisingly not sensibly
  * faster, uses SDL blitting directly by casting a char* into an int*.
  * The alpha is masked out because we don't want
  * to see video frames on top of each other on screen.
  * This hack might not work the same on different platforms.
      let s =
        Sdlvideo.create_RGB_surface_from_32
        (Obj.magic rgb.RGB.data)
        ~w:rgb.RGB.width
        ~h:rgb.RGB.height
        ~pitch:rgb.RGB.stride
        (* The masks might be endianness dependent *)
        ~rmask:0xffl ~gmask:0xff00l ~bmask:0xff0000l
        ~amask:0l
      in
        Sdlvideo.blit_surface ~src:s ~dst:surface ()
  *)
let to_32 rgb surface =
  let s = Sdlvideo.pixel_data_32 surface in
  let width,height,pitch = Sdlvideo.surface_dims surface in
  let pitch = pitch/4 in (* initial pitch was in bytes *)
  let fmt = Sdlvideo.surface_format surface in
  assert (width = rgb.RGB.width && height = rgb.RGB.height) ;
  assert (fmt.Sdlvideo.amask = 0l && not fmt.Sdlvideo.palette) ;
  let stride = rgb.RGB.stride in
  let rgb = rgb.RGB.data in
    for i = 0 to width-1 do
      for j = 0 to height-1 do
        let pos = i*4+j*stride in
        let color =
          Int32.of_int
            ((rgb.{pos} lsl fmt.Sdlvideo.rshift) lor
             (rgb.{1+pos} lsl fmt.Sdlvideo.gshift) lor
             (rgb.{2+pos} lsl fmt.Sdlvideo.bshift))
        in
          s.{i+j*pitch} <- color
      done
    done

let from_32 surface =
  let img = Sdlvideo.pixel_data_32 surface in
  let width,height,pitch = Sdlvideo.surface_dims surface in
  let fmt = Sdlvideo.surface_format surface in
  let pitch = pitch/4 in (* pitch is in bytes, convert for int32 array *)
  let a = RGB.create width height in
  let rgba = a.RGB.data in
  assert (fmt.Sdlvideo.rloss = 0 &&
          fmt.Sdlvideo.gloss = 0 &&
          fmt.Sdlvideo.bloss = 0) ;
  let (&&) = Int32.logand in
  let (>>) = Int32.shift_right in
    for i = 0 to width-1 do
      for j = 0 to height-1 do
        let pixel = img.{i+j*pitch} in
        let pos = i*4+j*a.RGB.stride in
        rgba.{0+pos} <-
          Int32.to_int ((pixel && fmt.Sdlvideo.rmask) >> fmt.Sdlvideo.rshift) ;
        rgba.{1+pos} <-
          Int32.to_int ((pixel && fmt.Sdlvideo.gmask) >> fmt.Sdlvideo.gshift) ;
        rgba.{2+pos} <-
          Int32.to_int ((pixel && fmt.Sdlvideo.bmask) >> fmt.Sdlvideo.bshift) ;
        rgba.{3+pos} <-
          Int32.to_int ((pixel && fmt.Sdlvideo.amask) >> fmt.Sdlvideo.ashift)
      done
    done ;
    a
