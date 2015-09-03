(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2015 Savonet team

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
  log#f 4 "SDL loaded %S as %dbpp." filename (Sdlvideo.surface_bpp surface);
  match Sdlvideo.surface_bpp surface with
  | 8 -> Sdl_utils.from_8 surface
  | 24 -> Sdl_utils.from_24 surface
  | 32 -> Sdl_utils.from_32 surface
  | _ -> failwith "unsupported pixel format"

let () =
  Decoder.image_file_decoders#register "SDL/image"
    ~sdoc:"Use SDL to decode images."
    (fun filename ->
      let img = load_image filename in
      Some img)
