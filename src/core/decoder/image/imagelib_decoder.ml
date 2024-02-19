(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
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

(* open Mm *)
(* module Img = Image.RGBA32 *)
(* module P = Image.Generic.Pixel *)

let log = Log.make ["decoder"; "imagelib"]

let load_image fname =
  let f = ImageLib_unix.openfile fname in
  let width = f.width in
  let height = f.height in
  let img = Mm.Video.Image.create width height in
  for j = 0 to height - 1 do
    for i = 0 to width - 1 do
      Image.read_rgba f i j (fun r g b a ->
          Mm.Video.Image.set_pixel_rgba img i j (r, g, b, a))
    done
  done;
  img

let () =
  Plug.register Decoder.image_file_decoders "imagelib"
    ~doc:"Use ImageLib to decode images." (fun fname -> Some (load_image fname))
