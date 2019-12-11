(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

module Img = Image.RGBA32
module P = Image.Generic.Pixel

let log = Log.make ["decoder"; "camlimages"]

(* TODO: find something more efficient? *)
let load_image filename =
  let img = OImages.load filename [] in
  let width, height = (img#width, img#height) in
  let p =
    let rgba32_p img i j =
      let p = img#get i j in
      let c = p.Color.color in
      (c.Color.r, c.Color.g, c.Color.b, p.Color.alpha)
    in
    match OImages.tag img with
      | OImages.Rgba32 img ->
          rgba32_p img
      | OImages.Rgb24 img ->
          fun i j ->
            let p = img#get i j in
            (p.Color.r, p.Color.g, p.Color.b, 0xff)
      | OImages.Index8 img ->
          rgba32_p img#to_rgba32
      | OImages.Index16 img ->
          rgba32_p img#to_rgba32
      | OImages.Cmyk32 _ ->
          failwith "CMYK32 images are not supported for now."
  in
  let img = Video.Image.create width height in
  for j = 0 to height - 1 do
    for i = 0 to width - 1 do
      Video.Image.set_pixel_rgba img i j (p i j)
    done
  done ;
  img

let () =
  Decoder.image_file_decoders#register "camlimages"
    ~sdoc:"Use camlimages library to decode images." (fun filename ->
      let img = load_image filename in
      Some img)
