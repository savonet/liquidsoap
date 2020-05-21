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

let log = Log.make ["decoder"; "ppm"]

let load_image fname =
  let ic = open_in_bin fname in
  let len = in_channel_length ic in
  let data = Bytes.create len in
  really_input ic data 0 len;
  close_in ic;
  Image.YUV420.of_PPM (Bytes.unsafe_to_string data)

let () =
  Decoder.image_file_decoders#register "ppm"
    ~sdoc:"Native decoding of PPM images." (fun filename ->
      let img = load_image filename in
      Some img)
