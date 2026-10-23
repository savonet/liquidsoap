(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

module Resampler = Swresample.Make (Swresample.Frame) (Swresample.S16Bytes)
module Scaler = Swscale.Make (Swscale.Frame) (Swscale.Bytes)

let audio frame =
  let layout = Avutil.Audio.frame_get_channel_layout frame in
  let rate = Avutil.Audio.frame_get_sample_rate frame in
  let resampler =
    Resampler.create layout
      ~in_sample_format:(Avutil.Audio.frame_get_sample_format frame)
      rate layout rate
  in
  let data = Resampler.convert resampler frame in
  let tail = Resampler.flush resampler in
  Digest.to_hex (Digest.bytes (Bytes.cat data tail))

let video frame =
  let width = Avutil.Video.frame_get_width frame in
  let height = Avutil.Video.frame_get_height frame in
  let scaler =
    Scaler.create [Swscale.Bicubic] width height
      (Avutil.Video.frame_get_pixel_format frame)
      width height `Yuv420p
  in
  Digest.to_hex
    (Digest.string
       (String.concat ""
          (Array.to_list (Array.map fst (Scaler.convert scaler frame)))))
