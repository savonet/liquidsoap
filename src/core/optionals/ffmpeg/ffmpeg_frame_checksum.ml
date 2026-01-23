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

open Avutil
module AudioConverter = Swresample.Make (Swresample.Frame) (Swresample.Bytes)
module VideoScaler = Swscale.Make (Swscale.Frame) (Swscale.Bytes)

let checksum_of_audio_frame frame =
  let channel_layout = Audio.frame_get_channel_layout frame in
  let sample_format = Audio.frame_get_sample_format frame in
  let sample_rate = Audio.frame_get_sample_rate frame in
  let converter =
    AudioConverter.create channel_layout ~in_sample_format:sample_format
      sample_rate channel_layout ~out_sample_format:`S16 sample_rate
  in
  let audio_bytes = AudioConverter.convert converter frame in
  Digest.bytes audio_bytes |> Digest.to_hex

let checksum_of_video_frame frame =
  let width = Video.frame_get_width frame in
  let height = Video.frame_get_height frame in
  let pixel_format = Video.frame_get_pixel_format frame in
  let scaler =
    VideoScaler.create [] width height pixel_format width height `Yuv420p
  in
  let planes = VideoScaler.convert scaler frame in
  let data = String.concat "" (Array.to_list (Array.map fst planes)) in
  Digest.string data |> Digest.to_hex
