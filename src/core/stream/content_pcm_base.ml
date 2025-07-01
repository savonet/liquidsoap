(*****************************************************************************

  Liquidsoap, a programmable stream generator.
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

open Frame_settings

type params = Content_audio.Specs.params

let string_of_params = Content_audio.Specs.string_of_params
let merge = Content_audio.Specs.merge
let compatible = Content_audio.Specs.compatible

let blit src src_pos dst dst_pos len =
  let ( ! ) = audio_of_main in
  let src_pos = !src_pos in
  let dst_pos = !dst_pos in
  let len = !len in
  Array.iter2
    (fun src dst ->
      Bigarray.Array1.blit
        (Bigarray.Array1.sub src src_pos len)
        (Bigarray.Array1.sub dst dst_pos len))
    src dst

let copy ~fmt =
  Array.map (fun c ->
      let c' =
        Bigarray.Array1.create fmt Bigarray.c_layout (Bigarray.Array1.dim c)
      in
      Bigarray.Array1.blit c c';
      c)

let free _ = ()
let param_of_channels = Content_audio.Specs.param_of_channels
let parse_param = Content_audio.Specs.parse_param
let params d = param_of_channels (Array.length d)

let default_params _ =
  param_of_channels (Lazy.force Frame_settings.audio_channels)

let make ~fmt ?(length = 0) { Content_audio.Specs.channel_layout } =
  let channels =
    match !!channel_layout with
      | `Mono -> 1
      | `Stereo -> 2
      | `Five_point_one -> 6
  in
  Array.init channels (fun _ ->
      Bigarray.Array1.create fmt Bigarray.c_layout (audio_of_main length))

let length = function
  | [||] -> 0
  | c -> main_of_audio (Bigarray.Array1.dim c.(0))

let clear_content ~v b ofs len =
  Array.iter (fun c -> Bigarray.Array1.fill (Bigarray.Array1.sub c ofs len) v) b

let channels_of_format ~get_params p =
  Content_audio.Specs.(
    channels_of_param (Lazy.force (get_params p).channel_layout))
