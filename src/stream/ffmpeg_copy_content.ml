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

open Avutil
open Avcodec

type 'a packet = { packet : 'a Packet.t; time_base : Avutil.rational }

module BaseSpecs = struct
  include Ffmpeg_content_base

  type kind = [ `Copy ]

  let kind = `Copy
end

module AudioSpecs = struct
  include BaseSpecs

  let string_of_kind = function `Copy -> "ffmpeg.audio.copy"
  let kind_of_string = function "ffmpeg.audio.copy" -> Some `Copy | _ -> None

  type param = audio Avcodec.params
  type data = (param, audio packet) content

  let mk_param params = params

  let string_of_param params =
    let id = Audio.get_params_id params in
    let channel_layout = Audio.get_channel_layout params in
    let sample_format = Audio.get_sample_format params in
    let sample_rate = Audio.get_sample_rate params in

    Printf.sprintf "codec=%S,channel_layout=%S,sample_format=%S,sample_rate=%d"
      (Audio.string_of_id id)
      (Channel_layout.get_description channel_layout)
      (Sample_format.get_name sample_format)
      sample_rate

  let check p p' =
    Audio.get_params_id p = Audio.get_params_id p'
    && Audio.get_channel_layout p = Audio.get_channel_layout p'
    && Audio.get_sample_format p = Audio.get_sample_format p'
    && Audio.get_sample_rate p = Audio.get_sample_rate p'

  let compatible = compatible ~check
  let merge = merge ~check ~merge_p:(fun p _ -> p)
end

module Audio = Frame_content.MkContent (AudioSpecs)

module VideoSpecs = struct
  include BaseSpecs

  let string_of_kind = function `Copy -> "ffmpeg.video.copy"
  let kind_of_string = function "ffmpeg.video.copy" -> Some `Copy | _ -> None

  type param = video Avcodec.params
  type data = (param, video packet) content

  let mk_param params = params

  let string_of_param params =
    let id = Video.get_params_id params in
    let width = Video.get_width params in
    let height = Video.get_height params in
    let sample_aspect_ratio = Video.get_sample_aspect_ratio params in
    let pixel_format = Video.get_pixel_format params in
    Printf.sprintf "codec=%S,size=\"%dx%d\",sample_ratio=%S,pixel_format=%S)"
      (Video.string_of_id id) width height
      (string_of_rational sample_aspect_ratio)
      ( match pixel_format with
        | None -> "unknown"
        | Some pf -> Pixel_format.to_string pf )

  let check p p' =
    Video.get_params_id p = Video.get_params_id p'
    && Video.get_width p = Video.get_width p'
    && Video.get_height p = Video.get_height p'
    && Video.get_sample_aspect_ratio p = Video.get_sample_aspect_ratio p'
    && Video.get_pixel_format p = Video.get_pixel_format p'

  let compatible = compatible ~check
  let merge = merge ~check ~merge_p:(fun p _ -> p)
end

module Video = Frame_content.MkContent (VideoSpecs)
