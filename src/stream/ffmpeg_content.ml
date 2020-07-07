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

type 'a packet = {
  params : 'a Avcodec.params;
  packet : 'a Packet.t;
  time_base : Avutil.rational;
}

module BaseCopySpecs = struct
  type kind = [ `Copy ]
  type 'a content = (int * 'a packet) list ref

  let make _ = ref []
  let clear d = d := []
  let param_of_string _ _ = None

  let bytes data =
    List.fold_left (fun c (_, { packet }) -> c + Packet.size packet) 0 !data

  let blit ~pos src src_pos dst dst_pos len =
    let src_start = pos src_pos in
    let src_end = pos (src_pos + len) in
    List.iter
      (fun (pos, p) ->
        if src_start <= pos && pos < src_end then (
          let pos = dst_pos + (pos - src_pos) in
          dst := (pos, p) :: !dst ))
      !src

  let copy data = ref !data
  let kind = `Copy
  let default_params _ = []

  let merge l l' =
    match (l, l') with
      | [], [p'] -> [p']
      | [p], [] -> [p]
      | [p], [p'] when p = p' -> [p]
      | _ -> failwith "Incompatible parameters"
end

module AudioCopySpecs = struct
  include BaseCopySpecs

  type param = {
    id : Audio.id;
    channel_layout : Channel_layout.t;
    sample_format : Sample_format.t;
    sample_rate : int;
  }

  type data = audio content

  let mk_param params =
    {
      id = Audio.get_params_id params;
      channel_layout = Audio.get_channel_layout params;
      sample_format = Audio.get_sample_format params;
      sample_rate = Audio.get_sample_rate params;
    }

  let params c =
    match !c with [] -> [] | (_, { params }) :: _ -> [mk_param params]

  let blit = blit ~pos:Frame.audio_of_master
  let string_of_kind = function `Copy -> "ffmpeg.encoded"
  let kind_of_string = function "ffmpeg.encoded" -> Some `Copy | _ -> None

  let string_of_param { id; channel_layout; sample_format; sample_rate } =
    Printf.sprintf "codec=%S,channel_layout=%S,sample_format=%S,sample_rate=%d"
      (Audio.string_of_id id)
      (Channel_layout.get_description channel_layout)
      (Sample_format.get_name sample_format)
      sample_rate
end

module AudioCopy = Frame_content.MkContent (AudioCopySpecs)

module VideoCopySpecs = struct
  include BaseCopySpecs

  type param = {
    id : Video.id;
    width : int;
    height : int;
    sample_aspect_ratio : Avutil.rational;
    pixel_format : Avutil.Pixel_format.t option;
  }

  type data = video content

  let mk_param params =
    {
      id = Video.get_params_id params;
      width = Video.get_width params;
      height = Video.get_height params;
      sample_aspect_ratio = Video.get_sample_aspect_ratio params;
      pixel_format = Video.get_pixel_format params;
    }

  let params c =
    match !c with [] -> [] | (_, { params }) :: _ -> [mk_param params]

  let blit = blit ~pos:Frame.video_of_master
  let string_of_kind = function `Copy -> "ffmpeg.encoded"
  let kind_of_string = function "ffmpeg.encoded" -> Some `Copy | _ -> None

  let string_of_param { id; width; height; sample_aspect_ratio; pixel_format } =
    Printf.sprintf "codec=%S,size=\"%dx%d\",sample_ratio=%S,pixel_format=%S)"
      (Video.string_of_id id) width height
      (string_of_rational sample_aspect_ratio)
      ( match pixel_format with
        | None -> "unknown"
        | Some pf -> Pixel_format.to_string pf )
end

module VideoCopy = Frame_content.MkContent (VideoCopySpecs)
