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

(* We need to keep track of both the original ['a Avcodec.params]
   and our internal param type (['b] in [type ('a, 'b) content].

   ['a Avcodec.params] is used to create the encoded stream while
   our internal param is used to display, compare & unify stream
   types. *)

type 'a packet = {
  params : 'a Avcodec.params;
  packet : 'a Packet.t;
  time_base : Avutil.rational;
}

type ('a, 'b) content = { param : 'b; mutable data : (int * 'a packet) list }

module BaseSpecs = struct
  type kind = [ `Copy ]

  let make = function [param] -> { param; data = [] } | _ -> assert false
  let clear d = d.data <- []
  let param_of_string _ _ = None

  let bytes { data } =
    List.fold_left (fun c (_, { packet }) -> c + Packet.get_size packet) 0 data

  let blit src src_pos dst dst_pos len =
    let src_end = src_pos + len in
    let data =
      List.fold_left
        (fun data (pos, p) ->
          if src_pos <= pos && pos < src_end then (
            let pos = dst_pos + (pos - src_pos) in
            (pos, p) :: data )
          else data)
        dst.data src.data
    in
    dst.data <- data

  let copy { data; param } = { data; param }
  let kind = `Copy
  let default_params _ = []
  let params { param } = [param]

  let merge l l' =
    match (l, l') with
      | [], [p'] -> [p']
      | [p], [] -> [p]
      | [p], [p'] when p = p' -> [p]
      | _ -> failwith "Incompatible parameters"
end

module AudioSpecs = struct
  include BaseSpecs

  type param = {
    id : Audio.id;
    channel_layout : Channel_layout.t;
    sample_format : Sample_format.t;
    sample_rate : int;
  }

  type data = (audio, param) content

  let mk_param params =
    {
      id = Audio.get_params_id params;
      channel_layout = Audio.get_channel_layout params;
      sample_format = Audio.get_sample_format params;
      sample_rate = Audio.get_sample_rate params;
    }

  let string_of_kind = function `Copy -> "ffmpeg.encoded"
  let kind_of_string = function "ffmpeg.encoded" -> Some `Copy | _ -> None

  let string_of_param { id; channel_layout; sample_format; sample_rate } =
    Printf.sprintf "codec=%S,channel_layout=%S,sample_format=%S,sample_rate=%d"
      (Audio.string_of_id id)
      (Channel_layout.get_description channel_layout)
      (Sample_format.get_name sample_format)
      sample_rate
end

module Audio = Frame_content.MkContent (AudioSpecs)

module VideoSpecs = struct
  include BaseSpecs

  type param = {
    id : Video.id;
    width : int;
    height : int;
    sample_aspect_ratio : Avutil.rational;
    pixel_format : Avutil.Pixel_format.t option;
  }

  type data = (video, param) content

  let mk_param params =
    {
      id = Video.get_params_id params;
      width = Video.get_width params;
      height = Video.get_height params;
      sample_aspect_ratio = Video.get_sample_aspect_ratio params;
      pixel_format = Video.get_pixel_format params;
    }

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

module Video = Frame_content.MkContent (VideoSpecs)
