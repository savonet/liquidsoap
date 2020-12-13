(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2020 Savonet team

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
  stream_idx : Int64.t;
  time_base : Avutil.rational;
  packet : 'a Packet.t;
}

let stream_idx = ref 0L

let new_stream_idx () =
  stream_idx := Int64.succ !stream_idx;
  !stream_idx

module BaseSpecs = struct
  include Ffmpeg_content_base

  type kind = [ `Copy ]

  let kind = `Copy
  let parse_param _ _ = None

  let merge ~compatible p p' =
    match (p, p') with
      | None, p | p, None -> p
      | p, p' when compatible p p' -> p
      | _ -> failwith "Incompatible format!"
end

module AudioSpecs = struct
  include BaseSpecs

  let string_of_kind = function `Copy -> "ffmpeg.audio.copy"
  let kind_of_string = function "ffmpeg.audio.copy" -> Some `Copy | _ -> None

  type params = audio Avcodec.params option
  type data = (params, audio packet) content

  let string_of_params params =
    Frame_content.print_optional
      [
        ( "codec",
          Option.map
            (fun p ->
              Printf.sprintf "%S" (Audio.string_of_id (Audio.get_params_id p)))
            params );
        ( "channel_layout",
          Option.map
            (fun p ->
              Printf.sprintf "%S"
                (Channel_layout.get_description (Audio.get_channel_layout p)))
            params );
        ( "sample_format",
          Option.map
            (fun p ->
              Printf.sprintf "%S"
                (Sample_format.get_name (Audio.get_sample_format p)))
            params );
        ( "sample_rate",
          Option.map (fun p -> string_of_int (Audio.get_sample_rate p)) params
        );
      ]

  let compatible p p' =
    match (p, p') with
      | None, _ | _, None -> true
      | Some p, Some p' ->
          Audio.get_params_id p = Audio.get_params_id p'
          && Audio.get_channel_layout p = Audio.get_channel_layout p'
          && Audio.get_sample_format p = Audio.get_sample_format p'
          && Audio.get_sample_rate p = Audio.get_sample_rate p'

  let merge = merge ~compatible
  let default_params _ = None
end

module Audio = struct
  include Frame_content.MkContent (AudioSpecs)

  let kind = lift_kind `Copy
end

module VideoSpecs = struct
  include BaseSpecs

  let string_of_kind = function `Copy -> "ffmpeg.video.copy"
  let kind_of_string = function "ffmpeg.video.copy" -> Some `Copy | _ -> None

  type params = video Avcodec.params option
  type data = (params, video packet) content

  let string_of_params params =
    Frame_content.print_optional
      [
        ( "codec",
          Option.map
            (fun p ->
              Printf.sprintf "%S" (Video.string_of_id (Video.get_params_id p)))
            params );
        ("width", Option.map (fun p -> string_of_int (Video.get_width p)) params);
        ( "height",
          Option.map (fun p -> string_of_int (Video.get_height p)) params );
        ( "sample_ratio",
          Option.map
            (fun p -> string_of_rational (Video.get_sample_aspect_ratio p))
            params );
        ( "pixel_format",
          Option.bind params (fun p ->
              Option.bind (Video.get_pixel_format p) (fun p ->
                  Some (Pixel_format.to_string p))) );
      ]

  let compatible p p' =
    match (p, p') with
      | None, _ | _, None -> true
      | Some p, Some p' ->
          Video.get_params_id p = Video.get_params_id p'
          && Video.get_width p = Video.get_width p'
          && Video.get_height p = Video.get_height p'
          && Video.get_sample_aspect_ratio p = Video.get_sample_aspect_ratio p'
          && Video.get_pixel_format p = Video.get_pixel_format p'

  let merge = merge ~compatible
  let default_params _ = None
end

module Video = struct
  include Frame_content.MkContent (VideoSpecs)

  let kind = lift_kind `Copy
end
