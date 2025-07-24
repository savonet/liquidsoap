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

open Avutil
open Avcodec
include Content_video.Base

let conf_ffmpeg_copy =
  Dtools.Conf.void
    ~p:(Ffmpeg_content_base.conf_ffmpeg_content#plug "copy")
    "FFmpeg copy content configuration"

let conf_ffmpeg_copy_relaxed =
  Dtools.Conf.bool
    ~p:(conf_ffmpeg_copy#plug "relaxed_compatibility_check")
    ~d:false
    "If `true`, relax content compatibility, e.g. allow audio tracks with \
     different samplerate or video tracks with different resolution."

type packet = [ `Audio of audio Packet.t | `Video of video Packet.t ]

type video_params = {
  avg_frame_rate : Avutil.rational option;
  params : video Avcodec.params;
}

type params_payload = [ `Audio of audio Avcodec.params | `Video of video_params ]

type packet_payload = {
  stream_idx : Int64.t;
  time_base : Avutil.rational;
  packet : packet;
}

module Specs = struct
  include Content_video.Base

  type kind = [ `Copy ]
  type params = params_payload option
  type data = (params, packet_payload) content

  let name = "ffmpeg.copy"
  let kind = `Copy
  let parse_param _ _ = None
  let internal_content_type = None
  let string_of_kind = function `Copy -> "ffmpeg.copy"
  let kind_of_string = function "ffmpeg.copy" -> Some `Copy | _ -> None

  let string_of_params params =
    String.concat ","
      (List.map
         (fun (k, v) -> Printf.sprintf "%s=%s" k v)
         (match params with
           | None -> []
           | Some (`Audio params) ->
               [
                 ( "codec",
                   Printf.sprintf "%S"
                     (Audio.string_of_id (Audio.get_params_id params)) );
                 ( "channel_layout",
                   Printf.sprintf "%S"
                     (Channel_layout.get_description
                        (Audio.get_channel_layout params)) );
                 ( "sample_format",
                   Printf.sprintf "%s"
                     (match
                        Sample_format.get_name (Audio.get_sample_format params)
                      with
                       | None -> "none"
                       | Some p -> p) );
                 ("sample_rate", string_of_int (Audio.get_sample_rate params));
               ]
           | Some (`Video { avg_frame_rate; params }) -> (
               [
                 ( "codec",
                   Printf.sprintf "%S"
                     (Video.string_of_id (Video.get_params_id params)) );
                 ("width", string_of_int (Video.get_width params));
                 ("height", string_of_int (Video.get_height params));
                 ( "aspect_ratio",
                   string_of_rational (Video.get_sample_aspect_ratio params) );
               ]
               @ (match avg_frame_rate with
                   | Some r -> [("framerate", Avutil.string_of_rational r)]
                   | None -> [])
               @
               match Video.get_pixel_format params with
                 | None -> []
                 | Some p ->
                     [
                       ( "pixel_format",
                         Option.value ~default:"none" (Pixel_format.to_string p)
                       );
                     ])))

  let compatible_aspect_radio p p' =
    match (p, p') with
      (* 0/1 aspect_ratio means undefined and is usually
         assumed to be 1 so we match it with 1/1. *)
      | Avutil.{ num = 0; den = 1 }, Avutil.{ num = 1; den = 1 }
      | Avutil.{ num = 1; den = 1 }, Avutil.{ num = 0; den = 1 } ->
          true
      | _ -> p = p'

  let compatible_audio p p' =
    Avutil.Channel_layout.compare
      (Audio.get_channel_layout p)
      (Audio.get_channel_layout p')
    && Audio.get_sample_format p = Audio.get_sample_format p'
    && Audio.get_sample_rate p = Audio.get_sample_rate p'

  let compatible_video (r, p) (r', p') =
    r = r'
    && Video.get_width p = Video.get_width p'
    && Video.get_height p = Video.get_height p'
    && compatible_aspect_radio
         (Video.get_sample_aspect_ratio p)
         (Video.get_sample_aspect_ratio p')
    && Video.get_pixel_format p = Video.get_pixel_format p'

  let compatible p p' =
    match (p, p') with
      | None, _ | _, None -> true
      | Some (`Audio p), Some (`Audio p') ->
          Audio.get_params_id p = Audio.get_params_id p'
          && (conf_ffmpeg_copy_relaxed#get || compatible_audio p p')
      | ( Some (`Video { avg_frame_rate = r; params = p }),
          Some (`Video { avg_frame_rate = r'; params = p' }) ) ->
          Video.get_params_id p = Video.get_params_id p'
          && (conf_ffmpeg_copy_relaxed#get || compatible_video (r, p) (r', p'))
      | _ -> false

  let merge p p' =
    match (p, p') with
      | None, p | p, None -> p
      | p, p' when compatible p p' -> p
      | _ -> failwith "Incompatible format!"

  let copy_packet (p : packet_payload) =
    {
      p with
      packet =
        (match p.packet with
          | `Audio p -> `Audio (Avcodec.Packet.dup p)
          | `Video p -> `Video (Avcodec.Packet.dup p));
    }

  let blit = blit ~copy:copy_packet
  let copy = copy ~copy:copy_packet
  let default_params _ = None
end

include Content.MkContent (Specs)

let kind = lift_kind `Copy
