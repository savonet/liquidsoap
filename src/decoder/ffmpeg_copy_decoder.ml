(*****************************************************************************

   Liquidsoap, a programmable audio stream generator.
   Copyright 2003-2017 Savonet team

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

(** Decode and read metadata using ffmpeg. *)

open Avcodec
module G = Decoder.G

let mk_decoder ~get_duration ~stream_time_base ~lift_data ~put_data params
    ~buffer packet =
  let duration = get_duration (Packet.get_duration packet) in
  let packet =
    { Ffmpeg_copy_content.params; packet; time_base = stream_time_base }
  in
  let data = lift_data (ref [(0, packet)]) in
  put_data ?pts:None buffer.Decoder.generator data 0 duration

let mk_audio_decoder container =
  let idx, stream, params = Av.find_best_audio_stream container in
  let stream_time_base = Av.get_time_base stream in
  let sample_time_base = Ffmpeg_utils.liq_audio_sample_time_base () in
  let get_duration =
    Ffmpeg_utils.convert_duration ~duration_time_base:stream_time_base
      ~sample_time_base
  in
  let lift_data = Ffmpeg_copy_content.Audio.lift_data in
  ( idx,
    stream,
    mk_decoder ~get_duration ~lift_data ~stream_time_base ~put_data:G.put_audio
      params )

let mk_video_decoder container =
  let idx, stream, params = Av.find_best_video_stream container in
  let stream_time_base = Av.get_time_base stream in
  let sample_time_base = Ffmpeg_utils.liq_video_sample_time_base () in
  let get_duration =
    Ffmpeg_utils.convert_duration ~duration_time_base:stream_time_base
      ~sample_time_base
  in
  let lift_data = Ffmpeg_copy_content.Video.lift_data in
  ( idx,
    stream,
    mk_decoder ~get_duration ~lift_data ~stream_time_base ~put_data:G.put_video
      params )
