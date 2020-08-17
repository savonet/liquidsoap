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

(** Decode ffmpeg packets. *)

open Avcodec
module G = Decoder.G

let mk_decoder ~stream_time_base ~mk_param ~lift_data ~put_data params =
  let get_duration =
    Ffmpeg_decoder_common.convert_duration ~src:stream_time_base
  in
  fun ~buffer packet ->
    let duration = get_duration (Packet.get_duration packet) in
    let packet =
      { Ffmpeg_copy_content.params; packet; time_base = stream_time_base }
    in
    let data =
      { Ffmpeg_content_base.param = mk_param params; data = [(0, packet)] }
    in
    let data = lift_data data in
    put_data ?pts:None buffer.Decoder.generator data 0 duration

let mk_audio_decoder container =
  let idx, stream, params = Av.find_best_audio_stream container in
  let stream_time_base = Av.get_time_base stream in
  let lift_data = Ffmpeg_copy_content.Audio.lift_data in
  let mk_param = Ffmpeg_copy_content.AudioSpecs.mk_param in
  ( idx,
    stream,
    mk_decoder ~lift_data ~mk_param ~stream_time_base ~put_data:G.put_audio
      params )

let mk_video_decoder container =
  let idx, stream, params = Av.find_best_video_stream container in
  let stream_time_base = Av.get_time_base stream in
  let lift_data = Ffmpeg_copy_content.Video.lift_data in
  let mk_param = Ffmpeg_copy_content.VideoSpecs.mk_param in
  ( idx,
    stream,
    mk_decoder ~mk_param ~lift_data ~stream_time_base ~put_data:G.put_video
      params )
