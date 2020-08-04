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

module G = Decoder.G

let mk_audio_decoder container =
  let idx, stream, _ = Av.find_best_audio_stream container in
  let get_duration =
    Ffmpeg_utils.convert_duration ~duration_time_base:(Av.get_time_base stream)
      ~sample_time_base:(Ffmpeg_utils.liq_audio_sample_time_base ())
  in
  ( idx,
    stream,
    fun ~buffer frame ->
      let data = Ffmpeg_raw_content.Audio.lift_data frame in
      G.put_audio buffer.Decoder.generator data 0
        (get_duration (Avutil.frame_pkt_duration frame)) )

let mk_video_decoder container =
  let idx, stream, _ = Av.find_best_video_stream container in
  let get_duration =
    Ffmpeg_utils.convert_duration ~duration_time_base:(Av.get_time_base stream)
      ~sample_time_base:(Ffmpeg_utils.liq_video_sample_time_base ())
  in
  ( idx,
    stream,
    fun ~buffer frame ->
      let data = Ffmpeg_raw_content.Video.lift_data (ref [(0, frame)]) in
      G.put_video buffer.Decoder.generator data 0
        (get_duration (Avutil.frame_pkt_duration frame)) )
