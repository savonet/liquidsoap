(*****************************************************************************

   Liquidsoap, a programmable audio stream generator.
   Copyright 2003-2022 Savonet team

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

let log = Log.make ["ffmpeg"; "decoder"; "copy"]

exception Corrupt
exception Empty

let mk_decoder ~stream_idx ~stream_time_base ~lift_data ~latest_keyframe
    ~put_data params =
  let duration_converter =
    Ffmpeg_utils.Duration.init ~src:stream_time_base ~get_ts:Packet.get_dts
  in
  fun ~buffer packet ->
    try
      let flags = Packet.get_flags packet in
      if List.mem `Corrupt flags then (
        log#important "Corrupted packet in stream!";
        raise Corrupt);
      let packets = Ffmpeg_utils.Duration.push duration_converter packet in
      if packets = None then raise Empty;
      let duration, packets = Option.get packets in
      let data =
        List.map
          (fun (pos, packet) ->
            ( pos,
              {
                Ffmpeg_copy_content.packet;
                time_base = stream_time_base;
                latest_keyframe = latest_keyframe ~pos packet;
                stream_idx;
              } ))
          packets
      in
      let data = { Ffmpeg_content_base.params = Some params; data } in
      let data = lift_data data in
      put_data ?pts:None buffer.Decoder.generator data 0 duration
    with Empty | Corrupt (* Might want to change that later. *) -> ()

let mk_audio_decoder ~stream_idx ~format container =
  let idx, stream, params = Av.find_best_audio_stream container in
  Ffmpeg_decoder_common.set_audio_stream_decoder stream;
  ignore
    (Frame_content.merge format
       Ffmpeg_copy_content.(Audio.lift_params (Some params)));
  let stream_time_base = Av.get_time_base stream in
  let lift_data = Ffmpeg_copy_content.Audio.lift_data in
  let latest_keyframe = Ffmpeg_copy_content.latest_keyframe params in
  ( idx,
    stream,
    mk_decoder ~stream_idx ~lift_data ~stream_time_base ~put_data:G.put_audio
      ~latest_keyframe params )

let mk_video_decoder ~stream_idx ~format container =
  let idx, stream, params = Av.find_best_video_stream container in
  Ffmpeg_decoder_common.set_video_stream_decoder stream;
  ignore
    (Frame_content.merge format
       Ffmpeg_copy_content.(Video.lift_params (Some params)));
  let stream_time_base = Av.get_time_base stream in
  let lift_data = Ffmpeg_copy_content.Video.lift_data in
  let latest_keyframe = Ffmpeg_copy_content.latest_keyframe params in
  ( idx,
    stream,
    mk_decoder ~stream_idx ~lift_data ~stream_time_base ~put_data:G.put_video
      ~latest_keyframe params )
