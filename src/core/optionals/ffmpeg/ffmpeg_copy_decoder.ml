(*****************************************************************************

   Liquidsoap, a programmable stream generator.
   Copyright 2003-2026 Savonet team

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

let log = Log.make ["ffmpeg"; "decoder"; "copy"]

exception Corrupt
exception Empty

let mk_decoder ~stream_idx ~stream_time_base ~mk_packet ~put_data params =
  let duration_converter =
    Ffmpeg_utils.Duration.init ~mode:`DTS ~src:stream_time_base
      ~convert_ts:false ~get_ts:Packet.get_dts ~set_ts:Packet.set_dts ()
  in
  let output_packets ~buffer length packets =
    let data =
      List.map
        (fun (pos, packet) ->
          ( pos,
            {
              Ffmpeg_copy_content.packet = mk_packet packet;
              time_base = stream_time_base;
              stream_idx;
            } ))
        packets
    in
    let data = { Content.Video.params = Some params; data; length } in
    let data = Ffmpeg_copy_content.lift_data data in
    put_data buffer.Decoder.generator data
  in
  let process_packet ~buffer packet =
    let flags = Packet.get_flags packet in
    if List.mem `Corrupt flags then (
      log#important "Corrupted packet in stream!";
      raise Corrupt);
    let packets = Ffmpeg_utils.Duration.push duration_converter packet in
    if packets = None then raise Empty;
    let length, packets = Option.get packets in
    output_packets ~buffer length packets
  in
  let flush ~buffer =
    let packets = Ffmpeg_utils.Duration.flush duration_converter in
    if packets <> [] then
      (* Use a minimal length of 1 for remaining packets at EOF *)
      output_packets ~buffer 1 packets
  in
  fun ~buffer -> function
    | `Flush -> flush ~buffer
    | `Packet packet -> (
        try process_packet ~buffer packet
        with Empty | Corrupt (* Might want to change that later. *) -> ())

let mk_audio_decoder ~stream_idx ~format ~field ~stream params =
  Ffmpeg_decoder_common.set_audio_stream_decoder stream;
  let params = `Audio params in
  ignore (Content.merge format (Ffmpeg_copy_content.lift_params (Some params)));
  let stream_time_base = Av.get_time_base stream in
  mk_decoder ~stream_idx
    ~mk_packet:(fun p -> `Audio p)
    ~stream_time_base
    ~put_data:(fun g c -> Generator.put g field c)
    params

let mk_video_decoder ~stream_idx ~format ~stream ~field params =
  Ffmpeg_decoder_common.set_video_stream_decoder stream;
  let params =
    `Video
      {
        Ffmpeg_copy_content.avg_frame_rate = Av.get_avg_frame_rate stream;
        params;
      }
  in
  ignore (Content.merge format (Ffmpeg_copy_content.lift_params (Some params)));
  let stream_time_base = Av.get_time_base stream in
  mk_decoder ~stream_idx
    ~mk_packet:(fun p -> `Video p)
    ~stream_time_base
    ~put_data:(fun g c -> Generator.put g field c)
    params

let mk_subtitle_decoder ~stream_idx ~format ~stream ~field params =
  Ffmpeg_decoder_common.set_subtitle_stream_decoder stream;
  let stream_time_base = Av.get_time_base stream in
  let params =
    `Subtitle { Ffmpeg_copy_content.time_base = stream_time_base; params }
  in
  ignore (Content.merge format (Ffmpeg_copy_content.lift_params (Some params)));
  mk_decoder ~stream_idx
    ~mk_packet:(fun p -> `Subtitle p)
    ~stream_time_base
    ~put_data:(fun g c -> Generator.put g field c)
    params
