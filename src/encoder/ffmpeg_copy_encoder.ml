(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

(** FFMPEG copy encoder *)

open Avcodec

let mk_stream_copy ~video_size ~get_data output =
  let stream = ref None in
  let video_size_ref = ref None in
  let codec_attr = ref None in
  let bitrate = ref None in
  let main_time_base = Ffmpeg_utils.liq_main_ticks_time_base () in

  let mk_stream frame =
    let { Ffmpeg_content_base.params } = get_data frame in
    video_size_ref := video_size frame;
    let s = Av.new_stream_copy ~params:(Option.get params) output in
    codec_attr := Av.codec_attr s;
    bitrate := Av.bitrate s;
    stream := Some s
  in

  let codec_attr () = !codec_attr in

  let bitrate () = !bitrate in

  let video_size () = !video_size_ref in

  let to_main_time_base ~time_base v =
    Ffmpeg_utils.convert_time_base ~src:time_base ~dst:main_time_base v
  in

  let total_duration = ref 0L in
  let last_start = ref 0L in
  let last_stream_idx = ref None in

  let incr_duration ~time_base d =
    ignore
      (Option.map
         (fun d ->
           total_duration :=
             Int64.add !total_duration (to_main_time_base ~time_base d))
         d)
  in
  let check_start stream_idx =
    match !last_stream_idx with
      | Some idx when idx = stream_idx -> ()
      | _ ->
          last_stream_idx := Some stream_idx;
          last_start := !total_duration
  in
  let adjust_ts ~time_base =
    Option.map (fun ts ->
        Int64.add (to_main_time_base ~time_base ts) !last_start)
  in

  let was_keyframe = ref false in

  let encode frame start len =
    let stop = start + len in
    let data = (get_data frame).Ffmpeg_content_base.data in

    was_keyframe := false;

    List.iter
      (fun (pos, { Ffmpeg_copy_content.packet; time_base; stream_idx }) ->
        let stream = Option.get !stream in
        if start <= pos && pos < stop then (
          check_start stream_idx;

          let packet = Avcodec.Packet.dup packet in

          incr_duration ~time_base (Avcodec.Packet.get_duration packet);

          let packet_pts =
            adjust_ts ~time_base (Avcodec.Packet.get_pts packet)
          in
          let packet_dts =
            adjust_ts ~time_base (Avcodec.Packet.get_dts packet)
          in

          Packet.set_pts packet packet_pts;
          Packet.set_dts packet packet_dts;
          Packet.set_duration packet
            (Option.map
               (to_main_time_base ~time_base)
               (Packet.get_duration packet));

          if List.mem `Keyframe Avcodec.Packet.(get_flags packet) then
            was_keyframe := true;

          Av.write_packet stream main_time_base packet))
      data
  in

  let was_keyframe () = !was_keyframe in

  {
    Ffmpeg_encoder_common.mk_stream;
    was_keyframe;
    encode;
    codec_attr;
    bitrate;
    video_size;
  }
