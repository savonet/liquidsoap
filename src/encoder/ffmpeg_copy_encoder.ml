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

  (* Keep track of latest DTS/PTS in main time_base
     since time_base can change between streams. *)
  let last_dts = ref None in
  let last_pts = ref None in
  let dts_offset = ref 0L in
  let pts_offset = ref 0L in
  let last_pts_stream_idx = ref None in
  let last_dts_stream_idx = ref None in
  let last_packet_duration = ref None in

  let to_main_time_base ~time_base v =
    Ffmpeg_utils.convert_time_base ~src:time_base ~dst:main_time_base v
  in

  let adjust ~stream_idx ~time_base ~last_stream_idx ~last_ts ~ts_offset ts =
    if Some stream_idx <> !last_stream_idx then (
      last_stream_idx := Some stream_idx;
      ts_offset :=
        (* Account for potential offset in new stream's TS. *)
        match (!last_ts, ts, !last_packet_duration) with
          | Some old_ts, None, None -> old_ts
          | Some old_ts, None, Some d ->
              Int64.add old_ts (to_main_time_base ~time_base d)
          | Some old_ts, Some new_ts, None ->
              Int64.sub old_ts (to_main_time_base ~time_base new_ts)
          | Some old_ts, Some new_ts, Some d ->
              Int64.add
                (to_main_time_base ~time_base d)
                (Int64.sub old_ts (to_main_time_base ~time_base new_ts))
          | None, Some new_ts, _ -> new_ts
          | None, None, _ -> 0L );
    let ret =
      match ts with
        | None -> Some !ts_offset
        | Some v -> Some (Int64.add !ts_offset (to_main_time_base ~time_base v))
    in
    ( match (!last_ts, ret) with
      | _, None -> ()
      | None, _ -> last_ts := ret
      | Some t, Some t' -> last_ts := Some (max t t') );
    ret
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
          let packet = Avcodec.Packet.dup packet in
          let packet_pts = Avcodec.Packet.get_pts packet in
          let packet_dts = Avcodec.Packet.get_dts packet in
          let packet_duration = Avcodec.Packet.get_duration packet in

          let packet_pts =
            adjust ~stream_idx ~time_base ~last_stream_idx:last_pts_stream_idx
              ~last_ts:last_pts ~ts_offset:pts_offset packet_pts
          in
          let packet_dts =
            adjust ~stream_idx ~time_base ~last_stream_idx:last_dts_stream_idx
              ~last_ts:last_dts ~ts_offset:dts_offset packet_dts
          in

          last_packet_duration := packet_duration;

          Packet.set_pts packet packet_pts;
          Packet.set_dts packet packet_dts;
          Packet.set_duration packet
            (Option.map
               (to_main_time_base ~time_base)
               (Packet.get_duration packet));

          if List.mem `Keyframe Avcodec.Packet.(get_flags packet) then
            was_keyframe := true;

          Av.write_packet stream main_time_base packet ))
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
