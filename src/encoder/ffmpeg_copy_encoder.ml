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

(** FFMPEG copy encoder *)

open Avcodec

let mk_stream_copy ~video_size ~get_data output =
  let stream = ref None in
  let video_size_ref = ref None in
  let codec_attr = ref None in
  let bitrate = ref None in
  let master_time_base = Ffmpeg_utils.liq_master_ticks_time_base () in

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

  (* Keep track of latest DTS/PTS in master time_base
     since time_base can change between streams. *)
  let last_dts = ref None in
  let last_pts = ref None in
  let dts_offset = ref None in
  let pts_offset = ref None in
  let last_stream_idx = ref None in

  let to_master_time_base ~time_base v =
    Ffmpeg_utils.convert_time_base ~src:time_base ~dst:master_time_base v
  in

  let adjust ~stream_idx ~time_base ~last_ts ~ts_offset ts =
    if Some stream_idx <> !last_stream_idx then (
      last_stream_idx := Some stream_idx;
      ts_offset :=
        (* Account for potential offset in new stream's TS. *)
        match (!last_ts, ts) with
          | Some old_ts, None -> Some old_ts
          | Some old_ts, Some new_ts ->
              Some (Int64.sub old_ts (to_master_time_base ~time_base new_ts))
          | None, _ -> None );
    let ret =
      match (!ts_offset, ts) with
        | Some ofs, None -> Some ofs
        | Some ofs, Some v ->
            Some (Int64.add ofs (to_master_time_base ~time_base v))
        | None, Some v -> Some (to_master_time_base ~time_base v)
        | None, None -> None
    in
    last_ts := ret;
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

          let packet_pts =
            adjust ~stream_idx ~time_base ~last_ts:last_pts
              ~ts_offset:pts_offset packet_pts
          in
          let packet_dts =
            adjust ~stream_idx ~time_base ~last_ts:last_dts
              ~ts_offset:dts_offset packet_dts
          in

          Packet.set_pts packet packet_pts;
          Packet.set_dts packet packet_dts;
          Packet.set_duration packet
            (Option.map
               (to_master_time_base ~time_base)
               (Packet.get_duration packet));

          if List.mem `Keyframe Avcodec.Packet.(get_flags packet) then
            was_keyframe := true;

          Av.write_packet stream master_time_base packet ))
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
