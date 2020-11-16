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
  let next_dts = ref None in
  let next_pts = ref None in

  let to_master_time_base ~time_base v =
    Ffmpeg_utils.convert_time_base ~src:time_base ~dst:master_time_base v
  in

  let init ~ts ~time_base v =
    match !v with
      | Some _ -> ()
      | None -> v := Option.map (to_master_time_base ~time_base) ts
  in

  let add ~duration ~time_base v =
    match (!v, duration) with
      | Some p, Some d ->
          v := Some (Int64.add p (to_master_time_base ~time_base d))
      | _ -> failwith "Packet missing duration!"
  in

  let was_keyframe = ref false in

  let encode frame start len =
    let stop = start + len in
    let data = (get_data frame).Ffmpeg_content_base.data in

    was_keyframe := false;

    List.iter
      (fun (pos, { Ffmpeg_copy_content.packet; time_base }) ->
        let stream = Option.get !stream in
        if start <= pos && pos < stop then (
          let packet = Avcodec.Packet.dup packet in

          init ~ts:(Packet.get_dts packet) ~time_base next_dts;
          init ~ts:(Packet.get_pts packet) ~time_base next_pts;

          let duration = Packet.get_duration packet in

          Packet.set_duration packet
            (Option.map (to_master_time_base ~time_base) duration);
          Packet.set_pts packet !next_pts;
          Packet.set_dts packet !next_dts;

          add ~duration ~time_base next_dts;
          add ~duration ~time_base next_pts;

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
