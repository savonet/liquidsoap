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

(** FFMPEG copy encoder *)

open Avcodec
open Ffmpeg_encoder_common

let mk_stream_copy ~video_size ~get_stream ~keyframe_opt ~get_data output =
  let stream = ref None in
  let video_size_ref = ref None in
  let codec_attr = ref None in
  let bitrate = ref None in
  let main_time_base = Ffmpeg_utils.liq_main_ticks_time_base () in

  (* This should be the same for all streams but it is lazily set
     when the first stream is created. *)
  let intra_only = ref true in

  let mk_stream frame =
    let { Ffmpeg_content_base.params } = get_data frame in
    let params = Option.get params in
    video_size_ref := video_size frame;
    let s = Av.new_stream_copy ~params output in
    codec_attr := Av.codec_attr s;
    bitrate := Av.bitrate s;
    (match Avcodec.descriptor params with
      | None -> ()
      | Some { properties } -> intra_only := List.mem `Intra_only properties);
    stream := Some s
  in

  let codec_attr () = !codec_attr in

  let bitrate () = !bitrate in

  let video_size () = !video_size_ref in

  let to_main_time_base ~time_base v =
    Ffmpeg_utils.convert_time_base ~src:time_base ~dst:main_time_base v
  in

  let current_position = ref 0L in
  let current_stream =
    ref { idx = 0L; last_start = 0L; waiting_for_keyframe = false }
  in
  let offset = ref 0L in

  let was_keyframe = ref false in
  let keyframe_action = ref `Ignore in

  let check_stream ~packet ~time_base stream_idx =
    let to_main = Option.map (to_main_time_base ~time_base) in
    let dts = to_main (Avcodec.Packet.get_dts packet) in
    let duration = to_main (Avcodec.Packet.get_duration packet) in
    if !current_stream.idx <> stream_idx then (
      offset := Option.value ~default:0L dts;
      let last_start = Int64.sub !current_position !offset in
      (match (keyframe_opt, !intra_only) with
        | _, true -> keyframe_action := `Ignore
        | `Wait_for_keyframe, _ -> keyframe_action := `Wait
        | `Replay_keyframe, _ -> keyframe_action := `Replay
        | `Ignore_keyframe, _ -> keyframe_action := `Ignore);
      current_stream :=
        get_stream ~last_start
          ~waiting_for_keyframe:(!keyframe_action = `Wait)
          stream_idx);
    ignore
      (Option.map
         (fun dts ->
           current_position := Int64.add dts !current_stream.last_start)
         dts);
    ignore
      (Option.map
         (fun duration ->
           current_position := Int64.add !current_position duration)
         duration)
  in
  let adjust_ts ~time_base =
    Option.map (fun ts ->
        Int64.add (to_main_time_base ~time_base ts) !current_stream.last_start)
  in

  let last_dts = ref 0L in

  let push ~time_base ~stream packet =
    let packet_pts = adjust_ts ~time_base (Avcodec.Packet.get_pts packet) in
    let packet_dts = adjust_ts ~time_base (Avcodec.Packet.get_dts packet) in

    (* Some formats will insert packets with no DTS. *)
    let packet_pts, packet_dts =
      match packet_dts with
        | Some dts ->
            last_dts := dts;
            (packet_pts, Some dts)
        | None ->
            last_dts := Int64.succ !last_dts;
            let packet_dts = Some !last_dts in
            (* Make sure packet pts >= packet dts *)
            let packet_pts =
              match packet_pts with
                | None -> packet_dts
                | Some pts when pts < !last_dts -> packet_dts
                | Some pts -> Some pts
            in
            (packet_pts, packet_dts)
    in

    let packet = Avcodec.Packet.dup packet in

    Packet.set_pts packet packet_pts;
    Packet.set_dts packet packet_dts;
    Packet.set_duration packet
      (Option.map (to_main_time_base ~time_base) (Packet.get_duration packet));

    Av.write_packet stream main_time_base packet
  in

  let encode frame start len =
    let stop = start + len in
    let data = (get_data frame).Ffmpeg_content_base.data in

    was_keyframe := false;

    List.iter
      (fun ( pos,
             {
               Ffmpeg_copy_content.packet;
               time_base;
               latest_keyframe;
               stream_idx;
             } ) ->
        let stream = Option.get !stream in
        if start <= pos && pos < stop then (
          check_stream ~packet ~time_base stream_idx;
          if List.mem `Keyframe Avcodec.Packet.(get_flags packet) then
            was_keyframe := true;
          (match !keyframe_action with
            | `Ignore -> ()
            | `Wait ->
                if !was_keyframe || latest_keyframe = `No_keyframe then (
                  !current_stream.waiting_for_keyframe <- false;
                  keyframe_action := `Ignore)
            | `Replay ->
                (match latest_keyframe with
                  | `No_keyframe | `Not_seen -> ()
                  | `Seen keyframe ->
                      Packet.set_pts keyframe
                        (Option.map Int64.pred (Packet.get_pts packet));
                      Packet.set_dts keyframe
                        (Option.map Int64.pred (Packet.get_dts packet));
                      push ~time_base ~stream keyframe);
                keyframe_action := `Ignore);
          if not !current_stream.waiting_for_keyframe then
            push ~time_base ~stream packet))
      data
  in

  let can_split () = !intra_only || !was_keyframe in

  {
    Ffmpeg_encoder_common.mk_stream;
    can_split;
    encode;
    codec_attr;
    bitrate;
    video_size;
  }
