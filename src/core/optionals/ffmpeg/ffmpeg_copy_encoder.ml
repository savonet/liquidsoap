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

(** FFMPEG copy encoder *)

open Avcodec
open Ffmpeg_encoder_common

let log = Log.make ["ffmpeg"; "copy"; "encoder"]

let mk_stream_copy ~get_stream ~on_keyframe ~remove_stream ~keyframe_opt ~field
    output =
  let stream = ref None in
  let video_size_ref = ref None in
  let codec_attr = ref None in
  let bitrate = ref None in
  let main_time_base = Ffmpeg_utils.liq_main_ticks_time_base () in

  (* This should be the same for all streams but it is lazily set
     when the first stream is created. *)
  let intra_only = ref true in

  let initialized_stream = Av.new_uninitialized_stream_copy output in

  let mk_stream frame =
    let { Content.Video.params } =
      Ffmpeg_copy_content.get_data (Frame.get frame field)
    in
    let mk_stream params =
      let s = Av.initialize_stream_copy ~params initialized_stream in
      codec_attr := Av.codec_attr s;
      bitrate := Av.bitrate s;
      (match Avcodec.descriptor params with
        | None -> ()
        | Some { properties } -> intra_only := List.mem `Intra_only properties);
      s
    in
    match Option.get params with
      | `Audio params -> stream := Some (`Audio (mk_stream params))
      | `Video { Ffmpeg_copy_content.avg_frame_rate; params } ->
          let width = Avcodec.Video.get_width params in
          let height = Avcodec.Video.get_height params in
          video_size_ref := Some (width, height);
          let s = mk_stream params in
          Av.set_avg_frame_rate s avg_frame_rate;
          stream := Some (`Video (mk_stream params))
  in

  let codec_attr () = !codec_attr in

  let bitrate () = !bitrate in

  let video_size () = !video_size_ref in

  let to_main_time_base ~time_base v =
    Ffmpeg_utils.convert_time_base ~src:time_base ~dst:main_time_base v
  in

  let current_stream =
    ref (get_stream ~last_start:Int64.min_int ~ready:false 0L)
  in
  let stream_started = ref false in
  let waiting_for_keyframe = ref false in
  let last_dts = ref None in
  let last_position = ref 0L in

  (* [true] if we should process new packets for this stream *)
  let check_stream ~packet stream_idx =
    if !current_stream.idx <> stream_idx then (
      waiting_for_keyframe :=
        keyframe_opt = `Wait_for_keyframe && not !intra_only;
      remove_stream !current_stream;
      last_position := !current_stream.position;
      (* Mark the stream as ready if it is not waiting for keyframes. *)
      let stream =
        get_stream ~last_start:Int64.min_int
          ~ready:(not !waiting_for_keyframe)
          stream_idx
      in
      stream.ready <- not !waiting_for_keyframe;
      current_stream := stream;
      stream_started := false);
    let is_keyframe = List.mem `Keyframe Avcodec.Packet.(get_flags packet) in
    if !waiting_for_keyframe then is_keyframe
    else !stream_started || !current_stream.ready
  in

  let begin_stream ~dts idx =
    let offset = Option.value ~default:0L dts in
    let last_start = Int64.sub !last_position offset in
    (* Mark the stream as ready if it was waiting for keyframes. *)
    current_stream := get_stream ~last_start ~ready:!waiting_for_keyframe idx;
    stream_started := true;
    waiting_for_keyframe := false
  in

  let adjust_ts =
    Option.map (fun ts -> Int64.add ts !current_stream.last_start)
  in

  let check_dts dts =
    let offset =
      if !stream_started then !current_stream.last_start
      else Int64.sub !last_position (Option.value ~default:0L dts)
    in
    match (dts, !last_dts) with
      | None, _ ->
          log#important "Packet has no dts!";
          true
      | Some d, Some d' when Int64.add offset d <= d' ->
          log#important "Dropping packet with non-monotomic dts!";
          false
      | _ -> true
  in

  let push ~time_base ~stream ~idx packet =
    let to_main = to_main_time_base ~time_base in
    let pts = Option.map to_main (Avcodec.Packet.get_pts packet) in
    let dts = Option.map to_main (Avcodec.Packet.get_dts packet) in
    let duration = Option.map to_main (Avcodec.Packet.get_duration packet) in

    if check_dts dts then (
      if not !stream_started then begin_stream ~dts idx;

      let pts = adjust_ts pts in
      let dts = adjust_ts dts in

      (match dts with
        | None -> ()
        | Some dts ->
            !current_stream.position <-
              max
                (Int64.add (Option.value ~default:1L duration) dts)
                !current_stream.position);

      last_dts := dts;

      (match
         ( on_keyframe,
           List.mem `Keyframe Avcodec.Packet.(get_flags packet) || !intra_only
         )
       with
        | Some on_keyframe, true ->
            if not !intra_only then Av.flush output;
            on_keyframe ()
        | _ -> ());

      let packet = Avcodec.Packet.dup packet in
      Packet.set_pts packet pts;
      Packet.set_dts packet dts;
      Packet.set_duration packet duration;

      Av.write_packet stream main_time_base packet)
  in

  let process ~packet ~stream_idx ~time_base stream =
    if check_stream ~packet stream_idx then
      push ~time_base ~stream ~idx:stream_idx packet
  in

  let encode frame =
    if 0 < Frame.position frame then (
      let content = Frame.get frame field in
      let data = (Ffmpeg_copy_content.get_data content).Content.Video.data in

      List.iter
        (fun (_, { Ffmpeg_copy_content.packet; time_base; stream_idx }) ->
          match (packet, !stream) with
            | `Audio packet, Some (`Audio stream) ->
                process ~packet ~stream_idx ~time_base stream
            | `Video packet, Some (`Video stream) ->
                process ~packet ~stream_idx ~time_base stream
            | _ -> assert false)
        data)
  in

  {
    Ffmpeg_encoder_common.mk_stream;
    encode;
    flush = (fun () -> ());
    codec_attr;
    bitrate;
    video_size;
  }
