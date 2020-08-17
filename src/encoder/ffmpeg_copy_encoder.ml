(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

let mk_stream_copy ~get_data output =
  let stream = ref None in
  let master_time_base = Ffmpeg_utils.liq_master_ticks_time_base () in

  let mk_stream frame =
    let _, { Ffmpeg_copy_content.params } =
      List.hd (get_data frame).Ffmpeg_content_base.data
    in
    stream := Some (Av.new_stream_copy ~params output)
  in

  let position = ref 0L in

  let encode frame start len =
    let stop = start + len in
    let data = (get_data frame).Ffmpeg_content_base.data in

    List.iter
      (fun (pos, { Ffmpeg_copy_content.packet; time_base }) ->
        let stream = Option.get !stream in
        if start <= pos && pos < stop then (
          let packet_pts =
            Ffmpeg_utils.convert_time_base ~src:master_time_base ~dst:time_base
              (Int64.add !position (Int64.of_int (pos - start)))
          in

          Packet.set_pts packet (Some packet_pts);
          Packet.set_dts packet (Some packet_pts);

          Av.write_packet stream time_base packet ))
      data;

    position := Int64.add !position (Int64.of_int len)
  in
  { Ffmpeg_encoder_common.mk_stream; encode }
