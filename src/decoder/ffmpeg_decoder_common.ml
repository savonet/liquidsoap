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

exception No_pts

(* Ffmpeg doesn't really support a consistent duration API. Thus, we
   use PTS increment between packets to emulate duration. This means that
   a packet's duration is, in effect, the time between the last packet and
   the current one. *)
let convert_duration ~src =
  let dst = Ffmpeg_utils.liq_master_ticks_time_base () in
  let last_pts = ref None in
  fun pts ->
    match (!last_pts, pts) with
      | None, Some _ ->
          last_pts := pts;
          0
      | Some old_pts, Some pts ->
          let d = Int64.sub old_pts pts in
          last_pts := Some pts;
          Int64.to_int (Ffmpeg_utils.convert_time_base ~src ~dst d)
      | _, None -> raise No_pts
