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

(** Avoids some source to play a new track before some delay elapses after the
  * end of the previous track played. *)

open Source

class delay ~kind ~initial (source : source) delay =
  object (self)
    inherit operator ~name:"delay" kind [source]

    method stype = Fallible

    method remaining = source#remaining

    method abort_track = source#abort_track

    method seek = source#seek

    method self_sync = source#self_sync

    val mutable last = if initial then Unix.time () else 0.

    val mutable in_track = false

    method private delay_ok = Unix.time () -. last >= delay

    method is_ready = in_track || (self#delay_ok && source#is_ready)

    method private get_frame buf =
      source#get buf;
      in_track <- true;
      (* The current track ends. *)
      if Frame.is_partial buf then (
        in_track <- false;
        last <- Unix.time () )
  end

let () =
  let kind = Lang.univ_t () in
  Lang.add_operator "delay"
    [
      ( "initial",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Start in unavailable state, as if a track had just finished." );
      ( "",
        Lang.float_t,
        None,
        Some
          "The source won't be ready less than this amount of seconds after \
           any end of track" );
      ("", Lang.source_t kind, None, None);
    ]
    ~category:Lang.TrackProcessing
    ~descr:
      "Prevents the child from being ready again too fast after a end of track"
    ~kind:(Lang.Unconstrained kind)
    (fun p kind ->
      let f n = Lang.assoc "" n p in
      let d = Lang.to_float (f 1) in
      let s = Lang.to_source (f 2) in
      let initial = Lang.to_bool (List.assoc "initial" p) in
      new delay ~kind ~initial s d)
