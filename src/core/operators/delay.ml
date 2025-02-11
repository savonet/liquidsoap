(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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
    end of the previous track played. *)

open Source

class delay ~initial (source : source) delay =
  let initial_last_track, time, delay_ok =
    let module Time = (val Clock.time_implementation () : Liq_time.T) in
    let initial_last_track =
      if initial then Time.time () else Time.of_float 0.
    in
    let time = Time.time in
    let delay_ok last_track =
      Time.(of_float (delay ()) |<=| (time () |-| last_track))
    in
    (initial_last_track, time, delay_ok)
  in
  object (self)
    inherit operator ~name:"delay" [source]
    val mutable last_track = initial_last_track
    val mutable first_track = true
    method fallible = true
    method remaining = source#remaining

    method abort_track =
      last_track <- time ();
      source#abort_track

    method seek_source = source#seek_source
    method self_sync = source#self_sync
    method private delay_ok = delay_ok last_track
    method private can_generate_frame = self#delay_ok && source#is_ready

    method private generate_frame =
      let frame = source#get_frame in
      match self#split_frame frame with
        | buf, Some _ when first_track && Frame.position buf = 0 ->
            first_track <- false;
            frame
        | buf, Some _ ->
            first_track <- false;
            last_track <- time ();
            buf
        | buf, None -> buf
  end

let _ =
  let return_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  Lang.add_operator "delay"
    [
      ( "initial",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Start in unavailable state, as if a track had just finished." );
      ( "",
        Lang.getter_t Lang.float_t,
        None,
        Some
          "The source won't be ready less than this amount of seconds after \
           any end of track" );
      ("", Lang.source_t return_t, None, None);
    ]
    ~category:`Track
    ~descr:"Make the source unavailable for a given time between tracks."
    ~return_t
    (fun p ->
      let f n = Lang.assoc "" n p in
      let d = Lang.to_float_getter (f 1) in
      let s = Lang.to_source (f 2) in
      let initial = Lang.to_bool (List.assoc "initial" p) in
      new delay ~initial s d)
