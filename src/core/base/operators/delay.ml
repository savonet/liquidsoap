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

    (* Whether [source] is inside a track we are playing. A track mark at
       position 0 ends that track when we are in one, and opens the next one
       otherwise. *)
    val mutable in_track = false
    method fallible = true
    method remaining = source#remaining

    (* delay controls when [source] advances by leaving it unavailable between
       tracks. This only works if [source] is animated solely by us, so refuse
       any other consumer as soon as it activates the source. *)
    initializer
      source#on_activation (fun () ->
          if List.length source#activations > 1 then
            Lang.raise_error ~pos:[]
              ~message:
                (Printf.sprintf
                   "delay must be applied to a leaf source such as a dedicated \
                    playlist or single, but %s is shared with another consumer"
                   source#id)
              "invalid")

    method abort_track =
      last_track <- time ();
      source#abort_track

    method effective_source = source#effective_source
    method self_sync = source#self_sync
    method private delay_ok = delay_ok last_track

    (* The start of the next track, saved at the boundary: it carries the track
       mark and metadata, and is held back until the delay has elapsed. *)
    val mutable pending = None

    method private can_generate_frame =
      Generator.length self#buffer > 0
      || (self#delay_ok && (pending <> None || source#is_ready))

    method private end_track =
      last_track <- time ();
      in_track <- false

    method private generate_frame =
      let size = Lazy.force Frame.size in
      let append frame =
        if Frame.position frame > 0 then in_track <- true;
        Generator.append self#buffer frame
      in
      if Generator.length self#buffer = 0 then (
        match pending with
          | Some starting ->
              pending <- None;
              append starting
          | None -> ());
      (* Top the buffer up to a full frame: a short frame in the middle of a
         track reads as "nothing more to play" and gets us dropped. *)
      if Generator.length self#buffer < size && source#is_ready then (
        let frame = source#get_frame in
        match self#split_frame frame with
          | buf, Some starting when Frame.position buf = 0 && not in_track ->
              append starting
          | buf, Some starting ->
              pending <- Some starting;
              append buf;
              self#end_track
          | buf, None ->
              append buf;
              (* A source that cannot fill the frame has nothing more to play:
                 its track ended without it ever emitting a mark for it. This
                 is how a leaf source running dry — an emptied `request.queue`,
                 a `playlist` between reloads — reports a track end. *)
              if in_track && Frame.position frame < size then self#end_track);
      Generator.slice self#buffer size
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
    ~descr:
      "Make the source unavailable for a given time between tracks. Because \
       delay controls when its source advances by leaving it unavailable \
       between tracks, it must be applied to a leaf source that it animates on \
       its own, such as a dedicated `playlist` or `single`: an active source \
       or a source shared with another operator would make it inconsistent."
    ~return_t
    (fun p ->
      let f n = Lang.assoc "" n p in
      let d = Lang.to_float_getter (f 1) in
      let s = Lang.to_source (f 2) in
      if s#active then
        raise
          (Error.Invalid_value
             ( f 2,
               "delay must be applied to a leaf source such as a dedicated \
                playlist or single, but the given source is active",
               [] ));
      let initial = Lang.to_bool (List.assoc "initial" p) in
      new delay ~initial s d)
