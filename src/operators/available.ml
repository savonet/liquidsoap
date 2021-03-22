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

(** Avoids some source to play a new track before some delay elapses after the
  * end of the previous track played. *)

open Source

class available ~kind ~track_sensitive ~delay p (source : source) =
  object
    inherit operator ~name:"available" kind [source]

    method stype = Fallible

    method remaining = source#remaining

    method abort_track = source#abort_track

    method seek = source#seek

    method self_sync = source#self_sync

    val mutable ready = p ()

    val mutable last = Unix.time ()

    method is_ready =
      if (not (track_sensitive && ready)) && Unix.time () -. last >= delay ()
      then (
        ready <- p ();
        last <- Unix.time () );
      ready && source#is_ready

    method private get_frame buf =
      source#get buf;
      if track_sensitive && Frame.is_partial buf then (
        ready <- p ();
        last <- Unix.time () )
  end

let () =
  let kind = Lang.any in
  let return_t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "available"
    [
      ( "track_sensitive",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Change availability only on end of tracks." );
      ( "delay",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 1.),
        Some "Delay between availability checks." );
      ( "",
        Lang.getter_t Lang.bool_t,
        None,
        Some
          "Predicate indicating whether the source should be available or not."
      );
      ( "",
        Lang.source_t return_t,
        None,
        Some "Source of which the availability should be changed." );
    ]
    ~category:Lang.TrackProcessing
    ~descr:"Change the availability of a source depending on a predicate."
    ~return_t
    (fun p ->
      let track_sensitive = List.assoc "track_sensitive" p |> Lang.to_bool in
      let delay = List.assoc "delay" p |> Lang.to_float_getter in
      let pred = Lang.assoc "" 1 p |> Lang.to_bool_getter in
      let s = Lang.assoc "" 2 p |> Lang.to_source in
      new available ~kind ~track_sensitive ~delay pred s)
