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

open Source

(** Given a list of [sources], play one track from each of the first sources,
    then loop on the last one. Optionally, merge tracks when advancing in the
    sequence. The [merge] flag will *not* merge tracks while looping on the last
    source -- this behavior would not be suited to the current use of [sequence]
    in transitions. *)
class sequence ?(merge = false) ?(single_track = true) sources =
  let self_sync_type = Clock_base.self_sync_type sources in
  let seq_sources = Atomic.make sources in
  object (self)
    inherit operator ~name:"sequence" sources

    inherit
      generate_from_multiple_sources
        ~merge:(fun () -> merge && List.length (Atomic.get seq_sources) <> 1)
        ~track_sensitive:(fun () -> true)
        ()

    method self_sync =
      ( Lazy.force self_sync_type,
        match sources with hd :: _ -> snd hd#self_sync | [] -> None )

    method fallible =
      match List.rev sources with hd :: _ -> hd#fallible | [] -> true

    (* We have to wait until at least one source is ready. *)
    val mutable has_started = false
    method queue = Atomic.get seq_sources

    method private has_started =
      match has_started with
        | true -> true
        | false ->
            has_started <- List.exists (fun s -> s#is_ready) self#queue;
            has_started

    method private get_stateful_source ?(source_skipped = false) ~reselect () =
      match (self#has_started, self#queue) with
        | _, [] -> None
        | true, s :: [] ->
            if
              self#can_reselect
                ~reselect:(match reselect with `Force -> `Ok | _ -> reselect)
                s
            then Some s
            else None
        | true, s :: rest ->
            if
              self#can_reselect
                ~reselect:
                  (match reselect with
                    | `After_position _
                      when (not source_skipped) && single_track ->
                        `Force
                    | v -> v)
                s
            then Some s
            else (
              self#log#info "Finished with %s" s#id;
              Atomic.set seq_sources rest;
              self#get_stateful_source ~source_skipped:true
                ~reselect:(match reselect with `Force -> `Ok | v -> v)
                ())
        | _ -> None

    method private get_source ~reselect () =
      self#get_stateful_source ~reselect ()

    method remaining =
      if merge then (
        let ( + ) a b = if a < 0 || b < 0 then -1 else a + b in
        List.fold_left ( + ) 0 (List.map (fun s -> s#remaining) self#queue))
      else (List.hd self#queue)#remaining

    method seek_source =
      match self#queue with
        | s :: _ -> s#seek_source
        | _ -> (self :> Source.source)

    method abort_track =
      if merge then (
        match List.rev self#queue with
          | [] -> assert false
          | hd :: _ -> Atomic.set seq_sources [hd]);
      match self#queue with hd :: _ -> hd#abort_track | _ -> ()
  end

class merge_tracks source =
  object
    inherit operator ~name:"sequence" [source]
    method fallible = source#fallible
    method private can_generate_frame = source#is_ready
    method abort_track = source#abort_track
    method remaining = -1
    method self_sync = source#self_sync
    method seek_source = source#seek_source

    method private generate_frame =
      let buf = source#get_frame in
      Frame.set buf Frame.Fields.track_marks
        (Content.make ~length:(Frame.position buf)
           Content_timed.Track_marks.format)
  end

let _ =
  let frame_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  Lang.add_operator "sequence"
    [
      ( "merge",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "Merge tracks when advancing from one source to the next one. This \
           will NOT merge consecutive tracks from the last source; see \
           merge_tracks() if you need that too." );
      ( "single_track",
        Lang.bool_t,
        Some (Lang.bool true),
        Some
          "Advance to the new track in the sequence on new track. Set to \
           `false` to play each source until it becomes unavailable." );
      ("", Lang.list_t (Lang.source_t frame_t), None, None);
    ]
    ~category:`Track
    ~descr:
      "Play a sequence of sources. By default, play one track per source, \
       except for the last one which is played as much as available."
    ~return_t:frame_t
    ~meth:
      [
        {
          name = "queue";
          scheme = ([], Lang.fun_t [] (Lang.list_t (Lang.source_t frame_t)));
          descr = "Return the current sequence of source";
          value =
            (fun s ->
              Lang.val_fun [] (fun _ ->
                  Lang.list (List.map Lang.source s#queue)));
        };
      ]
    (fun p ->
      new sequence
        ~merge:(Lang.to_bool (List.assoc "merge" p))
        ~single_track:(Lang.to_bool (List.assoc "single_track" p))
        (Lang.to_source_list (List.assoc "" p)))
