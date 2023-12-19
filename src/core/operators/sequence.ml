(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2023 Savonet team

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

(** Given a list of [sources], play one track from each of the first
  * sources, then loop on the last one. Optionally, merge tracks when
  * advancing in the sequence. The [merge] flag will *not* merge tracks
  * while looping on the last source -- this behavior would not be suited
  * to the current use of [sequence] in transitions. *)
class sequence ?(merge = false) sources =
  let self_sync_type = Utils.self_sync_type sources in
  let seq_sources = Atomic.make sources in
  object (self)
    inherit operator ~name:"sequence" sources

    inherit
      generate_from_multiple_sources
        ~merge:(fun () -> merge && Atomic.get seq_sources <> [])
        ~track_sensitive:(fun () -> true)
        ()

    method self_sync =
      ( Lazy.force self_sync_type,
        match sources with hd :: _ -> snd hd#self_sync | [] -> false )

    method stype =
      match List.rev sources with hd :: _ -> hd#stype | [] -> `Fallible

    method! private wake_up activation =
      List.iter
        (fun s -> (s :> source)#get_ready ((self :> source) :: activation))
        sources

    method! private sleep =
      List.iter (fun s -> (s :> source)#leave (self :> source)) sources

    (* We have to wait until at least one source is ready. *)
    val mutable has_started = false

    method private has_started =
      match has_started with
        | true -> true
        | false ->
            has_started <-
              List.exists (fun s -> s#is_ready) (Atomic.get seq_sources);
            has_started

    method private get_source ~reselect () =
      match (self#has_started, Atomic.get seq_sources) with
        | true, s :: [] when s#is_ready -> Some s
        | true, s :: rest when reselect || not s#is_ready ->
            self#log#info "Finished with %s" s#id;
            (s :> source)#leave (self :> source);
            Atomic.set seq_sources rest;
            self#get_source ~reselect:false ()
        | true, s :: _ -> Some s
        | _ -> None

    method remaining =
      if merge then (
        let ( + ) a b = if a < 0 || b < 0 then -1 else a + b in
        List.fold_left ( + ) 0
          (List.map (fun s -> s#remaining) (Atomic.get seq_sources)))
      else (List.hd (Atomic.get seq_sources))#remaining

    method seek_source =
      match Atomic.get seq_sources with
        | s :: _ -> s#seek_source
        | _ -> (self :> Source.source)

    method abort_track =
      if merge then (
        match List.rev (Atomic.get seq_sources) with
          | [] -> assert false
          | hd :: _ -> Atomic.set seq_sources [hd]);
      match Atomic.get seq_sources with hd :: _ -> hd#abort_track | _ -> ()
  end

class merge_tracks source =
  object
    inherit operator ~name:"sequence" [source]
    method stype = source#stype
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
      ("", Lang.list_t (Lang.source_t frame_t), None, None);
    ]
    ~category:`Track
    ~descr:
      "Play only one track of every successive source, except for the last one \
       which is played as much as available."
    ~return_t:frame_t
    (fun p ->
      new sequence
        ~merge:(Lang.to_bool (List.assoc "merge" p))
        (Lang.to_source_list (List.assoc "" p)))
