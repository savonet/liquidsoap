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

(** Given a list of [sources], play one track from each of the first
  * sources, then loop on the last one. Optionally, merge tracks when
  * advancing in the sequence. The [merge] flag will *not* merge tracks
  * while looping on the last source -- this behavior would not be suited
  * to the current use of [sequence] in transitions. *)
class sequence ?(merge = false) ?(single_track = true) sources =
  let self_sync_type = Utils.self_sync_type sources in
  object (self)
    inherit operator ~name:"sequence" sources
    val mutable seq_sources = sources

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

    (** When head_ready is true, it must be that:
    *  - (List.hd seq_sources)#is_ready
    *  - or we have started playing a track of (List.hd sources)
    *    and that track has not ended yet.
    * In case the head source becomes unavailable before its end of track,
    * head_ready keeps the sequence operator available, so that its #get_frame
    * can be called to properly end the track and cleanup the source if needed.
    * If instead the operator had become unavailable then source#get would have
    * inserted an end of track automatically instead of calling #get_frame. *)
    val mutable head_ready = false

    method private _is_ready ?frame () =
      head_ready || List.exists (fun s -> s#is_ready ?frame ()) seq_sources

    method remaining =
      if merge then (
        let ( + ) a b = if a < 0 || b < 0 then -1 else a + b in
        List.fold_left ( + ) 0 (List.map (fun s -> s#remaining) seq_sources))
      else (List.hd seq_sources)#remaining

    method seek_source =
      match seq_sources with
        | s :: _ -> s#seek_source
        | _ -> (self :> Source.source)

    method abort_track =
      if single_track && merge then (
        match List.rev seq_sources with
          | [] -> assert false
          | hd :: _ -> seq_sources <- [hd]);
      match seq_sources with hd :: _ -> hd#abort_track | _ -> ()

    method private get_frame buf =
      if head_ready then (
        let hd = List.hd seq_sources in
        hd#get buf;
        if Frame.is_partial buf then (
          head_ready <- false;
          if List.length seq_sources > 1 then (
            if single_track || not (hd#is_ready ~frame:buf ()) then
              seq_sources <- List.tl seq_sources;
            if ((not single_track) || merge) && self#is_ready ~frame:buf () then (
              let pos = Frame.position buf in
              self#get_frame buf;
              Frame.set_breaks buf
                (Utils.remove_one (( = ) pos) (Frame.breaks buf))))))
      else (
        match seq_sources with
          | a :: (_ :: _ as tl) ->
              if a#is_ready ~frame:buf () then head_ready <- true
              else seq_sources <- tl;
              self#get_frame buf
          | [a] ->
              assert (a#is_ready ~frame:buf ());

              (* Our #is_ready ensures that. *)
              head_ready <- true;
              self#get_frame buf
          | [] -> assert false)
  end

class merge_tracks source =
  object (self)
    inherit operator ~name:"sequence" [source]
    method stype = source#stype
    method private _is_ready = source#is_ready
    method abort_track = source#abort_track
    method remaining = -1
    method self_sync = source#self_sync
    method seek_source = source#seek_source

    method private get_frame buf =
      source#get buf;
      if Frame.is_partial buf && source#is_ready ~frame:buf () then (
        self#log#info "End of track: merging.";
        self#get_frame buf;
        Frame.set_breaks buf
          (match Frame.breaks buf with
            | b :: _ :: l -> b :: l
            | _ -> assert false))
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
    (fun p ->
      new sequence
        ~merge:(Lang.to_bool (List.assoc "merge" p))
        ~single_track:(Lang.to_bool (List.assoc "single_track" p))
        (Lang.to_source_list (List.assoc "" p)))
