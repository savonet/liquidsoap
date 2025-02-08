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

exception Error

(* This is an internal operator. *)
class insert_metadata source =
  object
    inherit operator ~name:"insert_metadata" [source]
    method fallible = source#fallible
    method private can_generate_frame = source#is_ready
    method remaining = source#remaining
    method seek_source = source#seek_source
    method abort_track = source#abort_track
    method self_sync = source#self_sync
    val metadata = Atomic.make None

    method insert_metadata ~new_track m =
      Atomic.set metadata (Some (new_track, m))

    method private generate_frame =
      let buf = source#get_frame in
      match Atomic.exchange metadata None with
        | Some (new_track, m) ->
            let m =
              Frame.Metadata.append
                (Option.value ~default:Frame.Metadata.empty
                   (Frame.get_metadata buf 0))
                m
            in
            let buf = Frame.add_metadata buf 0 m in
            if new_track then Frame.set_track_mark buf 0 else buf
        | None -> buf
  end

let _ =
  let return_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in

  Lang.add_operator "insert_metadata" ~category:`Track
    ~meth:
      [
        ( "insert_metadata",
          ( [],
            Lang.fun_t
              [(true, "new_track", Lang.bool_t); (false, "", Lang.metadata_t)]
              Lang.unit_t ),
          "Insert metadata in the source. The `new_track` parameter indicates \
           whether a track boundary should also be inserted (by default, no \
           track is inserted).",
          fun s ->
            Lang.val_fun
              [
                ("new_track", "new_track", Some (Lang.bool false));
                ("", "", None);
              ]
              (fun p ->
                let m = Lang.to_metadata (List.assoc "" p) in
                let new_track = Lang.to_bool (List.assoc "new_track" p) in
                s#insert_metadata ~new_track m;
                Lang.unit) );
      ]
    ~return_t
    ~descr:
      "Dynamically insert metadata in a stream. Returns the source decorated \
       with a method `insert_metadata` which is a function of type \
       `(?new_track,metadata)->unit`, used to insert metadata in the source. \
       This function also inserts a new track with the given metadata if \
       passed `new_track=true`."
    [("", Lang.source_t return_t, None, None)]
    (fun p ->
      let s = Lang.to_source (List.assoc "" p) in
      let s = new insert_metadata s in
      s)

(** Insert metadata at the beginning if none is set. Currently used by the
   switch classes. *)
class replay meta src =
  object
    inherit operator ~name:"replay_metadata" [src]
    val mutable first = true
    method fallible = src#fallible
    method private can_generate_frame = src#is_ready
    method abort_track = src#abort_track
    method remaining = src#remaining
    method self_sync = src#self_sync
    method seek_source = src#seek_source

    method private generate_frame =
      let buf = src#get_frame in
      if first then (
        first <- false;
        if Frame.get_all_metadata buf = [] then Frame.add_metadata buf 0 meta
        else buf)
      else buf
  end
