(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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
  object (self)
    inherit operator ~name:"insert_metadata" [source]
    method stype = source#stype
    method is_ready = source#is_ready
    method remaining = source#remaining
    method seek = source#seek
    method abort_track = source#abort_track
    method self_sync = source#self_sync
    val mutable metadata = None
    val mutable new_track = false
    val lock_m = Mutex.create ()
    val mutable ns = []

    method insert_metadata nt m : unit =
      Mutex.lock lock_m;
      metadata <- Some m;
      new_track <- nt;
      Mutex.unlock lock_m

    method private add_metadata frame pos =
      Tutils.mutexify lock_m
        (fun () ->
          (match (metadata, Frame.get_metadata frame pos) with
            | Some m, None ->
                metadata <- None;
                Frame.set_metadata frame pos m
            | _ -> ());
          metadata <- None)
        ()

    method private insert_track =
      Tutils.mutexify lock_m
        (fun () ->
          let ret = new_track in
          new_track <- false;
          ret)
        ()

    method private get_frame buf =
      let p = Frame.position buf in
      if self#insert_track then Frame.add_break buf p
      else (
        (* Insert new metadata _after_ the call to #get
            otherwise, it will be visible to sources under
           this one! See: #1115 *)
        source#get buf;
        self#add_metadata buf p)
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
                s#insert_metadata new_track m;
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
    method stype = src#stype
    method is_ready = src#is_ready
    method abort_track = src#abort_track
    method remaining = src#remaining
    method self_sync = src#self_sync
    method seek = src#seek

    method private get_frame ab =
      let start = Frame.position ab in
      src#get ab;
      let stop = Frame.position ab in
      if first then (
        if
          List.filter
            (fun (pos, _) -> start <= pos && pos <= stop)
            (Frame.get_all_metadata ab)
          = []
        then Frame.set_metadata ab start meta;
        first <- false)
  end
