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

open Source

exception Error

class insert_metadata ~kind source =
  object (self)
    inherit operator ~name:"insert_metadata" kind [source]

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
      Mutex.lock lock_m ;
      metadata <- Some m ;
      new_track <- nt ;
      Mutex.unlock lock_m

    method private add_metadata frame pos =
      Tutils.mutexify lock_m
        (fun () ->
          match metadata with
            | None ->
                ()
            | Some m ->
                metadata <- None ;
                Frame.set_metadata frame pos m)
        ()

    method private insert_track =
      Tutils.mutexify lock_m
        (fun () ->
          let ret = new_track in
          new_track <- false ;
          ret)
        ()

    method private get_frame buf =
      let p = Frame.position buf in
      if self#insert_track then Frame.add_break buf p
      else (self#add_metadata buf p ; source#get buf)
  end

let () =
  let kind = Lang.univ_t () in
  let return_t =
    Lang.product_t
      (Lang.fun_t
         [(true, "new_track", Lang.bool_t); (false, "", Lang.metadata_t)]
         Lang.unit_t)
      (Lang.source_t kind)
  in
  Lang.add_builtin "insert_metadata"
    ~category:(Lang.string_of_category Lang.TrackProcessing)
    ~descr:
      "Dynamically insert metadata in a stream. Returns a pair `(f,s)` where \
       s is a new source and `f` is a function of type \
       `(?new_track,metadata)->unit`, used to insert metadata in `s`. `f` \
       also inserts a new track with the given metadata if passed \
       `new_track=true`."
    [ ( "id",
        Lang.string_t,
        Some (Lang.string ""),
        Some "Force the value of the source ID." );
      ("", Lang.source_t kind, None, None) ]
    return_t
    (fun p t ->
      let s = Lang.to_source (List.assoc "" p) in
      let id = Lang.to_string (List.assoc "id" p) in
      let _, t = Lang.of_product_t t in
      let kind = Lang.frame_kind_of_kind_type (Lang.of_source_t t) in
      let s = new insert_metadata ~kind s in
      if id <> "" then s#set_id id ;
      let f =
        Lang.val_fun
          [ ("new_track", "new_track", Lang.bool_t, Some (Lang.bool false));
            ("", "", Lang.metadata_t, None) ]
          ~ret_t:Lang.unit_t
          (fun p _ ->
            let m = Lang.to_metadata (List.assoc "" p) in
            let new_track = Lang.to_bool (List.assoc "new_track" p) in
            s#insert_metadata new_track m ;
            Lang.unit)
      in
      Lang.product f (Lang.source (s :> Source.source)))

(** Insert metadata at the beginning if none is set. Currently used by the
   switch classes. *)
class replay ~kind meta src =
  object
    inherit operator ~name:"replay_metadata" kind [src]

    val mutable first = true

    method stype = src#stype

    method is_ready = src#is_ready

    method abort_track = src#abort_track

    method remaining = src#remaining

    method self_sync = src#self_sync

    method private get_frame ab =
      let start = Frame.position ab in
      src#get ab ;
      if first then (
        if Frame.get_metadata ab start = None then
          Frame.set_metadata ab start meta ;
        first <- false )
  end
