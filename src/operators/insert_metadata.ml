(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2013 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Source
open Genlex
exception Error

class insert_metadata ~kind source =
object (self)
  inherit operator ~name:"insert_metadata" kind [source] as super

  method stype = source#stype
  method is_ready = source#is_ready
  method remaining = source#remaining
  method abort_track = source#abort_track

  val mutable metadata = None
  val lock_m = Mutex.create ()
  val mutable ns = []

  method insert_metadata m =
    Mutex.lock lock_m ;
    metadata <- Some m ;
    Mutex.unlock lock_m

  method private get_frame buf =
    let p = Frame.position buf in
      source#get buf ;
      Mutex.lock lock_m ;
      Tutils.finalize ~k:(fun () -> Mutex.unlock lock_m)
      (fun () ->
        match metadata with
          | Some m ->
              Frame.set_metadata buf p m ;
              metadata <- None
          | None -> ())

end

let () =
  let kind = Lang.univ_t 1 in
  let return_t =
    Lang.product_t
     (Lang.fun_t [false,"",Lang.metadata_t] Lang.unit_t)
     (Lang.source_t kind)
  in
  Lang.add_builtin "insert_metadata"
    ~category:(Lang.string_of_category Lang.TrackProcessing)
    ~descr:"Dynamically insert metadata in a stream. \
            Returns a pair (f,s) where s is a new source and \
            f is a function of type (metadata)->unit, used to \
            insert metadata in s."
    [ "id",Lang.string_t,Some (Lang.string ""),
      Some "Force the value of the source ID.";
      "",Lang.source_t kind,None,None ] return_t
    (fun p t ->
       let s = Lang.to_source (List.assoc "" p) in
       let id = Lang.to_string (List.assoc "id" p) in
       let (_,t) = Lang.of_product_t t in
       let kind =
         Lang.frame_kind_of_kind_type (Lang.of_source_t t)
       in
       let s = new insert_metadata ~kind s in
       if id <> "" then s#set_id id ;
       let f =
         Lang.val_fun ["","",Lang.metadata_t,None] ~ret_t:Lang.unit_t
                      (fun p t ->
                         s#insert_metadata
                           (Lang.to_metadata (List.assoc "" p));
                         Lang.unit)
       in
       Lang.product f (Lang.source (s :> Source.source)))

(** Insert metadata at the beginning if none is set.
  * Currently used by the switch classes. *)
class replay ~kind meta src =
object (self)
  inherit operator ~name:"replay_metadata" kind [src]

  val mutable first = true

  method stype = src#stype
  method is_ready = src#is_ready
  method abort_track = src#abort_track
  method remaining = src#remaining

  method private get_frame ab =
    let start = Frame.position ab in
      src#get ab ;
      if first then begin
        if Frame.get_metadata ab start = None then
          Frame.set_metadata ab start meta ;
        first <- false
      end
end
