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

include Frame_settings
include Frame_base
open Content

(** Compatibilities between content kinds, types and values. [sub a b] if [a] is
    more permissive than [b].. TODO this is the other way around... it's correct
    in Lang, phew! *)

let string_of_format = string_of_format

let string_of_fields fn fields =
  Printf.sprintf "{%s}"
    (String.concat ","
       (List.sort Stdlib.compare
          (Fields.fold
             (fun f v cur ->
               Printf.sprintf "%s=%s" (Fields.string_of_field f) (fn v) :: cur)
             fields [])))

let string_of_content_type = string_of_fields string_of_format

module S = Set.Make (struct
  type t = Fields.field

  let compare = Stdlib.compare
end)

let filter_implicit_fields (lbl, _) =
  if List.mem lbl [Fields.metadata; Fields.track_marks] then None else Some lbl

let assert_compatible c c' =
  let f = List.filter_map filter_implicit_fields (Fields.bindings c) in
  let f' = List.filter_map filter_implicit_fields (Fields.bindings c') in
  if not S.(equal (of_list f) (of_list f')) then
    failwith
      (Printf.sprintf
         "Content_types %s and %s do not have the same set of fields!"
         (string_of_content_type c)
         (string_of_content_type c'));
  Fields.iter
    (fun k v ->
      if not (Content.compatible v (Fields.find k c')) then
        failwith
          (Printf.sprintf "Content_types %s and %s: incompatible field %s"
             (string_of_content_type c)
             (string_of_content_type c')
             (Fields.string_of_field k)))
    c

let compatible c c' =
  try
    assert_compatible c c';
    true
  with _ -> false

(* Frames *)

let create ~length content_type =
  add_timed_content ~length
    (Frame_base.Fields.map (Content.make ~length) content_type)

let content_type = Fields.map Content.format
let sub frame ofs len = Fields.map (fun c -> Content.sub c ofs len) frame
let slice frame len = sub frame 0 (min len (position frame))
let after frame offset = sub frame offset (position frame - offset)

let append f f' =
  Fields.mapi (fun field c -> Content.append c (Fields.find field f')) f

let get frame field = Fields.find field frame
let set frame field c = Fields.add field c frame

let set_data frame field lift c =
  Fields.add field (lift ?offset:None ?length:(Some (position frame)) c) frame

let audio frame = get frame Fields.audio
let video frame = get frame Fields.video
let midi frame = get frame Fields.midi

(** Content independent *)

let track_marks frame =
  Content.Track_marks.get_data (get frame Fields.track_marks)

let has_track_marks frame = track_marks frame <> []
let has_track_mark frame pos = List.mem pos (track_marks frame)

let add_track_marks frame l =
  let old_marks = get frame Fields.track_marks in
  let length =
    List.fold_left
      (fun length pos -> max pos length)
      (Content.length old_marks) l
  in
  let new_marks = Content.make ~length (Content.format old_marks) in
  Content.Track_marks.set_data new_marks
    (List.sort_uniq Stdlib.compare (l @ Content.Track_marks.get_data old_marks));
  Fields.add Fields.track_marks new_marks frame

let add_track_mark frame pos = add_track_marks frame [pos]

let drop_track_marks frame =
  Fields.add Fields.track_marks
    (Content.make ~length:(position frame) Content_timed.Track_marks.format)
    frame

(** Metadata stuff *)

exception No_metadata

let get_all_metadata frame =
  Content.Metadata.get_data (get frame Fields.metadata)

let get_metadata b t =
  try Some (List.assoc t (get_all_metadata b)) with Not_found -> None

let add_all_metadata frame l =
  let old_metadata = get frame Fields.metadata in
  let length =
    List.fold_left
      (fun length (pos, _) -> max pos length)
      (Content.length old_metadata)
      l
  in
  let new_metadata = Content.make ~length (Content.format old_metadata) in
  Content.Metadata.set_data new_metadata
    (List.sort_uniq
       (fun (p, _) (p', _) -> Stdlib.compare p p')
       (l @ Content.Metadata.get_data old_metadata));
  Fields.add Fields.metadata new_metadata frame

let map_metadata frame fn =
  let metadata = get_all_metadata frame in
  let metadata = List.filter_map fn metadata in
  add_all_metadata frame metadata

let add_metadata frame pos m = add_all_metadata frame [(pos, m)]

let free_metadata frame pos =
  let metadata = get frame Fields.metadata in
  let new_metadata =
    Content.make ~length:(Content.length metadata) (Content.format metadata)
  in
  Content.Metadata.set_data new_metadata
    (List.filter (fun (p, _) -> p <> pos) (Content.Metadata.get_data metadata));
  Fields.add Fields.metadata new_metadata frame
