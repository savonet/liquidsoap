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

(** Compatibilities between content kinds, types and values.
  * [sub a b] if [a] is more permissive than [b]..
  * TODO this is the other way around... it's correct in Lang, phew! *)

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

let set_track_mark frame pos =
  let old_marks = get frame Fields.track_marks in
  let length = max (Content.length old_marks) pos in
  let new_marks = Content.make ~length (Content.format old_marks) in
  Content.Track_marks.set_data new_marks [pos];
  Fields.add Fields.track_marks new_marks frame

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

let map_metadata frame fn =
  let metadata = get frame Fields.metadata in
  let new_metadata = fn (Content.Metadata.get_data metadata) in
  let length =
    max (Content.length metadata)
      (List.fold_left (fun p (p', _) -> max p p') 0 new_metadata)
  in
  let new_metadata_content = Content.make ~length (Content.format metadata) in
  Content.Metadata.set_data new_metadata_content new_metadata;
  Fields.add Fields.metadata new_metadata_content frame

let clear_metadata frame = map_metadata frame (fun _ -> [])
let add_metadata frame pos m = map_metadata frame (fun meta -> (pos, m) :: meta)
let set_all_metadata frame m' = map_metadata frame (fun _ -> m')

let free_metadata frame pos =
  map_metadata frame (fun meta -> List.filter (fun (p, _) -> p <> pos) meta)
