(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
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

let compatible c c' =
  let f = List.map fst (Fields.bindings c) in
  let f' = List.map fst (Fields.bindings c') in
  S.(equal (of_list f) (of_list f'))
  && Fields.for_all (fun k v -> Content.compatible v (Fields.find k c')) c

(* Frames *)

let create content_type =
  Frame_base.Fields.map (Content.make ~length:0) content_type

let content_type = Fields.map Content.format

let slice frame len =
  Fields.map
    (fun c -> if Content.length c < len then c else Content.sub c 0 len)
    frame

let append f f' =
  Fields.mapi (fun field c -> Content.append c (Fields.find field f')) f

let get frame field = Fields.find field frame
let audio frame = get frame Fields.audio
let video frame = get frame Fields.video
let midi frame = get frame Fields.midi

(** Content independent *)

let track_marks frame =
  Content.Track_marks.get_data (get frame Fields.track_marks)

let add_track_mark frame pos =
  let track_marks = track_marks frame in
  Fields.add Fields.track_marks
    (Content.Track_marks.lift_data (pos :: track_marks))
    frame

let position frame =
  Option.value ~default:0
    (Fields.fold
       (fun field c -> function
         | v when field = Fields.track_marks || field = Fields.metadata -> v
         | None -> Some (Content.length c)
         | Some p -> Some (min p (Content.length c)))
       frame None)

let remaining b = !!size - position b
let is_partial b = 0 < remaining b

(** Metadata stuff *)

exception No_metadata

let get_all_metadata frame =
  Content.Metadata.get_data (get frame Fields.metadata)

let get_metadata b t =
  try Some (List.assoc t (get_all_metadata b)) with Not_found -> None

let add_metadata frame pos m =
  let metadata = get_all_metadata frame in
  Fields.add Fields.metadata
    (Content.Metadata.lift_data ((pos, m) :: metadata))
    frame
