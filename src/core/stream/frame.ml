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

let create ~length content_type =
  add_timed_content ~length
    (Frame_base.Fields.map (Content.make ~length) content_type)

let content_type = Fields.map Content.format

let position frame =
  Option.value ~default:0
    (Fields.fold
       (fun _ c -> function
         | None -> Some (Content.length c)
         | Some p -> Some (min p (Content.length c)))
       frame None)

let remaining b = !!size - position b
let is_partial b = 0 < remaining b

let chunk ~start ~stop frame =
  Fields.map (fun c -> Content.sub c start (stop - start)) frame

let slice frame len =
  Fields.map
    (fun c -> if Content.length c < len then c else Content.sub c 0 len)
    frame

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

let add_track_marks frame l =
  let old_marks = get frame Fields.track_marks in
  let length =
    List.fold_left
      (fun length pos -> max pos length)
      (Content.length old_marks) l
  in
  let new_marks = Content.make ~length (Content.format old_marks) in
  Content.Track_marks.set_data new_marks
    (l @ Content.Track_marks.get_data old_marks);
  Fields.add Fields.track_marks new_marks frame

let add_track_mark frame pos = add_track_marks frame [pos]

let map_chunks fn f =
  let rec map (cur : t) = function
    | _ :: [] -> cur
    | start :: stop :: rest ->
        map (append cur (fn (chunk ~start ~stop f))) (stop :: rest)
    | _ -> assert false
  in
  map (slice f 0) (0 :: (track_marks f @ [position f]))

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
    (l @ Content.Metadata.get_data old_metadata);
  Fields.add Fields.metadata new_metadata frame

let add_metadata frame pos m = add_all_metadata frame [(pos, m)]
