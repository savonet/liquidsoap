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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** Operations on frames, which are small portions of streams. *)

(** {2 Frame definitions} *)

type field = int

let field_idx = Atomic.make 0

module FieldNames = Hashtbl.Make (struct
  type t = int

  let equal (x : int) (y : int) = x = y [@@inline always]
  let hash (x : int) = x [@@inline always]
end)

module Fields = Map.Make (struct
  type t = field

  let compare (x : int) (y : int) = x - y [@@inline always]
end)

let field_names = FieldNames.create 0
let name_fields = Hashtbl.create 0
let string_of_field = FieldNames.find field_names
let field_of_string = Hashtbl.find name_fields

let register_field name =
  try field_of_string name
  with Not_found ->
    let field = Atomic.fetch_and_add field_idx 1 in
    FieldNames.replace field_names field name;
    Hashtbl.replace name_fields name field;
    field

(** Precise description of the channel types for the current track. *)
type content_type = Content_base.format Fields.t

(** Metadata of a frame. *)
type metadata = (string, string) Hashtbl.t

let audio_field = register_field "audio"
let video_field = register_field "video"
let midi_field = register_field "midi"

let audio_field_n = function
  | 0 -> audio_field
  | n -> register_field (Printf.sprintf "audio_%d" n)

let video_field_n = function
  | 0 -> video_field
  | n -> register_field (Printf.sprintf "video_%d" n)

let find_field c field = Fields.find field c
let find_field_opt c field = Fields.find_opt field c
let find_audio c = find_field_opt c audio_field
let find_video c = find_field_opt c video_field
let find_midi c = find_field_opt c midi_field
let set_field fields field value = Fields.add field value fields
let set_audio_field fields = set_field fields audio_field
let set_video_field fields = set_field fields video_field
let set_midi_field fields = set_field fields midi_field
let has_field fields field = Fields.mem field fields

let mk_fields ?audio ?video ?midi () =
  List.fold_left
    (fun fields -> function
      | _, None -> fields
      | field, Some v -> Fields.add field v fields)
    Fields.empty
    [(audio_field, audio); (video_field, video); (midi_field, midi)]
