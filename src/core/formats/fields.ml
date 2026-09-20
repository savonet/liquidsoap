(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

(** The fields of a frame -- audio, video, metadata and whatever else a script
    names -- as the language sees them. *)

let field_idx = Atomic.make 0

module FieldNames = Hashtbl.Make (struct
  type t = int

  let equal (x : int) (y : int) = x = y [@@inline always]
  let hash (x : int) = x [@@inline always]
end)

include Liquidsoap_lang_data.Methods

type field = int
type nonrec 'a t = (field, 'a) t

let field_names = FieldNames.create 0
let name_fields = Hashtbl.create 0
let string_of_field = FieldNames.find field_names
let field_of_string = Hashtbl.find name_fields

let register name =
  try field_of_string name
  with Not_found ->
    let field = Atomic.fetch_and_add field_idx 1 in
    FieldNames.replace field_names field name;
    Hashtbl.replace name_fields name field;
    field

let metadata = register "metadata"
let track_marks = register "track_marks"
let audio = register "audio"
let video = register "video"
let data = register "data"
let midi = register "midi"
let subtitles = register "subtitles"

let audio_n = function
  | 0 -> audio
  | n -> register (Printf.sprintf "audio_%d" (n + 1))

let video_n = function
  | 0 -> video
  | n -> register (Printf.sprintf "video_%d" (n + 1))

let data_n = function
  | 0 -> data
  | n -> register (Printf.sprintf "data_%d" (n + 1))

let subtitles_n = function
  | 0 -> subtitles
  | n -> register (Printf.sprintf "subtitles_%d" (n + 1))

let make =
  let audio_f = audio in
  let video_f = video in
  let midi_f = midi in
  fun ?audio ?video ?midi () ->
    List.fold_left
      (fun fields -> function
        | _, None -> fields | field, Some v -> add field v fields)
      empty
      [(audio_f, audio); (video_f, video); (midi_f, midi)]
