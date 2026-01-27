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

(** Operations on frames, which are small portions of streams. *)

(** {2 Frame definitions} *)

let field_idx = Atomic.make 0

module FieldNames = Hashtbl.Make (struct
  type t = int

  let equal (x : int) (y : int) = x = y [@@inline always]
  let hash (x : int) = x [@@inline always]
end)

module Fields = struct
  include Liquidsoap_lang.Methods

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
end

type field = Fields.field

(** Precise description of the channel types for the current track. *)
type content_type = Content_base.format Fields.t

type t = Content_base.data Fields.t

let position frame =
  Option.value ~default:0
    (Fields.fold
       (fun _ c -> function
         | None -> Some (Content_base.length c)
         | Some p -> Some (min p (Content_base.length c)))
       frame None)

let remaining b = Lazy.force Frame_settings.size - position b
let is_partial b = 0 < remaining b

(** Metadata of a frame. *)
module Metadata = Metadata_base

type metadata = Metadata_base.t

let audio_format ~pcm_kind params =
  let lift_params =
    match pcm_kind with
      | _ when Content_audio.is_kind pcm_kind -> Content_audio.lift_params
      | _ when Content_pcm_s16.is_kind pcm_kind -> Content_pcm_s16.lift_params
      | _ when Content_pcm_f32.is_kind pcm_kind -> Content_pcm_f32.lift_params
      | _ -> raise Content_base.Invalid
  in
  lift_params params

let format_of_channels ~pcm_kind n =
  audio_format ~pcm_kind
    {
      Content_audio.Specs.channel_layout =
        Lazy.from_val (Audio_converter.Channel_layout.layout_of_channels n);
    }

let add_timed_content ?length content =
  Fields.add Fields.track_marks
    (Content_base.make ?length Content_timed.Track_marks.format)
    (Fields.add Fields.metadata
       (Content_base.make ?length Content_timed.Metadata.format)
       content)
