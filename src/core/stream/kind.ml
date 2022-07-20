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

open Frame
module Content_unifier = Liquidsoap_lang.Content_unifier

exception Conflict = Liquidsoap_lang.Content_unifier.Conflict

type kind = Content_unifier.content
type t = Content_unifier.t

let of_fields fields = Content_unifier.make ~sealed:true ~fields ()

let of_kind fields =
  of_fields (Frame.Fields.map Content_unifier.make_content fields)

let set_field field fields k =
  let fields = Content_unifier.fields fields in
  let content = Content_unifier.make_content k in
  of_fields (Frame.Fields.add field content fields)

let set_audio = set_field Frame.audio_field
let set_video = set_field Frame.video_field
let set_midi = set_field Frame.midi_field

let find_field field fields =
  Content_unifier.content
    (Frame.Fields.find field (Content_unifier.fields fields))

let find_audio = find_field Frame.audio_field
let find_video = find_field Frame.video_field
let find_midi = find_field Frame.midi_field

let content_type k =
  let get field v =
    match Content_unifier.content v with
      | `Internal | `Any -> (
          match field with
            | v when v = Frame.audio_field -> Content.default_audio ()
            | v when v = Frame.video_field -> Content.default_video ()
            | v when v = Frame.midi_field -> Content.default_midi ()
            | _ ->
                failwith
                  ("No default content for field: " ^ string_of_field field))
      | `Format f -> f
      | `Kind k -> Content.default_format k
  in
  Frame.mapi_fields get (Content_unifier.fields k)

let to_string = Content_unifier.to_string
let unify_kind = Content_unifier.unify_content
let unify k k' = Content_unifier.(k <: k')
