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

exception Conflict of string * string

type kind = Frame.kind Unifier.t
type t = kind Fields.t

let deref = Unifier.deref
let of_kind = Frame.map_fields Unifier.make
let set_audio fields k = Frame.set_audio_field fields (Unifier.make k)
let set_video fields k = Frame.set_video_field fields (Unifier.make k)
let set_midi fields k = Frame.set_midi_field fields (Unifier.make k)

let content_type =
  let get field v =
    match deref v with
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
  Frame.mapi_fields get

let to_string fields =
  Frame.string_of_content_kind (Frame.map_fields deref fields)

let unify_kind k k' =
  let to_s k = Frame.string_of_kind (deref k) in
  try
    match (deref k, deref k') with
      (* Formats *)
      | `Kind ki, `Kind ki' when ki = ki' -> Unifier.(k <-- k')
      (* Params *)
      | `Format f, `Format f' -> Content.merge f f'
      (* Format/params *)
      | `Kind ki, `Format f when Content.kind f = ki -> Unifier.(k <-- k')
      | `Format f, `Kind ki when Content.kind f = ki -> Unifier.(k' <-- k)
      (* `Internal/'a *)
      | `Internal, `Internal -> Unifier.(k <-- k')
      | `Internal, `Kind ki when Content.is_internal_kind ki ->
          Unifier.(k <-- k')
      | `Internal, `Format f when Content.(is_internal_format f) ->
          Unifier.(k <-- k')
      | `Kind ki, `Internal when Content.is_internal_kind ki ->
          Unifier.(k' <-- k)
      | `Format f, `Internal when Content.(is_internal_format f) ->
          Unifier.(k' <-- k)
      (* Any/'a *)
      | `Any, _ -> Unifier.(k <-- k')
      | _, `Any -> Unifier.(k' <-- k)
      | _ -> assert false
  with _ -> raise (Conflict (to_s k, to_s k'))

let unify k k' =
  unify_kind (Frame.find_audio k) (Frame.find_audio k');
  unify_kind (Frame.find_video k) (Frame.find_video k');
  unify_kind (Frame.find_midi k) (Frame.find_midi k')
