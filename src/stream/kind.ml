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
type t = kind fields

let deref = Unifier.deref

let of_kind { audio; video; midi } =
  let f = Unifier.make in
  { audio = f audio; video = f video; midi = f midi }

let set_audio { video; midi } audio =
  { Frame.audio = Unifier.make audio; video; midi }

let set_video { audio; midi } video =
  { Frame.audio; video = Unifier.make video; midi }

let set_midi { audio; video } midi =
  { Frame.audio; video; midi = Unifier.make midi }

let content_type { audio; video; midi } =
  let get t v =
    match deref v with
      | `Internal | `Any -> (
          match t with
            | `Audio -> Content.default_audio ()
            | `Video -> Content.default_video ()
            | `Midi -> Content.default_midi ())
      | `Format f -> f
      | `Kind k -> Content.default_format k
  in
  {
    Frame.audio = get `Audio audio;
    video = get `Video video;
    midi = get `Midi midi;
  }

let to_string { audio; video; midi } =
  Frame.string_of_content_kind
    { Frame.audio = deref audio; video = deref video; midi = deref midi }

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
      | `Internal, `Kind ki when Content.is_internal ki -> Unifier.(k <-- k')
      | `Internal, `Format f when Content.(is_internal (kind f)) ->
          Unifier.(k <-- k')
      | `Kind ki, `Internal when Content.is_internal ki -> Unifier.(k' <-- k)
      | `Format f, `Internal when Content.(is_internal (kind f)) ->
          Unifier.(k' <-- k)
      (* Any/'a *)
      | `Any, _ -> Unifier.(k <-- k')
      | _, `Any -> Unifier.(k' <-- k)
      | _ -> assert false
  with _ -> raise (Conflict (to_s k, to_s k'))

let unify k k' =
  unify_kind k.audio k'.audio;
  unify_kind k.video k'.video;
  unify_kind k.midi k'.midi
