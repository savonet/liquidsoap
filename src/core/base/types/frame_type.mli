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

open Liquidsoap_lang

(* This module implements frame types. This makes it possible to change
   the frame type implementation without impacting users of these types. *)

(* Same as [Lang.frame_t] (with position) *)
val make : ?pos:Pos.t -> Type.t -> Type.t Frame.Fields.t -> Type.t

(* Same as [Lang.internal_tracks_t] (with position) *)
val internal_tracks : ?pos:Pos.t -> unit -> Type.t

(* Same as [Lang.pcm_audio_t] (with position) *)
val pcm_audio : ?pos:Pos.t -> unit -> Type.t

(* [set_field frame_type field field_type] assigns a field to a frame type. *)
val set_field : Type.t -> Frame.field -> Type.t -> Type.t

(* [get_field frame_type field] returns the frame field's type.
   Raises [Not_found] if field does not exist. *)
val get_field : Type.t -> Frame.field -> Type.t

(* [get_fields frame_type] returns all the field currently
   set for the given type. *)
val get_fields : Type.t -> Frame.field list

(* Resolve a frame type into a content type. If the frame has explicit fields,
   this seales the frame type and resolves kind formats with their default format.
   If the frame is a universal variable ['a], default audio/video fields are added
   and the type is sealed. *)
val content_type : Type.t -> Content.format Frame.Fields.t
