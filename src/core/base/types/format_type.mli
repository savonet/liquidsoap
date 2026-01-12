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

type descr = [ `Format of Content_base.format | `Kind of Content_base.kind ]

exception Never_type

val descr : descr -> Type.descr
val pcm_audio : Type.constr
val track : Type.constr
val muxed_tracks : Type.constr
val internal_tracks : Type.constr
val content_type : Type.t -> Content_base.format
val kind_handler : Content_base.kind * Type.t -> Type.custom_handler

(** Some common types *)
val audio : ?pcm_kind:Content_base.kind -> unit -> Type.t

val audio_mono : ?pcm_kind:Content_base.kind -> unit -> Type.t
val audio_stereo : ?pcm_kind:Content_base.kind -> unit -> Type.t
val audio_n : ?pcm_kind:Content_base.kind -> int -> Type.t
val video : unit -> Type.t
val midi : unit -> Type.t
val midi_n : int -> Type.t
val track_marks : Type.t
val metadata : Type.t
