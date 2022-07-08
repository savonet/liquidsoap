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

type field

val string_of_field : field -> string
val field_of_string : string -> field
val register_field : string -> field

module Fields : Map.S with type key = field

(** High-level description of the content. *)
type kind =
  [ `Any | `Internal | `Kind of Content.kind | `Format of Content.format ]

val none : kind

type content_kind = kind Fields.t

(** Precise description of the channel types for the current track. *)
type content_type = Content.format Fields.t

(** Metadata of a frame. *)
type metadata = (string, string) Hashtbl.t

val audio_field : field
val video_field : field
val midi_field : field

(* The following raise [Not_found] when the field does not exist. *)
val find_audio : 'a Fields.t -> 'a
val find_video : 'a Fields.t -> 'a
val find_midi : 'a Fields.t -> 'a
val set_audio_field : 'a Fields.t -> 'a -> 'a Fields.t
val set_video_field : 'a Fields.t -> 'a -> 'a Fields.t
val set_midi_field : 'a Fields.t -> 'a -> 'a Fields.t
val mk_fields : audio:'a -> video:'a -> midi:'a -> unit -> 'a Fields.t
