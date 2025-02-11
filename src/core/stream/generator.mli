(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

(** A generator is a collection of content with different length. The set of
    content contained by a generator is given by its [content_type] to which
    metadata and track marks are always added.

    For now, metadata and track breaks content have infinite length. All other
    content have finite length.

    A generator buffered length is the largest content length, excluding
    metadata and track breaks. If that length exceeds [max_length], the
    generator is truncated to keep it under that value.

    A generator length is the smallest content length, again excluding metadata
    and track breaks. This is the maximum content length that can be taken out
    of the generator. *)

type t

val create :
  ?log:Log.t ->
  ?max_length:int ->
  ?length:int ->
  ?content:Frame.t ->
  Frame_base.content_type ->
  t

val max_length : t -> int option
val set_max_length : t -> int option -> unit
val content_type : t -> Frame_base.content_type

(* Return the content associated with a given [field]. *)
val get_field : t -> Frame_base.field -> Content.data

(* Set the content associated with a given [field]. *)
val set_field : t -> Frame_base.field -> Content.data -> unit

(* Length of content for the given field. *)
val field_length : t -> Frame_base.field -> int
val length : t -> int
val buffered_length : t -> int

(* Remaining time before the next track mark or -1 if no
   track marks are present. *)
val remaining : t -> int

(* Drop given length of content at the beginning of the generator. *)
val truncate : t -> int -> unit

(* Keep only the given length of data from the beginning of the generator. *)
val keep : t -> int -> unit

(* Return at most the given len of data from the start of the generator
   and truncate the generator of that data. *)
val slice : t -> int -> Frame.t

(* Empty the generator. *)
val clear : t -> unit

(* Add content to one of the generator's field. Do not use
   with metadata or track marks, use the corresponding add method
   for these type of content. *)
val put : t -> Frame_base.field -> Content.data -> unit

(* Return the generator's content without removing it. *)
val peek : t -> Content.data Frame_base.Fields.t

(* Return the generator's media content (all tracks excluding metadata and track_marks) without removing it. *)
val peek_media : t -> Content.data Frame_base.Fields.t

(* Insert a metadata at the given position. To be used over [put]
   for metadata. Default position is generator's length. *)
val add_metadata : ?pos:int -> t -> Frame_base.metadata -> unit

(* Insert a track mark at the given position. To be used over [put]
   for track mark. Default position is generator's length. *)
val add_track_mark : ?pos:int -> t -> unit

(* Append a frame content to a generator. *)
val append : ?offset:int -> ?length:int -> t -> Frame_base.t -> unit
