(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

(** String buffers where the main operation is to add strings at the end. *)

(** A buffer of strings. *)
type t

(** The empty buffer. *)
val empty : t

val of_string : string -> t

(** Render a buffer into a string. This operation can be costly (in terms of memory copies), avoid it. *)
val to_string : t -> string

val substring : t -> int -> int -> string

(** Concatenation of strings. *)
val of_list : string list -> t

(** Add a string at the end of a buffer. *)
val add : t -> string -> t

val add_substring : t -> string -> int -> int -> t

(** Add subbytes at the end of a buffer. *)
val add_subbytes : t -> bytes -> int -> int -> t

(** Add a string at the beginning of a buffer. *)
val dda : string -> t -> t

(** Iterate a function on all the strings (with given offset and length)
    contained in the buffer. *)
val iter : (string -> int -> int -> unit) -> t -> unit

val iter_view : (StringView.t -> unit) -> t -> unit

(** Drop the first given chars. *)
val drop : t -> int -> t

(** Sub-buffer of a buffer. *)
val sub : t -> int -> int -> t

(** Copy a substring. *)
val blit : t -> bytes -> int -> unit

(** Whether the buffer is the empty string. *)
val is_empty : t -> bool

(** Length of the buffer. *)
val length : t -> int

(** Append two buffers. *)
val append : t -> t -> t

(** Concatenate a list of buffers. *)
val concat : t list -> t

(** Mutable and thread-safe variant. *)
module Mutable : sig
  type m

  (** Convert from a immutable strings. *)
  val of_strings : t -> m

  (** Convert to a immutable strings. *)
  val to_strings : m -> t

  (** Add a string at the end of a buffer. *)
  val add : m -> string -> unit

  val add_substring : m -> string -> int -> int -> unit

  (** Add subbytes at the end of a buffer. *)
  val add_subbytes : m -> bytes -> int -> int -> unit

  (** Add a string at the beginning of a buffer. *)
  val dda : string -> m -> unit

  (** Drop the first given chars. *)
  val drop : m -> int -> unit

  (** Whether the buffer is the empty string. *)
  val is_empty : m -> bool

  (** Length of the buffer. *)
  val length : m -> int
end

