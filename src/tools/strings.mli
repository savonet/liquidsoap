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

type buffer = t

(** The empty buffer. *)
val empty : t

(** Create a buffer from a string. *)
val of_string : string -> t

(** Create a buffer from the given bytes which will be copied. *)
val of_bytes : bytes -> t

(** Create a buffer from the given bytes which will not be copied (be careful). *)
val unsafe_of_bytes : bytes -> t

(** Render a buffer into bytes/string. This operation can be costly (in terms of
    memory copies), avoid it. *)
val to_string : t -> string

val to_bytes : t -> bytes

val substring : t -> int -> int -> string

(** Concatenation of strings. *)
val of_list : string list -> t

(** Add a string at the end of a buffer. *)
val add : t -> string -> t

(** Add bytes at the end of a buffer, bytes will be copied. *)
val add_bytes : t -> bytes -> t

(** Add bytes at the end of a buffer, bytes will not be copied. *)
val unsafe_add_bytes : t -> bytes -> t

val add_substring : t -> string -> int -> int -> t

(** Add subbytes at the end of a buffer. *)
val add_subbytes : t -> bytes -> int -> int -> t

(** Add subbytes at the end of a buffer with copying them. *)
val unsafe_add_subbytes : t -> bytes -> int -> int -> t

(** Add a string at the beginning of a buffer. *)
val dda : string -> t -> t

(** Iterate a function on all the strings (with given offset and length)
    contained in the buffer. *)
val iter : (string -> int -> int -> unit) -> t -> unit

val iter_view : (StringView.t -> unit) -> t -> unit

(** Fold a function over all the strings (with given offset and length)
    contained in the buffer. *)
val fold : ('a -> string -> int -> int -> 'a) -> 'a -> t -> 'a

val fold_view : ('a -> StringView.t -> 'a) -> 'a -> t -> 'a

(** Map a function over all the strings (with given offset and length) contained
    in the buffer. *)
val map : (string -> int -> int -> string * int * int) -> t -> t

val map_view : (StringView.t -> StringView.t) -> t -> t

(** Drop the first given bytes. *)
val drop : t -> int -> t

(** Keep the last given bytes. *)
val keep : t -> int -> t

(** Sub-buffer of a buffer. *)
val sub : t -> int -> int -> t

(** Copy a substring at given offset in the destination. Use [sub] on the first
    argument in the case you want to blit a substring. *)
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
  type t

  (** The empty buffer. *)
  val empty : unit -> t

  (** Create a buffer from a string. *)
  val of_string : string -> t

  val of_strings : buffer -> t

  (** Create a buffer from the given bytes which will be copied. *)
  val of_bytes : bytes -> t

  (** Create a buffer from the given bytes which will not be copied (be
      careful). *)
  val unsafe_of_bytes : bytes -> t

  (** Render a buffer into a string/bytes. This operation can be costly (in terms of
      memory copies), avoid it. *)
  val to_string : t -> string

  val to_bytes : t -> bytes

  val to_strings : t -> buffer

  val substring : t -> int -> int -> string

  (** Concatenation of strings. *)
  val of_list : string list -> t

  (** Add a string at the end of a buffer. *)
  val add : t -> string -> unit

  (** Add bytes at the end of a buffer, bytes will be copied. *)
  val add_bytes : t -> bytes -> unit

  (** Add bytes at the end of a buffer, bytes will not be copied. *)
  val unsafe_add_bytes : t -> bytes -> unit

  val add_substring : t -> string -> int -> int -> unit

  (** Add subbytes at the end of a buffer. *)
  val add_subbytes : t -> bytes -> int -> int -> unit

  (** Add subbytes at the end of a buffer with copying them. *)
  val unsafe_add_subbytes : t -> bytes -> int -> int -> unit

  (** Add a string at the beginning of a buffer. *)
  val dda : string -> t -> unit

  (** Iterate a function on all the strings (with given offset and length)
      contained in the buffer. *)
  val iter : (string -> int -> int -> unit) -> t -> unit

  val iter_view : (StringView.t -> unit) -> t -> unit

  (** Fold a function over all the strings (with given offset and length)
      contained in the buffer. *)
  val fold : ('a -> string -> int -> int -> 'a) -> 'a -> t -> 'a

  val fold_view : ('a -> StringView.t -> 'a) -> 'a -> t -> 'a

  (** Map a function over all the strings (with given offset and length)
     contained in the buffer. *)
  val map : (string -> int -> int -> string * int * int) -> t -> t

  val map_view : (StringView.t -> StringView.t) -> t -> t

  (** Drop the first given bytes. *)
  val drop : t -> int -> unit

  (** Keep the last given bytes. *)
  val keep : t -> int -> unit

  (** Sub-buffer of a buffer. *)
  val sub : t -> int -> int -> t

  (** Copy a substring. *)
  val blit : t -> int -> bytes -> int -> int -> unit

  (** Whether the buffer is the empty string. *)
  val is_empty : t -> bool

  (** Length of the buffer. *)
  val length : t -> int

  (** Append two buffers. *)
  val append : t -> t -> unit

  (** Append strings to the buffer. *)
  val append_strings : t -> buffer -> unit

  (** Empty the buffer and return its content. *)
  val flush : t -> buffer
end
