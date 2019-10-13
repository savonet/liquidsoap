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

(** Bytes buffers where the main operation is to add bytes at the end. 
  * Main purpose of these buffers is to avoid data copy as much as possible.
  * therefore, all functions receiving buffer entries have signatures of the form:
  * [fn bytes offset length] where [offset] and [length] represents the portion of
  * [bytes] that is actually held by the buffer. *)

(** A buffer of bytes. *)
type t

(** The empty buffer. *)
val empty : unit -> t

(** Initialize a buffer wih the given string. *)
val of_string : string -> t

(** Initialize a buffer wih the given bytes. 
  * bytes will be copied. *)
val of_bytes : bytes -> t

(** Initialize a buffer wih the given bytes.
  * bytes will not be copied. *)
val unsafe_of_bytes : bytes -> t

(** Render a buffer as bytes. *)
val to_bytes : t -> bytes

(** Render a buffer as a string. *)
val to_string : t -> string

(** Concatenation of strings. *)
val of_list : strings list -> t

(** Concatenation of bytes.
  * bytes will be copied. *)
val of_bytes_list : bytes list -> t

(** Concatenation of bytes.
  * bytes will not be copied. *)
val unsafe_of_bytes_list : bytes list -> t

(** Copy a buffer. *)
val copy : t -> t

(** Add bytes at the end of the buffer. *)
val add : t -> bytes -> unit

(** Add a subbytes of bytes at the end of the buffer. *)
val add_subbytes : t -> bytes -> int -> int -> unit

(** Add bytes at the beginning of the buffer. *)
val dda : t -> bytes -> unit

(** Add a subbytes of bytes at the beginning of the buffer. *)
val dda_subbytes : t -> bytes -> int -> int -> unit

(** Iterate a function on all the bytes contained in the buffer. *)
val iter : (bytes -> int -> int -> unit) -> t -> unit

(** Fold a function over all the bytes in a buffer. *)
val fold : ('a -> bytes -> int -> int -> 'a) -> 'a -> t -> 'a

(** Drop the first given chars. *)
val drop : t -> int -> unit

(** Keep a suffix of at most the given length. *)
val keep : t -> int -> unit

(** Sub-buffer of a buffer. *)
val sub : t -> int -> int -> t

(** Copy a subbytes. *)
val blit : t -> int -> bytes -> int -> int -> unit

(** Total bytes currently in the buffer. *)
val length : t -> int

(** Whether the buffer is the empty bytes. *)
val is_empty : t -> bool

(** Append second buffer at the end of first one. *)
val append : t -> t -> unit

(** Concatenate a list of buffers. *)
val concat : t list -> t
