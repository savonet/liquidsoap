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

(** Bytes buffers where the main operation is to add a string at the end. 
  * Main purpose of these buffers is to avoid data copy as much as possible.
  * therefore, all functions receiving buffer entries have signatures of the form:
  * [fn string offset length] where [offset] and [length] represents the portion of
  * [string] that is actually held by the buffer. *)

(** Buffer operational mode. *)
type mode = [
  | `RW
  | `RO
]

(** A buffer of strings. *)
type 'a t

(** The empty buffer. *)
val empty : unit -> [< mode] t

(** Initialize a buffer wih the given string. *)
val of_string : string -> [< mode] t

(** Initialize a buffer wih the given bytes.
  * bytes will be copied. *)
val of_bytes : bytes -> [< mode] t

(** Initialize a buffer wih the given bytes.
  * bytes will not be copied. *)
val unsafe_of_bytes : bytes -> [< mode] t

(** Render a buffer as bytes. *)
val to_bytes : [< mode] t -> bytes

(** Render a buffer as a string. *)
val to_string : [< mode] t -> string

(** Concatenation of string. *)
val of_list : string list -> [< mode] t

(** Concatenation of bytes.
  * bytes will be copied. *)
val of_bytes_list : bytes list -> [< mode] t

(** Concatenation of bytes.
  * bytes will not be copied. *)
val unsafe_of_bytes_list : bytes list -> [< mode] t

(** Turn a buffer read-only. *) 
val seal : [< mode] t -> [`RO] t

(** Return a buffer with the given string added
  * as first element. *)
val prepend : string -> [< mode] t -> [< mode] t

(** Return a buffer with the given bytes added
  * as first element. bytes will be copied. *)
val prepend_bytes : bytes -> [< mode] t -> [< mode] t

(** Return a buffer with the given bytes added
  * as first element. bytes will not be copied. *)
val unsafe_prepend_bytes : bytes -> [< mode] t -> [< mode] t

(** Copy a buffer. *)
val copy : [< mode] t -> 'b t

(** Add a string at the end of the buffer. *)
val add : [< `RW] t -> string -> unit

(** Add a bytes at the end of the buffer. 
  * Bytes will be copied. *)
val add_bytes : [< `RW] t -> bytes -> unit

(** Add a bytes at the end of the buffer.
  * Bytes will not be copied. *)
val unsafe_add_bytes : [< `RW] t -> bytes -> unit

(** Add a substring of a string at the end of the buffer. *)
val add_substring : [< `RW] t -> string -> int -> int -> unit

(** Add a subbytes of some bytes at the end of the buffer.
  * bytes will be copied. *)
val add_subbytes : [< `RW] t -> bytes -> int -> int -> unit

(** Add a subbytes of some bytes at the end of the buffer.
  * bytes will not be copied. *)
val unsafe_add_subbytes : [< `RW] t -> bytes -> int -> int -> unit

(** Iterate a function on all the strings contained in the buffer. *)
val iter : (string -> int -> int -> unit) -> [< mode] t -> unit

(** Fold a function over all the strings in a buffer. *)
val fold : ('a -> string -> int -> int -> 'a) -> 'a -> [< mode] t -> 'a

(** Return a list of the strings contained in the buffer. *)
val to_list : [< mode] t -> (string*int*int) list

(** Return a list of the bytes contained in the buffer.
  * bytes will be copied. *)
val to_bytes_list : [< mode] t -> (bytes*int*int) list

(** Return a list of the bytes contained in the buffer.
  * bytes will not be copied. *)
val unsafe_to_bytes_list : [< mode] t -> (bytes*int*int) list

(** Drop the first [n] characters. *)
val drop : [< `RW] t -> int -> unit

(** Keep a suffix of at most [n] characters. *)
val keep : [< `RW] t -> int -> unit

(** Sub-buffer of a buffer. *)
val sub : [< mode] t -> int -> int -> [< mode] t

(** Copy a subbytes. *)
val blit : [< mode] t -> int -> bytes -> int -> int -> unit

(** Total bytes currently in the buffer. *)
val length : [< mode] t -> int

(** Whether the buffer is the empty bytes. *)
val is_empty : [< mode] t -> bool

(** Append second buffer at the end of first one. *)
val append : [< `RW] t -> [< mode] t -> unit

(** Concatenate a list of buffers. *)
val concat : [< mode] t list -> [< mode] t

(** Empty a buffer. *)
val flush : [< `RW] t -> unit
