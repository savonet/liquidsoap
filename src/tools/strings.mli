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
  
module type Strings = sig
  (** A buffer of strings. *)
  type buffer

  type return
  
  (* Create a buffer from a string. *)
  val of_string : string -> buffer

  (* Create a buffer from the given bytes.
   * bytes will be copied. *)
  val of_bytes : bytes -> buffer

  (* Create a buffer from the given bytes.
   * bytes will not be copied. *)
  val unsafe_of_bytes : bytes -> buffer
  
  (** Render a buffer into a string. This operation can be costly (in terms of memory copies), avoid it. *)
  val to_string : buffer -> string
  
  val substring : buffer -> int -> int -> string
  
  (** Concatenation of strings. *)
  val of_list : string list -> buffer
  
  (** Add a string at the end of a buffer. *)
  val add : buffer -> string -> return

  (** Add bytes at the end of a buffer.
    * bytes will be copied. *)
  val add_bytes : buffer -> bytes -> return

  (** Add bytes at the end of a buffer.
    * bytes will not be copied. *)
  val unsafe_add_bytes : buffer -> bytes -> return  
  
  val add_substring : buffer -> string -> int -> int -> return
  
  (** Add subbytes at the end of a buffer. *)
  val add_subbytes : buffer -> bytes -> int -> int -> return
  
  (** Add subbytes at the end of a buffer with copying them. *)
  val unsafe_add_subbytes : buffer -> bytes -> int -> int -> return
  
  (** Add a string at the beginning of a buffer. *)
  val dda : string -> buffer -> return
  
  (** Iterate a function on all the strings (with given offset and length)
      contained in the buffer. *)
  val iter : (string -> int -> int -> unit) -> buffer -> unit
  
  val iter_view : (StringView.t -> unit) -> buffer -> unit
  
  (** Fold a function over all the strings (with given offset and length)
      contained in the buffer. *)
  val fold : ('a -> string -> int -> int -> 'a) -> 'a -> buffer -> 'a
  
  val fold_view : ('a -> StringView.t -> 'a) -> 'a -> buffer -> 'a
  
  (** Map a function over all the strings (with given offset and length)
      contained in the buffer. *)
  val map : (string -> int -> int -> (string*int*int)) -> buffer -> buffer
  
  val map_view : (StringView.t -> StringView.t) -> buffer -> buffer
  
  (** Drop the first given bytes. *)
  val drop : buffer -> int -> return
  
  (** Keep at most the last given bytes. *)
  val keep : buffer -> int -> return
  
  (** Sub-buffer of a buffer. *)
  val sub : buffer -> int -> int -> buffer
  
  (** Copy a substring. *)
  val blit : buffer -> bytes -> int -> unit
  
  (** Whether the buffer is the empty string. *)
  val is_empty : buffer -> bool
  
  (** Length of the buffer. *)
  val length : buffer -> int
  
  (** Append two buffers. *)
  val append : buffer -> buffer -> return
end

module Immutable : sig
  type t

  include Strings with type buffer := t and type return := t

  (** The empty buffer. *)
  val empty : t

  (** Concatenate a list of buffers. *)
  val concat : t list -> t
end
  
(** Mutable and thread-safe variant. *)
module Mutable : sig
  type t

  include Strings with type buffer := t and type return := unit
  
  val empty : unit -> t
  
  (** Convert from a immutable strings. *)
  val of_strings : Immutable.t -> t
  
  (** Convert to a immutable strings. *)
  val to_strings : t -> Immutable.t

  (** Append strings to the buffer. *)
  val append_strings : t -> Immutable.t -> unit

  (** Empty the buffer and return its content. *)
  val flush : t -> Immutable.t
end 

include module type of Immutable with type t = Immutable.t
