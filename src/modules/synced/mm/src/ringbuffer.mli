(*
 * Copyright 2011 The Savonet Team
 *
 * This file is part of ocaml-mm.
 *
 * ocaml-mm is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-mm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-mm; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with a publicly
 * distributed version of the Library to produce an executable file containing
 * portions of the Library, and distribute that executable file under terms of
 * your choice, without any of the additional requirements listed in clause 6
 * of the GNU Library General Public License.
 * By "a publicly distributed version of the Library", we mean either the unmodified
 * Library as distributed by The Savonet Team, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU Library General
 * Public License. This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU Library General Public License.
 *
 *)

(** Operations on ringbuffers. *)

(** Underlying buffers of ringbuffers. *)
module type Buffer = sig
  (** Type of a buffer. *)
  type t

  (** Create a buffer of given length. *)
  val create : int -> t

  (** Blitting function. *)
  val blit : t -> int -> t -> int -> int -> unit
end

(** Signature for ringbuffer modules. *)
module type R = sig
  (** A buffer of elements. *)
  type buffer

  (** A ringbuffer. *)
  type t

  (** Create a ringbuffer of given size. *)
  val create : int -> t

  (** Size of data available for reading. *)
  val read_space : t -> int

  (** Size of space available for writing. *)
  val write_space : t -> int

  (** Drop data. *)
  val read_advance : t -> int -> unit

  (** Advance the write pointer. *)
  val write_advance : t -> int -> unit

  (** Read data. *)
  val read : t -> buffer -> int -> int -> unit

  (** Same as [read] but does not advance the read pointer. *)
  val peek : t -> buffer -> int -> int -> unit

  (** Write data. *)
  val write : t -> buffer -> int -> int -> unit

  (** Read all the data in the ringbuffer. *)
  val transmit : t -> (buffer -> int -> int -> int) -> int
end

(** Create a ringbuffer. *)
module Make : functor (B : Buffer) -> R with type buffer = B.t

(** Create an extensible ringbuffer: the size of the ringbuffer is extended if
    write space is too small at some point. *)
module Make_ext : functor (B : Buffer) -> R with type buffer = B.t
