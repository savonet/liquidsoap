(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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

(**
  * Ringbuffers.
  *
  * @author David Baelde, Samuel Mimram
  *)

(** A ringbuffer value. *)
type t

(** Create a ringbuffer of a given number of channels and length (in samples)
  * per channel. *)
val create : int -> int -> t

(** Get the length of available data. *)
val read_space : t -> int
(** Get the length of available read space. *)
val write_space : t -> int

(** Advance the read pointer. *)
val read_advance : t -> int -> unit
(** Advance the write pointer. *)
val write_advance : t -> int -> unit

(** In the following:
  - calls to write have to be sequential.
  - calls to read and transmit have to be sequential.
  - both kinds of operations can be done from different threads. *)

(** [read rb buf ofs len] reads [len] data from the ringbuffer [rb] and writes
  * it in [buf] starting at position [ofs]. *)
val read : t -> float array array -> int -> int -> unit
(** [write rb buf ofs len] writes [len] data from the buffer [buf] starting at
  * position [ofs] into the ringbuffer [rb]. *)
val write : t -> float array array -> int -> int -> unit

(** [transmit rb f] transmits the next available chunk of data in the
  * ringbuffer [rb] through [f] and returns the length of the chunk actually
  * read by [f]. *)
val transmit : t -> (float array array -> int -> int -> int) -> int

