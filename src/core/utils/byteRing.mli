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

(** A byte ring with one writer and any number of readers, none of them locking.
    The writer appends and publishes the tail; a reader copies a range behind
    the tail and learns whether the writer overtook it. *)

type t

val create : capacity:int -> t

(** Absolute offset of the next byte to be appended. *)
val tail : t -> int

val capacity : t -> int

(** Writer only. A chunk larger than the capacity grows the ring to twice the
    chunk. *)
val append : t -> Strings.t -> unit

(** [read t ~ofs dst dst_ofs len] copies the bytes at absolute offsets
    [ofs, ofs + len) into [dst]. Returns [false] when they are not all
    available: not yet written, or overwritten before or during the copy. *)
val read : t -> ofs:int -> Bytes.t -> int -> int -> bool
