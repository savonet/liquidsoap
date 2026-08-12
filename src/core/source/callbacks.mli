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

(** An ordered list of callbacks that a callback can be removed from.

    Callbacks a source holds live as long as it does, so anything registering on
    a source that outlives it has to be able to take its callbacks back. *)

type 'a t

val create : unit -> 'a t

(** Callbacks in registration order. Iteration is over a snapshot, so a callback
    may be released while the list is being iterated. *)
val elements : 'a t -> 'a list

val count : 'a t -> int

(** Register a callback and return the function releasing it. Calling that
    function more than once is a no-op. *)
val register : 'a t -> 'a -> unit -> unit

(** Register a callback that its registrant will never release. *)
val add : 'a t -> 'a -> unit
