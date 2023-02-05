(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

(** Support for multicore processors. *)

(** Whether we actually try to run things in parallel. *)
val parallelism : bool ref

(** Iterate a function on a list in parallel. *)
val iter : ('a -> unit) -> 'a list -> unit

(** Fold a function on a list in parallel. *)
val fold : reconcile:('b -> 'b -> 'b) -> ('a -> 'b) -> 'b -> 'a list -> 'b
