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

(** Note: these queues are lock-free and not intended to hold large number of
    values. *)

module Queue : sig
  type 'a t

  val create : unit -> 'a t
  val is_empty : 'a t -> bool
  val push : 'a t -> 'a -> unit

  (** Raises [Not_found] when no element can be found. *)
  val pop : 'a t -> 'a

  val append : 'a t -> 'a -> unit
  val pop_opt : 'a t -> 'a option

  (** Raises [Not_found] when no element can be found. *)
  val peek : 'a t -> 'a

  val peek_opt : 'a t -> 'a option
  val flush_iter : 'a t -> ('a -> unit) -> unit
  val flush_fold : 'a t -> ('a -> 'b -> 'b) -> 'b -> 'b
  val flush_elements : 'a t -> 'a list
  val elements : 'a t -> 'a list
  val exists : 'a t -> ('a -> bool) -> bool
  val iter : 'a t -> ('a -> unit) -> unit
  val fold : 'a t -> ('a -> 'b -> 'b) -> 'b -> 'b
  val length : 'a t -> int
  val filter : 'a t -> ('a -> bool) -> unit
  val filter_out : 'a t -> ('a -> bool) -> unit
end

module WeakQueue : sig
  type 'a t

  val create : unit -> 'a t
  val push : 'a t -> 'a -> unit
  val flush_iter : 'a t -> ('a -> unit) -> unit
  val flush_elements : 'a t -> 'a list
  val elements : 'a t -> 'a list
  val exists : 'a t -> ('a -> bool) -> bool
  val iter : 'a t -> ('a -> unit) -> unit
  val fold : 'a t -> ('a -> 'b -> 'b) -> 'b -> 'b
  val length : 'a t -> int
  val filter : 'a t -> ('a -> bool) -> unit
  val filter_out : 'a t -> ('a -> bool) -> unit
end
