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

(* Immutable fast hash *)

type ('a, 'b) t

val hash_fold_t :
  (Term_hash.state -> 'a -> Term_hash.state) ->
  (Term_hash.state -> 'b -> Term_hash.state) ->
  Term_hash.state ->
  ('a, 'b) t ->
  Term_hash.state

val from_list : ('a * 'b) list -> ('a, 'b) t
val is_empty : ('a, 'b) t -> bool
val empty : ('a, 'b) t
val cardinal : ('a, 'b) t -> int
val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
val bindings : ('a, 'b) t -> ('a * 'b) list
val find : 'a -> ('a, 'b) t -> 'b
val find_opt : 'a -> ('a, 'b) t -> 'b option
val mem : 'a -> ('a, 'b) t -> bool
val remove : 'a -> ('a, 'b) t -> ('a, 'b) t
val append : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
val mapi : ('a -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
val filter : ('a -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t
val for_all : ('a -> 'b -> bool) -> ('a, 'b) t -> bool
val exists : ('a -> 'b -> bool) -> ('a, 'b) t -> bool
val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
