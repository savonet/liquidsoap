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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)
type 'a cell
type 'a t

exception Not_found

(** [create ()] return an empty queue *)
val create : unit -> 'a t

(** [length q] return the number of cells in [q] *)
val length : 'a t -> int

(** [is_empty q] return [true] if [q] has no cell *)
val is_empty : 'a t -> bool

(** [to_list q] return a list containing all the cells of [q] *)
val to_list : 'a t -> 'a list

(** [top q] return the cell at the top of the queue *)
val top : 'a t -> 'a

(** [push q c] add the cell [c] at the top of the queue [q] *)
val push : 'a t -> 'a -> unit

(** [unshift q c] add the cell [c] at the end of the queue [q] *)
val unshift : 'a t -> 'a -> unit

(** [shift q] remove the first cell from [q] and return it *)
val shift : 'a t -> 'a

(** [pop q] remove the last cell from [q] and return it *)
val pop : 'a t -> 'a

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val insert_pred : ?top:bool -> 'a t -> (int -> 'a -> bool) -> 'a -> unit
val remove_pred_index : 'a t -> (int -> 'a -> bool) -> 'a * int
val remove_pred : 'a t -> (int -> 'a -> bool) -> 'a
val insert : 'a t -> int -> 'a -> unit
val remove : 'a t -> int -> 'a
