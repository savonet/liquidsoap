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

(** Fast iteration over weak arrays.

    These functions iterate over the live (non-GC'd) elements of a {!Weak.t}
    using direct field access via {!Obj} to avoid the per-element [option]
    allocation that {!Weak.get} incurs. Empty slots are detected by physical
    equality against the runtime sentinel [caml_ephe_none]. *)

(** [iter w f] calls [f] on each live element of [w], in index order. Empty
    slots (GC'd or never set) are skipped. *)
val iter : 'a Weak.t -> ('a -> unit) -> unit

(** [fold_left f init w] folds [f] over the live elements of [w] from left to
    right, starting with accumulator [init]. Empty slots are skipped. *)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b Weak.t -> 'a
