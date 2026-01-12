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

(** {1 Functions for typing} *)

open Type

(** Print debugging messages for subtyping. *)
val debug_subtyping : bool ref

(** Find all the free variables satisfying a predicate. *)
val filter_vars : (var -> bool) -> t -> var list

(** A typing environment. *)
type env = (string * scheme) list

(** Instantiate a type. *)
val instantiate : level:int -> scheme -> t

(** Find all generalizable variables. *)
val generalize : level:int -> t -> scheme

(** Lower all type variables to given level. *)
val update_level : int -> t -> unit

(** Subtyping. *)
val ( <: ) : t -> t -> unit

(** Suptyping. *)
val ( >: ) : t -> t -> unit

(** Supremeum of two types. *)
val sup : pos:Pos.Option.t -> t -> t -> t

(** Bind a variable *)
val bind : ?variance:Type.variance -> t -> t -> unit

(** Ensure that a type satisfies a given constraint, i.e. morally that b <: c.
*)
val satisfies_constraint : t -> constr -> unit

val do_occur_check : bool ref
