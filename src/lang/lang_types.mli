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

val debug : bool ref

type pos = Lexing.position * Lexing.position

val print_single_pos : Lexing.position -> string

val print_pos : ?prefix:string -> pos -> string

type variance = Covariant | Contravariant | Invariant

type ground = Bool | Int | String | Float

val print_ground : ground -> string

type constr = Num | Ord | Getter of ground | Dtools | Arity_fixed | Arity_any

type constraints = constr list

val print_constr : constr -> string

type t = {pos: pos option; mutable level: int; mutable descr: descr}

and constructed = {name: string; params: (variance * t) list}

and descr =
  | Constr of constructed
  | Ground of ground
  | List of t
  | Tuple of t list
  | Zero
  | Succ of t
  | Variable
  | Arrow of (bool * string * t) list * t
  | EVar of int * constraints
  | Link of t

val unit : descr

val make : ?pos:pos option -> ?level:int -> descr -> t

val dummy : t

val pp_type : Format.formatter -> t -> unit

val pp_type_generalized :
  (int * constraints) list -> Format.formatter -> t -> unit

val print : ?generalized:(int * constraints) list -> t -> string

val doc_of_type : generalized:(int * constraints) list -> t -> Doc.item

exception Occur_check of t * t

val occur_check : t -> t -> unit

exception Unsatisfied_constraint of constr * t

val bind : t -> t -> unit

val deref : t -> t

val filter_vars : (t -> bool) -> t -> (int * constraints) list

val copy_with : ((int * constraints) * t) list -> t -> t

val instantiate : level:int -> generalized:(int * constraints) list -> t -> t

val generalizable : level:int -> t -> (int * constraints) list

type explanation

exception Type_Error of explanation

val print_type_error : (string -> unit) -> explanation -> unit

val ( <: ) : t -> t -> unit

val ( >: ) : t -> t -> unit

val fresh : constraints:constraints -> level:int -> pos:pos option -> t

val fresh_evar : level:int -> pos:pos option -> t

val iter_constr : (bool -> constructed -> unit) -> t -> bool * bool
