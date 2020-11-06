(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2020 Savonet team

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
val print_pos_opt : ?prefix:string -> pos option -> string
val print_pos_list : ?prefix:string -> pos list -> string

type variance = Covariant | Contravariant | Invariant
type ground = ..

type ground +=
  | Bool
  | Int
  | String
  | Float
  | Request
  | Format of Frame_content.format

val register_ground_printer : (ground -> string option) -> unit
val print_ground : ground -> string

type constr = Num | Ord | Getter of ground | Dtools | InternalMedia
type constraints = constr list

val print_constr : constr -> string

type t = { pos : pos option; mutable level : int; mutable descr : descr }

and constructed = { name : string; params : (variance * t) list }

and descr =
  | Constr of constructed
  | Ground of ground
  | List of t
  | Tuple of t list
  | Nullable of t
  | Meth of string * scheme * t
  | Arrow of (bool * string * t) list * t
  | EVar of var
  | Link of t

and var = int * constraints

and scheme = var list * t

type env = (string * scheme) list

module Subst : sig
  type subst

  val of_seq : (var * t) Seq.t -> subst
  val filter : (var -> t -> bool) -> subst -> subst

  type t = subst
end

val unit : descr
val make : ?pos:pos option -> ?level:int -> descr -> t
val dummy : t
val pp_type : Format.formatter -> t -> unit
val pp_type_generalized : var list -> Format.formatter -> t -> unit
val print : ?generalized:var list -> t -> string
val print_scheme : scheme -> string
val doc_of_type : generalized:var list -> t -> Doc.item

exception Occur_check of t * t

(** [occur_check x t] ensures that a variable [x] does not occur in [t]. Raises
    [Occur_check] if it is the case. *)
val occur_check : t -> t -> unit

exception Unsatisfied_constraint of constr * t

(** [bind x t] assigns a value [t] to a variable [x]. *)
val bind : t -> t -> unit

(** Remove links in a type: this function should always be called before
    matching on types. *)
val deref : t -> t

(** Add a method to a type. *)
val meth : ?pos:pos option -> ?level:int -> string -> scheme -> t -> t

(** Add a submethod to a type. *)
val meths : ?pos:pos option -> ?level:int -> string list -> scheme -> t -> t

(** Remove all methods in a type. *)
val demeth : t -> t

(** Put the methods of the first type around the second type. *)
val remeth : t -> t -> t

(** Type of a method in a type. *)
val invoke : t -> string -> scheme

(** Type of a submethod in a type. *)
val invokes : t -> string list -> scheme

(** Find all the free variables satisfying a predicate. *)
val filter_vars : (t -> bool) -> t -> var list

val copy_with : Subst.t -> t -> t
val instantiate : level:int -> generalized:var list -> t -> t
val generalizable : level:int -> t -> var list

type explanation

exception Type_Error of explanation

val print_type_error : (string -> unit) -> explanation -> unit
val ( <: ) : t -> t -> unit
val ( >: ) : t -> t -> unit
val fresh : constraints:constraints -> level:int -> pos:pos option -> t
val fresh_evar : level:int -> pos:pos option -> t
val min_type : ?pos:pos option -> ?level:int -> t -> t -> t
