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

val debug : bool ref
val debug_levels : bool ref
val debug_variance : bool ref

(** {2 Types} *)

type variance = [ `Covariant | `Invariant ]
type descr = Type_base.descr = ..
type t = Type_base.t = private { pos : Pos.Option.t; descr : descr }
type constr_t = Type_base.constr_t = ..
type constr_t += Num | Ord

type constr = Type_base.constr = {
  t : constr_t;
  constr_descr : string;
  satisfied : subtype:(t -> t -> unit) -> satisfies:(t -> unit) -> t -> unit;
}

module Constraints = Type_base.Constraints

type constructed = Type_base.constructed = {
  constructor : string;
  params : (variance * t) list;
}

type var = Type_base.var = {
  name : int;
  mutable level : int;
  mutable constraints : Constraints.t;
}

type invar = Free of var | Link of variance * t
type scheme = var list * t

type meth = Type_base.meth = {
  meth : string;
  optional : bool;
  scheme : scheme;
  doc : string;
  json_name : string option;
}

type repr_t = Type_base.repr_t = { t : t; json_repr : [ `Tuple | `Object ] }

val string_of_constr : constr -> string
val num_constr : constr
val ord_constr : constr

module Subst = Type_base.Subst
module R = Type_base.R

type custom = Type_base.custom = ..

type custom_handler = Type_base.custom_handler = {
  typ : custom;
  copy_with : (t -> t) -> custom -> custom;
  occur_check : (t -> unit) -> custom -> unit;
  filter_vars : (var list -> t -> var list) -> var list -> custom -> var list;
  repr : (var list -> t -> Repr.t) -> var list -> custom -> Repr.t;
  subtype : (t -> t -> unit) -> custom -> custom -> unit;
  sup : (t -> t -> t) -> custom -> custom -> custom;
  to_string : custom -> string;
  to_json : custom -> Json.t;
}

type descr +=
  | Custom of custom_handler
  | Constr of constructed
  | Getter of t
  | List of repr_t
  | Tuple of t list
  | Nullable of t
  | Meth of meth * t
  | Arrow of (bool * string * t) list * t
  | Var of invar ref

exception NotImplemented
exception Exists of Pos.Option.t * string
exception Unsatisfied_constraint

val unit : descr

module Var = Type_base.Var
module Vars = Type_base.Vars

val make : ?pos:Pos.t -> descr -> t
val deref : t -> t
val demeth : t -> t
val remeth : t -> t -> t
val invoke : t -> string -> scheme
val has_meth : t -> string -> bool
val invokes : t -> string list -> var list * t

val meth :
  ?pos:Pos.t ->
  ?json_name:string ->
  ?optional:bool ->
  string ->
  scheme ->
  ?doc:string ->
  t ->
  t

val meths : ?pos:Pos.t -> string list -> scheme -> t -> t
val split_meths : t -> meth list * t
val var : ?constraints:constr list -> ?level:int -> ?pos:Pos.t -> unit -> t
val mk_invariant : t -> unit
val to_string_fun : (?generalized:var list -> t -> string) ref
val to_string : ?generalized:var list -> t -> string
val to_json : t -> Json.t
val string_of_scheme : scheme -> string
val is_fun : t -> bool
val is_source : t -> bool

module Ground = Ground_type

val register_type : string -> (unit -> t) -> unit
val find_type_opt : string -> (unit -> t) option
