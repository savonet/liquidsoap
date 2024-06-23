(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

open Type_base

type variance = [ `Covariant | `Invariant ]

type t = Type_base.t =
  | String of Pos.Option.t
  | Int of Pos.Option.t
  | Float of Pos.Option.t
  | Bool of Pos.Option.t
  | Never of Pos.Option.t
  | Custom of {
      typ : custom;
      custom_name : string;
      copy_with : (t -> t) -> custom -> custom;
      occur_check : (t -> unit) -> custom -> unit;
      filter_vars :
        (var list -> t -> var list) -> var list -> custom -> var list;
      repr : (var list -> t -> constr R.t) -> var list -> custom -> constr R.t;
      subtype : (t -> t -> unit) -> custom -> custom -> unit;
      sup : (t -> t -> t) -> custom -> custom -> custom;
      to_string : custom -> string;
      pos : Pos.Option.t;
    }
  | Constr of {
      constructor : string;
      params : (variance * t) list;
      pos : Pos.Option.t;
    }
  | Getter of { t : t; pos : Pos.Option.t }
      (** a getter: something that is either a t or () -> t *)
  | List of { t : t; json_repr : [ `Tuple | `Object ]; pos : Pos.Option.t }
  | Tuple of { t : t list; pos : Pos.Option.t }
  | Nullable of { t : t; pos : Pos.Option.t }
      (** something that is either t or null *)
  | Meth of {
      meth : string;  (** name of the method *)
      optional : bool;  (** is the method optional? *)
      scheme : scheme;  (** type scheme *)
      doc : string;  (** documentation *)
      json_name : string option;  (** name when represented as JSON *)
      t : t;
      pos : Pos.Option.t;
    }
  | Arrow of { args : t argument list; t : t; pos : Pos.Option.t }
      (** a function *)
  | Var of Type_base.var_t  (** a type variable *)

type custom = Type_base.custom

type custom_handler = Type_base.custom_handler = {
  typ : custom;
  custom_name : string;
  copy_with : (t -> t) -> custom -> custom;
  occur_check : (t -> unit) -> custom -> unit;
  filter_vars : (var list -> t -> var list) -> var list -> custom -> var list;
  repr : (var list -> t -> constr R.t) -> var list -> custom -> constr R.t;
  subtype : (t -> t -> unit) -> custom -> custom -> unit;
  sup : (t -> t -> t) -> custom -> custom -> custom;
  to_string : custom -> string;
}

type invar = Type_base.invar = Free of var | Link of variance * t
type var_t = Type_base.var_t = { id : int; mutable contents : invar }

type constr = Type_base.constr = {
  constr_descr : string;
  univ_descr : string option;
  satisfied : subtype:(t -> t -> unit) -> satisfies:(t -> unit) -> t -> unit;
}

type descr = Type_base.descr

module Constraints = Type_base.Constraints

type constructed = Type_base.constructed = {
  constructor : string;
  params : (variance * t) list;
}

type var = Type_base.var = {
  name : int;
  mutable pos : Pos.Option.t;
  mutable level : int;
  mutable constraints : Constraints.t;
}

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
val record_constr : constr
val num_constr : constr
val ord_constr : constr

module R = Type_base.R

type 'a argument = bool * string * 'a

exception NotImplemented
exception Exists of Pos.Option.t * string
exception Unsatisfied_constraint

val unit : descr
val is_unit : t -> bool
val pos : t -> Pos.Option.t

module Var = Type_base.Var
module Vars = Type_base.Vars

(** Generate fresh types from existing types. *)
module Fresh : sig
  type mapper = Type_base.Fresh.mapper

  (* Use [selector] to pick variables to be re-freshed. If [level] is passed,
     all new variables are created with the given level. *)
  val init :
    ?preserve_positions:bool ->
    ?selector:(var -> bool) ->
    ?level:int ->
    unit ->
    mapper

  (* Generate a fresh var using the parameters passed when initializing
     the corresponding handler. Generated variables are memoized. *)
  val make_var : mapper -> var -> var

  (* Generate a fresh type using the parameters passed when initializing
     the corresponding handler. *)
  val make : mapper -> t -> t
end

(* Generate a fully refreshed type. Shared variables are mapped
   to shared fresh variables. *)
val fresh : t -> t
val make : ?pos:Pos.t -> descr -> t
val descr : t -> descr
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

(** Type of references on a given type. *)
val reference : ?pos:Pos.t -> t -> t

val meths : ?pos:Pos.t -> string list -> scheme -> t -> t
val split_meths : t -> meth list * t
val filter_meths : t -> (meth -> bool) -> t
val var : ?constraints:constr list -> ?level:int -> ?pos:Pos.t -> unit -> t
val mk_invariant : t -> unit
val to_string_fun : (?generalized:var list -> t -> string) ref
val to_string : ?generalized:var list -> t -> string
val string_of_scheme : scheme -> string
val is_fun : t -> bool
val is_source : t -> bool

module Custom = Type_custom

val register_type : string -> (unit -> t) -> unit
val find_opt_typ : string -> (unit -> t) option
