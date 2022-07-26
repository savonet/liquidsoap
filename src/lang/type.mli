(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

type constr = Type_base.constr = Num | Ord | Dtools | InternalMedia
type constraints = constr list

module DS = Type_base.DS

val string_of_constr : constr -> string

type variance = Type_base.variance = Covariant | Contravariant | Invariant

type t = Type_base.t = { pos : Pos.Option.t; descr : descr }

and constructed = Type_base.constructed = {
  constructor : string;
  params : (variance * t) list;
}

and meth = Type_base.meth = {
  meth : string;
  scheme : scheme;
  doc : string;
  json_name : string option;
}

and repr_t = Type_base.repr_t = { t : t; json_repr : [ `Object | `Tuple ] }

and descr = Type_base.descr = ..

and invar = Type_base.invar = Free of var | Link of variance * t

and var = Type_base.var = {
  name : int;
  mutable level : int;
  mutable constraints : constraints;
}

and scheme = var list * t

module Subst = Type_base.Subst

type custom = Type_base.custom = ..

type custom_handler = Type_base.custom_handler = {
  typ : custom;
  copy_with : (t -> t) -> Subst.t -> custom -> custom;
  occur_check : var -> custom -> unit;
  filter_vars :
    (var list -> t -> var list) ->
    var list ->
    (var -> bool) ->
    custom ->
    var list;
  print : Format.formatter -> custom -> DS.t;
  satisfies_constraint : (t -> unit) -> t -> custom -> unit;
  subtype : (t -> t -> unit) -> t -> t -> unit;
  sup : (t -> t -> t) -> t -> t -> t;
  to_string : custom -> string;
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
  ?pos:Pos.t -> ?json_name:string -> string -> scheme -> ?doc:string -> t -> t

val meths : ?pos:Pos.t -> string list -> scheme -> t -> t
val split_meths : t -> meth list * t
val var : ?constraints:constraints -> ?level:int -> ?pos:Pos.t -> unit -> t
val to_string_fun : (?generalized:var list -> t -> string) ref
val to_string : ?generalized:var list -> t -> string
val is_fun : t -> bool
val is_source : t -> bool

module Ground = Ground_type
