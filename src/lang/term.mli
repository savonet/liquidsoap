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

exception Internal_error of (Pos.t list * string)
exception Unsupported_encoder of (Pos.t option * string)

val conf_debug : bool ref
val conf_debug_errors : bool ref
val debug : bool Lazy.t
val profile : bool ref
val ref_t : ?pos:Pos.t -> Type.t -> Type.t

module Vars = Term_base.Vars
module Ground = Term_base.Ground

module type GroundDef = sig
  type content

  val descr : content -> string
  val to_json : pos:Pos.t list -> content -> Json.t
  val compare : content -> content -> int
  val typ : (module Type.Ground.Custom)
end

module MkGround : functor (D : GroundDef) -> sig
  type Ground.t += Ground of D.content
end

module Methods = Term_base.Methods

type pattern =
  [ `PVar of string list  (** a field *)
  | `PTuple of pattern list  (** a tuple *)
  | `PList of pattern list * string option * pattern list  (** a list *)
  | `PMeth of pattern option * (string * pattern option) list
    (** a value with methods *) ]

(** Documentation for declarations: general documentation, parameters, methods. *)
type doc = Doc.Value.t

type 'a term = 'a Term_base.term = {
  mutable t : Type.t;
  term : 'a;
  methods : 'a term Methods.t;
}

type 'a let_t = 'a Term_base.let_t = {
  doc : doc option;
  replace : bool;
  pat : pattern;
  mutable gen : Type.var list;
  def : 'a;
  body : 'a;
}

type 'a ast_encoder_params =
  (string * [ `Encoder of 'a ast_encoder | `Term of 'a ]) list

and 'a ast_encoder = string * 'a ast_encoder_params

type 'a invoke = 'a Term_base.invoke = {
  invoked : 'a;
  default : 'a option;
  meth : string;
}

type ('a, 'b) func_argument = ('a, 'b) Term_base.func_argument = {
  label : string;
  as_variable : string option;
  typ : 'a;
  default : 'b option;
}

type ('a, 'b) func = ('a, 'b) Term_base.func = {
  mutable free_vars : Vars.t option;
  arguments : ('a, 'b) func_argument list;
  body : 'b;
}

type 'a ast =
  [ `Ground of Ground.t
  | `Encoder of 'a ast_encoder
  | `List of 'a list
  | `Tuple of 'a list
  | `Null
  | `Cast of 'a * Type.t
  | `Invoke of 'a invoke
  | `Open of 'a * 'a
  | `Let of 'a let_t
  | `Var of string
  | `Seq of 'a * 'a
  | `App of 'a * (string * 'a) list
  | `Fun of (Type.t, 'a) func
  | (* A recursive function, the first string is the name of the recursive
        variable. *)
    `RFun of
    string * (Type.t, 'a) func ]

type t = runtime_ast term
and runtime_ast = t ast

type encoder_params = t ast_encoder_params
type encoder = t ast_encoder

val unit : runtime_ast
val is_ground : t -> bool
val string_of_pat : pattern -> string
val to_string : t -> string
val make : ?pos:Pos.t -> ?t:Type.t -> ?methods:t Methods.t -> runtime_ast -> t
val trim_runtime_types : unit -> unit
val free_vars_pat : pattern -> Vars.t
val bound_vars_pat : pattern -> Vars.t
val free_vars : ?bound:Vars.elt list -> t -> Vars.t
val free_fun_vars : (Type.t, t) func -> Vars.t
val can_ignore : Type.t -> bool

exception Unbound of Pos.Option.t * string
exception Ignored of t
exception No_label of t * string * bool * t
exception Duplicate_label of Pos.Option.t * string
exception Missing_arguments of Pos.Option.t * (string * Type.t) list
exception Unused_variable of (string * Pos.t)

val check_unused : throw:(exn -> unit) -> lib:bool -> t -> unit

module type Abstract = sig
  type content

  val t : Type.t
  val to_ground : content -> Ground.t
  val of_ground : Ground.t -> content
  val is_ground : Ground.t -> bool
  val to_term : content -> t
  val of_term : t -> content
  val is_term : t -> bool
end

module type AbstractDef = sig
  type content

  val name : string
  val to_json : pos:Pos.t list -> content -> Json.t
  val descr : content -> string
  val compare : content -> content -> int
end

module MkAbstract : functor (Def : AbstractDef) -> sig
  module T : Type.Ground.Custom

  type Ground.t += Value of Def.content
  type content = Def.content

  val t : Type.t
  val of_ground : Ground.t -> Def.content
  val to_ground : Def.content -> Ground.t
  val is_ground : Ground.t -> bool
  val of_term : t -> Def.content
  val to_term : Def.content -> t
  val is_term : t -> bool
end
