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

(** Syntactically exact terms before runtime reductions are applied. *)

module Methods = Term.Methods
module Vars = Term.Vars
module Ground = Term.Ground

type pattern = Term.pattern

type meth_annotation = {
  optional : bool;
  name : string;
  typ : type_annotation;
  json_name : string option;
}

and source_track_annotation = {
  track_name : string;
  track_type : string;
  track_params : (string * string) list;
}

and source_annotation = {
  extensible : bool;
  tracks : source_track_annotation list;
}

and type_annotation =
  [ `Named of string
  | `Nullable of type_annotation
  | `List of type_annotation
  | `Json_object of type_annotation
  | `Tuple of type_annotation list
  | `Arrow of type_annotation Type.argument list * type_annotation
  | `Record of meth_annotation list
  | `Method of type_annotation * meth_annotation list
  | `Invoke of type_annotation * string
  | `Source of string * source_annotation ]

type 'a term = 'a Term.term = {
  mutable t : Type.t;
  term : 'a;
  methods : 'a term Methods.t;
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

type 'a _if = { if_condition : 'a; if_then : 'a; if_else : 'a }
type 'a _while = { while_condition : 'a; while_loop : 'a }

type 'a _for = {
  for_variable : string;
  for_from : 'a;
  for_to : 'a;
  for_loop : 'a;
}

type 'a iterable_for = {
  iterable_for_variable : string;
  iterable_for_iterator : 'a;
  iterable_for_loop : 'a;
}

type 'a _try = {
  try_body : 'a;
  try_variable : string;
  try_errors_list : 'a;
  try_handler : 'a;
}

type 'a _meth = { base : 'a; meth : string; value : 'a }

(* These terms are reduced at runtime *)
type 'a reduced_ast =
  [ `If of 'a _if
  | `Inline_if of 'a _if
  | `While of 'a _while
  | `For of 'a _for
  | `Iterable_for of 'a iterable_for
  | `Try of 'a _try
  | `Regexp of string * char list
  | `Not of 'a
  | `Get of 'a
  | `Set of 'a * 'a
  | `Negative of 'a
  | `Append of 'a * 'a
  | `Assoc of 'a * 'a
  | `Infix of 'a * string * 'a
  | `Bool of 'a * string * 'a
  | `Simple_fun of 'a
  | `Parsed_cast of 'a * type_annotation
  | `Parsed_fun of (type_annotation, 'a) func
  | `Parsed_rfun of string * (type_annotation, 'a) func ]

type t = parsed_ast Term.term
and parsed_ast = [ t reduced_ast | t Term.ast ]

type encoder_params = t Term.ast_encoder_params

val of_term : Term.t -> t
val make : ?pos:Pos.t -> ?t:Type.t -> ?methods:t Methods.t -> parsed_ast -> t
val unit : parsed_ast
