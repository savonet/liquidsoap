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

type 'a term = 'a Term_base.term = {
  mutable t : Type.t;
  term : 'a;
  methods : 'a term Methods.t;
}

type 'a _if = { if_condition : 'a; if_then : 'a; if_else : 'a }
type 'a _while = { while_condition : 'a; while_loop : 'a }

type 'a _for = {
  for_variable : string;
  for_variable_position : Pos.t;
  for_from : 'a;
  for_to : 'a;
  for_loop : 'a;
}

type 'a iterable_for = {
  iterable_for_variable : string;
  iterable_for_variable_position : Pos.t;
  iterable_for_iterator : 'a;
  iterable_for_loop : 'a;
}

type 'a _try = {
  try_body : 'a;
  try_variable : string;
  try_variable_position : Pos.t;
  try_errors_list : 'a;
  try_handler : 'a;
}

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
  | `Simple_fun of 'a ]

type t = parsed_ast Term.term
and parsed_ast = [ t reduced_ast | t Term.ast ]

type encoder_params = t Term.ast_encoder_params

val of_term : Term.t -> t
val make : ?pos:Pos.t -> ?t:Type.t -> ?methods:t Methods.t -> parsed_ast -> t
val unit : parsed_ast
