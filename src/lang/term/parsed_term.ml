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

module Methods = Term_base.Methods
module Vars = Term_base.Vars
module Ground = Term_base.Ground

type pattern = Term_base.pattern

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
  | `Append of 'a * 'a
  | `Assoc of 'a * 'a
  | `Simple_fun of 'a ]

type t = parsed_ast Term_base.term
and parsed_ast = [ t reduced_ast | t Term_base.ast ]

type encoder_params = t Term.ast_encoder_params

let rec of_ast : Term_base.runtime_ast -> parsed_ast = function
  | `Ground g -> `Ground g
  | `Encoder e -> `Encoder (of_encoder e)
  | `List l -> `List (List.map of_term l)
  | `Tuple l -> `Tuple (List.map of_term l)
  | `Null -> `Null
  | `Cast (t, typ) -> `Cast (of_term t, typ)
  | `Invoke { Term_base.invoked; default; meth } ->
      `Invoke
        {
          Term_base.invoked = of_term invoked;
          default = Option.map of_term default;
          meth;
        }
  | `Open (t, t') -> `Open (of_term t, of_term t')
  | `Let _let ->
      `Let
        Term_base.{ _let with def = of_term _let.def; body = of_term _let.body }
  | `Var s -> `Var s
  | `Seq (t, t') -> `Seq (of_term t, of_term t')
  | `App (t, l) -> `App (of_term t, List.map (fun (v, t) -> (v, of_term t)) l)
  | `Fun p -> `Fun (of_func p)
  | `RFun (lbl, p) -> `RFun (lbl, of_func p)

and of_func { Term_base.arguments; body } =
  {
    Term_base.arguments =
      List.map
        (fun arg ->
          Term_base.{ arg with default = Option.map of_term arg.default })
        arguments;
    body = of_term body;
    free_vars = None;
  }

and of_encoder_params l =
  List.map
    (function
      | lbl, `Term t -> (lbl, `Term (of_term t))
      | lbl, `Encoder e -> (lbl, `Encoder (of_encoder e)))
    l

and of_encoder (lbl, params) = (lbl, of_encoder_params params)

and of_term (tm : Term.t) : t =
  { tm with methods = Methods.map of_term tm.methods; term = of_ast tm.term }

let make = Term_base.make
let unit = `Tuple []
