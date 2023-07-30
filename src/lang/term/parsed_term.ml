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

include Term_types
module Ground = Term_base.Ground

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

type _of = { only : string list; except : string list; source : string }

type _if = { if_condition : t; if_then : t; if_else : t }
and _while = { while_condition : t; while_loop : t }
and _for = { for_variable : string; for_from : t; for_to : t; for_loop : t }

and iterable_for = {
  iterable_for_variable : string;
  iterable_for_iterator : t;
  iterable_for_loop : t;
}

and _try = {
  try_body : t;
  try_variable : string;
  try_errors_list : t;
  try_handler : t;
}

and let_decoration =
  [ `None
  | `Recursive
  | `Replaces
  | `Eval
  | `Yaml_parse
  | `Json_parse of (string * t) list ]

and _let = {
  doc : Doc.Value.t option;
  decoration : let_decoration;
  pat : pattern;
  arglist : fun_arg list option;
  cast : type_annotation option;
  def : t;
}

and invoke_meth = [ `String of string | `App of string * app_arg list ]
and app_arg = [ `Term of string * t | `Argsof of _of ]
and fun_arg = [ `Term of (t, type_annotation) func_argument | `Argsof of _of ]

(* These terms are reduced at runtime *)
and parsed_ast =
  [ `If of _if
  | `Inline_if of _if
  | `While of _while
  | `For of _for
  | `Iterable_for of iterable_for
  | `Try of _try
  | `Regexp of string * char list
  | `Def of _let * t
  | `Let of _let * t
  | `Binding of _let * t
  | `Cast of t * type_annotation
  | `App of t * app_arg list
  | `Invoke of (t, invoke_meth) invoke
  | `Fun of fun_arg list * t
  | `RFun of string * fun_arg list * t
  | `Not of t
  | `Get of t
  | `Set of t * t
  | `Negative of t
  | `Append of t * t
  | `Assoc of t * t
  | `Infix of t * string * t
  | `Bool of t * string * t
  | `Coalesce of t * t
  | `Simple_fun of t
  | t ast ]

and t = parsed_ast term

type encoder_params = t ast_encoder_params

let make = Term_base.make
let unit = `Tuple []
