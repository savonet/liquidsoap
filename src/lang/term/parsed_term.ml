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

include Runtime_term
module Ground = Term_base.Ground

type inc_type = [ `Lib | `Extra | `Default ]

type inc = {
  inc_type : inc_type;
  inc_name : string;
  inc_pos : Lexing.position * Lexing.position;
}

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

and fun_arg =
  [ `Term of (t, type_annotation option) func_argument | `Argsof of _of ]

and list_el = [ `Term of t | `Ellipsis of t ]

and time_el = {
  week : int option;
  hours : int option;
  minutes : int option;
  seconds : int option;
}

(* These terms are reduced at runtime *)
and parsed_ast =
  [ `If of _if
  | `Inline_if of _if
  | `While of _while
  | `For of _for
  | `Iterable_for of iterable_for
  | `List of list_el list
  | `Try of _try
  | `Regexp of string * char list
  | `Time_interval of time_el * time_el
  | `Time of time_el
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
  | `String_interpolation of string_interpolation list
  | `Include of inc
  | t ast ]

and t = parsed_ast term
and string_interpolation = [ `String of string | `Term of t ]

type encoder_params = t ast_encoder_params

let unit = `Tuple []

let rec iter_term fn ({ term; methods } as tm) =
  fn tm;
  Methods.iter (fun _ tm -> iter_term fn tm) methods;
  match term with
    | `If p | `Inline_if p ->
        iter_term fn p.if_condition;
        iter_term fn p.if_then;
        iter_term fn p.if_else
    | `While { while_condition; while_loop } ->
        iter_term fn while_condition;
        iter_term fn while_loop
    | `For { for_from; for_to; for_loop } ->
        iter_term fn for_from;
        iter_term fn for_to;
        iter_term fn for_loop
    | `Iterable_for { iterable_for_iterator; iterable_for_loop } ->
        iter_term fn iterable_for_iterator;
        iter_term fn iterable_for_loop
    | `List l ->
        List.iter (function `Term tm | `Ellipsis tm -> iter_term fn tm) l
    | `Try { try_body; try_errors_list; try_handler } ->
        iter_term fn try_body;
        iter_term fn try_errors_list;
        iter_term fn try_handler
    | `Regexp _ -> ()
    | `Time_interval _ -> ()
    | `Time _ -> ()
    | `Def (_let, tm) | `Let (_let, tm) | `Binding (_let, tm) ->
        (match _let.arglist with
          | Some args -> iter_fun_args fn args
          | None -> ());
        iter_term fn _let.def;
        iter_term fn tm
    | `Cast (tm, _) -> iter_term fn tm
    | `App (tm, args) ->
        iter_term fn tm;
        List.iter
          (function `Term (_, tm) -> iter_term fn tm | `Argsof _ -> ())
          args
    | `Invoke { invoked } -> iter_term fn invoked
    | `Fun (args, tm) | `RFun (_, args, tm) ->
        iter_fun_args fn args;
        iter_term fn tm
    | `Not tm -> iter_term fn tm
    | `Get tm -> iter_term fn tm
    | `Set (tm, tm') ->
        iter_term fn tm;
        iter_term fn tm'
    | `Negative tm -> iter_term fn tm
    | `Append (tm, tm') ->
        iter_term fn tm;
        iter_term fn tm'
    | `Assoc (tm, tm') ->
        iter_term fn tm;
        iter_term fn tm'
    | `Infix (tm, _, tm') ->
        iter_term fn tm;
        iter_term fn tm'
    | `Bool (tm, _, tm') ->
        iter_term fn tm;
        iter_term fn tm'
    | `Coalesce (tm, tm') ->
        iter_term fn tm;
        iter_term fn tm'
    | `Simple_fun tm -> iter_term fn tm
    | `Include _ -> ()
    | `Ground _ -> ()
    | `Encoder _ -> ()
    | `Tuple l -> List.iter (iter_term fn) l
    | `Null -> ()
    | `Open (tm, tm') ->
        iter_term fn tm;
        iter_term fn tm'
    | `Var _ -> ()
    | `String_interpolation l ->
        List.iter (function `String _ -> () | `Term tm -> iter_term fn tm) l
    | `Seq (tm, tm') ->
        iter_term fn tm;
        iter_term fn tm'

and iter_fun_args fn args =
  List.iter
    (function
      | `Term tm -> (
          match tm.default with Some tm -> iter_term fn tm | None -> ())
      | `Argsof _ -> ())
    args
