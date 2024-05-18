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

include Runtime_term
module Ground = Term_base.Ground

type comment = [ `Before of string list | `After of string list ]
type string_param = [ `Verbatim of string | `String of Pos.t * (char * string) ]
type track_annotation = string * string_param
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
  track_params : track_annotation list;
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

type time_el = {
  week : int option;
  hours : int option;
  minutes : int option;
  seconds : int option;
}

type 't term = {
  term : 't;
  pos : Pos.t;
  mutable comments : (Pos.t * comment) list;
}

module Generic = struct
  type 't _if = {
    if_condition : 't;
    if_then : 't;
    if_elsif : ('t * 't) list;
    if_else : 't option;
  }

  type 't _while = { while_condition : 't; while_loop : 't }

  type 't _for = {
    for_variable : string;
    for_from : 't;
    for_to : 't;
    for_loop : 't;
  }

  type 't iterable_for = {
    iterable_for_variable : string;
    iterable_for_iterator : 't;
    iterable_for_loop : 't;
  }

  type 't _try = {
    try_body : 't;
    try_variable : string;
    try_errors_list : 't option;
    try_handler : 't option;
    try_finally : 't option;
  }

  type 't let_decoration =
    [ `None
    | `Recursive
    | `Replaces
    | `Eval
    | `Sqlite_query
    | `Sqlite_row
    | `Yaml_parse
    | `Json_parse of (string * 't) list ]

  type 't fun_arg =
    [ `Term of ('t, type_annotation option) func_argument | `Argsof of _of ]

  type 't _let = {
    decoration : 't let_decoration;
    pat : pattern;
    arglist : 't fun_arg list option;
    cast : type_annotation option;
    def : 't;
  }

  type 't app_arg = [ `Term of string * 't | `Argsof of _of ]
  type 't invoke_meth = [ `String of string | `App of string * 't app_arg list ]
  type 't invoke = { invoked : 't; optional : bool; meth : 't invoke_meth }
  type 't list_el = [ `Term of 't | `Ellipsis of 't ]

  type 't if_def = {
    if_def_negative : bool;
    if_def_condition : string;
    if_def_then : 't;
    if_def_else : 't option;
  }

  type 't if_version = {
    if_version_op : [ `Eq | `Geq | `Leq | `Gt | `Lt ];
    if_version_version : Lang_string.Version.t;
    if_version_then : 't;
    if_version_else : 't option;
  }

  type 't if_encoder = {
    if_encoder_negative : bool;
    if_encoder_condition : string;
    if_encoder_then : 't;
    if_encoder_else : 't option;
  }

  type 't methods = [ `Ellipsis of 't | `Method of string * 't ]
  type 't string_interpolation = [ `String of string | `Term of 't ]

  type 't encoder_params =
    [ `Anonymous of string_param
    | `Encoder of 't encoder
    | `Labelled of string_param * 't ]
    list

  and 't encoder = string * 't encoder_params

  (* These terms are reduced at runtime *)
  type 't methods_ast = [ `Methods of 't option * 't methods list ]

  type 't no_methods_ast =
    [ `Include of inc
    | `If_def of 't if_def
    | `If_version of 't if_version
    | `If_encoder of 't if_encoder
    | `If of 't _if
    | `Inline_if of 't _if
    | `While of 't _while
    | `For of 't _for
    | `Iterable_for of 't iterable_for
    | `List of 't list_el list
    | `Try of 't _try
    | `Regexp of string * char list
    | `Time_interval of time_el * time_el
    | `Time of time_el
    | `Def of 't _let * 't
    | `Let of 't _let * 't
    | `Binding of 't _let * 't
    | `Cast of 't * type_annotation
    | `App of 't * 't app_arg list
    | `Invoke of 't invoke
    | `Fun of 't fun_arg list * 't
    | `RFun of string * 't fun_arg list * 't
    | `Not of 't
    | `Get of 't
    | `Set of 't * 't
    | `Negative of 't
    | `Append of 't * 't
    | `Assoc of 't * 't
    | `Infix of 't * string * 't
    | `Bool of string * 't list
    | `Coalesce of 't * 't
    | `At of 't * 't
    | `Simple_fun of 't
    | `String_interpolation of char * 't string_interpolation list
    | `Int of string
    | `Float of bool * string * string
    | `String of char * string
    | `Encoder of 't encoder
    | 't ast ]

  type 't expanded_ast = [ 't no_methods_ast | 't methods_ast ]

  type 't parsed_ast =
    [ 't expanded_ast
    | 't methods_ast
    | `Parenthesis of 't
    | `Block of 't
    | `Eof ]
end

open Generic

type t = t Generic.parsed_ast term
type fun_arg = t Generic.fun_arg
type _let = t Generic._let
type let_decoration = t Generic.let_decoration
type parsed_ast = t Generic.parsed_ast
type list_el = t Generic.list_el
type methods = t Generic.methods
type app_arg = t Generic.app_arg
type encoder_params = t Generic.encoder_params

let unit = `Tuple []
let make ?(comments = []) ~pos term = { pos; term; comments }

let rec iter_term fn ({ term } as tm) =
  if term <> `Eof then fn tm;
  match term with
    | `If p | `Inline_if p -> (
        iter_term fn p.if_condition;
        iter_term fn p.if_then;
        List.iter
          (fun (t, t') ->
            iter_term fn t;
            iter_term fn t')
          p.if_elsif;
        match p.if_else with None -> () | Some t -> iter_term fn t)
    | `If_def { if_def_then; if_def_else } -> (
        iter_term fn if_def_then;
        match if_def_else with None -> () | Some term -> iter_term fn term)
    | `If_version { if_version_then; if_version_else } -> (
        iter_term fn if_version_then;
        match if_version_else with None -> () | Some term -> iter_term fn term)
    | `If_encoder { if_encoder_then; if_encoder_else } -> (
        iter_term fn if_encoder_then;
        match if_encoder_else with None -> () | Some term -> iter_term fn term)
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
    | `Try { try_body; try_errors_list; try_handler; try_finally } -> (
        iter_term fn try_body;
        (match try_errors_list with None -> () | Some tm -> iter_term fn tm);
        (match try_handler with Some tm -> iter_term fn tm | None -> ());
        match try_finally with Some tm -> iter_term fn tm | None -> ())
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
    | `Invoke { invoked; meth } -> (
        iter_term fn invoked;
        match meth with
          | `String _ -> ()
          | `App (_, args) ->
              List.iter
                (function `Argsof _ -> () | `Term (_, tm) -> iter_term fn tm)
                args)
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
    | `Bool (_, l) -> List.iter (iter_term fn) l
    | `Coalesce (tm, tm') ->
        iter_term fn tm;
        iter_term fn tm'
    | `At (tm, tm') ->
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
    | `Parenthesis tm -> iter_term fn tm
    | `Block tm -> iter_term fn tm
    | `Methods (base, methods) ->
        (match base with None -> () | Some tm -> iter_term fn tm);
        List.iter
          (function `Method (_, tm) | `Ellipsis tm -> iter_term fn tm)
          methods
    | `Int _ -> ()
    | `Float _ -> ()
    | `String _ -> ()
    | `Var _ -> ()
    | `Eof -> ()
    | `String_interpolation (_, l) ->
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
