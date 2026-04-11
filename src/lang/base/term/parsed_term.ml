(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

open Term_hash
include Runtime_term
module Custom = Term_base.Custom

type comment = [ `Before of string list | `After of string list ]
type pos = Term_base.parsed_pos
type term_annotation = [ `Deprecated of string ]

type string_param =
  [ `Verbatim of string | `String of (pos[@hash.ignore]) * (char * string) ]
[@@deriving hash]

type track_annotation = string * string_param [@@deriving hash]
type inc_type = [ `Lib | `Extra | `Default ] [@@deriving hash]

type inc = {
  inc_type : inc_type;
  inc_name : string;
  inc_pos : pos; [@hash.ignore]
}
[@@deriving hash]

type pattern = { pat_pos : pos; [@hash.ignore] pat_entry : pattern_entry }
[@@deriving hash]

and pattern_entry =
  [ `PVar of string list  (** a field *)
  | `PTuple of pattern list  (** a tuple *)
  | `PList of
    pattern list * ((pos[@hash.ignore]) * string) option * pattern list
    (** a list *)
  | `PMeth of pattern option * (string * meth_term_default) list
    (** a value with methods *) ]

and meth_term_default = [ `Nullable | `Pattern of pattern | `None ]

type _of = { only : string list; except : string list; source : string }
[@@deriving hash]

type _if = {
  if_condition : t;
  if_then_block : block;
  if_elsif : if_elsif list;
  if_else_block : block option;
  if_end_pos : pos; [@hash.ignore]
}

and block = { block_body : t; block_pos : pos [@hash.ignore] }

and if_elsif = {
  elsif_condition : t;
  elsif_then_block : block;
  elsif_pos : pos; [@hash.ignore]
}

and _while = { while_condition : t; while_do_block : block }

and _for = {
  for_variable : string;
  for_from : t;
  for_to : t;
  for_do_block : block;
}

and iterable_for = {
  iterable_for_variable : string;
  iterable_for_iterator : t;
  iterable_for_do_block : block;
}

and _try_handler = {
  try_handler_variable : string;
  try_handler_errors_list : t option;
  try_handler_block : block;
  try_handler_pos : pos; [@hash.ignore]
}

and _try = {
  try_body_block : block;
  try_handler : _try_handler option;
  try_finally_block : block option;
}

and let_decoration =
  [ `None
  | `Recursive
  | `Replaces
  | `Eval
  | `Sqlite_query
  | `Sqlite_row
  | `Yaml_parse
  | `Xml_parse
  | `Json_parse of (string * t) list ]

and _let = {
  decoration : let_decoration;
  pat : pattern;
  arglist : fun_arg list option;
  cast : type_annotation option;
  def : t;
}

and invoke = { invoked : t; optional : bool; meth : invoke_meth }
and invoke_meth = [ `String of string | `App of string * app_arg list ]
and app_arg = [ `Term of string * t | `Argsof of _of ]

and parsed_func_argument = {
  label : string;
  as_variable : pattern option;
  default : t option;
  typ : type_annotation option;
  pos : pos; [@hash.ignore]
  annotations : term_annotation list; [@hash.ignore]
}

and fun_arg = [ `Term of parsed_func_argument | `Argsof of _of ]
and list_el = [ `Term of t | `Ellipsis of t ]

and if_def = {
  if_def_negative : bool;
  if_def_condition : string;
  if_def_then_block : block;
  if_def_else_block : block option;
}

and if_version = {
  if_version_op : [ `Eq | `Geq | `Leq | `Gt | `Lt ];
  if_version_version : Lang_string.Version.t;
  if_version_then_block : block;
  if_version_else_block : block option;
}

and if_encoder = {
  if_encoder_negative : bool;
  if_encoder_condition : string;
  if_encoder_then_block : block;
  if_encoder_else_block : block option;
}

and time_el = {
  week : int option;
  hours : int option;
  minutes : int option;
  seconds : int option;
}

and meth_annotation = {
  optional_meth : bool;
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

and argument = bool * string * type_annotation

and type_annotation =
  [ `Named of string
  | `Nullable of type_annotation
  | `List of type_annotation
  | `Ref of type_annotation
  | `Json_object of type_annotation
  | `Tuple of type_annotation list
  | `Arrow of argument list * type_annotation
  | `Record of meth_annotation list
  | `Method of type_annotation * meth_annotation list
  | `Invoke of type_annotation * string
  | `Source of string * source_annotation
  | `Getter of type_annotation ]

(* These terms are reduced at runtime *)
and parsed_ast =
  [ `If of _if
  | `Inline_if of _if
  | `If_def of if_def
  | `If_version of if_version
  | `If_encoder of if_encoder
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
  | `App of t * app_arg list
  | `Invoke of invoke
  | `Fun of fun_arg list * t
  | `RFun of string * fun_arg list * t
  | `Not of t
  | `Get of t
  | `Set of t * t
  | `Methods of t option * methods list
  | `Negative of t
  | `Append of t * t
  | `Assoc of t * t
  | `Infix of t * string * t
  | `BoolOp of string * t list
  | `Coalesce of t * t
  | `At of t * t
  | `Simple_fun of t
  | `String_interpolation of char * string_interpolation list
  | `Include of inc
  | `Int of string
  | `Bool of bool
  | `Float of string
  | `String of char * string
  | `Raw_string of string * string
  | `Block of t
  | `Parenthesis of t
  | `Encoder of encoder
  | `Eof
  | (t, type_annotation) common_ast ]

and t = {
  term : parsed_ast;
  pos : pos; [@hash.ignore]
  mutable comments : (pos * comment) list; [@hash.ignore]
  annotations : term_annotation list; [@hash.ignore]
}
[@@deriving hash]

and methods = [ `Ellipsis of t | `Method of string * t ]
and string_interpolation = [ `String of string | `Term of t ]

and encoder_params =
  [ `Anonymous of string_param
  | `Encoder of encoder
  | `Labelled of string_param * t ]
  list

and encoder = string * encoder_params

let unit = `Tuple []

let make ?(comments = []) ?(annotations = []) ~pos term =
  { pos; term; comments; annotations }

let rec iter_term fn ({ term } as tm) =
  if term <> `Eof then fn tm;
  match term with
    | `If p | `Inline_if p -> (
        iter_term fn p.if_condition;
        iter_term fn p.if_then_block.block_body;
        List.iter
          (fun { elsif_condition; elsif_then_block; _ } ->
            iter_term fn elsif_condition;
            iter_term fn elsif_then_block.block_body)
          p.if_elsif;
        match p.if_else_block with
          | None -> ()
          | Some b -> iter_term fn b.block_body)
    | `If_def { if_def_then_block; if_def_else_block } -> (
        iter_term fn if_def_then_block.block_body;
        match if_def_else_block with
          | None -> ()
          | Some b -> iter_term fn b.block_body)
    | `If_version { if_version_then_block; if_version_else_block } -> (
        iter_term fn if_version_then_block.block_body;
        match if_version_else_block with
          | None -> ()
          | Some b -> iter_term fn b.block_body)
    | `If_encoder { if_encoder_then_block; if_encoder_else_block } -> (
        iter_term fn if_encoder_then_block.block_body;
        match if_encoder_else_block with
          | None -> ()
          | Some b -> iter_term fn b.block_body)
    | `While { while_condition; while_do_block } ->
        iter_term fn while_condition;
        iter_term fn while_do_block.block_body
    | `For { for_from; for_to; for_do_block } ->
        iter_term fn for_from;
        iter_term fn for_to;
        iter_term fn for_do_block.block_body
    | `Iterable_for { iterable_for_iterator; iterable_for_do_block } ->
        iter_term fn iterable_for_iterator;
        iter_term fn iterable_for_do_block.block_body
    | `List l ->
        List.iter (function `Term tm | `Ellipsis tm -> iter_term fn tm) l
    | `Try { try_body_block; try_handler; try_finally_block } -> (
        iter_term fn try_body_block.block_body;
        (match try_handler with
          | None -> ()
          | Some { try_handler_errors_list; try_handler_block; _ } ->
              (match try_handler_errors_list with
                | None -> ()
                | Some tm -> iter_term fn tm);
              iter_term fn try_handler_block.block_body);
        match try_finally_block with
          | None -> ()
          | Some b -> iter_term fn b.block_body)
    | `Regexp _ -> ()
    | `Time_interval _ -> ()
    | `Time _ -> ()
    | `Def (_let, tm) | `Let (_let, tm) | `Binding (_let, tm) ->
        (match _let.arglist with
          | Some args -> iter_fun_args fn args
          | None -> ());
        iter_term fn _let.def;
        iter_term fn tm
    | `Cast { cast } -> iter_term fn cast
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
    | `BoolOp (_, l) -> List.iter (iter_term fn) l
    | `Coalesce (tm, tm') ->
        iter_term fn tm;
        iter_term fn tm'
    | `At (tm, tm') ->
        iter_term fn tm;
        iter_term fn tm'
    | `Simple_fun tm -> iter_term fn tm
    | `Include _ -> ()
    | `Custom _ -> ()
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
    | `Raw_string _ -> ()
    | `Bool _ -> ()
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
