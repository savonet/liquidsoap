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
module Custom = Term.Custom

type comment = [ `Before of string list | `After of string list ]
type pos = Term.parsed_pos
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

(* A binding scopes over the statements that follow it in its own block.
   Constructs that stand for several statements -- `%include`, the static
   conditionals -- splice into the list. *)
and block = { block_body : statement list; block_pos : pos [@hash.ignore] }

and statement = {
  stmt : statement_ast;
  stmt_pos : pos; [@hash.ignore]
  mutable stmt_comments : (pos * comment) list; [@hash.ignore]
}

and statement_ast =
  [ `Expr of t | `Binding of _let | `Open of t | `Include of inc ]

(* %ifdef / %ifversion / %ifencoder: resolved before typechecking, against the
   builtin environment, the build version and the available encoders. *)
and static_cond =
  [ `Defined of bool * string
  | `Version of [ `Eq | `Geq | `Leq | `Gt | `Lt ] * Lang_string.Version.t
  | `Encoder of bool * string ]

and static_if = {
  static_cond : static_cond;
  static_then : block;
  static_else : block option;
}

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

(* Which surface syntax produced the binding. Semantically irrelevant -- all
   three reduce to the same `Let` -- but the formatter round-trips it. *)
and binding_kind = [ `Bare | `Let | `Def ]

and _let = {
  kind : binding_kind;
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

(* [`Abstract] is [source(_)], whose content is unknown. *)
and source_annotation = [ `Abstract | `Tracks of source_tracks ]
and source_tracks = { extensible : bool; tracks : source_track_annotation list }
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
  | `Static_if of static_if
  | `While of _while
  | `For of _for
  | `Iterable_for of iterable_for
  | `List of list_el list
  | `Try of _try
  | `Regexp of string * char list
  | `Time_interval of time_el * time_el
  | `Time of time_el
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
  | `Simple_fun of block
  | `String_interpolation of char * string_interpolation list
  | `Int of string
  | `Bool of bool
  | `Float of string
  | `String of char * string
  | `Raw_string of string * string
  | `Block of block
  | `Parenthesis of t
  | `Encoder of encoder
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

(** [map_children fn ast] applies [fn] to every direct child term of [ast], and
    [map_block] / [map_statement] do the same for blocks and statements.

    Children are visited in source order, using explicit [let] bindings rather
    than record literals: OCaml leaves record field evaluation order
    unspecified, and [iter_term] below relies on the order to break ties when
    attaching a comment to the closest of two equally distant terms.

    These are the one place that knows the shape of [parsed_ast]; [iter_term]
    and [Term_preprocessor.expand_term] are both written on top of them.

    [?block] overrides how blocks are rebuilt. `%include` expansion needs it:
    splicing turns one statement into several, which [map_statement] alone
    cannot express. *)
let rec map_children ?block:on_block fn ast =
  let block b =
    match on_block with Some f -> f b | None -> map_block ?block:on_block fn b
  in
  let opt_block = Option.map block in
  let opt = Option.map fn in
  let _if ({ if_condition; if_then_block; if_elsif; if_else_block; _ } as p) =
    let if_condition = fn if_condition in
    let if_then_block = block if_then_block in
    let if_elsif =
      List.map
        (fun ({ elsif_condition; elsif_then_block; _ } as e) ->
          let elsif_condition = fn elsif_condition in
          let elsif_then_block = block elsif_then_block in
          { e with elsif_condition; elsif_then_block })
        if_elsif
    in
    let if_else_block = opt_block if_else_block in
    { p with if_condition; if_then_block; if_elsif; if_else_block }
  in
  let fun_args =
    List.map (function
      | `Term arg -> `Term { arg with default = opt arg.default }
      | `Argsof _ as v -> v)
  in
  let app_args =
    List.map (function
      | `Term (lbl, tm) -> `Term (lbl, fn tm)
      | `Argsof _ as v -> v)
  in
  let rec encoder (lbl, params) =
    ( lbl,
      List.map
        (function
          | `Anonymous _ as v -> v
          | `Labelled (s, tm) -> `Labelled (s, fn tm)
          | `Encoder e -> `Encoder (encoder e))
        params )
  in
  match ast with
    | `If p -> `If (_if p)
    | `Inline_if p -> `Inline_if (_if p)
    | `Static_if p -> `Static_if (map_static_if ?block:on_block fn p)
    | `While { while_condition; while_do_block } ->
        let while_condition = fn while_condition in
        let while_do_block = block while_do_block in
        `While { while_condition; while_do_block }
    | `For ({ for_from; for_to; for_do_block; _ } as p) ->
        let for_from = fn for_from in
        let for_to = fn for_to in
        let for_do_block = block for_do_block in
        `For { p with for_from; for_to; for_do_block }
    | `Iterable_for ({ iterable_for_iterator; iterable_for_do_block; _ } as p)
      ->
        let iterable_for_iterator = fn iterable_for_iterator in
        let iterable_for_do_block = block iterable_for_do_block in
        `Iterable_for { p with iterable_for_iterator; iterable_for_do_block }
    | `List l ->
        `List
          (List.map
             (function
               | `Term tm -> `Term (fn tm) | `Ellipsis tm -> `Ellipsis (fn tm))
             l)
    | `Try { try_body_block; try_handler; try_finally_block } ->
        let try_body_block = block try_body_block in
        let try_handler =
          Option.map
            (fun ({ try_handler_errors_list; try_handler_block; _ } as h) ->
              let try_handler_errors_list = opt try_handler_errors_list in
              let try_handler_block = block try_handler_block in
              { h with try_handler_errors_list; try_handler_block })
            try_handler
        in
        let try_finally_block = opt_block try_finally_block in
        `Try { try_body_block; try_handler; try_finally_block }
    | `Cast { cast; typ } -> `Cast { cast = fn cast; typ }
    | `App (tm, args) ->
        let tm = fn tm in
        `App (tm, app_args args)
    | `Invoke ({ invoked; meth; _ } as p) ->
        let invoked = fn invoked in
        let meth =
          match meth with
            | `String _ as s -> s
            | `App (n, args) -> `App (n, app_args args)
        in
        `Invoke { p with invoked; meth }
    | `Fun (args, tm) ->
        let args = fun_args args in
        `Fun (args, fn tm)
    | `RFun (name, args, tm) ->
        let args = fun_args args in
        `RFun (name, args, fn tm)
    | `Not tm -> `Not (fn tm)
    | `Get tm -> `Get (fn tm)
    | `Set (tm, tm') ->
        let tm = fn tm in
        `Set (tm, fn tm')
    | `Negative tm -> `Negative (fn tm)
    | `Append (tm, tm') ->
        let tm = fn tm in
        `Append (tm, fn tm')
    | `Assoc (tm, tm') ->
        let tm = fn tm in
        `Assoc (tm, fn tm')
    | `Infix (tm, op, tm') ->
        let tm = fn tm in
        `Infix (tm, op, fn tm')
    | `BoolOp (op, l) -> `BoolOp (op, List.map fn l)
    | `Coalesce (tm, tm') ->
        let tm = fn tm in
        `Coalesce (tm, fn tm')
    | `At (tm, tm') ->
        let tm = fn tm in
        `At (tm, fn tm')
    | `Simple_fun b -> `Simple_fun (block b)
    | `Tuple l -> `Tuple (List.map fn l)
    | `Open (tm, tm') ->
        let tm = fn tm in
        `Open (tm, fn tm')
    | `Seq (tm, tm') ->
        let tm = fn tm in
        `Seq (tm, fn tm')
    | `Parenthesis tm -> `Parenthesis (fn tm)
    | `Block b -> `Block (block b)
    | `Methods (base, methods) ->
        let base = opt base in
        `Methods
          ( base,
            List.map
              (function
                | `Method (name, tm) -> `Method (name, fn tm)
                | `Ellipsis tm -> `Ellipsis (fn tm))
              methods )
    | `String_interpolation (sep, l) ->
        `String_interpolation
          ( sep,
            List.map
              (function `String _ as s -> s | `Term tm -> `Term (fn tm))
              l )
    | `Encoder e -> `Encoder (encoder e)
    (* Leaves: no child terms. *)
    | ( `Regexp _ | `Time_interval _ | `Time _ | `Int _ | `Float _ | `String _
      | `Raw_string _ | `Bool _ | `Var _ | `Null | `Custom _ ) as ast ->
        ast

and map_block ?block fn b =
  { b with block_body = List.map (map_statement ?block fn) b.block_body }

and map_static_if ?block:on_block fn ({ static_then; static_else; _ } as p) =
  let block b =
    match on_block with Some f -> f b | None -> map_block ?block:on_block fn b
  in
  let static_then = block static_then in
  let static_else = Option.map block static_else in
  { p with static_then; static_else }

and map_let fn ({ decoration; arglist; def; _ } as l) =
  let decoration =
    match decoration with
      | `Json_parse args ->
          `Json_parse (List.map (fun (lbl, tm) -> (lbl, fn tm)) args)
      | ( `None | `Recursive | `Replaces | `Eval | `Sqlite_query | `Sqlite_row
        | `Yaml_parse | `Xml_parse ) as d ->
          d
  in
  let arglist =
    Option.map
      (List.map (function
        | `Term arg -> `Term { arg with default = Option.map fn arg.default }
        | `Argsof _ as v -> v))
      arglist
  in
  let def = fn def in
  { l with decoration; arglist; def }

and map_statement ?block:_ fn stmt =
  let stmt_ast =
    match stmt.stmt with
      | `Expr tm -> `Expr (fn tm)
      | `Binding l -> `Binding (map_let fn l)
      | `Open tm -> `Open (fn tm)
      | `Include _ as v -> v
  in
  { stmt with stmt = stmt_ast }

(** Visit every node a comment can be attached to: terms and statements. [fn]
    receives the node's position, whether it is a statement, and a function that
    updates its comment list. A binding is a statement, so its doc comment has
    no term to land on. Nodes are visited outermost first. *)
let iter_anchors fn tm =
  let rec go_term (tm : t) =
    fn tm.pos `Term (fun update -> tm.comments <- update tm.comments);
    ignore
      (map_children ~block:go_block
         (fun tm ->
           go_term tm;
           tm)
         tm.term)
  and go_block (b : block) =
    List.iter
      (fun stmt ->
        fn stmt.stmt_pos `Statement (fun update ->
            stmt.stmt_comments <- update stmt.stmt_comments);
        ignore
          (map_statement
             (fun tm ->
               go_term tm;
               tm)
             stmt))
      b.block_body;
    b
  in
  go_term tm

let rec iter_term fn tm =
  fn tm;
  ignore
    (map_children
       (fun tm ->
         iter_term fn tm;
         tm)
       tm.term)

(** Visit every term in [b], including those inside its statements. *)
let iter_block fn b =
  ignore
    (map_block
       (fun tm ->
         iter_term fn tm;
         tm)
       b)
