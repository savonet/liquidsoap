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

open Liquidsoap_lang
open Parsed_term

let json_of_position { Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum } : Json.t
    =
  `Assoc
    [
      ("fname", `String pos_fname);
      ("lnum", `Int pos_lnum);
      ("bol", `Int pos_bol);
      ("cnum", `Int pos_cnum);
    ]

let json_of_positions (p, p') = `Tuple [json_of_position p; json_of_position p']

let json_of_if_def ~to_json
    { if_def_negative; if_def_condition; if_def_then_block; if_def_else_block }
    =
  [
    ("negative", `Bool if_def_negative);
    ("condition", `String if_def_condition);
    ("then", to_json if_def_then_block.block_body);
    ( "else",
      match if_def_else_block with
        | None -> `Null
        | Some b -> to_json b.block_body );
  ]

let json_of_if_encoder ~to_json
    {
      if_encoder_negative;
      if_encoder_condition;
      if_encoder_then_block;
      if_encoder_else_block;
    } =
  [
    ("negative", `Bool if_encoder_negative);
    ("condition", `String if_encoder_condition);
    ("then", to_json if_encoder_then_block.block_body);
    ( "else",
      match if_encoder_else_block with
        | None -> `Null
        | Some b -> to_json b.block_body );
  ]

let json_of_if_version ~to_json
    {
      if_version_op;
      if_version_version;
      if_version_then_block;
      if_version_else_block;
    } =
  [
    ( "opt",
      `String
        (match if_version_op with
          | `Eq -> "=="
          | `Geq -> ">="
          | `Leq -> "<="
          | `Gt -> ">"
          | `Lt -> "<") );
    ("version", `String (Lang_string.Version.to_string if_version_version));
    ("then", to_json if_version_then_block.block_body);
    ( "else",
      match if_version_else_block with
        | None -> `Null
        | Some b -> to_json b.block_body );
  ]

let json_of_while ~to_json { while_condition; while_do_block } =
  [
    ("condition", to_json while_condition);
    ("loop", to_json while_do_block.block_body);
  ]

let json_of_for ~to_json { for_variable; for_from; for_to; for_do_block } =
  [
    ("variable", `String for_variable);
    ("from", to_json for_from);
    ("to", to_json for_to);
    ("loop", to_json for_do_block.block_body);
  ]

let json_of_iterable_for ~to_json
    { iterable_for_variable; iterable_for_iterator; iterable_for_do_block } =
  [
    ("variable", `String iterable_for_variable);
    ("iterator", to_json iterable_for_iterator);
    ("loop", to_json iterable_for_do_block.block_body);
  ]

let json_of_try ~to_json { try_body_block; try_handler; try_finally_block } =
  [
    ("body", to_json try_body_block.block_body);
    ( "variable",
      `String
        (match try_handler with
          | Some h -> h.try_handler_variable
          | None -> "_") );
    ( "errors_list",
      match try_handler with
        | Some { try_handler_errors_list = Some tm; _ } -> to_json tm
        | _ -> `Null );
    ( "handler",
      match try_handler with
        | Some h -> to_json h.try_handler_block.block_body
        | None -> `Null );
    ( "finally",
      match try_finally_block with
        | Some b -> to_json b.block_body
        | None -> `Null );
  ]

let type_node ~typ ?(extra = []) value =
  `Assoc
    ([
       ("type", `String "type_annotation");
       ("subtype", `String typ);
       ("value", value);
     ]
    @ extra)

let ast_node ~typ value = ("type", `String typ) :: value

let json_of_annotated_string = function
  | `Verbatim s -> ast_node ~typ:"var" [("value", `String s)]
  | `String (_, (sep, s)) ->
      ast_node ~typ:"ground"
        [("value", `String (Printf.sprintf "%c%s%c" sep s sep))]

let rec json_of_type_annotation = function
  | `Named n -> type_node ~typ:"named" (`String n)
  | `Nullable t -> type_node ~typ:"nullable" (json_of_type_annotation t)
  | `List t -> type_node ~typ:"list" (json_of_type_annotation t)
  | `Ref t -> type_node ~typ:"ref" (json_of_type_annotation t)
  | `Json_object t -> type_node ~typ:"json_object" (json_of_type_annotation t)
  | `Tuple l ->
      type_node ~typ:"tuple" (`Tuple (List.map json_of_type_annotation l))
  | `Arrow (args, t) ->
      type_node ~typ:"arrow"
        ~extra:[("args", `Tuple (List.map json_of_type_fun_arg args))]
        (json_of_type_annotation t)
  | `Record l ->
      type_node ~typ:"record" (`Tuple (List.map json_of_meth_annotation l))
  | `Method (t, l) ->
      type_node ~typ:"method"
        ~extra:[("base", json_of_type_annotation t)]
        (`Tuple (List.map json_of_meth_annotation l))
  | `Invoke (t, s) ->
      type_node ~typ:"invoke"
        ~extra:[("method", `String s)]
        (json_of_type_annotation t)
  | `Source (n, t) ->
      type_node ~typ:"source"
        ~extra:[("base", `String n)]
        (json_of_source_annotation t)
  | `Getter t -> type_node ~typ:"getter" (json_of_type_annotation t)

and json_of_type_fun_arg (b, s, t) =
  type_node ~typ:"fun_arg"
    ~extra:[("optional", `Bool b); ("label", `String s)]
    (json_of_type_annotation t)

and json_of_meth_annotation { optional_meth; name; typ; json_name } =
  type_node ~typ:"method_annotation"
    ~extra:
      [
        ("optional", `Bool optional_meth);
        ("name", `String name);
        ("json_name", match json_name with None -> `Null | Some n -> `String n);
      ]
    (json_of_type_annotation typ)

and json_of_source_annotation { extensible; tracks } =
  type_node ~typ:"source_annotation"
    ~extra:[("extensible", `Bool extensible)]
    (`Tuple (List.map json_of_source_track_annotation tracks))

and json_of_source_track_annotation { track_name; track_type; track_params } =
  type_node ~typ:"source_track_annotation"
    ~extra:
      [
        ("name", `String track_name);
        ( "params",
          `Tuple
            (List.map
               (fun (l, v) ->
                 `Assoc
                   (ast_node ~typ:"app_arg"
                      [
                        ("label", `String l);
                        ("value", `Assoc (json_of_annotated_string v));
                      ]))
               track_params) );
      ]
    (`String track_type)

let json_of_if ~to_json
    { if_condition; if_then_block; if_elsif; if_else_block; _ } =
  [
    ("condition", to_json if_condition);
    ("then", to_json if_then_block.block_body);
    ( "elsif",
      `Tuple
        (List.map
           (fun { elsif_condition; elsif_then_block; _ } ->
             `Assoc
               (ast_node ~typ:"elsif"
                  [
                    ("condition", to_json elsif_condition);
                    ("then", to_json elsif_then_block.block_body);
                  ]))
           if_elsif) );
    ( "else",
      match if_else_block with None -> `Null | Some b -> to_json b.block_body );
  ]

let rec base_json_of_pat = function
  | `PVar l ->
      ast_node ~typ:"pvar" [("value", `Tuple (List.map (fun v -> `String v) l))]
  | `PTuple l ->
      ast_node ~typ:"ptuple" [("value", `Tuple (List.map json_of_pat l))]
  | `PList (l, v, l') ->
      ast_node ~typ:"plist"
        [
          ("left", `Tuple (List.map json_of_pat l));
          ("middle", match v with None -> `Null | Some (_, s) -> `String s);
          ("right", `Tuple (List.map json_of_pat l'));
        ]
  | `PMeth (ellipsis, methods) ->
      ast_node ~typ:"pmeth"
        [
          ( "value",
            `Tuple
              (List.map
                 (function
                   | var, `None ->
                       `Assoc (ast_node ~typ:"var" [("value", `String var)])
                   | var, `Nullable ->
                       `Assoc
                         (ast_node ~typ:"var" [("value", `String (var ^ "?"))])
                   | var, `Pattern pat ->
                       `Assoc
                         (ast_node ~typ:"infix"
                            [
                              ( "left",
                                `Assoc
                                  (ast_node ~typ:"var" [("value", `String var)])
                              );
                              ("op", `String "=");
                              ("right", json_of_pat pat);
                            ]))
                 methods
              @
                match ellipsis with
                | None -> []
                | Some pat ->
                    [
                      `Assoc
                        (ast_node ~typ:"ellipsis" [("value", json_of_pat pat)]);
                    ]) );
        ]

and json_of_pat p = `Assoc (base_json_of_pat p.pat_entry)

let json_of_of { only; except; source } =
  [
    ("only", `Tuple (List.map (fun s -> `String s) only));
    ("except", `Tuple (List.map (fun s -> `String s) except));
    ("source", `String source);
  ]

let json_of_fun_arg ~to_json : Parsed_term.fun_arg -> (string * Json.t) list =
  function
  | `Argsof _of -> ast_node ~typ:"argsof" (json_of_of _of)
  | `Term { Parsed_term.label; as_variable; typ; default } ->
      ast_node ~typ:"term"
        [
          ( "value",
            `Assoc
              (ast_node ~typ:"fun_arg"
                 [
                   ("label", `String label);
                   ( "as_variable",
                     match as_variable with
                       | None -> `Null
                       | Some pat -> json_of_pat pat );
                   ( "typ",
                     match typ with
                       | None -> `Null
                       | Some typ -> json_of_type_annotation typ );
                   ( "default",
                     match default with None -> `Null | Some d -> to_json d );
                 ]) );
        ]

let json_of_fun ~to_json arguments body =
  [
    ( "arguments",
      `Tuple
        (List.map (fun arg -> `Assoc (json_of_fun_arg ~to_json arg)) arguments)
    );
    ("body", to_json body);
  ]

let json_of_let_decoration ~to_json : Parsed_term.let_decoration -> Json.t =
  function
  | `None -> `Null
  | `Recursive -> `Assoc (ast_node ~typ:"var" [("value", `String "rec")])
  | `Replaces -> `Assoc (ast_node ~typ:"var" [("value", `String "replaces")])
  | `Eval -> `Assoc (ast_node ~typ:"var" [("value", `String "eval")])
  | `Sqlite_query ->
      `Assoc (ast_node ~typ:"var" [("value", `String "sqlite.query")])
  | `Sqlite_row ->
      `Assoc (ast_node ~typ:"var" [("value", `String "sqlite.row")])
  | `Yaml_parse ->
      `Assoc (ast_node ~typ:"var" [("value", `String "yaml.parse")])
  | `Xml_parse -> `Assoc (ast_node ~typ:"var" [("value", `String "xml.parse")])
  | `Json_parse [] ->
      `Assoc (ast_node ~typ:"var" [("value", `String "json.parse")])
  | `Json_parse args ->
      `Assoc
        (ast_node ~typ:"app"
           [
             ( "op",
               `Assoc (ast_node ~typ:"var" [("value", `String "json.parse")]) );
             ( "args",
               `Tuple
                 (List.map
                    (fun (l, t) ->
                      `Assoc
                        (ast_node ~typ:"term"
                           [
                             ( "value",
                               `Assoc
                                 (ast_node ~typ:"app_arg"
                                    [("label", `String l); ("value", to_json t)])
                             );
                           ]))
                    args) );
           ])

let args_of_json_let ~to_json { decoration; pat; arglist; cast; def } =
  [
    ("decoration", json_of_let_decoration ~to_json decoration);
    ("pat", json_of_pat pat);
    ( "arglist",
      match arglist with
        | None -> `Null
        | Some arglist ->
            `Tuple
              (List.map
                 (fun arg -> `Assoc (json_of_fun_arg ~to_json arg))
                 arglist) );
    ( "cast",
      match cast with None -> `Null | Some t -> json_of_type_annotation t );
    ("definition", to_json def);
  ]

let json_of_let ~to_json ast =
  let typ, args, body =
    match ast with
      | `Def (p, body) -> ("def", args_of_json_let ~to_json p, body)
      | `Let (p, body) -> ("let", args_of_json_let ~to_json p, body)
      | `Binding (p, body) -> ("binding", args_of_json_let ~to_json p, body)
  in
  ast_node ~typ (("body", to_json body) :: args)

let json_of_app_arg ~to_json = function
  | `Term (l, v) ->
      ("position", json_of_positions v.pos)
      :: ast_node ~typ:"term"
           [
             ( "value",
               `Assoc
                 (ast_node ~typ:"app_arg"
                    [("label", `String l); ("value", to_json v)]) );
           ]
  | `Argsof _of -> ast_node ~typ:"argsof" (json_of_of _of)

let json_of_app_args ~to_json args =
  `Tuple (List.map (fun arg -> `Assoc (json_of_app_arg ~to_json arg)) args)

let json_of_invoke_meth ~to_json = function
  | `String s -> ast_node ~typ:"var" [("value", `String s)]
  | `App (s, args) ->
      ast_node ~typ:"app"
        [
          ("op", `Assoc (ast_node ~typ:"var" [("value", `String s)]));
          ("args", json_of_app_args ~to_json args);
        ]

let json_of_list_el ~to_json = function
  | `Term t ->
      ("position", json_of_positions t.pos)
      :: ast_node ~typ:"term" [("value", to_json t)]
  | `Ellipsis t ->
      ("position", json_of_positions t.pos)
      :: ast_node ~typ:"ellipsis" [("value", to_json t)]

let json_of_time_el { week; hours; minutes; seconds } =
  let to_int = function None -> `Null | Some i -> `Int i in
  [
    ("week", to_int week);
    ("hours", to_int hours);
    ("minutes", to_int minutes);
    ("seconds", to_int seconds);
  ]

let rec to_ast_json ~to_json = function
  | `Get t -> ast_node ~typ:"get" [("value", to_json t)]
  | `Set (t, t') ->
      ast_node ~typ:"infix"
        [("left", to_json t); ("op", `String ":="); ("right", to_json t')]
  | `Inline_if p -> ast_node ~typ:"inline_if" (json_of_if ~to_json p)
  | `If p -> ast_node ~typ:"if" (json_of_if ~to_json p)
  | `If_def p -> ast_node ~typ:"if_def" (json_of_if_def ~to_json p)
  | `If_version p -> ast_node ~typ:"if_version" (json_of_if_version ~to_json p)
  | `If_encoder p -> ast_node ~typ:"if_encoder" (json_of_if_encoder ~to_json p)
  | `While p -> ast_node ~typ:"while" (json_of_while ~to_json p)
  | `For p -> ast_node ~typ:"for" (json_of_for ~to_json p)
  | `Iterable_for p ->
      ast_node ~typ:"iterable_for" (json_of_iterable_for ~to_json p)
  | `Not t -> ast_node ~typ:"not" [("value", to_json t)]
  | `Negative t -> ast_node ~typ:"negative" [("value", to_json t)]
  | `String_interpolation (c, l) ->
      let l =
        `String (Printf.sprintf "%c" c)
        :: (l @ [`String (Printf.sprintf "%c" c)])
      in
      let l =
        List.map
          (function
            | `String s ->
                `Assoc
                  (ast_node ~typ:"interpolated_string" [("value", `String s)])
            | `Term tm ->
                `Assoc
                  (ast_node ~typ:"interpolated_term" [("value", to_json tm)]))
          l
      in
      ast_node ~typ:"string_interpolation" [("value", `Tuple l)]
  | `Append (t, t') ->
      ast_node ~typ:"append" [("left", to_json t); ("right", to_json t')]
  | `Assoc (t, t') ->
      ast_node ~typ:"assoc" [("left", to_json t); ("right", to_json t')]
  | `Infix (t, op, t') ->
      ast_node ~typ:"infix"
        [("left", to_json t); ("op", `String op); ("right", to_json t')]
  | `BoolOp (op, l) ->
      ast_node ~typ:"bool"
        [("op", `String op); ("value", `Tuple (List.map to_json l))]
  | `Simple_fun t -> ast_node ~typ:"simple_fun" [("value", to_json t)]
  | `Time t -> ast_node ~typ:"time" (json_of_time_el t)
  | `Time_interval (t, t') ->
      ast_node ~typ:"time_interval"
        [
          ("left", `Assoc (ast_node ~typ:"time" (json_of_time_el t)));
          ("right", `Assoc (ast_node ~typ:"time" (json_of_time_el t')));
        ]
  | `Regexp (name, flags) ->
      ast_node ~typ:"regexp"
        [
          ("name", `String name);
          ( "flags",
            `Tuple
              (List.sort Stdlib.compare
                 (List.map (fun c -> `String (Char.escaped c)) flags)) );
        ]
  | `Try p -> ast_node ~typ:"try" (json_of_try ~to_json p)
  | `Custom g ->
      ast_node ~typ:"ground"
        [
          ( "value",
            `String (Json.to_string (Term_base.Custom.to_json ~pos:[] g)) );
        ]
  | `Bool b -> ast_node ~typ:"ground" [("value", `String (string_of_bool b))]
  | `Int i -> ast_node ~typ:"ground" [("value", `String i)]
  | `Float v -> ast_node ~typ:"ground" [("value", `String v)]
  | `Parenthesis tm -> ast_node ~typ:"parenthesis" [("value", to_json tm)]
  | `Block tm -> ast_node ~typ:"block" [("value", to_json tm)]
  | `Raw_string (id, s) ->
      ast_node ~typ:"raw_string" [("id", `String id); ("value", `String s)]
  | `String (c, s) ->
      ast_node ~typ:"string"
        [("value", `String (Printf.sprintf "%c%s%c" c s c))]
  | `Encoder e -> ast_node ~typ:"encoder" (to_encoder_json ~to_json e)
  | `List l ->
      ast_node ~typ:"list"
        [
          ( "value",
            `Tuple (List.map (fun p -> `Assoc (json_of_list_el ~to_json p)) l)
          );
        ]
  | `Tuple l -> ast_node ~typ:"tuple" [("value", `Tuple (List.map to_json l))]
  | `Null -> ast_node ~typ:"null" []
  | `Cast { cast = t; typ } ->
      ast_node ~typ:"cast"
        [("left", to_json t); ("right", json_of_type_annotation typ)]
  | `Invoke { invoked; optional; meth } ->
      ast_node ~typ:"invoke"
        [
          ("invoked", to_json invoked);
          ("optional", `Bool optional);
          ("meth", `Assoc (json_of_invoke_meth ~to_json meth));
        ]
  | `Methods (base, methods) ->
      let base, base_methods =
        match base with None -> (`Null, []) | Some t -> (to_json t, [])
      in
      ast_node ~typ:"methods"
        [
          ("base", base);
          ( "methods",
            `Tuple
              (List.map
                 (function
                   | `Ellipsis v ->
                       `Assoc (ast_node ~typ:"ellipsis" [("value", to_json v)])
                   | `Method (k, v) ->
                       `Assoc
                         (ast_node ~typ:"method"
                            [("name", `String k); ("value", to_json v)]))
                 methods
              @ base_methods) );
        ]
  | `Eof -> ast_node ~typ:"eof" []
  | `Open (t, t') ->
      ast_node ~typ:"open" [("left", to_json t); ("right", to_json t')]
  | `Let _ as ast -> json_of_let ~to_json ast
  | `Def _ as ast -> json_of_let ~to_json ast
  | `Binding _ as ast -> json_of_let ~to_json ast
  | `Include { inc_type = `Lib; inc_name } ->
      ast_node ~typ:"include_lib" [("value", `String inc_name)]
  | `Include { inc_type = `Default; inc_name } ->
      ast_node ~typ:"include" [("value", `String inc_name)]
  | `Include { inc_type = `Extra; inc_name } ->
      ast_node ~typ:"include_extra" [("value", `String inc_name)]
  | `Coalesce (t, t') ->
      ast_node ~typ:"coalesce" [("left", to_json t); ("right", to_json t')]
  | `At (t, t') ->
      ast_node ~typ:"infix"
        [("left", to_json t); ("op", `String "@"); ("right", to_json t')]
  | `Var s -> ast_node ~typ:"var" [("value", `String s)]
  | `Seq (t, t') ->
      ast_node ~typ:"seq" [("left", to_json t); ("right", to_json t')]
  | `App (t, args) ->
      ast_node ~typ:"app"
        [("op", to_json t); ("args", json_of_app_args ~to_json args)]
  | `Fun (args, body) -> ast_node ~typ:"fun" (json_of_fun ~to_json args body)
  | `RFun (lbl, args, body) ->
      ast_node ~typ:"rfun"
        (("name", `String lbl) :: json_of_fun ~to_json args body)

and to_encoder_json ~to_json (lbl, params) =
  [
    ("label", `String lbl);
    ("params", `Tuple (List.map (to_encoder_param_json ~to_json) params));
  ]

and to_encoder_param_json ~to_json = function
  | `Encoder e -> `Assoc (ast_node ~typ:"encoder" (to_encoder_json ~to_json e))
  | `Labelled (lbl, v) ->
      `Assoc
        (ast_node ~typ:"infix"
           [
             ("left", `Assoc (json_of_annotated_string lbl));
             ("op", `String "=");
             ("right", to_json v);
           ])
  | `Anonymous s -> `Assoc (json_of_annotated_string s)

let rec to_json { pos; term; _ } : Json.t =
  `Assoc (("position", json_of_positions pos) :: to_ast_json ~to_json term)

(* ---- Canonical flat JSON format ----
   parse_string always emits this format. Block bodies are flat arrays ("body"),
   if/elsif/else use a unified "branches" array, try uses a "handler" sub-object. *)

(* Flatten a body chain into a list of JSON statement nodes. *)
let rec statements_of_chain ~to_json t : Json.t list =
  match t.term with
    | `Def (p, body) ->
        `Assoc (("position", json_of_positions t.pos) :: json_of_def ~to_json p)
        :: statements_of_chain ~to_json body
    | `Let (p, body) ->
        `Assoc
          (("position", json_of_positions t.pos)
          :: ast_node ~typ:"let" (args_of_json_let ~to_json p))
        :: statements_of_chain ~to_json body
    | `Binding (p, body) ->
        `Assoc
          (("position", json_of_positions t.pos)
          :: ast_node ~typ:"binding" (args_of_json_let ~to_json p))
        :: statements_of_chain ~to_json body
    | `Seq (t1, t2) -> to_json t1 :: statements_of_chain ~to_json t2
    | `Open (t, body) ->
        `Assoc
          (("position", json_of_positions t.pos)
          :: ast_node ~typ:"open" [("left", to_json t)])
        :: statements_of_chain ~to_json body
    | `Eof -> []
    | _ -> [to_json t]

(* Emit a def node with its function body flattened into "body". *)
and json_of_def ~to_json { decoration; pat; arglist; cast; def } =
  ast_node ~typ:"def"
    [
      ("decoration", json_of_let_decoration ~to_json decoration);
      ("pat", json_of_pat pat);
      ( "arglist",
        match arglist with
          | None -> `Null
          | Some arglist ->
              `Tuple
                (List.map
                   (fun arg -> `Assoc (json_of_fun_arg ~to_json arg))
                   arglist) );
      ( "cast",
        match cast with None -> `Null | Some t -> json_of_type_annotation t );
      ("body", `Tuple (statements_of_chain ~to_json def));
    ]

let rec to_json_canonical ({ pos; term; _ } : Parsed_term.t) : Json.t =
  `Assoc (("position", json_of_positions pos) :: to_ast_json_canonical pos term)

and to_ast_json_canonical pos term =
  let to_json = to_json_canonical in
  let body t = `Tuple (statements_of_chain ~to_json t) in
  let block_node ?(typ = "") { Parsed_term.block_body; block_pos } =
    let type_field = if typ = "" then [] else [("type", `String typ)] in
    `Assoc
      (type_field
      @ [("position", json_of_positions block_pos); ("body", body block_body)])
  in
  match term with
    | `If { if_condition; if_then_block; if_elsif; if_else_block; if_end_pos }
      ->
        let elsif_json i
            { Parsed_term.elsif_condition; elsif_then_block; elsif_pos } =
          let then_end =
            match List.nth_opt if_elsif (i + 1) with
              | Some { Parsed_term.elsif_pos = p; _ } -> fst p
              | None -> (
                  match if_else_block with
                    | Some b -> fst b.block_pos
                    | None -> fst if_end_pos)
          in
          `Assoc
            [
              ("type", `String "elsif");
              ("position", json_of_positions (fst elsif_pos, then_end));
              ("condition", to_json elsif_condition);
              ("body", body elsif_then_block.block_body);
            ]
        in
        ast_node ~typ:"if"
          [
            ("condition", to_json if_condition);
            ("then_block", block_node ~typ:"then_block" if_then_block);
            ("elsif", `Tuple (List.mapi elsif_json if_elsif));
            ( "else_block",
              match if_else_block with
                | None -> `Null
                | Some b -> block_node ~typ:"else_block" b );
          ]
    | `If_def
        {
          if_def_negative;
          if_def_condition;
          if_def_then_block;
          if_def_else_block;
        } ->
        ast_node ~typ:"if_def"
          [
            ("negative", `Bool if_def_negative);
            ("condition", `String if_def_condition);
            ("then_block", block_node ~typ:"ifdef_block" if_def_then_block);
            ( "else_block",
              match if_def_else_block with
                | None -> `Null
                | Some b -> block_node ~typ:"ifdef_block" b );
          ]
    | `If_version
        {
          if_version_op;
          if_version_version;
          if_version_then_block;
          if_version_else_block;
        } ->
        ast_node ~typ:"if_version"
          [
            ( "opt",
              `String
                (match if_version_op with
                  | `Eq -> "=="
                  | `Geq -> ">="
                  | `Leq -> "<="
                  | `Gt -> ">"
                  | `Lt -> "<") );
            ( "version",
              `String (Lang_string.Version.to_string if_version_version) );
            ("then_block", block_node ~typ:"ifdef_block" if_version_then_block);
            ( "else_block",
              match if_version_else_block with
                | None -> `Null
                | Some b -> block_node ~typ:"ifdef_block" b );
          ]
    | `If_encoder
        {
          if_encoder_negative;
          if_encoder_condition;
          if_encoder_then_block;
          if_encoder_else_block;
        } ->
        ast_node ~typ:"if_encoder"
          [
            ("negative", `Bool if_encoder_negative);
            ("condition", `String if_encoder_condition);
            ("then_block", block_node ~typ:"ifdef_block" if_encoder_then_block);
            ( "else_block",
              match if_encoder_else_block with
                | None -> `Null
                | Some b -> block_node ~typ:"ifdef_block" b );
          ]
    | `While { while_condition; while_do_block } ->
        let header =
          `Assoc
            [
              ("type", `String "while_header");
              ( "position",
                json_of_positions (fst pos, fst while_do_block.block_pos) );
              ("condition", to_json while_condition);
            ]
        in
        let body_node =
          `Assoc
            [
              ("type", `String "while_body");
              ("position", json_of_positions while_do_block.block_pos);
              ("body", body while_do_block.block_body);
            ]
        in
        ast_node ~typ:"while" [("parts", `Tuple [header; body_node])]
    | `For { for_variable; for_from; for_to; for_do_block } ->
        let header =
          `Assoc
            [
              ("type", `String "for_header");
              ( "position",
                json_of_positions (fst pos, fst for_do_block.block_pos) );
              ("variable", `String for_variable);
              ("from", to_json for_from);
              ("to", to_json for_to);
            ]
        in
        let body_node =
          `Assoc
            [
              ("type", `String "for_body");
              ("position", json_of_positions for_do_block.block_pos);
              ("body", body for_do_block.block_body);
            ]
        in
        ast_node ~typ:"for" [("parts", `Tuple [header; body_node])]
    | `Iterable_for
        { iterable_for_variable; iterable_for_iterator; iterable_for_do_block }
      ->
        let header =
          `Assoc
            [
              ("type", `String "iterable_for_header");
              ( "position",
                json_of_positions (fst pos, fst iterable_for_do_block.block_pos)
              );
              ("variable", `String iterable_for_variable);
              ("iterator", to_json iterable_for_iterator);
            ]
        in
        let body_node =
          `Assoc
            [
              ("type", `String "iterable_for_body");
              ("position", json_of_positions iterable_for_do_block.block_pos);
              ("body", body iterable_for_do_block.block_body);
            ]
        in
        ast_node ~typ:"iterable_for" [("parts", `Tuple [header; body_node])]
    | `Try { try_body_block; try_handler; try_finally_block } ->
        let try_body_node =
          `Assoc
            [
              ("type", `String "try_body");
              ("position", json_of_positions try_body_block.block_pos);
              ("body", body try_body_block.block_body);
            ]
        in
        let catch_nodes =
          match try_handler with
            | Some
                {
                  try_handler_variable;
                  try_handler_errors_list;
                  try_handler_block;
                  try_handler_pos;
                } ->
                [
                  `Assoc
                    [
                      ("type", `String "try_catch");
                      ( "position",
                        json_of_positions
                          (fst try_handler_pos, snd try_handler_block.block_pos)
                      );
                      ("variable", `String try_handler_variable);
                      ( "errors_list",
                        match try_handler_errors_list with
                          | None -> `Null
                          | Some t -> to_json t );
                      ("body", body try_handler_block.block_body);
                    ];
                ]
            | None -> []
        in
        let finally_nodes =
          match try_finally_block with
            | Some finally_block ->
                [
                  `Assoc
                    [
                      ("type", `String "try_finally");
                      ("position", json_of_positions finally_block.block_pos);
                      ("body", body finally_block.block_body);
                    ];
                ]
            | None -> []
        in
        ast_node ~typ:"try"
          [("parts", `Tuple ((try_body_node :: catch_nodes) @ finally_nodes))]
    | `Fun (args, fun_body) ->
        ast_node ~typ:"fun"
          [
            ( "arguments",
              `Tuple
                (List.map
                   (fun arg -> `Assoc (json_of_fun_arg ~to_json arg))
                   args) );
            ("body", body fun_body);
          ]
    | `RFun (lbl, args, fun_body) ->
        ast_node ~typ:"rfun"
          [
            ("name", `String lbl);
            ( "arguments",
              `Tuple
                (List.map
                   (fun arg -> `Assoc (json_of_fun_arg ~to_json arg))
                   args) );
            ("body", body fun_body);
          ]
    | `Simple_fun t -> ast_node ~typ:"simple_fun" [("body", body t)]
    | `Def (p, _) -> json_of_def ~to_json p
    | `Let (p, _) -> ast_node ~typ:"let" (args_of_json_let ~to_json p)
    | `Binding (p, _) -> ast_node ~typ:"binding" (args_of_json_let ~to_json p)
    | `Open (t, _) -> ast_node ~typ:"open" [("left", to_json t)]
    | `Block tm -> ast_node ~typ:"block" [("body", body tm)]
    | other -> to_ast_json ~to_json other

let parse_string ?(formatter = Format.err_formatter) content =
  let lexbuf = Sedlexing.Utf8.from_string content in
  let throw = Runtime.throw ~formatter ~lexbuf:(Some lexbuf) () in
  try
    let tokenizer = Preprocessor.mk_tokenizer lexbuf in
    Parser_helper.clear_comments ();
    let term = Runtime.program tokenizer in
    let raw_comments = Parser_helper.get_pending_comments () in
    Parser_helper.attach_comments term;
    let start_pos =
      { Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
    in
    let end_pos = { start_pos with Lexing.pos_cnum = String.length content } in
    let ast =
      `Assoc
        [
          ("type", `String "program");
          ("position", json_of_positions (start_pos, end_pos));
          ("body", `Tuple (statements_of_chain ~to_json:to_json_canonical term));
        ]
    in
    let comments =
      `Tuple
        (List.rev_map
           (fun ((startp, endp), lines) ->
             `Assoc
               [
                 ("start", `Int startp.Lexing.pos_cnum);
                 ("end", `Int endp.Lexing.pos_cnum);
                 ("lnum", `Int startp.Lexing.pos_lnum);
                 ("value", `String (String.concat "\n" lines));
               ])
           raw_comments)
    in
    `Assoc [("ast", ast); ("comments", comments)]
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    throw ~bt exn;
    exit 1
