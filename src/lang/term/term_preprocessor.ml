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

open Parsed_term

type processor =
  ( Parser.token * Lexing.position * Lexing.position,
    Parsed_term.t )
  MenhirLib.Convert.revised

let program = MenhirLib.Convert.Simplified.traditional2revised Parser.program

let let_script_path ~filename tm =
  Parser_helper.mk ~pos:tm.Parsed_term.pos
    (`Let
       ( {
           Parsed_term.decoration = `None;
           pat =
             {
               pat_pos = tm.pos;
               pat_entry = `PVar ["liquidsoap"; "script"; "path"];
             };
           arglist = None;
           cast = None;
           def = Parser_helper.mk ~pos:tm.pos filename;
         },
         tm ))

let mk_expr ?fname processor lexbuf =
  let tokenizer = Preprocessor.mk_tokenizer ?fname lexbuf in
  Parser_helper.clear_comments ();
  let parsed_term = processor tokenizer in
  Parser_helper.attach_comments parsed_term;
  match fname with
    (* This happens with the interactive top-level. *)
    | None when processor != program -> parsed_term
    | None -> let_script_path ~filename:`Null parsed_term
    | Some fname ->
        let_script_path
          ~filename:(`String ('"', Lang_string.escape_utf8_string fname))
          parsed_term

exception No_extra

let rec concat_term (t : t) (t' : t) =
  match t with
    | { term = `Let (def, body) } ->
        { t with term = `Let (def, concat_term body t') }
    | { term = `Def (def, body) } ->
        { t with term = `Def (def, concat_term body t') }
    | { term = `Binding (def, body) } ->
        { t with term = `Binding (def, concat_term body t') }
    | { term = `Seq (t1, t2) } as tm ->
        { tm with term = `Seq (t1, concat_term t2 t') }
    | { term = `Eof } | { term = `Tuple [] } -> t'
    | _ -> Parsed_term.make ~pos:t.Parsed_term.pos (`Seq (t, t'))

let includer_reducer ~pos = function
  | `Include { inc_type; inc_name; inc_pos } -> (
      try
        let fname =
          match inc_type with
            | `Lib -> Filename.concat (!Hooks.liq_libs_dir ()) inc_name
            | v -> (
                try
                  let current_dir =
                    Filename.dirname (fst inc_pos).Lexing.pos_fname
                  in
                  Utils.check_readable ~current_dir
                    ~pos:[Pos.of_lexing_pos inc_pos]
                    inc_name
                with _ when v = `Extra -> raise No_extra)
        in
        let fname =
          match fname with "-" -> fname | _ -> FilePath.reduce fname
        in
        let ic = if fname = "-" then stdin else open_in fname in
        Fun.protect
          ~finally:(fun () -> if fname <> "-" then close_in ic)
          (fun () ->
            let lexbuf = Sedlexing.Utf8.from_channel ic in
            mk_expr ~fname program lexbuf)
      with No_extra -> Parsed_term.make ~pos (`Tuple []))

let rec expand_encoder (lbl, params) =
  ( lbl,
    List.map
      (function
        | `Encoder enc -> `Encoder (expand_encoder enc)
        | `Labelled (lbl, t) -> `Labelled (lbl, expand_term t)
        | `Anonymous _ as v -> v)
      params )

and expand_term tm =
  let expand_decoration = function
    | `None -> `None
    | `Recursive -> `Recursive
    | `Replaces -> `Replaces
    | `Eval -> `Eval
    | `Sqlite_query -> `Sqlite_query
    | `Sqlite_row -> `Sqlite_row
    | `Yaml_parse -> `Yaml_parse
    | `Xml_parse -> `Xml_parse
    | `Json_parse l ->
        `Json_parse (List.map (fun (lbl, t) -> (lbl, expand_term t)) l)
  in
  let expand_func_arg ({ default } as arg) =
    { arg with default = Option.map expand_term default }
  in
  let expand_arglist =
    List.map (function
      | `Term arg -> `Term (expand_func_arg arg)
      | `Argsof _ as el -> el)
  in
  let expand_app_arg =
    List.map (function
      | `Argsof _ as el -> el
      | `Term (lbl, t) -> `Term (lbl, expand_term t))
  in
  let expand_fun_args =
    List.map (function
      | `Term arg -> `Term (expand_func_arg arg)
      | `Argsof _ as v -> v)
  in
  let comments = ref tm.Parsed_term.comments in
  let term =
    match tm.Parsed_term.term with
      | `Include _ as ast ->
          let { term; comments = expanded_comments } =
            expand_term (includer_reducer ~pos:tm.Parsed_term.pos ast)
          in
          comments := expanded_comments;
          term
      | `Seq ({ pos; term = `Include _ as ast }, t') ->
          let t = expand_term (includer_reducer ~pos ast) in
          let t' = expand_term t' in
          let { term; comments = expanded_comments } = concat_term t t' in
          comments := expanded_comments;
          term
      | `Seq (t, t') -> `Seq (expand_term t, expand_term t')
      | `If_def ({ if_def_then; if_def_else } as if_def) ->
          `If_def
            {
              if_def with
              if_def_then = expand_term if_def_then;
              if_def_else = Option.map expand_term if_def_else;
            }
      | `If_version ({ if_version_then; if_version_else } as if_version) ->
          `If_version
            {
              if_version with
              if_version_then = expand_term if_version_then;
              if_version_else = Option.map expand_term if_version_else;
            }
      | `If_encoder ({ if_encoder_then; if_encoder_else } as if_encoder) ->
          `If_encoder
            {
              if_encoder with
              if_encoder_then = expand_term if_encoder_then;
              if_encoder_else = Option.map expand_term if_encoder_else;
            }
      | `If { if_condition; if_then; if_elsif; if_else } ->
          `If
            {
              if_condition = expand_term if_condition;
              if_then = expand_term if_then;
              if_elsif =
                List.map
                  (fun (t, t') -> (expand_term t, expand_term t'))
                  if_elsif;
              if_else = Option.map expand_term if_else;
            }
      | `Inline_if { if_condition; if_then; if_elsif; if_else } ->
          `If
            {
              if_condition = expand_term if_condition;
              if_then = expand_term if_then;
              if_elsif =
                List.map
                  (fun (t, t') -> (expand_term t, expand_term t'))
                  if_elsif;
              if_else = Option.map expand_term if_else;
            }
      | `While { while_condition; while_loop } ->
          `While
            {
              while_condition = expand_term while_condition;
              while_loop = expand_term while_loop;
            }
      | `For ({ for_from; for_to; for_loop } as _for) ->
          `For
            {
              _for with
              for_from = expand_term for_from;
              for_to = expand_term for_to;
              for_loop = expand_term for_loop;
            }
      | `Iterable_for ({ iterable_for_iterator; iterable_for_loop } as _for) ->
          `Iterable_for
            {
              _for with
              iterable_for_iterator = expand_term iterable_for_iterator;
              iterable_for_loop = expand_term iterable_for_loop;
            }
      | `List l ->
          `List
            (List.map
               (function
                 | `Term t -> `Term (expand_term t)
                 | `Ellipsis t -> `Ellipsis (expand_term t))
               l)
      | `Try ({ try_body; try_errors_list; try_handler; try_finally } as _try)
        ->
          `Try
            {
              _try with
              try_body = expand_term try_body;
              try_errors_list = Option.map expand_term try_errors_list;
              try_handler = Option.map expand_term try_handler;
              try_finally = Option.map expand_term try_finally;
            }
      | `Regexp _ as ast -> ast
      | `Time_interval _ as ast -> ast
      | `Time _ as ast -> ast
      | `Def (({ decoration; arglist; def } as _let), body) ->
          `Def
            ( {
                _let with
                decoration = expand_decoration decoration;
                def = expand_term def;
                arglist = Option.map expand_arglist arglist;
              },
              expand_term body )
      | `Let (({ decoration; arglist; def } as _let), body) ->
          `Let
            ( {
                _let with
                decoration = expand_decoration decoration;
                def = expand_term def;
                arglist = Option.map expand_arglist arglist;
              },
              expand_term body )
      | `Binding (({ decoration; arglist; def } as _let), body) ->
          `Binding
            ( {
                _let with
                decoration = expand_decoration decoration;
                def = expand_term def;
                arglist = Option.map expand_arglist arglist;
              },
              expand_term body )
      | `Cast { cast = t; typ } -> `Cast { cast = expand_term t; typ }
      | `App (t, app_arg) -> `App (expand_term t, expand_app_arg app_arg)
      | `Invoke ({ invoked; meth } as invoke) ->
          `Invoke
            {
              invoke with
              invoked = expand_term invoked;
              meth =
                (match meth with
                  | `String _ as s -> s
                  | `App (n, app_arg) -> `App (n, expand_app_arg app_arg));
            }
      | `Fun (fun_args, t) -> `Fun (expand_fun_args fun_args, expand_term t)
      | `RFun (name, fun_args, t) ->
          `RFun (name, expand_fun_args fun_args, expand_term t)
      | `Not t -> `Not (expand_term t)
      | `Get t -> `Get (expand_term t)
      | `Set (t, t') -> `Set (expand_term t, expand_term t')
      | `Methods (t, methods) ->
          `Methods
            ( Option.map expand_term t,
              List.map
                (function
                  | `Ellipsis t -> `Ellipsis (expand_term t)
                  | `Method (name, t) -> `Method (name, expand_term t))
                methods )
      | `Negative t -> `Negative (expand_term t)
      | `Append (t, t') -> `Append (expand_term t, expand_term t')
      | `Assoc (t, t') -> `Assoc (expand_term t, expand_term t')
      | `Infix (t, op, t') -> `Infix (expand_term t, op, expand_term t')
      | `BoolOp (b, l) -> `BoolOp (b, List.map expand_term l)
      | `Coalesce (t, t') -> `Coalesce (expand_term t, expand_term t')
      | `At (t, t') -> `At (expand_term t, expand_term t')
      | `Simple_fun t -> `Simple_fun (expand_term t)
      | `String_interpolation (c, l) ->
          `String_interpolation
            ( c,
              List.map
                (function
                  | `String _ as s -> s | `Term t -> `Term (expand_term t))
                l )
      | `Int _ as ast -> ast
      | `Float _ as ast -> ast
      | `String _ as ast -> ast
      | `Bool _ as ast -> ast
      | `Eof -> `Eof
      | `Block t -> `Block (expand_term t)
      | `Parenthesis t -> `Parenthesis (expand_term t)
      | `Encoder enc -> `Encoder (expand_encoder enc)
      | `Custom _ as ast -> ast
      | `Open (t, t') -> `Open (expand_term t, expand_term t')
      | `Tuple l -> `Tuple (List.map expand_term l)
      | `Var _ as ast -> ast
      | `Null -> `Null
  in
  { tm with Parsed_term.term; comments = !comments }
