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

(** Helper functions for the parser. *)

open Parsed_term
module Vars = Parsed_term.Vars

type arglist = Parsed_term.fun_arg list
type pos = Parsed_term.pos

type lexer_let_decoration =
  [ `None
  | `Recursive
  | `Replaces
  | `Eval
  | `Json_parse
  | `Yaml_parse
  | `Xml_parse
  | `Sqlite_row
  | `Sqlite_query ]

let render_string ~pos (sep, s) = String_literal.render ~pos ~sep s
let pending_comments = ref []
let clear_comments () = pending_comments := []
let get_pending_comments () = !pending_comments

let append_comment ~pos c =
  let comments = List.map String.trim (String.split_on_char '\n' c) in
  pending_comments := (pos, comments) :: !pending_comments

let comment_distance term_pos comment_pos =
  if (fst comment_pos).Lexing.pos_lnum = (snd term_pos).Lexing.pos_lnum then
    (`Before, 0)
  else (
    let before_distance =
      (fst term_pos).Lexing.pos_lnum - (snd comment_pos).Lexing.pos_lnum
    in
    let after_distance =
      (fst comment_pos).Lexing.pos_lnum - (snd term_pos).Lexing.pos_lnum
    in
    if
      0 <= after_distance
      && (before_distance < 0 || after_distance < before_distance)
    then (`After, after_distance)
    else (`Before, before_distance))

let sort_comments comments =
  List.sort
    (fun (p, _) (p', _) ->
      Stdlib.compare (fst p).Lexing.pos_cnum (fst p').Lexing.pos_cnum)
    comments

let attach_comments term =
  List.iter
    (fun (comment_pos, c) ->
      let attach = ref (fun update -> term.comments <- update term.comments) in
      let distance = ref (comment_distance term.pos comment_pos) in
      let kind_of_closest = ref `Term in
      Parsed_term.iter_anchors
        (fun pos kind set ->
          match (comment_distance pos comment_pos, !distance) with
            | (t, d), (t', d')
              when 0 <= d
                   && (d' < 0
                      ||
                      if t = `Before && t' = `After then d <= d'
                      else if d = d' then
                        (* Nodes are visited outermost first, so an enclosing
                           block spans the same lines as the statement it opens
                           with. A doc comment belongs to the statement. *)
                        kind = `Statement && !kind_of_closest <> `Statement
                      else d < d') ->
                distance := (t, d);
                kind_of_closest := kind;
                attach := set
            | _ -> ())
        term;
      let comment =
        match !distance with `Before, _ -> `Before c | `After, _ -> `After c
      in
      !attach (fun comments ->
          sort_comments ((comment_pos, comment) :: comments)))
    !pending_comments;
  pending_comments := []

let let_args ~kind ~decoration ~pat ?arglist ~def ?cast () =
  { kind; decoration; pat; arglist; def; cast }

let mk = Parsed_term.make
let mk_stmt ~pos stmt = { stmt; stmt_pos = pos; stmt_comments = [] }
let mk_block ~pos block_body = { block_body; block_pos = pos }

(* A block used where an expression is expected. The singleton case is by far
   the most common (`if a then`, `def f() = e end`) and would otherwise wrap
   every one of them in a redundant `Block` node. *)
let expr_of_block ~pos b =
  match b.block_body with
    | [{ stmt = `Expr tm; _ }] -> tm
    | _ -> mk ~pos (`Block b)

let block_expr ~pos stmts = expr_of_block ~pos (mk_block ~pos stmts)

(* A bare binding's left-hand side is parsed as an expression and converted
   here. `(a, b)` is both a tuple expression and a tuple pattern, and LR(1)
   cannot tell which until it reaches the `=`, so the grammar commits to the
   expression and this decides afterwards. Invalid targets are reported here,
   with a message naming what is allowed. *)
let rec pattern_of_expr (tm : Parsed_term.t) : Parsed_term.pattern =
  let pat_pos = tm.pos in
  let entry =
    match tm.term with
      | `Var v -> `PVar [v]
      (* `x.y.z = ...`: an invoke chain is a dotted path. *)
      | `Invoke _ -> `PVar (path_of_invoke tm)
      | `Tuple l -> `PTuple (List.map pattern_of_expr l)
      | `List l ->
          `PList
            ( List.map
                (function
                  | `Term tm -> pattern_of_expr tm
                  | `Ellipsis tm ->
                      raise
                        (Term.Parse_error
                           ( tm.pos,
                             "Invalid binding: use `...x` for the spread \
                              element." )))
                l,
              None,
              [] )
      | `Parenthesis tm | `Block { block_body = [{ stmt = `Expr tm; _ }]; _ } ->
          (pattern_of_expr tm).pat_entry
      | `Methods (base, methods) ->
          `PMeth
            ( Option.map pattern_of_expr base,
              List.map
                (function
                  | `Method (name, tm) -> (name, `Pattern (pattern_of_expr tm))
                  | `Ellipsis tm ->
                      raise
                        (Term.Parse_error (tm.pos, "Invalid binding target.")))
                methods )
      | _ ->
          raise
            (Term.Parse_error
               ( tm.pos,
                 "Invalid binding: the left-hand side of `=` must be a \
                  variable, a field path or a destructuring pattern." ))
  in
  { pat_pos; pat_entry = entry }

(* `(n : int) = 3`: the annotation parses as a cast expression around the
   target, and belongs on the binding rather than in the pattern. *)
and binding_target (tm : Parsed_term.t) =
  match tm.term with
    | `Cast { cast; typ } -> (pattern_of_expr cast, Some typ)
    | _ -> (pattern_of_expr tm, None)

and path_of_invoke (tm : Parsed_term.t) : string list =
  match tm.term with
    | `Var v -> [v]
    | `Invoke { invoked; meth = `String m; optional = false } ->
        path_of_invoke invoked @ [m]
    | _ ->
        raise
          (Term.Parse_error (tm.pos, "Invalid binding: not a valid field path."))

let mk_json_assoc_object_ty ~pos = function
  | `Tuple [`Named "string"; ty], "as", "json", "object" -> `Json_object ty
  | _ -> raise (Term.Parse_error (pos, "Invalid type constructor"))

let mk_source_ty ~pos name tracks =
  match name with
    | "source" -> `Source (name, tracks)
    | _ -> raise (Term.Parse_error (pos, "Invalid type constructor: " ^ name))

let mk_named_ty ~pos name ty =
  match (name, ty) with
    | "ref", Some t -> `Ref t
    | "ref", None ->
        raise (Term.Parse_error (pos, "ref type requires a type parameter"))
    | "getter", Some t -> `Getter t
    | "getter", None ->
        raise (Term.Parse_error (pos, "getter type requires a type parameter"))
    | "source", _ ->
        mk_source_ty ~pos name { Parsed_term.extensible = false; tracks = [] }
    | _, _ ->
        raise (Term.Parse_error (pos, "Invalid type constructor: " ^ name))

type let_opt_el = string * Parsed_term.t

let let_decoration_of_lexer_let_decoration = function
  | `Json_parse -> `Json_parse []
  | `Yaml_parse -> `Yaml_parse
  | `Xml_parse -> `Xml_parse
  | `Sqlite_query -> `Sqlite_query
  | `Sqlite_row -> `Sqlite_row
  | `Eval -> `Eval
  | `Recursive -> `Recursive
  | `None -> `None
  | `Replaces -> `Replaces

let args_of_json_parse ~pos = function
  | [] -> []
  | [("json5", v)] -> [("json5", v)]
  | (lbl, _) :: _ ->
      raise
        (Term.Parse_error
           (pos, "Invalid argument " ^ lbl ^ " for json.parse let constructor"))

let mk_fun ~pos arguments body = mk ~pos (`Fun (arguments, body))

let mk_try ?handler ?finally_block ~body_block ~pos () =
  mk ~pos
    (`Try
       {
         try_body_block = body_block;
         try_handler = handler;
         try_finally_block = finally_block;
       })

let mk_encoder ~pos e p = mk ~pos (`Encoder (e, p))
