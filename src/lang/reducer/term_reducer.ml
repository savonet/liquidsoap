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

(** Reduce a parsed term to a runtime term: desugaring, macro expansion and
    type-annotation resolution. The reducers for each construct live in the
    [term_reducer_*] modules; this one dispatches to them. *)

type processor = Term_preprocessor.processor

open Parsed_term
include Runtime_term
open Term_reducer_helpers
open Term_reducer_ty
open Term_reducer_pattern
open Term_reducer_argsof
open Term_reducer_sugar
open Term_reducer_let

let program = Term_preprocessor.program

let mk_expr ?fname processor lexbuf =
  let parsed_term = Term_preprocessor.mk_expr ?fname processor lexbuf in
  Term_preprocessor.expand_term parsed_term

(* Select the branch of a static conditional. Returns the statements of the
   selected block, which the caller splices into the enclosing block. *)
let static_branch ~env { static_cond; static_then; static_else } =
  let taken =
    match static_cond with
      | `Defined (negative, name) ->
          let defined =
            List.mem_assoc name env || Environment.has_builtin name
          in
          defined <> negative
      | `Version (op, version) -> (
          let current = Lang_string.Version.of_string Build_config.version in
          match (op, Lang_string.Version.compare current version) with
            | `Eq, 0 -> true
            | `Geq, v -> v >= 0
            | `Leq, v -> v <= 0
            | `Gt, v -> v > 0
            | `Lt, v -> v < 0
            | _ -> false)
      | `Encoder (negative, name) -> (
          try
            let encoder = !Hooks.make_encoder ~pos:None (name, []) in
            !Hooks.has_encoder encoder <> negative
            (* An unknown encoder name is treated as "not available", i.e. the
               else branch, for both %ifencoder and %ifnencoder. *)
          with _ -> false)
  in
  if taken then static_then.block_body
  else (match static_else with Some b -> b.block_body | None -> [])

let to_encoder_string = function
  | `Verbatim s -> s
  | `String (pos, (sep, s)) -> render_string ~pos ~sep s

let rec to_encoder_params ~env ~to_term l =
  List.map
    (function
      | `Anonymous s -> `Anonymous (to_encoder_string s)
      | `Labelled (s, t) -> `Labelled (to_encoder_string s, to_term ~env t)
      | `Encoder e -> `Encoder (to_encoder ~to_term ~env e))
    l

and to_encoder ~env ~to_term (lbl, params) =
  (lbl, to_encoder_params ~env ~to_term params)

let rec to_ast ~throw ~env ~pos ~comments ast =
  let to_ast = to_ast ~throw in
  let to_term = to_term ~throw in
  let to_block = to_block ~throw in
  match ast with
    | `Methods _ | `Block _ | `Parenthesis _ -> assert false
    | `Static_if p ->
        (* Value position: the selected branch is a block used as a value. *)
        (to_block ~env { block_body = static_branch ~env p; block_pos = pos })
          .term
    | `Get _ as ast -> get_reducer ~pos ~env ~to_term ast
    | `Set _ as ast -> set_reducer ~pos ~env ~to_term ast
    | `Inline_if _ as ast -> if_reducer ~pos ~env ~to_term ~to_block ast
    | `If _ as ast -> if_reducer ~pos ~env ~to_term ~to_block ast
    | `While _ as ast -> while_reducer ~pos ~env ~to_term ~to_block ast
    | `For _ as ast -> for_reducer ~pos ~env ~to_term ~to_block ast
    | `Iterable_for _ as ast ->
        iterable_for_reducer ~pos ~env ~to_term ~to_block ast
    | `Not _ as ast -> not_reducer ~pos ~env ~to_term ast
    | `Negative _ as ast -> negative_reducer ~pos ~env ~to_term ast
    | `Append _ as ast -> append_reducer ~pos ~env ~to_term ast
    | `Assoc _ as ast -> assoc_reducer ~pos ~env ~to_term ast
    | `Infix _ as ast -> infix_reducer ~pos ~env ~to_term ast
    | `Bool _ as ast -> ast
    | `BoolOp _ as ast -> bool_op_reducer ~pos ~env ~to_term ast
    | `Simple_fun _ as ast -> simple_fun_reducer ~pos ~env ~to_block ast
    | `Regexp _ as ast -> regexp_reducer ~pos ~env ~to_term ast
    | `Try _ as ast -> try_reducer ~pos ~env ~to_term ~to_block ast
    | `String_interpolation (sep, l) ->
        let l =
          List.map
            (function
              | `String s -> `Term (mk_parsed ~pos (`String (sep, s)))
              | `Term tm ->
                  `Term
                    (mk_parsed ~pos
                       (`App (mk_parsed ~pos (`Var "string"), [`Term ("", tm)]))))
            l
        in
        let op =
          mk_parsed ~pos
            (`Invoke
               {
                 invoked = mk_parsed ~pos (`Var "string");
                 meth = `String "concat";
                 optional = false;
               })
        in
        to_ast ~env ~pos ~comments
          (`App (op, [`Term ("", mk_parsed ~pos (`List l))]))
    | `Coalesce (t, default) -> mk_coalesce ~pos ~env ~to_term ~default t
    | `At (t, t') -> `App (to_term ~env t', [("", to_term ~env t)])
    | `Time t -> mk_time_pred ~pos (during ~pos t)
    | `Time_interval (t, t') -> mk_time_pred ~pos (between ~pos t t')
    | `Custom _ as ast -> ast
    | `Encoder e -> `Encoder (to_encoder ~to_term ~env e)
    | `List l -> list_reducer ~pos ~env ~to_term (List.rev l)
    | `Tuple l -> `Tuple (List.map (to_term ~env) l)
    | `Raw_string (_, s) -> `String s
    | `String (sep, s) -> `String (render_string ~pos ~sep s)
    | `Int i -> `Int (int_of_string i)
    | `Float f -> (
        try `Float (Scanf.sscanf f "%f" (fun v -> v))
        with _ ->
          parse_error ~pos (Printf.sprintf "Invalid float value: %s" f))
    | `Null -> `Null
    | `Cast { cast = t; typ } ->
        `Cast
          { cast = to_term ~env t; typ = mk_parsed_ty ~pos ~env ~to_term typ }
    | `Invoke { invoked; optional; meth } ->
        let default = if optional then Some (mk_parsed ~pos `Null) else None in
        mk_invoke ~pos ~env ?default ~to_term invoked meth
    | `Open (t, t') -> `Open (to_term ~env t, to_term ~env t')
    | `Var s -> `Var s
    | `Seq (t, t') -> `Seq (to_term ~env t, to_term ~env t')
    | `App (t, args) ->
        (match (t, args) with
          | { term = `Var "_null"; pos }, [] ->
              let bt = Printexc.get_callstack 0 in
              throw ~bt (Term.Deprecated ("use `null`", Pos.of_lexing_pos pos))
          | _ -> ());
        let args = expand_appof ~pos ~env ~to_term args in
        `App (to_term ~env t, args)
    | `Fun (args, body) -> `Fun (to_func ~throw ~pos ~env ~to_term args body)
    | `RFun (name, args, body) ->
        `Fun (to_func ~throw ~pos ~env ~to_term ~name args body)

and to_func ~pos ~env ~to_term ~throw ?name arguments body =
  let mk_def, arguments = expand_argsof ~throw ~pos ~env ~to_term arguments in
  { name; arguments; body = mk_def (to_term ~env body); free_vars = None }

(* The scoping rule, in one place: a binding scopes over the statements that
   follow it in its own block, and a static conditional splices the statements
   of the branch it selects in front of them. *)
and to_block ~throw ~env ({ block_body; block_pos } : Parsed_term.block) :
    Term.t =
  let rec fold ~env = function
    | [] -> mk ~pos:block_pos (`Tuple [])
    (* A static conditional in statement position splices the statements of the
       branch it selects, so a binding made under `%ifdef` scopes over the rest
       of the block. In expression position it is a value: see `to_ast`. *)
    | { stmt = `Expr { term = `Static_if p; _ }; _ } :: rest ->
        fold ~env (static_branch ~env p @ rest)
    | [{ stmt = `Expr tm; _ }] -> to_term ~throw ~env tm
    | { stmt; stmt_pos = pos; stmt_comments = comments } :: rest -> (
        match stmt with
          | `Expr tm -> mk ~pos (`Seq (to_term ~throw ~env tm, fold ~env rest))
          | `Open tm -> mk ~pos (`Open (to_term ~throw ~env tm, fold ~env rest))
          | `Binding l ->
              mk ~pos
                (mk_let ~throw ~pos ~env ~to_term:(to_term ~throw) ~comments
                   ~body:(fun env -> fold ~env rest)
                   l)
          (* Removed by Term_preprocessor.expand_term. *)
          | `Include _ -> assert false)
  in
  fold ~env block_body

and to_term ~throw ~env (tm : Parsed_term.t) : Term.t =
  let to_term = to_term ~throw in
  report_annotations ~throw ~pos:tm.pos tm.annotations;
  match tm.term with
    | `Block b -> to_block ~throw ~env b
    | `Parenthesis tm -> to_term ~env tm
    | `Methods (base, methods) ->
        (* let _ = src in
           let replaces _ = dst in
           _ *)
        let replace_methods ~src dst =
          mk ~pos:tm.pos
            (`Let
               {
                 doc = None;
                 replace = false;
                 pat = `PVar ["_"];
                 gen = [];
                 def = src;
                 body =
                   mk ~pos:tm.pos
                     (`Let
                        {
                          doc = None;
                          replace = true;
                          pat = `PVar ["_"];
                          gen = [];
                          def = dst;
                          body = mk ~pos:tm.pos (`Var "_");
                        });
               })
        in
        let term =
          match base with
            | None -> mk ~pos:tm.pos (`Tuple [])
            | Some tm -> to_term ~env tm
        in
        List.fold_left
          (fun term -> function
            | `Ellipsis src -> replace_methods ~src:(to_term ~env src) term
            | `Method (name, tm) ->
                {
                  term with
                  methods = Methods.add name (to_term ~env tm) term.methods;
                })
          term methods
    | term ->
        let flags =
          match term with
            | `Int i
              when String.length i >= 2
                   && String.(lowercase_ascii (sub i 0 2)) = "0x" ->
                Flags.(add empty hex_int)
            | `Int i
              when String.length i >= 2
                   && String.(lowercase_ascii (sub i 0 2)) = "0o" ->
                Flags.(add empty octal_int)
            | _ -> Flags.empty
        in
        let term = to_ast ~throw ~env ~pos:tm.pos ~comments:tm.comments term in
        { t = mk_var ~pos:tm.pos (); term; methods = Methods.empty; flags }

let to_encoder_params ~throw =
  let to_term = to_term ~throw in
  to_encoder_params ~env:[] ~to_term

let to_term ~throw tm = to_term ~throw ~env:[] tm

(* `let eval` desugars to a call to `_0_eval`, which parses a string at run time
   and so needs the standard library to still be around. Asking the term rather
   than remembering that we reduced one keeps this a property of the script. *)
let needs_toplevel term = Vars.mem "_0_eval" (Term.free_vars term)
