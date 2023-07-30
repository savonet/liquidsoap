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

open Parsed_term

let mk = Term_base.make

let mk_fun ?pos arguments body =
  Term.make ?pos (`Fun Term.{ free_vars = None; arguments; body })

let get_reducer ?pos ~to_term = function
  | `Get tm ->
      Printf.eprintf
        "Warning, %s: the notation !x for references is deprecated, please use \
         x() instead.\n\
         %!"
        (match pos with
          | None -> "at unknown position"
          | Some pos -> Pos.to_string pos);
      `App (to_term tm, [])

let set_reducer ?pos ~to_term = function
  | `Set (tm, v) ->
      let op =
        mk ?pos
          (`Invoke
            { Term_base.invoked = to_term tm; default = None; meth = "set" })
      in
      `App (op, [("", to_term v)])

let if_reducer ?pos ~to_term = function
  | `Inline_if { if_condition; if_then; if_else }
  | `If { if_condition; if_then; if_else } ->
      let op = mk ?pos (`Var "if") in
      let if_condition = to_term if_condition in
      let if_then = mk_fun ?pos [] (to_term if_then) in
      let if_else = mk_fun ?pos [] (to_term if_else) in
      `App (op, [("", if_condition); ("then", if_then); ("else", if_else)])

let while_reducer ?pos ~to_term = function
  | `While { while_condition; while_loop } ->
      let op = mk ?pos (`Var "while") in
      let while_condition = mk_fun ?pos [] (to_term while_condition) in
      let while_loop = mk_fun ?pos [] (to_term while_loop) in
      `App (op, [("", while_condition); ("", while_loop)])

let base_for_reducer ?pos for_variable for_iterator for_loop =
  let for_op = mk ?pos (`Var "for") in
  let for_loop =
    mk_fun ?pos
      [
        {
          label = "";
          as_variable = Some for_variable;
          typ = Type.var ();
          default = None;
        };
      ]
      for_loop
  in
  `App (for_op, [("", for_iterator); ("", for_loop)])

let iterable_for_reducer ?pos ~to_term = function
  | `Iterable_for
      { iterable_for_variable; iterable_for_iterator; iterable_for_loop } ->
      base_for_reducer ?pos iterable_for_variable
        (to_term iterable_for_iterator)
        (to_term iterable_for_loop)

let for_reducer ?pos ~to_term = function
  | `For { for_variable; for_from; for_to; for_loop } ->
      let to_op = mk ?pos (`Var "iterator") in
      let to_op =
        mk ?pos
          (`Invoke { Term_base.invoked = to_op; default = None; meth = "int" })
      in
      let for_condition =
        mk ?pos (`App (to_op, [("", to_term for_from); ("", to_term for_to)]))
      in
      base_for_reducer ?pos for_variable for_condition (to_term for_loop)

let infix_reducer ?pos ~to_term = function
  | `Infix (tm, op, tm') ->
      let op = mk ?pos (`Var op) in
      `App (op, [("", to_term tm); ("", to_term tm')])

let bool_reducer ?pos ~to_term = function
  | `Bool (tm, op, tm') ->
      let op = mk ?pos (`Var op) in
      let tm = mk_fun ?pos [] (to_term tm) in
      let tm' = mk_fun ?pos [] (to_term tm') in
      `App (op, [("", tm); ("", tm')])

let simple_fun_reducer ?pos:_ ~to_term = function
  | `Simple_fun tm ->
      `Fun { Term_base.arguments = []; body = to_term tm; free_vars = None }

let negative_reducer ?pos ~to_term = function
  | `Negative tm ->
      let op = mk ?pos (`Var "~-") in
      `App (op, [("", to_term tm)])

let not_reducer ?pos ~to_term = function
  | `Not tm ->
      let op = mk ?pos (`Var "not") in
      `App (op, [("", to_term tm)])

let append_reducer ?pos ~to_term = function
  | `Append (tm, tm') ->
      let op = mk ?pos (`Var "_::_") in
      `App (op, [("", to_term tm); ("", to_term tm')])

let assoc_reducer ?pos ~to_term = function
  | `Assoc (tm, tm') ->
      let op = mk ?pos (`Var "_[_]") in
      `App (op, [("", to_term tm); ("", to_term tm')])

let regexp_reducer ?pos ~to_term:_ = function
  | `Regexp (regexp, flags) ->
      let regexp = mk ?pos (`Ground (Term_base.Ground.String regexp)) in
      let flags = List.map Char.escaped flags in
      let flags =
        List.map (fun s -> mk ?pos (`Ground (Term_base.Ground.String s))) flags
      in
      let flags = mk ?pos (`List flags) in
      let op = mk ?pos (`Var "regexp") in
      `App (op, [("", regexp); ("flags", flags)])

let try_reducer ?pos ~to_term = function
  | `Try { try_body; try_variable; try_errors_list; try_handler } ->
      let try_body = mk_fun ?pos [] (to_term try_body) in
      let err_arg =
        [
          Term_base.
            {
              label = "";
              as_variable = Some try_variable;
              typ = Type.var ();
              default = None;
            };
        ]
      in
      let handler =
        mk_fun ?pos:try_handler.t.pos err_arg (to_term try_handler)
      in
      let error_module = mk ?pos (`Var "error") in
      let try_errors_list = to_term try_errors_list in
      let op =
        mk ?pos
          (`Invoke
            { Term_base.invoked = error_module; default = None; meth = "catch" })
      in
      `App (op, [("errors", try_errors_list); ("", try_body); ("", handler)])

let fun_arg_reducer ?pos ~to_term arg =
  {
    arg with
    typ = Parser_helper.mk_ty ?pos arg.typ;
    default = Option.map to_term arg.default;
  }

let fun_reducer ?pos ~to_term = function
  | `Parsed_fun p ->
      `Fun
        {
          p with
          arguments = List.map (fun_arg_reducer ?pos ~to_term) p.arguments;
          body = to_term p.body;
        }
  | `Parsed_rfun (n, p) ->
      `RFun
        ( n,
          {
            p with
            arguments = List.map (fun_arg_reducer ?pos ~to_term) p.arguments;
            body = to_term p.body;
          } )

let rec to_ast ?pos : parsed_ast -> Term.runtime_ast = function
  | `Get _ as ast -> get_reducer ?pos ~to_term ast
  | `Set _ as ast -> set_reducer ?pos ~to_term ast
  | `Inline_if _ as ast -> if_reducer ?pos ~to_term ast
  | `If _ as ast -> if_reducer ?pos ~to_term ast
  | `While _ as ast -> while_reducer ?pos ~to_term ast
  | `For _ as ast -> for_reducer ?pos ~to_term ast
  | `Iterable_for _ as ast -> iterable_for_reducer ?pos ~to_term ast
  | `Not _ as ast -> not_reducer ?pos ~to_term ast
  | `Negative _ as ast -> negative_reducer ?pos ~to_term ast
  | `Append _ as ast -> append_reducer ?pos ~to_term ast
  | `Assoc _ as ast -> assoc_reducer ?pos ~to_term ast
  | `Infix _ as ast -> infix_reducer ?pos ~to_term ast
  | `Bool _ as ast -> bool_reducer ?pos ~to_term ast
  | `Simple_fun _ as ast -> simple_fun_reducer ?pos ~to_term ast
  | `Regexp _ as ast -> regexp_reducer ?pos ~to_term ast
  | `Try _ as ast -> try_reducer ?pos ~to_term ast
  | `Parsed_fun _ as ast -> fun_reducer ?pos ~to_term ast
  | `Parsed_rfun _ as ast -> fun_reducer ?pos ~to_term ast
  | `Parsed_cast (tm, t) -> `Cast (to_term tm, Parser_helper.mk_ty ?pos t)
  | `Ground g -> `Ground g
  | `Encoder e -> `Encoder (to_encoder e)
  | `List l -> `List (List.map to_term l)
  | `Tuple l -> `Tuple (List.map to_term l)
  | `Null -> `Null
  | `Cast (t, typ) -> `Cast (to_term t, typ)
  | `Invoke { Term_base.invoked; default; meth } ->
      `Invoke
        {
          Term_base.invoked = to_term invoked;
          default = Option.map to_term default;
          meth;
        }
  | `Open (t, t') -> `Open (to_term t, to_term t')
  | `Let _let ->
      `Let
        Term_base.{ _let with def = to_term _let.def; body = to_term _let.body }
  | `Var s -> `Var s
  | `Seq (t, t') -> `Seq (to_term t, to_term t')
  | `App (t, l) -> `App (to_term t, List.map (fun (v, t) -> (v, to_term t)) l)
  | `Fun p -> `Fun (to_func p)
  | `RFun (lbl, p) -> `RFun (lbl, to_func p)

and to_func { arguments; body } =
  {
    Term_base.arguments =
      List.map
        (fun arg ->
          Term_base.{ arg with default = Option.map to_term arg.default })
        arguments;
    body = to_term body;
    free_vars = None;
  }

and to_encoder_params l =
  List.map
    (function
      | lbl, `Term t -> (lbl, `Term (to_term t))
      | lbl, `Encoder e -> (lbl, `Encoder (to_encoder e)))
    l

and to_encoder (lbl, params) = (lbl, to_encoder_params params)

and to_term (tm : t) : Term.t =
  {
    tm with
    methods = Methods.map to_term tm.methods;
    term = to_ast ?pos:tm.t.pos tm.term;
  }
