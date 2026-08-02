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

(** Reduce syntactic sugar: time predicates, optional invokes and coalescing,
    control flow, operators, list and regexp literals, [try]. *)

open Parsed_term
include Runtime_term
open Term_reducer_helpers
open Term_reducer_argsof

let time_units = [| 7 * 24 * 60 * 60; 24 * 60 * 60; 60 * 60; 60; 1 |]

(** Given a date specified as a list of four values (whms), return a date in
    seconds from the beginning of the week. *)
let date ~pos =
  let to_int = function None -> 0 | Some i -> i in
  let rec aux = function
    | None :: tl -> aux tl
    | [] -> parse_error ~pos "Invalid time."
    | l ->
        let a = Array.of_list l in
        let n = Array.length a in
        let tu = time_units and tn = Array.length time_units in
        Array.fold_left ( + ) 0
          (Array.mapi
             (fun i s ->
               let s = if n = 4 && i = 0 then to_int s mod 7 else to_int s in
               tu.(tn - 1 + i - n + 1) * s)
             a)
  in
  aux

(** Give the index of the first non-None value in the list. *)
let last_index l =
  let rec last_index n = function
    | x :: tl -> if x = None then last_index (n + 1) tl else n
    | [] -> n
  in
  last_index 0 l

(** Give the precision of a date-as-list. For example, the precision of Xs is 1,
    XmYs is 60, XhYmZs 3600, etc. *)
let precision d = time_units.(last_index d)

(** Give the duration of a data-as-list. For example, the duration of Xs is 1,
    Xm 60, XhYm 60, etc. *)
let duration d =
  time_units.(Array.length time_units - 1 - last_index (List.rev d))

let between ~pos d1 d2 =
  let d1 = [d1.week; d1.hours; d1.minutes; d1.seconds] in
  let d2 = [d2.week; d2.hours; d2.minutes; d2.seconds] in
  let p1 = precision d1 in
  let p2 = precision d2 in
  let t1 = date ~pos d1 in
  let t2 = date ~pos d2 in
  if p1 <> p2 then parse_error ~pos "Invalid time interval: precisions differ.";
  (t1, t2, p1)

let during ~pos d =
  let d = [d.week; d.hours; d.minutes; d.seconds] in
  let t, d, p = (date ~pos d, duration d, precision d) in
  (t, t + d, p)

let mk_time_pred ~pos (a, b, c) =
  let args = List.map (fun x -> ("", mk ~pos (`Int x))) [a; b; c] in
  `App (mk ~pos (`Var "time_in_mod"), args)

(** When doing chained calls, we want to update all nested defaults so that, e.g.
    in:
      `x?.foo.gni.bla(123)?.gno.gni`,
    the default for `x.foo` becomes:
      `any.{gni = any.{ bla = fun (_) -> any.{ gno = any.{ gni = null }}}}`
    we also need to keep track of which methods are optional in the default value's type
    to make sure it doesn't force optional methods to be mandatory during type checking. *)
let mk_app_invoke_default ~pos ~args body =
  let app_args =
    List.map
      (fun (label, _) ->
        {
          Term.label;
          as_variable = None;
          typ = mk_var ();
          default = None;
          pos = None;
        })
      args
  in
  mk_fun ~pos app_args body

let mk_any ~pos () =
  let op = mk ~pos (`Var "💣") in
  mk ~pos (`App (op, []))

let rec mk_invoke_default ~pos ~optional ~name value
    { invoked; meth; invoke_default } =
  let t =
    Type.meth ~pos:(Pos.of_lexing_pos pos) ~optional name
      ([], mk_var ~pos ())
      (mk_var ~pos ())
  in
  let tm = mk_any ~pos () in
  let value =
    mk ~t ~methods:(Methods.add name value Term.Methods.empty) tm.Term.term
  in
  ( value,
    update_invoke_default ~pos ~optional:(invoke_default <> None) invoked meth
      value )

and update_invoke_default ~pos ~optional expr name value =
  match expr.term with
    | `Invoke ({ meth; invoke_default } as invoked) ->
        let value, invoked =
          mk_invoke_default ~pos ~name ~optional value invoked
        in
        mk ~t:expr.Term.t ~methods:expr.Term.methods
          (`Invoke
             {
               invoked;
               meth;
               invoke_default = Option.map (fun _ -> value) invoke_default;
             })
    | `App ({ term = `Invoke ({ meth; invoke_default } as invoked) }, args) ->
        let value, invoked =
          let invoke_default =
            match invoke_default with
              | Some { term = `Fun { Term.body } } -> Some body
              | None -> Some (mk_any ~pos ())
              | _ -> assert false
          in
          mk_invoke_default ~pos ~name ~optional value
            { invoked with invoke_default }
        in
        mk ~t:expr.Term.t ~methods:expr.Term.methods
          (`App
             ( mk ~pos
                 (`Invoke
                    {
                      invoked;
                      meth;
                      invoke_default =
                        Option.map
                          (fun _ -> mk_app_invoke_default ~pos ~args value)
                          invoke_default;
                    }),
               args ))
    | _ -> expr

let mk_invoke ?(default : Parsed_term.t option) ~pos ~env ~to_term expr v =
  let expr = to_term ~env expr in
  let default = Option.map (to_term ~env) default in
  let optional, value =
    match default with Some v -> (true, v) | None -> (false, mk ~pos `Null)
  in
  match v with
    | `String meth ->
        let expr = update_invoke_default ~pos ~optional expr meth value in
        `Invoke
          {
            invoked = expr;
            invoke_default = Option.map (fun _ -> value) default;
            meth;
          }
    | `App (meth, args) ->
        let args = expand_appof ~pos ~env ~to_term args in
        let value = mk_app_invoke_default ~pos ~args value in
        let expr = update_invoke_default ~pos ~optional expr meth value in
        `App
          ( mk ~pos
              (`Invoke
                 {
                   invoked = expr;
                   invoke_default = Option.map (fun _ -> value) default;
                   meth;
                 }),
            args )

let mk_coalesce ~pos ~(default : Parsed_term.t) ~env ~to_term
    (computed : Parsed_term.t) =
  match computed.term with
    | `Invoke { invoked; meth = `String m } ->
        mk_invoke ~pos ~env ~default ~to_term invoked (`String m)
    | _ ->
        let null = mk ~pos (`Var "_null") in
        let op =
          mk ~pos
            (`Invoke { invoked = null; invoke_default = None; meth = "default" })
        in
        let handler = mk_fun ~pos [] (to_term ~env default) in
        `App (op, [("", to_term ~env computed); ("", handler)])

let get_reducer ~pos ~env ~to_term = function
  | `Get tm ->
      Printf.eprintf
        "Warning, %s: the notation !x for references is deprecated, please use \
         x() instead.\n\
         %!"
        Pos.(to_string (of_lexing_pos pos));
      `App (to_term ~env tm, [])

let set_reducer ~pos ~env ~to_term = function
  | `Set (tm, v) ->
      let op =
        mk ~pos
          (`Invoke
             { invoked = to_term ~env tm; invoke_default = None; meth = "set" })
      in
      `Cast
        {
          cast = mk ~pos (`App (op, [("", to_term ~env v)]));
          typ = mk_ty ~pos Type.unit;
        }

let if_reducer ~pos ~env ~to_term = function
  | `Inline_if { if_condition; if_then_block; if_elsif; if_else_block; _ }
  | `If { if_condition; if_then_block; if_elsif; if_else_block; _ } ->
      let if_else =
        match if_else_block with
          | None -> mk ~pos (`Tuple [])
          | Some b -> to_term ~env b.block_body
      in
      let branches =
        (if_condition, if_then_block.block_body)
        :: List.map
             (fun { elsif_condition; elsif_then_block; _ } ->
               (elsif_condition, elsif_then_block.block_body))
             if_elsif
      in
      let term =
        List.fold_left
          (fun if_else (condition, _then) ->
            let op = mk ~pos (`Var "if") in
            mk ~pos
              (`App
                 ( op,
                   [
                     ("", to_term ~env condition);
                     ("then", mk_fun ~pos [] (to_term ~env _then));
                     ("else", mk_fun ~pos [] if_else);
                   ] )))
          if_else (List.rev branches)
      in
      term.term

let while_reducer ~pos ~env ~to_term = function
  | `While { while_condition; while_do_block } ->
      let op = mk ~pos (`Var "while") in
      let while_condition = mk_fun ~pos [] (to_term ~env while_condition) in
      let while_loop =
        mk_fun ~pos [] (to_term ~env while_do_block.block_body)
      in
      `App (op, [("", while_condition); ("", while_loop)])

let base_for_reducer ~pos for_variable for_iterator for_loop =
  let for_op = mk ~pos (`Var "for") in
  let for_loop =
    mk_fun ~pos
      [
        {
          label = "";
          as_variable = Some for_variable;
          typ = mk_var ();
          default = None;
          pos = None;
        };
      ]
      for_loop
  in
  `App (for_op, [("", for_iterator); ("", for_loop)])

let iterable_for_reducer ~pos ~env ~to_term = function
  | `Iterable_for
      { iterable_for_variable; iterable_for_iterator; iterable_for_do_block } ->
      base_for_reducer ~pos iterable_for_variable
        (to_term ~env iterable_for_iterator)
        (to_term ~env iterable_for_do_block.block_body)

let for_reducer ~pos ~env ~to_term = function
  | `For { for_variable; for_from; for_to; for_do_block } ->
      let to_op = mk ~pos (`Var "iterator") in
      let to_op =
        mk ~pos
          (`Invoke { invoked = to_op; invoke_default = None; meth = "int" })
      in
      let for_condition =
        mk ~pos
          (`App (to_op, [("", to_term ~env for_from); ("", to_term ~env for_to)]))
      in
      base_for_reducer ~pos for_variable for_condition
        (to_term ~env for_do_block.block_body)

let infix_reducer ~pos ~env ~to_term = function
  | `Infix (tm, op, tm') ->
      let op = mk ~pos (`Var op) in
      `App (op, [("", to_term ~env tm); ("", to_term ~env tm')])

let bool_op_reducer ~pos ~env ~to_term = function
  | `BoolOp (op, tm :: terms) ->
      List.fold_left
        (fun tm tm' ->
          let op = mk ~pos (`Var op) in
          let tm = mk_fun ~pos [] (mk ~pos tm) in
          let tm' = mk_fun ~pos [] (to_term ~env tm') in
          `App (op, [("", tm); ("", tm')]))
        (to_term ~env tm).term terms
  | `BoolOp (_, []) -> assert false

let simple_fun_reducer ~pos:_ ~env ~to_term = function
  | `Simple_fun tm ->
      `Fun
        {
          name = None;
          arguments = [];
          body = to_term ~env tm;
          free_vars = None;
        }

let negative_reducer ~pos ~env ~to_term = function
  | `Negative tm ->
      let op = mk ~pos (`Var "~-") in
      `App (op, [("", to_term ~env tm)])

let not_reducer ~pos ~env ~to_term = function
  | `Not tm ->
      let op = mk ~pos (`Var "not") in
      `App (op, [("", to_term ~env tm)])

let append_term ~pos a b =
  let op = mk ~pos (`Var "_::_") in
  `App (op, [("", a); ("", b)])

let append_reducer ~pos ~env ~to_term = function
  | `Append (tm, tm') -> append_term ~pos (to_term ~env tm) (to_term ~env tm')

let rec list_reducer ~pos ?(cur = `List []) ~env ~to_term l =
  match (l, cur) with
    | [], cur -> cur
    | `Term v :: rem, `List cur ->
        list_reducer ~cur:(`List (to_term ~env v :: cur)) ~pos ~env ~to_term rem
    | `Term v :: rem, cur ->
        let cur = append_term ~pos (to_term ~env v) (mk ~pos cur) in
        list_reducer ~pos ~cur ~env ~to_term rem
    | `Ellipsis v :: rem, cur ->
        let list = mk ~pos (`Var "list") in
        let op =
          mk ~pos
            (`Invoke { invoked = list; invoke_default = None; meth = "append" })
        in
        let cur = `App (op, [("", to_term ~env v); ("", mk ~pos cur)]) in
        list_reducer ~pos ~cur ~env ~to_term rem

let assoc_reducer ~pos ~env ~to_term = function
  | `Assoc (tm, tm') ->
      let op = mk ~pos (`Var "_[_]") in
      `App (op, [("", to_term ~env tm); ("", to_term ~env tm')])

let regexp_reducer ~pos ~env:_ ~to_term:_ = function
  | `Regexp (regexp, flags) ->
      let regexp = render_string ~pos ~sep:'/' regexp in
      let regexp = mk ~pos (`String regexp) in
      let flags = List.map Char.escaped flags in
      let flags = List.map (fun s -> mk ~pos (`String s)) flags in
      let flags = mk ~pos (`List flags) in
      let op = mk ~pos (`Var "regexp") in
      `App (op, [("", regexp); ("flags", flags)])

let try_reducer ~pos ~env ~to_term = function
  | `Try { try_body_block; try_handler; try_finally_block } ->
      let try_body = mk_fun ~pos [] (to_term ~env try_body_block.block_body) in
      let try_variable =
        match try_handler with Some h -> h.try_handler_variable | None -> "_"
      in
      let err_arg =
        [
          {
            label = "";
            as_variable = Some try_variable;
            typ = mk_var ();
            default = None;
            pos = None;
          };
        ]
      in
      let finally_pos, finally =
        match try_finally_block with
          | None -> (pos, mk ~pos (`Tuple []))
          | Some b -> (b.block_body.pos, to_term ~env b.block_body)
      in
      let finally = mk_fun ~pos:finally_pos [] finally in
      let handler_pos, handler =
        match try_handler with
          | None -> (pos, mk ~pos (`Tuple []))
          | Some h ->
              ( h.try_handler_block.block_body.pos,
                to_term ~env h.try_handler_block.block_body )
      in
      let handler = mk_fun ~pos:handler_pos err_arg handler in
      let error_module = mk ~pos (`Var "error") in
      let try_errors_list =
        match try_handler with
          | None -> mk ~pos `Null
          | Some { try_handler_errors_list = None; _ } -> mk ~pos `Null
          | Some { try_handler_errors_list = Some tm; _ } -> to_term ~env tm
      in
      let op =
        mk ~pos
          (`Invoke
             { invoked = error_module; invoke_default = None; meth = "catch" })
      in
      `App
        ( op,
          [
            ("errors", try_errors_list);
            ("body", try_body);
            ("catch", handler);
            ("finally", finally);
          ] )
