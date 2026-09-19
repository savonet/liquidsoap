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
open Liquidsoap_lang_types

type diagnostic = {
  severity : [ `Warning | `Error ];
  code : int;
  pos : Pos.t option;
  message : string;
}

type env = (string * Type.scheme) list
type result = { diagnostics : diagnostic list; term : Term.t option }

exception Stop

let load_env ~version dump = Jsoo_safe_env.(restore (of_string ~version dump))

(* The header that [Runtime.throw] prints opens the box [message] closes. *)
let render message = String.trim (Format.asprintf "@[%t" message)

let check ~env source =
  let diagnostics = ref [] in
  let lexbuf = Sedlexing.Utf8.from_string source in
  let record ~bt exn =
    match Runtime.describe ~lexbuf:(Some lexbuf) ~bt exn with
      | Some { Runtime.severity; code; pos; message } ->
          let severity =
            match severity with `Warning _ -> `Warning | `Error -> `Error
          in
          diagnostics :=
            { severity; code; pos; message = render message } :: !diagnostics;
          if severity = `Error then raise Stop
      | None -> Printexc.raise_with_backtrace exn bt
  in
  let term =
    try
      let parsed_term =
        Liquidsoap_lang_reducer.Term_reducer.(mk_expr program lexbuf)
      in
      let term =
        Liquidsoap_lang_reducer.Term_reducer.to_term ~throw:record parsed_term
      in
      Typechecking.check ~env ~check_top_level_override:true ~throw:record term;
      Term.check_unused ~throw:record ~lib:false term;
      Some term
    with
      | Stop -> None
      | exn -> (
          let bt = Printexc.get_raw_backtrace () in
          try
            record ~bt exn;
            None
          with Stop -> None)
  in
  { diagnostics = List.rev !diagnostics; term }

(* Code spliced in by [%include] is positioned in its own file, while the
   analysed buffer has no file name. *)
let contains pos ~line ~column =
  let { Pos.fname; lstart; cstart; lstop; cstop } = Pos.unpack pos in
  fname = ""
  && (lstart, cstart) <= (line, column)
  && (line, column) <= (lstop, cstop)

let span pos =
  let { Pos.lstart; cstart; lstop; cstop } = Pos.unpack pos in
  (lstop - lstart, cstop - cstart)

(* A [let]'s position covers only its binding, not the statements after it, so
   no subtree can be skipped from its root's position. *)
let rec containing ~line ~column tm =
  let here =
    match tm.Term.t.Type.pos with
      | Some pos
        when contains pos ~line ~column && not (Term.has_flag tm Flags.implicit)
        ->
          [(span pos, tm)]
      | _ -> []
  in
  here @ List.concat_map (containing ~line ~column) (Term.children tm)

(* The smallest enclosing span, since an operator's variable spans its whole
   application. On a binding, a [let] stands for its definition, as its own
   type is its body's. *)
let innermost ~line ~column tm =
  let smallest =
    List.fold_left
      (fun best ((size, _) as candidate) ->
        match best with
          | Some (best_size, _) when best_size < size -> best
          | _ -> Some candidate)
      None
      (containing ~line ~column tm)
  in
  Option.map
    (fun (_, tm) ->
      match tm.Term.term with `Let { Term.def } -> def | _ -> tm)
    smallest

let type_at { term } ~line ~column =
  Option.bind term (fun term ->
      Option.map
        (fun tm -> Repr.string_of_type tm.Term.t)
        (innermost ~line ~column term))

let covers ~line ~column tm = innermost ~line ~column tm <> None

let rec local_names ~line ~column tm =
  let bound =
    match tm.Term.term with
      | `Let { Term.pat = `PVar [name]; body } when covers ~line ~column body ->
          [name]
      | `Let { Term.pat = `PTuple names; body } when covers ~line ~column body
        ->
          names
      | `Fun { Term.arguments; body } when covers ~line ~column body ->
          List.map
            (fun { Term.label; as_variable } ->
              Option.value as_variable ~default:label)
            arguments
      | _ -> []
  in
  bound @ List.concat_map (local_names ~line ~column) (Term.children tm)

let locals_at { term } ~line ~column =
  match term with
    | Some term ->
        List.sort_uniq compare
          (List.filter
             (fun name -> name <> "" && name <> "_" && Lexer.is_var name)
             (local_names ~line ~column term))
    | None -> []

let scope_at ~env result ~line ~column =
  List.sort_uniq compare
    (locals_at result ~line ~column
    @ List.filter Lexer.is_var (List.map fst env))

let methods_of typ =
  List.map
    (fun { Type.meth; scheme } -> (meth, Repr.string_of_scheme scheme))
    (fst (Type.split_meths typ))

let methods_at { term } ~line ~column =
  match Option.bind term (innermost ~line ~column) with
    | None -> []
    | Some tm -> methods_of tm.Term.t

let null_methods ~env =
  match List.assoc_opt Reserved.null env with
    | Some (_, typ) -> methods_of typ
    | None -> []
