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

type result = {
  diagnostics : diagnostic list;
  term : Term.t option;
  file : string;
}

exception Stop

let load_env ~version dump = Jsoo_safe_env.(restore (of_string ~version dump))

(* The header that [Runtime.throw] prints opens the box [message] closes. *)
let render message = String.trim (Format.asprintf "@[%t" message)

(* Each type error is replaced with the universal placeholder and the script
   checked again, so that the code after it is typed too. Each retry adds a new
   position, so this ends. *)
let rec with_placeholders ~positions tm =
  match tm.Term.t.Type.pos with
    | Some pos
      when List.mem pos positions && not (Term.has_flag tm Flags.implicit) ->
        Term.make ~pos (`App (Term.make ~pos (`Var Reserved.any), []))
    | _ -> Term.map_children (with_placeholders ~positions) tm

let in_file ~file pos = (Pos.unpack pos).Pos.fname = file

let check ?(file = "") ~env source =
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
  let guard f =
    try
      f ();
      true
    with
      | Stop -> false
      | exn -> (
          let bt = Printexc.get_raw_backtrace () in
          try
            record ~bt exn;
            true
          with Stop -> false)
  in
  (* The typechecker binds the types of the terms it checks, so each attempt
     checks its own copy. *)
  let rec typecheck ~reduced ~positions =
    let handler = Type.Fresh.init ~preserve_positions:true () in
    let term = with_placeholders ~positions (Term.fresh ~handler reduced) in
    let before = !diagnostics in
    let typed =
      guard (fun () ->
          Typechecking.check ~env ~check_top_level_override:true ~throw:record
            term)
    in
    match !diagnostics with
      | ({ severity = `Error; pos = Some pos } as error) :: _
        when (not typed) && in_file ~file pos && not (List.mem pos positions) ->
          diagnostics := error :: before;
          typecheck ~reduced ~positions:(pos :: positions)
      | { pos = Some pos } :: _ when (not typed) && List.mem pos positions ->
          diagnostics := before;
          term
      | _ ->
          (* A placeholder hides the uses of the names in the code it replaces. *)
          if typed && positions = [] then
            ignore
              (guard (fun () -> Term.check_unused ~throw:record ~lib:false term));
          term
  in
  let reduced = ref None in
  ignore
    (guard (fun () ->
         let fname = if file = "" then None else Some file in
         let parsed_term =
           Liquidsoap_lang_reducer.Term_reducer.(mk_expr ?fname program lexbuf)
         in
         reduced :=
           Some
             (Liquidsoap_lang_reducer.Term_reducer.to_term ~throw:record
                parsed_term)));
  let term =
    Option.map (fun reduced -> typecheck ~reduced ~positions:[]) !reduced
  in
  { diagnostics = List.rev !diagnostics; term; file }

(* Code spliced in by [%include] is positioned in its own file. *)
let contains pos ~file ~line ~column =
  let { Pos.fname; lstart; cstart; lstop; cstop } = Pos.unpack pos in
  fname = file
  && (lstart, cstart) <= (line, column)
  && (line, column) <= (lstop, cstop)

let span pos =
  let { Pos.lstart; cstart; lstop; cstop } = Pos.unpack pos in
  (lstop - lstart, cstop - cstart)

(* A [let]'s position covers only its binding, not the statements after it, so
   no subtree can be skipped from its root's position. *)
let rec containing ~file ~line ~column tm =
  let here =
    match tm.Term.t.Type.pos with
      | Some pos
        when contains pos ~file ~line ~column
             && not (Term.has_flag tm Flags.implicit) ->
          [(span pos, tm)]
      | _ -> []
  in
  here @ List.concat_map (containing ~file ~line ~column) (Term.children tm)

(* The smallest enclosing span, since an operator's variable spans its whole
   application. On a binding, a [let] stands for its definition, as its own
   type is its body's. *)
let innermost ~file ~line ~column tm =
  let smallest =
    List.fold_left
      (fun best ((size, _) as candidate) ->
        match best with
          | Some (best_size, _) when best_size < size -> best
          | _ -> Some candidate)
      None
      (containing ~file ~line ~column tm)
  in
  Option.map
    (fun (_, tm) ->
      match tm.Term.term with `Let { Term.def } -> def | _ -> tm)
    smallest

let type_at { term; file } ~line ~column =
  Option.bind term (fun term ->
      Option.map
        (fun tm -> Repr.string_of_type tm.Term.t)
        (innermost ~file ~line ~column term))

let covers ~file ~line ~column tm = innermost ~file ~line ~column tm <> None

let rec local_names ~file ~line ~column tm =
  let bound =
    match tm.Term.term with
      | `Let { Term.pat = `PVar [name]; body }
        when covers ~file ~line ~column body ->
          [name]
      | `Let { Term.pat = `PTuple names; body }
        when covers ~file ~line ~column body ->
          names
      | `Fun { Term.arguments; body } when covers ~file ~line ~column body ->
          List.map
            (fun { Term.label; as_variable } ->
              Option.value as_variable ~default:label)
            arguments
      | _ -> []
  in
  bound @ List.concat_map (local_names ~file ~line ~column) (Term.children tm)

let locals_at { term; file } ~line ~column =
  match term with
    | Some term ->
        List.sort_uniq compare
          (List.filter
             (fun name -> name <> "" && name <> "_" && Lexer.is_var name)
             (local_names ~file ~line ~column term))
    | None -> []

let scope_at ~env result ~line ~column =
  List.sort_uniq compare
    (locals_at result ~line ~column
    @ List.filter Lexer.is_var (List.map fst env))

let methods_of typ =
  List.map
    (fun { Type.meth; scheme } -> (meth, Repr.string_of_scheme scheme))
    (fst (Type.split_meths typ))

let methods_at { term; file } ~line ~column =
  match Option.bind term (innermost ~file ~line ~column) with
    | None -> []
    | Some tm -> methods_of tm.Term.t

let pattern_names = function
  | `PVar [name] -> [name]
  | `PTuple names -> names
  | `PVar _ -> []

let bind names pos scope = List.map (fun name -> (name, pos)) names @ scope

(* [scope] maps the names bound around [tm] to where they are bound. *)
let rec binding_of ~var ~scope tm =
  if tm == var then (
    match tm.Term.term with
      | `Var name -> Option.join (List.assoc_opt name scope)
      | _ -> None)
  else (
    let pos = tm.Term.t.Type.pos in
    let scoped =
      match tm.Term.term with
        | `Let { Term.pat; def; body } ->
            [(def, scope); (body, bind (pattern_names pat) pos scope)]
        | `Fun { Term.name; arguments; body } ->
            let scope = bind (Option.to_list name) pos scope in
            let arguments_scope =
              List.fold_left
                (fun scope { Term.label; as_variable; pos } ->
                  bind [Option.value as_variable ~default:label] pos scope)
                scope arguments
            in
            List.filter_map
              (fun { Term.default } ->
                Option.map (fun default -> (default, scope)) default)
              arguments
            @ [(body, arguments_scope)]
        | _ ->
            List.map
              (fun child -> (child, scope))
              (Term.children { tm with Term.methods = Methods.empty })
    in
    let methods =
      List.map (fun (_, meth) -> (meth, scope)) (Methods.bindings tm.methods)
    in
    List.find_map
      (fun (child, scope) -> binding_of ~var ~scope child)
      (scoped @ methods))

let definition_at { term; file } ~line ~column =
  Option.bind term (fun term ->
      match innermost ~file ~line ~column term with
        | Some ({ Term.term = `Var _ } as var) -> binding_of ~var ~scope:[] term
        | _ -> None)

let null_methods ~env =
  match List.assoc_opt Reserved.null env with
    | Some (_, typ) -> methods_of typ
    | None -> []
