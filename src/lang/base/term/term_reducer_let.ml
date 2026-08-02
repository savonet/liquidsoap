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

(** Reduce the decorated [let] forms: [let rec], [let replaces], [let eval] and
    the [let json.parse] / [xml.parse] / [yaml.parse] / [sqlite.row] /
    [sqlite.query] parsers. *)

open Parsed_term
include Runtime_term
open Term_reducer_helpers
open Term_reducer_argsof
open Term_reducer_ty
open Term_reducer_pattern

let mk_let_json_parse ~pos (args, pat, def, cast) body =
  let ty = match cast with Some ty -> ty | None -> mk_var ~pos () in
  let tty = Value.RuntimeType.to_term ty in
  let json5 =
    match List.assoc_opt "json5" args with
      | Some v -> v
      | None -> Term.(make (`Bool false))
  in
  let parser = mk ~pos (`Var "_internal_json_parser_") in
  let def =
    mk ~pos (`App (parser, [("json5", json5); ("type", tty); ("", def)]))
  in
  let def = mk ~pos (`Cast { cast = def; typ = ty }) in
  pattern_reducer ~body ~pat def

let mk_let_xml_parse ~pos (pat, def, cast) body =
  let ty = match cast with Some ty -> ty | None -> mk_var ~pos () in
  let tty = Value.RuntimeType.to_term ty in
  let parser = mk ~pos (`Var "_internal_xml_parser_") in
  let def = mk ~pos (`App (parser, [("type", tty); ("", def)])) in
  let def = mk ~pos (`Cast { cast = def; typ = ty }) in
  pattern_reducer ~body ~pat def

let mk_let_yaml_parse ~pos (pat, def, cast) body =
  let ty = match cast with Some ty -> ty | None -> mk_var ~pos () in
  let tty = Value.RuntimeType.to_term ty in
  let parser = mk ~pos (`Var "_internal_yaml_parser_") in
  let def = mk ~pos (`App (parser, [("type", tty); ("", def)])) in
  let def = mk ~pos (`Cast { cast = def; typ = ty }) in
  pattern_reducer ~body ~pat def

let mk_let_sqlite_row ~pos (pat, def, cast) body =
  let ty = match cast with Some ty -> ty | None -> mk_var ~pos () in
  let tty = Value.RuntimeType.to_term ty in
  let parser = mk ~pos (`Var "_sqlite_row_parser_") in
  let def = mk ~pos (`App (parser, [("type", tty); ("", def)])) in
  let def = mk ~pos (`Cast { cast = def; typ = ty }) in
  pattern_reducer ~body ~pat def

let mk_let_sqlite_query ~pos (pat, def, cast) body =
  let ty = match cast with Some ty -> ty | None -> mk_var ~pos () in
  let inner_list_ty = mk_var ~pos () in
  Typing.(
    ty <: mk_ty ~pos (Type.List { Type.t = inner_list_ty; json_repr = `Tuple }));
  let tty = Value.RuntimeType.to_term inner_list_ty in
  let parser = mk ~pos (`Var "_sqlite_row_parser_") in
  let mapper =
    let query = mk ~pos (`Var "query") in
    mk ~pos (`App (parser, [("type", tty); ("", query)]))
  in
  let mapper =
    mk ~pos
      (`Fun
         {
           free_vars = None;
           name = None;
           arguments =
             [
               {
                 label = "";
                 as_variable = Some "query";
                 default = None;
                 typ = mk_var ~pos ();
                 pos = None;
               };
             ];
           body = mapper;
         })
  in
  let list = mk ~pos (`Var "list") in
  let map =
    mk ~pos (`Invoke { invoked = list; invoke_default = None; meth = "map" })
  in
  let def = mk ~pos (`App (map, [("", mapper); ("", def)])) in
  let def = mk ~pos (`Cast { cast = def; typ = ty }) in
  pattern_reducer ~body ~pat def

let mk_rec_fun ~pos pat arguments body =
  let name =
    match pat with
      | `PVar l when l <> [] -> List.hd (List.rev l)
      | _ -> assert false
  in
  mk ~pos (`Fun { name = Some name; arguments; body; free_vars = None })

let needs_toplevel = ref false

let mk_eval ~pos (pat, def, body, cast) =
  needs_toplevel := true;
  let ty = match cast with Some ty -> ty | None -> mk_var ~pos () in
  let tty = Value.RuntimeType.to_term ty in
  let eval = mk ~pos (`Var "_eval_") in
  let def = mk ~pos (`App (eval, [("type", tty); ("", def)])) in
  let def = mk ~pos (`Cast { cast = def; typ = ty }) in
  pattern_reducer ~body ~pat def

let needs_toplevel () = !needs_toplevel

let string_of_let_decoration = function
  | `None -> ""
  | `Recursive -> "rec"
  | `Replaces -> "replaces"
  | `Eval -> "eval"
  | `Sqlite_query -> "sqlite.query"
  | `Sqlite_row -> "sqlite.row"
  | `Yaml_parse -> "yaml.parse"
  | `Xml_parse -> "xml.parse"
  | `Json_parse _ -> "json.parse"

let mk_let ~env ~pos ~to_term ~comments ~throw
    ({ decoration; pat; arglist; def; cast }, body) =
  let def = to_term ~env def in
  let mk_body def =
    let env =
      match pat.pat_entry with
        | `PVar path ->
            let path = String.concat "." path in
            let env =
              if decoration <> `Replaces then
                List.filter
                  (fun (p, _) ->
                    not (String.starts_with ~prefix:(path ^ ".") p))
                  env
              else env
            in
            (path, def) :: env
        | _ -> env
    in
    to_term ~env body
  in
  let cast = Option.map (mk_parsed_ty ~pos ~env ~to_term) cast in
  let arglist = Option.map (expand_argsof ~throw ~pos ~env ~to_term) arglist in
  let doc =
    match
      List.rev
        (List.filter_map
           (function pos, `Before c -> Some (pos, c) | _ -> None)
           comments)
    with
      | (pos, doc) :: _ ->
          Doc.parse_doc ~pos:(Pos.of_lexing_pos pos) (String.concat "\n" doc)
      | _ -> None
  in
  match (arglist, decoration) with
    | Some (mk_def, arglist), `None | Some (mk_def, arglist), `Replaces ->
        let replace = decoration = `Replaces in
        let def = mk_def def in
        let def = mk_fun ~pos arglist def in
        let def =
          match cast with
            | Some ty -> mk ~pos (`Cast { cast = def; typ = ty })
            | None -> def
        in
        let body = mk_body def in
        pattern_reducer ?doc ~body ~pat ~replace def
    | Some (mk_def, arglist), `Recursive ->
        let def = mk_def def in
        let def = mk_rec_fun ~pos pat.pat_entry arglist def in
        let def =
          match cast with
            | Some ty -> mk ~pos (`Cast { cast = def; typ = ty })
            | None -> def
        in
        let body = mk_body def in
        pattern_reducer ?doc ~body ~pat def
    | None, `None | None, `Replaces ->
        let replace = decoration = `Replaces in
        let def =
          match cast with
            | Some ty -> mk ~pos (`Cast { cast = def; typ = ty })
            | None -> def
        in
        let body = mk_body def in
        pattern_reducer ?doc ~body ~pat ~replace def
    | None, `Eval ->
        let body = mk_body def in
        mk_eval ~pos (pat, def, body, cast)
    | None, `Json_parse args ->
        let args = List.map (fun (l, v) -> (l, to_term ~env v)) args in
        let body = mk_body def in
        mk_let_json_parse ~pos (args, pat, def, cast) body
    | None, `Yaml_parse ->
        let body = mk_body def in
        mk_let_yaml_parse ~pos (pat, def, cast) body
    | None, `Xml_parse ->
        let body = mk_body def in
        mk_let_xml_parse ~pos (pat, def, cast) body
    | None, `Sqlite_row ->
        let body = mk_body def in
        mk_let_sqlite_row ~pos (pat, def, cast) body
    | None, `Sqlite_query ->
        let body = mk_body def in
        mk_let_sqlite_query ~pos (pat, def, cast) body
    | Some _, v ->
        parse_error ~pos
          (string_of_let_decoration v
         ^ " does not apply to function assignments")
    | None, v ->
        parse_error ~pos
          (string_of_let_decoration v ^ " only applies to function assignments")
