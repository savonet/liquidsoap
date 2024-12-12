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

(** Helper functions for the parser. *)

open Parsed_term
module Term = Parsed_term
module Vars = Term_base.Vars

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

type explicit_binding = [ `Def of Term._let | `Let of Term._let ]
type binding = [ explicit_binding | `Binding of Term._let ]

let render_string_ref = ref (fun ~pos:_ _ -> assert false)

(* This is filled by Lexer to make it possible to use this function in the parser. *)
let render_string ~pos s =
  let fn = !render_string_ref in
  fn ~pos s

let pending_comments = ref []
let clear_comments () = pending_comments := []

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
      let closest_term = ref term in
      let distance = ref (comment_distance term.pos comment_pos) in
      Parsed_term.iter_term
        (fun term ->
          match (comment_distance term.pos comment_pos, !distance) with
            | (t, d), (t', d')
              when 0 <= d
                   && (d' < 0
                      || if t = `Before && t' = `After then d <= d' else d < d'
                      ) ->
                distance := (t, d);
                closest_term := term
            | _ -> ())
        term;
      let comment =
        match !distance with `Before, _ -> `Before c | `After, _ -> `After c
      in
      !closest_term.comments <-
        sort_comments ((comment_pos, comment) :: !closest_term.comments))
    !pending_comments;
  pending_comments := []

let mk_source_ty ?pos name args =
  let fn = !Hooks.mk_source_ty in
  fn ?pos name args

let mk_clock_ty ?pos () =
  let fn = !Hooks.mk_clock_ty in
  fn ?pos ()

let mk_named_ty ?pos = function
  | "_" -> Type.var ?pos:(Option.map Pos.of_lexing_pos pos) ()
  | "unit" -> Type.make ?pos:(Option.map Pos.of_lexing_pos pos) Type.unit
  | "never" -> Type.make ?pos:(Option.map Pos.of_lexing_pos pos) Type.Never
  | "bool" -> Type.make ?pos:(Option.map Pos.of_lexing_pos pos) Type.Bool
  | "int" -> Type.make ?pos:(Option.map Pos.of_lexing_pos pos) Type.Int
  | "float" -> Type.make ?pos:(Option.map Pos.of_lexing_pos pos) Type.Float
  | "string" -> Type.make ?pos:(Option.map Pos.of_lexing_pos pos) Type.String
  | "ref" -> Type.reference (Type.var ())
  | "clock" -> mk_clock_ty ?pos ()
  | "source" -> mk_source_ty ?pos "source" { extensible = true; tracks = [] }
  | "source_methods" -> !Hooks.source_methods_t ()
  | name -> (
      match Type.find_opt_typ name with
        | Some c -> c ()
        | None ->
            let pos =
              Option.value ~default:(Lexing.dummy_pos, Lexing.dummy_pos) pos
            in
            raise
              (Term_base.Parse_error
                 (pos, "Unknown type constructor: " ^ name ^ ".")))

let rec mk_ty ?pos = function
  | `Named s -> mk_named_ty ?pos s
  | `Nullable t -> Type.(make (Nullable (mk_ty ?pos t)))
  | `List t -> Type.(make (List { t = mk_ty ?pos t; json_repr = `Tuple }))
  | `Json_object t ->
      Type.(
        make
          (List
             {
               t = mk_ty ?pos (`Tuple [`Named "string"; t]);
               json_repr = `Object;
             }))
  | `Tuple l -> Type.(make (Tuple (List.map (mk_ty ?pos) l)))
  | `Arrow (args, t) ->
      Type.(
        make
          (Arrow
             ( List.map
                 (fun (optional, name, t) -> (optional, name, mk_ty ?pos t))
                 args,
               mk_ty ?pos t )))
  | `Record l -> List.fold_left (mk_meth_ty ?pos) Type.(make (Tuple [])) l
  | `Method (t, l) -> List.fold_left (mk_meth_ty ?pos) (mk_ty ?pos t) l
  | `Invoke (t, s) -> snd (Type.invoke (mk_ty ?pos t) s)
  | `Source (s, p) -> mk_source_ty ?pos s p

and mk_meth_ty ?pos base { Term.name; optional; typ; json_name } =
  Type.(
    make
      (Meth
         ( {
             meth = name;
             optional;
             scheme = ([], mk_ty ?pos typ);
             doc = "";
             json_name;
           },
           base )))

let let_args ~decoration ~pat ?arglist ~def ?cast () =
  { decoration; pat; arglist; def; cast }

let mk_json_assoc_object_ty ~pos = function
  | `Tuple [`Named "string"; ty], "as", "json", "object" -> `Json_object ty
  | _ -> raise (Term_base.Parse_error (pos, "Invalid type constructor"))

type let_opt_el = string * Term.t

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
        (Term_base.Parse_error
           (pos, "Invalid argument " ^ lbl ^ " for json.parse let constructor"))

let mk = Parsed_term.make
let mk_fun ~pos arguments body = mk ~pos (`Fun (arguments, body))

let mk_try ?ensure ?handler ?errors_list ~variable ~body ~pos () =
  mk ~pos
    (`Try
      {
        try_body = body;
        try_variable = variable;
        try_errors_list = errors_list;
        try_handler = handler;
        try_finally = ensure;
      })

let mk_let ~pos _let body =
  let ast =
    match _let with
      | `Let v -> `Let (v, body)
      | `Def v -> `Def (v, body)
      | `Binding v -> `Binding (v, body)
  in
  mk ~pos ast

let mk_encoder ~pos e p = mk ~pos (`Encoder (e, p))
