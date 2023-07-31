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

(** Helper functions for the parser. *)

open Parsed_term
module Term = Parsed_term
module Vars = Term_base.Vars

type arglist = Parsed_term.fun_arg list

type lexer_let_decoration =
  [ `None | `Recursive | `Replaces | `Eval | `Json_parse | `Yaml_parse ]

type explicit_binding = [ `Def of Term._let | `Let of Term._let ]
type binding = [ explicit_binding | `Binding of Term._let ]

let mk_source_ty ?pos name args =
  let fn = !Hooks.mk_source_ty in
  fn ?pos name args

let mk_named_ty ?pos = function
  | "_" -> Type.var ?pos ()
  | "unit" -> Type.make Type.unit
  | "never" -> Type.make Type.Ground.never
  | "bool" -> Type.make Type.Ground.bool
  | "int" -> Type.make Type.Ground.int
  | "float" -> Type.make Type.Ground.float
  | "string" -> Type.make Type.Ground.string
  | "ref" -> Type.reference (Type.var ())
  | "source" -> mk_source_ty ?pos "source" { extensible = true; tracks = [] }
  | "source_methods" -> !Hooks.source_methods_t ()
  | name -> (
      match Type.find_type_opt name with
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

let let_args ?doc ~decoration ~pat ?arglist ~def ?cast () =
  { Parsed_term.doc; decoration; pat; arglist; def; cast }

let mk_json_assoc_object_ty ~pos = function
  | `Tuple [`Named "string"; ty], "as", "json", "object" -> `Json_object ty
  | _ -> raise (Term_base.Parse_error (pos, "Invalid type constructor"))

type encoder_param =
  string * [ `Term of Term.t | `Encoder of string * encoder_opt ]

and encoder_opt = encoder_param list

type let_opt_el = string * Term.t
type record = pos:Lexing.position * Lexing.position -> Term.t -> Term.t
type ty_content_arg = string * string
type ty_content_args = ty_content_arg list
type ty_content = string * ty_content_args
type meth_pattern_el = string * Term.pattern option

let let_decoration_of_lexer_let_decoration = function
  | `Json_parse -> `Json_parse []
  | `Yaml_parse -> `Yaml_parse
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

let mk = Term.make
let mk_fun ~pos arguments body = mk ~pos (`Fun (arguments, body))

let mk_let ~pos _let body =
  let ast =
    match _let with
      | `Let v -> `Let (v, body)
      | `Def v -> `Def (v, body)
      | `Binding v -> `Binding (v, body)
  in
  mk ~pos ast

let mk_encoder ~pos e p = mk ~pos (`Encoder (e, p))
