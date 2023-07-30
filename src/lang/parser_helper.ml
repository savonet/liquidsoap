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

open Term
open Ground
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

type inner_list_item = [ `Ellipsis of Term.t | `Expr of Term.t ]
type inner_list = [ `App of Term.t | `List of Term.t list ]
type let_opt_el = string * Term.t
type record = pos:Lexing.position * Lexing.position -> Term.t -> Term.t
type ty_content_arg = string * string
type ty_content_args = ty_content_arg list
type ty_content = string * ty_content_args
type varlist = [ `List of Term.t list | `App of Term.t ]
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

let append_list ~pos x v =
  match (x, v) with
    | `Expr x, `List l -> `List (x :: l)
    | `Expr x, `App v ->
        let list = mk ~pos (`Var "list") in
        let op =
          mk ~pos
            (`Invoke { invoked = list; default = None; meth = `String "add" })
        in
        `App (mk ~pos (`App (op, [`Term ("", x); `Term ("", v)])))
    | `Ellipsis x, `App v ->
        let list = mk ~pos (`Var "list") in
        let op =
          mk ~pos
            (`Invoke
              { invoked = list; default = None; meth = `String "append" })
        in
        `App (mk ~pos (`App (op, [`Term ("", x); `Term ("", v)])))
    | `Ellipsis x, `List l ->
        let list = mk ~pos (`Var "list") in
        let op =
          mk ~pos
            (`Invoke
              { invoked = list; default = None; meth = `String "append" })
        in
        let l = mk ~pos (`List l) in
        `App (mk ~pos (`App (op, [`Term ("", x); `Term ("", l)])))

let mk_list ~pos = function `List l -> mk ~pos (`List l) | `App a -> a
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

(** Time intervals *)

let time_units = [| 7 * 24 * 60 * 60; 24 * 60 * 60; 60 * 60; 60; 1 |]

(** Given a date specified as a list of four values (whms), return a date in
    seconds from the beginning of the week. *)
let date ~pos =
  let to_int = function None -> 0 | Some i -> i in
  let rec aux = function
    | None :: tl -> aux tl
    | [] -> raise (Term_base.Parse_error (pos, "Invalid time."))
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

(** Give the precision of a date-as-list.
    For example, the precision of Xs is 1, XmYs is 60, XhYmZs 3600, etc. *)
let precision d = time_units.(last_index d)

(** Give the duration of a data-as-list.
    For example, the duration of Xs is 1, Xm 60, XhYm 60, etc. *)
let duration d =
  time_units.(Array.length time_units - 1 - last_index (List.rev d))

let between ~pos d1 d2 =
  let p1 = precision d1 in
  let p2 = precision d2 in
  let t1 = date ~pos d1 in
  let t2 = date ~pos d2 in
  if p1 <> p2 then
    raise
      (Term_base.Parse_error (pos, "Invalid time interval: precisions differ."));
  (t1, t2, p1)

let during ~pos d =
  let t, d, p = (date ~pos d, duration d, precision d) in
  (t, t + d, p)

let mk_time_pred ~pos (a, b, c) =
  let args =
    List.map (fun x -> `Term ("", mk ~pos (`Ground (Int x)))) [a; b; c]
  in
  mk ~pos (`App (mk ~pos (`Var "time_in_mod"), args))
