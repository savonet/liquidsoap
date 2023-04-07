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

type arglist = (string * string * Type.t * Term.t option) list

type lexer_let_decoration =
  [ `None | `Recursive | `Replaces | `Eval | `Json_parse | `Yaml_parse ]

type let_decoration =
  [ `None
  | `Recursive
  | `Replaces
  | `Eval
  | `Yaml_parse
  | `Json_parse of (string * Term.t) list ]

type app_list_elem = (string * Term.t) list

type binding =
  Doc.Value.t option
  * let_decoration
  * Term.pattern
  * arglist option
  * Term.t
  * Type.t option

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

type meth_ty_opt = {
  meth_ty_name : string;
  meth_ty_typ : Type.t;
  meth_ty_optional : bool;
  meth_ty_json_name : string option;
}

let let_decoration_of_lexer_let_decoration = function
  | `Json_parse -> `Json_parse []
  | `Yaml_parse -> `Yaml_parse
  | `Eval -> `Eval
  | `Recursive -> `Recursive
  | `None -> `None
  | `Replaces -> `Replaces

let string_of_let_decoration = function
  | `None -> ""
  | `Recursive -> "rec"
  | `Replaces -> "replaces"
  | `Eval -> "eval"
  | `Yaml_parse -> "yaml.parse"
  | `Json_parse _ -> "json.parse"

let args_of_json_parse ~pos = function
  | [] -> []
  | [("json5", v)] -> [("json5", v)]
  | (lbl, _) :: _ ->
      raise
        (Parse_error
           (pos, "Invalid argument " ^ lbl ^ " for json.parse let constructor"))

let gen_args_of ~only ~except ~pos get_args name =
  match Environment.get_builtin name with
    | Some ((_, t), Value.{ value = Fun (args, _, _) })
    | Some ((_, t), Value.{ value = FFI (args, _) }) ->
        let filtered_args = List.filter (fun (n, _, _) -> n <> "") args in
        let filtered_args =
          if only <> [] then
            List.map
              (fun n ->
                try List.find (fun (n', _, _) -> n = n') filtered_args
                with Not_found ->
                  raise
                    (Parse_error
                       ( pos,
                         Printf.sprintf
                           "Builtin %s does not have an argument named %s" name
                           n )))
              only
          else filtered_args
        in
        List.iter
          (fun n ->
            match List.find_opt (fun (n', _, _) -> n = n') args with
              | Some _ -> ()
              | None ->
                  raise
                    (Parse_error
                       ( pos,
                         Printf.sprintf
                           "Builtin %s does not have an argument named %s" name
                           n )))
          except;
        let filtered_args =
          List.filter (fun (n, _, _) -> not (List.mem n except)) filtered_args
        in
        get_args ~pos t filtered_args
    | Some _ ->
        raise
          (Parse_error (pos, Printf.sprintf "Builtin %s is not a function!" name))
    | None ->
        raise
          (Parse_error (pos, Printf.sprintf "Builtin %s is not registered!" name))

let args_of, app_of =
  let rec get_args ~pos t args =
    let get_arg_type t name =
      match (Type.deref t).Type.descr with
        | Type.Arrow (l, _) ->
            let _, _, t = List.find (fun (_, n, _) -> n = name) l in
            t
        | _ ->
            raise
              (Parse_error
                 ( pos,
                   Printf.sprintf
                     "Cannot get argument type of %s, this is not a function, \
                      it has type: %s."
                     name (Type.to_string t) ))
    in
    List.map
      (fun (n, n', v) ->
        let t = Type.make ~pos (get_arg_type t n).Type.descr in
        (n, n', t, Option.map (term_of_value ~pos t) v))
      args
  and get_app ~pos _ args =
    List.map
      (fun (n, _, _) ->
        ( n,
          Term.
            { t = Type.var ~pos (); term = Var n; methods = Term.Methods.empty }
        ))
      args
  and term_of_value ~pos t v =
    let get_list_type () =
      match (Type.deref t).Type.descr with
        | Type.(List { t }) -> t
        | _ -> assert false
    in
    let get_tuple_type pos =
      match (Type.deref t).Type.descr with
        | Type.Tuple t -> List.nth t pos
        | _ -> assert false
    in
    let process_value ~t v =
      let mk_tm term =
        Term.
          {
            t = Type.make ~pos t.Type.descr;
            term;
            methods = Term.Methods.empty;
          }
      in
      match v.Value.value with
        | Value.Ground g -> mk_tm (Term.Ground g)
        | Value.List l ->
            mk_tm
              (Term.List (List.map (term_of_value ~pos (get_list_type ())) l))
        | Value.Tuple l ->
            mk_tm
              (Term.Tuple
                 (List.mapi
                    (fun idx v -> term_of_value ~pos (get_tuple_type idx) v)
                    l))
        | Value.Null -> mk_tm Term.Null
        | Value.Fun (args, [], body) ->
            let body =
              Term.{ body with t = Type.make ~pos body.t.Type.descr }
            in
            mk_tm (Term.Fun (Term.free_vars body, get_args ~pos t args, body))
        | _ ->
            raise
              (Parse_error
                 ( pos,
                   Printf.sprintf "Value %s cannot be represented as a term"
                     (Value.to_string v) ))
    in
    let meths, _ = Type.split_meths t in
    {
      (process_value ~t v) with
      methods =
        Methods.mapi
          (fun key meth ->
            let { Type.scheme = _, t } =
              List.find (fun { Type.meth } -> meth = key) meths
            in
            process_value ~t meth)
          v.Value.methods;
    }
  in
  let args_of = gen_args_of get_args in
  let app_of = gen_args_of get_app in
  (args_of, app_of)

let mk = Term.make

let append_list ~pos x v =
  match (x, v) with
    | `Expr x, `List l -> `List (x :: l)
    | `Expr x, `App v ->
        let list = mk ~pos (Var "list") in
        let op =
          mk ~pos (Invoke { invoked = list; default = None; meth = "add" })
        in
        `App (mk ~pos (App (op, [("", x); ("", v)])))
    | `Ellipsis x, `App v ->
        let list = mk ~pos (Var "list") in
        let op =
          mk ~pos (Invoke { invoked = list; default = None; meth = "append" })
        in
        `App (mk ~pos (App (op, [("", x); ("", v)])))
    | `Ellipsis x, `List l ->
        let list = mk ~pos (Var "list") in
        let op =
          mk ~pos (Invoke { invoked = list; default = None; meth = "append" })
        in
        let l = mk ~pos (List l) in
        `App (mk ~pos (App (op, [("", x); ("", l)])))

let mk_list ~pos = function `List l -> mk ~pos (List l) | `App a -> a

let mk_fun ~pos args body =
  let bound = List.map (fun (_, x, _, _) -> x) args in
  let fv =
    List.fold_left
      (fun fv (_, _, _, d) ->
        match d with
          | Some d -> Term.Vars.union fv (Term.free_vars d)
          | None -> fv)
      Term.Vars.empty args
  in
  let fv = Term.Vars.union fv (Term.free_vars ~bound body) in
  mk ~pos (Fun (fv, args, body))

(** When doing chained calls, we want to update all nested defaults so that, e.g.
    in:
      `x?.foo.gni.bla(123)?.gno.gni`,
    the default for `x.foo` becomes:
      `any.{gni = any.{ bla = fun (_) -> any.{ gno = any.{ gni = null() }}}}}`
    we also need to keep track of which methods are optional in the default value's type
    to make sure it doesn't force optional methods to be mandatory during type checking. *)
let mk_app_invoke_default ~pos ~args body =
  let app_args =
    List.map (fun (name, _) -> (name, name, Type.var (), None)) args
  in
  mk_fun ~pos app_args body

let mk_any ~pos () = mk ~pos Any

let rec mk_invoke_default ~pos ~optional ~name value { invoked; meth; default }
    =
  let t =
    Type.meth ~pos ~optional name ([], Type.var ~pos ()) (Type.var ~pos ())
  in
  let value =
    Term.
      {
        (mk_any ~pos ()) with
        t;
        methods = Methods.add name value Term.Methods.empty;
      }
  in
  ( value,
    update_invoke_default ~pos ~optional:(default <> None) invoked meth value )

and update_invoke_default ~pos ~optional expr name value =
  match expr.term with
    | Invoke ({ meth; default } as invoked) ->
        let value, invoked =
          mk_invoke_default ~pos ~name ~optional value invoked
        in
        {
          expr with
          term =
            Invoke
              { invoked; meth; default = Option.map (fun _ -> value) default };
        }
    | App ({ term = Invoke ({ meth; default } as invoked) }, args) ->
        let value, invoked =
          let default =
            match default with
              | Some { term = Fun (_, _, body) } -> Some body
              | None -> Some (mk_any ~pos ())
              | _ -> assert false
          in
          mk_invoke_default ~pos ~name ~optional value { invoked with default }
        in
        {
          expr with
          term =
            App
              ( mk ~pos
                  (Invoke
                     {
                       invoked;
                       meth;
                       default =
                         Option.map
                           (fun _ -> mk_app_invoke_default ~pos ~args value)
                           default;
                     }),
                args );
        }
    | _ -> expr

let mk_invoke ?default ~pos expr v =
  let optional, value =
    match default with Some v -> (true, v) | None -> (false, mk ~pos Null)
  in
  match v with
    | `String meth ->
        let expr = update_invoke_default ~pos ~optional expr meth value in
        mk ~pos
          (Invoke
             {
               invoked = expr;
               default = Option.map (fun _ -> value) default;
               meth;
             })
    | `App (meth, args) ->
        let value = mk_app_invoke_default ~pos ~args value in
        let expr = update_invoke_default ~pos ~optional expr meth value in
        mk ~pos
          (App
             ( mk ~pos
                 (Invoke
                    {
                      invoked = expr;
                      default = Option.map (fun _ -> value) default;
                      meth;
                    }),
               args ))

let mk_coalesce ~pos ~default computed =
  match computed.term with
    | Invoke { invoked; meth } -> mk_invoke ~pos ~default invoked (`String meth)
    | _ ->
        let null = mk ~pos (Var "null") in
        let op =
          mk ~pos (Invoke { invoked = null; default = None; meth = "default" })
        in
        let handler = mk_fun ~pos [] default in
        mk ~pos (App (op, [("", computed); ("", handler)]))

let mk_let_json_parse ~pos (args, pat, def, cast) body =
  let ty = match cast with Some ty -> ty | None -> Type.var ~pos () in
  let tty = Value.RuntimeType.to_term ty in
  let json5 =
    match List.assoc_opt "json5" args with
      | Some v -> v
      | None -> Term.(make (Ground (Ground.Bool false)))
  in
  let parser = mk ~pos (Var "_internal_json_parser_") in
  let def =
    mk ~pos (App (parser, [("json5", json5); ("type", tty); ("", def)]))
  in
  let def = mk ~pos (Cast (def, ty)) in
  mk ~pos (Let { doc = None; replace = false; pat; gen = []; def; body })

let mk_let_yaml_parse ~pos (pat, def, cast) body =
  let ty = match cast with Some ty -> ty | None -> Type.var ~pos () in
  let tty = Value.RuntimeType.to_term ty in
  let parser = mk ~pos (Var "_internal_yaml_parser_") in
  let def = mk ~pos (App (parser, [("type", tty); ("", def)])) in
  let def = mk ~pos (Cast (def, ty)) in
  mk ~pos (Let { doc = None; replace = false; pat; gen = []; def; body })

let mk_rec_fun ~pos pat args body =
  let name =
    match pat with
      | PVar l when l <> [] -> List.hd (List.rev l)
      | _ -> assert false
  in
  let bound = List.map (fun (_, x, _, _) -> x) args in
  let bound = name :: bound in
  let fv = Term.free_vars ~bound body in
  mk ~pos (RFun (name, fv, args, body))

let mk_eval ~pos (doc, pat, def, body, cast) =
  let ty = match cast with Some ty -> ty | None -> Type.var ~pos () in
  let tty = Value.RuntimeType.to_term ty in
  let eval = mk ~pos (Var "_eval_") in
  let def = mk ~pos (App (eval, [("type", tty); ("", def)])) in
  let def = mk ~pos (Cast (def, ty)) in
  mk ~pos (Let { doc; replace = false; pat; gen = []; def; body })

let mk_let ~pos (doc, decoration, pat, arglist, def, cast) body =
  match (arglist, decoration) with
    | Some arglist, `None | Some arglist, `Replaces ->
        let replace = decoration = `Replaces in
        let def = mk_fun ~pos arglist def in
        let def =
          match cast with Some ty -> mk ~pos (Cast (def, ty)) | None -> def
        in
        mk ~pos (Let { doc; replace; pat; gen = []; def; body })
    | Some arglist, `Recursive ->
        let def = mk_rec_fun ~pos pat arglist def in
        let def =
          match cast with Some ty -> mk ~pos (Cast (def, ty)) | None -> def
        in
        mk ~pos (Let { doc; replace = false; pat; gen = []; def; body })
    | None, `None | None, `Replaces ->
        let replace = decoration = `Replaces in
        let def =
          match cast with Some ty -> mk ~pos (Cast (def, ty)) | None -> def
        in
        mk ~pos (Let { doc; replace; pat; gen = []; def; body })
    | None, `Eval -> mk_eval ~pos (doc, pat, def, body, cast)
    | None, `Json_parse args ->
        mk_let_json_parse ~pos (args, pat, def, cast) body
    | None, `Yaml_parse -> mk_let_yaml_parse ~pos (pat, def, cast) body
    | Some _, v ->
        raise
          (Parse_error
             ( pos,
               string_of_let_decoration v
               ^ " does not apply to function assignments" ))
    | None, v ->
        raise
          (Parse_error
             ( pos,
               string_of_let_decoration v
               ^ " only applies to function assignments" ))

let mk_encoder ~pos e p = mk ~pos (Encoder (e, p))

(** Time intervals *)

let time_units = [| 7 * 24 * 60 * 60; 24 * 60 * 60; 60 * 60; 60; 1 |]

(** Given a date specified as a list of four values (whms), return a date in
    seconds from the beginning of the week. *)
let date ~pos =
  let to_int = function None -> 0 | Some i -> i in
  let rec aux = function
    | None :: tl -> aux tl
    | [] -> raise (Parse_error (pos, "Invalid time."))
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
    raise (Parse_error (pos, "Invalid time interval: precisions differ."));
  (t1, t2, p1)

let during ~pos d =
  let t, d, p = (date ~pos d, duration d, precision d) in
  (t, t + d, p)

let mk_time_pred ~pos (a, b, c) =
  let args = List.map (fun x -> ("", mk ~pos (Ground (Int x)))) [a; b; c] in
  mk ~pos (App (mk ~pos (Var "time_in_mod"), args))

let mk_source_ty ~pos name args =
  let fn = !Hooks.mk_source_ty in
  fn ~pos name args

let mk_json_assoc_object_ty ~pos = function
  | ( {
        Type.descr =
          Type.Tuple
            [
              { Type.descr = Type.Custom { Type.typ = Type.Ground.String.Type } };
              ty;
            ];
      },
      "as",
      "json",
      "object" ) ->
      Type.(
        make ~pos
          (List
             {
               t = make ~pos (Tuple [make Type.Ground.string; ty]);
               json_repr = `Object;
             }))
  | _ -> raise (Parse_error (pos, "Invalid type constructor"))

let mk_ty ~pos name =
  match name with
    | "_" -> Type.var ()
    | "unit" -> Type.make Type.unit
    | "bool" -> Type.make Type.Ground.bool
    | "int" -> Type.make Type.Ground.int
    | "float" -> Type.make Type.Ground.float
    | "string" -> Type.make Type.Ground.string
    | "ref" -> Type.reference (Type.var ())
    | "source" -> mk_source_ty ~pos "source" []
    | "source_methods" -> !Hooks.source_methods_t ()
    | name -> (
        match Type.find_type_opt name with
          | Some c -> c ()
          | None ->
              raise
                (Parse_error (pos, "Unknown type constructor: " ^ name ^ ".")))

let mk_invoke_ty ~pos ty name =
  try snd (Type.invoke ty name)
  with Not_found ->
    raise
      (Parse_error (pos, "Unknown type: " ^ Type.to_string ty ^ "." ^ name ^ "."))
