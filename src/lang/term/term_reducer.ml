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

type processor =
  ( Parser.token * Lexing.position * Lexing.position,
    Parsed_term.t )
  MenhirLib.Convert.revised

open Parsed_term
open Term.Ground
include Runtime_term

let parse_error ~pos msg = raise (Term_base.Parse_error (pos, msg))
let render_string ~pos ~sep s = Lexer.render_string ~pos ~sep s
let mk = Term.make
let mk_parsed = Parsed_term.make

let mk_fun ~pos arguments body =
  Term.make ~pos (`Fun Term.{ free_vars = None; name = None; arguments; body })

(** Time intervals *)

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

(** Give the precision of a date-as-list.
    For example, the precision of Xs is 1, XmYs is 60, XhYmZs 3600, etc. *)
let precision d = time_units.(last_index d)

(** Give the duration of a data-as-list.
    For example, the duration of Xs is 1, Xm 60, XhYm 60, etc. *)
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
  let args = List.map (fun x -> ("", mk ~pos (`Ground (Int x)))) [a; b; c] in
  `App (mk ~pos (`Var "time_in_mod"), args)

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
                  parse_error ~pos
                    (Printf.sprintf
                       "Builtin %s does not have an argument named %s" name n))
              only
          else filtered_args
        in
        List.iter
          (fun n ->
            match List.find_opt (fun (n', _, _) -> n = n') args with
              | Some _ -> ()
              | None ->
                  parse_error ~pos
                    (Printf.sprintf
                       "Builtin %s does not have an argument named %s" name n))
          except;
        let filtered_args =
          List.filter (fun (n, _, _) -> not (List.mem n except)) filtered_args
        in
        get_args ~pos t filtered_args
    | Some _ ->
        parse_error ~pos (Printf.sprintf "Builtin %s is not a function!" name)
    | None ->
        parse_error ~pos (Printf.sprintf "Builtin %s is not registered!" name)

let args_of, app_of =
  let rec get_args ~pos t args =
    let get_arg_type t name =
      match (Type.deref t).Type.descr with
        | Type.Arrow (l, _) ->
            let _, _, t = List.find (fun (_, n, _) -> n = name) l in
            t
        | _ ->
            parse_error ~pos
              (Printf.sprintf
                 "Cannot get argument type of %s, this is not a function, it \
                  has type: %s."
                 name (Type.to_string t))
    in
    List.map
      (fun (n, n', v) ->
        let t = Type.make ~pos (get_arg_type t n).Type.descr in
        let as_variable = if n = n' then None else Some n' in
        {
          label = n;
          as_variable;
          typ = t;
          default = Option.map (term_of_value ~pos ~name:n t) v;
        })
      args
  and get_app ~pos _ args =
    List.map (fun (n, _, _) -> (n, mk ~t:(Type.var ~pos ()) (`Var n))) args
  and term_of_value_base ~pos t v =
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
      let mk_tm term = mk ~t:(Type.make ~pos t.Type.descr) term in
      match v.Value.value with
        | Value.Ground g -> mk_tm (`Ground g)
        | Value.List l ->
            mk_tm
              (`List (List.map (term_of_value_base ~pos (get_list_type ())) l))
        | Value.Tuple l ->
            mk_tm
              (`Tuple
                (List.mapi
                   (fun idx v -> term_of_value_base ~pos (get_tuple_type idx) v)
                   l))
        | Value.Null -> mk_tm `Null
        (* Ignoring env is not correct here but this is an internal operator
           so we have to trust that devs using it via %argsof now that they are doing. *)
        | Value.Fun (args, _, body) ->
            let body =
              mk
                ~t:(Type.make ~pos body.t.Type.descr)
                ~methods:body.Term.methods body.Term.term
            in
            mk_tm
              (`Fun
                {
                  Term_base.name = None;
                  arguments = get_args ~pos t args;
                  body;
                  free_vars = None;
                })
        | _ -> assert false
    in
    let meths, _ = Type.split_meths t in
    let tm = process_value ~t v in
    mk ~t:tm.Term.t
      ~methods:
        (Methods.mapi
           (fun key meth ->
             let { Type.scheme = _, t } =
               List.find (fun { Type.meth } -> meth = key) meths
             in
             process_value ~t meth)
           v.Value.methods)
      tm.Term.term
  and term_of_value ~pos ~name t v =
    try term_of_value_base ~pos t v
    with _ ->
      parse_error ~pos
        (Printf.sprintf "Argument %s: value %s cannot be represented as a term"
           name (Value.to_string v))
  in
  let args_of ~pos = gen_args_of ~pos get_args in
  let app_of ~pos = gen_args_of ~pos get_app in
  (args_of, app_of)

let expand_appof ~pos ~to_term (args : Parsed_term.app_arg list) =
  List.rev
    (List.fold_left
       (fun args -> function
         | `Argsof { only; except; source } ->
             List.rev (app_of ~pos ~only ~except source) @ args
         | `Term (l, v) -> (l, to_term v) :: args)
       [] args)

let expand_argsof ~pos ~to_term args =
  List.rev
    (List.fold_left
       (fun args -> function
         | `Argsof { only; except; source } ->
             List.rev (args_of ~pos ~only ~except source) @ args
         | `Term arg ->
             {
               arg with
               typ =
                 (match arg.typ with
                   | None -> Type.var ()
                   | Some typ -> Parser_helper.mk_ty typ);
               default = Option.map to_term arg.default;
             }
             :: args)
       [] args)

(** When doing chained calls, we want to update all nested defaults so that, e.g.
    in:
      `x?.foo.gni.bla(123)?.gno.gni`,
    the default for `x.foo` becomes:
      `any.{gni = any.{ bla = fun (_) -> any.{ gno = any.{ gni = null() }}}}`
    we also need to keep track of which methods are optional in the default value's type
    to make sure it doesn't force optional methods to be mandatory during type checking. *)
let mk_app_invoke_default ~pos ~args body =
  let app_args =
    List.map
      (fun (label, _) ->
        {
          Term_base.label;
          as_variable = None;
          typ = Type.var ();
          default = None;
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
    Type.meth ~pos ~optional name ([], Type.var ~pos ()) (Type.var ~pos ())
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
              | Some { term = `Fun { Term_base.body } } -> Some body
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

let mk_invoke ?(default : Parsed_term.t option) ~pos ~to_term expr v =
  let expr = to_term expr in
  let default = Option.map to_term default in
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
        let args = expand_appof ~pos ~to_term args in
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

let mk_coalesce ~pos ~(default : Parsed_term.t) ~to_term
    (computed : Parsed_term.t) =
  match computed.term with
    | `Invoke { invoked; meth } -> mk_invoke ~pos ~default ~to_term invoked meth
    | _ ->
        let null = mk ~pos (`Var "null") in
        let op =
          mk ~pos
            (`Invoke
              { invoked = null; invoke_default = None; meth = "default" })
        in
        let handler = mk_fun ~pos [] (to_term default) in
        `App (op, [("", to_term computed); ("", handler)])

let get_reducer ~pos ~to_term = function
  | `Get tm ->
      Printf.eprintf
        "Warning, %s: the notation !x for references is deprecated, please use \
         x() instead.\n\
         %!"
        (Pos.to_string pos);
      `App (to_term tm, [])

let set_reducer ~pos ~to_term = function
  | `Set (tm, v) ->
      let op =
        mk ~pos
          (`Invoke
            { invoked = to_term tm; invoke_default = None; meth = "set" })
      in
      `App (op, [("", to_term v)])

let if_reducer ~pos ~to_term = function
  | `Inline_if { if_condition; if_then; if_elsif; if_else }
  | `If { if_condition; if_then; if_elsif; if_else } ->
      let if_else =
        match if_else with None -> mk ~pos (`Tuple []) | Some t -> to_term t
      in
      let term =
        List.fold_left
          (fun if_else (condition, _then) ->
            let op = mk ~pos (`Var "if") in
            mk ~pos
              (`App
                ( op,
                  [
                    ("", to_term condition);
                    ("then", mk_fun ~pos [] (to_term _then));
                    ("else", mk_fun ~pos [] if_else);
                  ] )))
          if_else
          (List.rev ((if_condition, if_then) :: if_elsif))
      in
      term.term

let pp_if_reducer ~pos ~to_term = function
  | `If_def { if_def_negative; if_def_condition; if_def_then; if_def_else } -> (
      let if_def_else =
        Option.value ~default:(mk_parsed ~pos (`Tuple [])) if_def_else
      in
      match (Environment.has_builtin if_def_condition, if_def_negative) with
        | true, false | false, true -> (to_term if_def_then).term
        | _ -> (to_term if_def_else).term)
  | `If_version
      { if_version_op; if_version_version; if_version_then; if_version_else }
    -> (
      let if_version_else =
        Option.value ~default:(mk_parsed ~pos (`Tuple [])) if_version_else
      in
      let current_version =
        Lang_string.Version.of_string Build_config.version
      in
      match
        ( if_version_op,
          Lang_string.Version.compare current_version if_version_version )
      with
        | `Eq, 0 -> (to_term if_version_then).term
        | `Geq, v when v >= 0 -> (to_term if_version_then).term
        | `Leq, v when v <= 0 -> (to_term if_version_then).term
        | `Gt, v when v > 0 -> (to_term if_version_then).term
        | `Lt, v when v < 0 -> (to_term if_version_then).term
        | _ -> (to_term if_version_else).term)
  | `If_encoder
      {
        if_encoder_negative;
        if_encoder_condition;
        if_encoder_then;
        if_encoder_else;
      } -> (
      let if_encoder_else =
        Option.value ~default:(mk_parsed ~pos (`Tuple [])) if_encoder_else
      in
      try
        let encoder =
          !Hooks.make_encoder ~pos:None (mk Term.unit) (if_encoder_condition, [])
        in
        match (!Hooks.has_encoder encoder, if_encoder_negative) with
          | true, false | false, true -> (to_term if_encoder_then).term
          | _ -> (to_term if_encoder_else).term
      with _ -> (to_term if_encoder_else).term)

let while_reducer ~pos ~to_term = function
  | `While { while_condition; while_loop } ->
      let op = mk ~pos (`Var "while") in
      let while_condition = mk_fun ~pos [] (to_term while_condition) in
      let while_loop = mk_fun ~pos [] (to_term while_loop) in
      `App (op, [("", while_condition); ("", while_loop)])

let base_for_reducer ~pos for_variable for_iterator for_loop =
  let for_op = mk ~pos (`Var "for") in
  let for_loop =
    mk_fun ~pos
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

let iterable_for_reducer ~pos ~to_term = function
  | `Iterable_for
      { iterable_for_variable; iterable_for_iterator; iterable_for_loop } ->
      base_for_reducer ~pos iterable_for_variable
        (to_term iterable_for_iterator)
        (to_term iterable_for_loop)

let for_reducer ~pos ~to_term = function
  | `For { for_variable; for_from; for_to; for_loop } ->
      let to_op = mk ~pos (`Var "iterator") in
      let to_op =
        mk ~pos
          (`Invoke { invoked = to_op; invoke_default = None; meth = "int" })
      in
      let for_condition =
        mk ~pos (`App (to_op, [("", to_term for_from); ("", to_term for_to)]))
      in
      base_for_reducer ~pos for_variable for_condition (to_term for_loop)

let infix_reducer ~pos ~to_term = function
  | `Infix (tm, op, tm') ->
      let op = mk ~pos (`Var op) in
      `App (op, [("", to_term tm); ("", to_term tm')])

let bool_reducer ~pos ~to_term = function
  | `Bool (op, tm :: terms) ->
      List.fold_left
        (fun tm tm' ->
          let op = mk ~pos (`Var op) in
          let tm = mk_fun ~pos [] (mk ~pos tm) in
          let tm' = mk_fun ~pos [] (to_term tm') in
          `App (op, [("", tm); ("", tm')]))
        (to_term tm).term terms
  | `Bool (_, []) -> assert false

let simple_fun_reducer ~pos:_ ~to_term = function
  | `Simple_fun tm ->
      `Fun { name = None; arguments = []; body = to_term tm; free_vars = None }

let negative_reducer ~pos ~to_term = function
  | `Negative tm ->
      let op = mk ~pos (`Var "~-") in
      `App (op, [("", to_term tm)])

let not_reducer ~pos ~to_term = function
  | `Not tm ->
      let op = mk ~pos (`Var "not") in
      `App (op, [("", to_term tm)])

let append_term ~pos a b =
  let op = mk ~pos (`Var "_::_") in
  `App (op, [("", a); ("", b)])

let append_reducer ~pos ~to_term = function
  | `Append (tm, tm') -> append_term ~pos (to_term tm) (to_term tm')

let rec list_reducer ~pos ?(cur = `List []) ~to_term l =
  match (l, cur) with
    | [], cur -> cur
    | `Term v :: rem, `List cur ->
        list_reducer ~cur:(`List (to_term v :: cur)) ~pos ~to_term rem
    | `Term v :: rem, cur ->
        let cur = append_term ~pos (to_term v) (mk ~pos cur) in
        list_reducer ~pos ~cur ~to_term rem
    | `Ellipsis v :: rem, cur ->
        let list = mk ~pos (`Var "list") in
        let op =
          mk ~pos
            (`Invoke { invoked = list; invoke_default = None; meth = "append" })
        in
        let cur = `App (op, [("", to_term v); ("", mk ~pos cur)]) in
        list_reducer ~pos ~cur ~to_term rem

let assoc_reducer ~pos ~to_term = function
  | `Assoc (tm, tm') ->
      let op = mk ~pos (`Var "_[_]") in
      `App (op, [("", to_term tm); ("", to_term tm')])

let regexp_reducer ~pos ~to_term:_ = function
  | `Regexp (regexp, flags) ->
      let regexp = render_string ~pos ~sep:'/' regexp in
      let regexp = mk ~pos (`Ground (Term_base.Ground.String regexp)) in
      let flags = List.map Char.escaped flags in
      let flags =
        List.map (fun s -> mk ~pos (`Ground (Term_base.Ground.String s))) flags
      in
      let flags = mk ~pos (`List flags) in
      let op = mk ~pos (`Var "regexp") in
      `App (op, [("", regexp); ("flags", flags)])

let try_reducer ~pos ~to_term = function
  | `Try { try_body; try_variable; try_errors_list; try_handler } ->
      let try_body = mk_fun ~pos [] (to_term try_body) in
      let err_arg =
        [
          {
            label = "";
            as_variable = Some try_variable;
            typ = Type.var ();
            default = None;
          };
        ]
      in
      let handler = mk_fun ~pos:try_handler.pos err_arg (to_term try_handler) in
      let error_module = mk ~pos (`Var "error") in
      let try_errors_list = to_term try_errors_list in
      let op =
        mk ~pos
          (`Invoke
            { invoked = error_module; invoke_default = None; meth = "catch" })
      in
      `App (op, [("errors", try_errors_list); ("", try_body); ("", handler)])

let mk_let_json_parse ~pos (args, pat, def, cast) body =
  let ty = match cast with Some ty -> ty | None -> Type.var ~pos () in
  let tty = Value.RuntimeType.to_term ty in
  let json5 =
    match List.assoc_opt "json5" args with
      | Some v -> v
      | None -> Term.(make (`Ground (Ground.Bool false)))
  in
  let parser = mk ~pos (`Var "_internal_json_parser_") in
  let def =
    mk ~pos (`App (parser, [("json5", json5); ("type", tty); ("", def)]))
  in
  let def = mk ~pos (`Cast (def, ty)) in
  `Let { Term_base.doc = None; replace = false; pat; gen = []; def; body }

let mk_let_yaml_parse ~pos (pat, def, cast) body =
  let ty = match cast with Some ty -> ty | None -> Type.var ~pos () in
  let tty = Value.RuntimeType.to_term ty in
  let parser = mk ~pos (`Var "_internal_yaml_parser_") in
  let def = mk ~pos (`App (parser, [("type", tty); ("", def)])) in
  let def = mk ~pos (`Cast (def, ty)) in
  `Let { Term_base.doc = None; replace = false; pat; gen = []; def; body }

let mk_rec_fun ~pos pat arguments body =
  let name =
    match pat with
      | `PVar l when l <> [] -> List.hd (List.rev l)
      | _ -> assert false
  in
  mk ~pos (`Fun { name = Some name; arguments; body; free_vars = None })

let mk_eval ~pos (pat, def, body, cast) =
  let ty = match cast with Some ty -> ty | None -> Type.var ~pos () in
  let tty = Value.RuntimeType.to_term ty in
  let eval = mk ~pos (`Var "_eval_") in
  let def = mk ~pos (`App (eval, [("type", tty); ("", def)])) in
  let def = mk ~pos (`Cast (def, ty)) in
  `Let { Term_base.doc = None; replace = false; pat; gen = []; def; body }

let string_of_let_decoration = function
  | `None -> ""
  | `Recursive -> "rec"
  | `Replaces -> "replaces"
  | `Eval -> "eval"
  | `Yaml_parse -> "yaml.parse"
  | `Json_parse _ -> "json.parse"

let mk_let ~pos ~to_term ({ decoration; pat; arglist; def; cast }, body) =
  let def = to_term def in
  let body = to_term body in
  let cast = Option.map (Parser_helper.mk_ty ~pos) cast in
  let arglist = Option.map (expand_argsof ~pos ~to_term) arglist in
  match (arglist, decoration) with
    | Some arglist, `None | Some arglist, `Replaces ->
        let replace = decoration = `Replaces in
        let def = mk_fun ~pos arglist def in
        let def =
          match cast with Some ty -> mk ~pos (`Cast (def, ty)) | None -> def
        in
        `Let { Term_base.doc = None; replace; pat; gen = []; def; body }
    | Some arglist, `Recursive ->
        let def = mk_rec_fun ~pos pat arglist def in
        let def =
          match cast with Some ty -> mk ~pos (`Cast (def, ty)) | None -> def
        in
        `Let { Term_base.doc = None; replace = false; pat; gen = []; def; body }
    | None, `None | None, `Replaces ->
        let replace = decoration = `Replaces in
        let def =
          match cast with Some ty -> mk ~pos (`Cast (def, ty)) | None -> def
        in
        `Let { Term_base.doc = None; replace; pat; gen = []; def; body }
    | None, `Eval -> mk_eval ~pos (pat, def, body, cast)
    | None, `Json_parse args ->
        let args = List.map (fun (l, v) -> (l, to_term v)) args in
        mk_let_json_parse ~pos (args, pat, def, cast) body
    | None, `Yaml_parse -> mk_let_yaml_parse ~pos (pat, def, cast) body
    | Some _, v ->
        parse_error ~pos
          (string_of_let_decoration v
         ^ " does not apply to function assignments")
    | None, v ->
        parse_error ~pos
          (string_of_let_decoration v ^ " only applies to function assignments")

let rec concat_term t t' =
  match t with
    | { term = `Let p } as t ->
        { t with term = `Let { p with body = concat_term p.body t' } }
    | { term = `Seq (s, s') } -> { t with term = `Seq (s, concat_term s' t') }
    | _ -> mk ?pos:t.t.Type.pos (`Seq (t, t'))

exception No_extra

let program = MenhirLib.Convert.Simplified.traditional2revised Parser.program

let mk_expr ?fname processor lexbuf =
  let tokenizer = Preprocessor.mk_tokenizer ?fname lexbuf in
  Parser_helper.clear_comments ();
  let parsed_term = processor tokenizer in
  Parser_helper.attach_comments ~pos:parsed_term.Parsed_term.pos parsed_term;
  parsed_term

let includer_reducer ~to_term = function
  | `Include { inc_type; inc_name; inc_pos } -> (
      try
        let fname =
          match inc_type with
            | `Lib -> Filename.concat (!Hooks.liq_libs_dir ()) inc_name
            | v -> (
                try
                  let current_dir =
                    Filename.dirname (fst inc_pos).Lexing.pos_fname
                  in
                  Utils.check_readable ~current_dir ~pos:[inc_pos] inc_name
                with _ when v = `Extra -> raise No_extra)
        in
        let ic = open_in fname in
        let term =
          Fun.protect
            ~finally:(fun () -> close_in ic)
            (fun () ->
              let lexbuf = Sedlexing.Utf8.from_channel ic in
              mk_expr ~fname program lexbuf)
        in
        (to_term term).term
      with No_extra -> `Tuple [])

let rec to_ast ~pos : parsed_ast -> Term.runtime_ast = function
  | `Include _ as ast -> includer_reducer ~to_term ast
  | `Get _ as ast -> get_reducer ~pos ~to_term ast
  | `Set _ as ast -> set_reducer ~pos ~to_term ast
  | `Inline_if _ as ast -> if_reducer ~pos ~to_term ast
  | `If _ as ast -> if_reducer ~pos ~to_term ast
  | (`If_def _ as ast) | (`If_encoder _ as ast) | (`If_version _ as ast) ->
      pp_if_reducer ~pos ~to_term ast
  | `While _ as ast -> while_reducer ~pos ~to_term ast
  | `For _ as ast -> for_reducer ~pos ~to_term ast
  | `Iterable_for _ as ast -> iterable_for_reducer ~pos ~to_term ast
  | `Not _ as ast -> not_reducer ~pos ~to_term ast
  | `Negative _ as ast -> negative_reducer ~pos ~to_term ast
  | `Append _ as ast -> append_reducer ~pos ~to_term ast
  | `Assoc _ as ast -> assoc_reducer ~pos ~to_term ast
  | `Infix _ as ast -> infix_reducer ~pos ~to_term ast
  | `Bool _ as ast -> bool_reducer ~pos ~to_term ast
  | `Simple_fun _ as ast -> simple_fun_reducer ~pos ~to_term ast
  | `Regexp _ as ast -> regexp_reducer ~pos ~to_term ast
  | `Try _ as ast -> try_reducer ~pos ~to_term ast
  | `String_interpolation (sep, l) ->
      let l =
        List.map
          (function
            | `String s ->
                `Term
                  (mk_parsed ~pos
                     (`Ground (Term.Ground.String (render_string ~pos ~sep s))))
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
      to_ast ~pos (`App (op, [`Term ("", mk_parsed ~pos (`List l))]))
  | `Def p | `Let p | `Binding p -> mk_let ~pos ~to_term p
  | `Coalesce (t, default) -> mk_coalesce ~pos ~to_term ~default t
  | `Time t -> mk_time_pred ~pos (during ~pos t)
  | `Time_interval (t, t') -> mk_time_pred ~pos (between ~pos t t')
  | `Ground g -> `Ground g
  | `Encoder e -> `Encoder (to_encoder e)
  | `List l -> list_reducer ~pos ~to_term (List.rev l)
  | `Tuple l -> `Tuple (List.map to_term l)
  | `String (sep, s) -> `Ground (String (render_string ~pos ~sep s))
  | `Int i -> `Ground (Int (int_of_string i))
  | `Float (ipart, fpart) ->
      let fpart =
        if fpart = "" then 0.
        else float_of_string fpart /. (10. ** float_of_int (String.length fpart))
      in
      let ipart = if ipart = "" then 0. else float_of_string ipart in
      `Ground (Float (ipart +. fpart))
  | `Eof -> `Tuple []
  | `Null -> `Null
  | `Cast (t, typ) -> `Cast (to_term t, Parser_helper.mk_ty ~pos typ)
  | `Invoke { invoked; optional; meth } ->
      let default = if optional then Some (mk_parsed ~pos `Null) else None in
      mk_invoke ~pos ?default ~to_term invoked meth
  | `Open (t, t') -> `Open (to_term t, to_term t')
  | `Var s -> `Var s
  | `Seq (t, t') -> `Seq (to_term t, to_term t')
  | `App (t, args) ->
      let args = expand_appof ~pos ~to_term args in
      `App (to_term t, args)
  | `Fun (args, body) -> `Fun (to_func ~pos ~to_term args body)
  | `RFun (name, args, body) -> `Fun (to_func ~pos ~to_term ~name args body)
  | `Parenthesis _ | `Block _ | `Methods _ -> assert false

and to_func ~pos ~to_term ?name arguments body =
  {
    name;
    arguments = expand_argsof ~pos ~to_term arguments;
    body = to_term body;
    free_vars = None;
  }

and to_encoder_string = function
  | `Verbatim s -> s
  | `String (pos, (sep, s)) -> render_string ~pos ~sep s

and to_encoder_params l =
  List.map
    (function
      | `Anonymous s -> `Anonymous (to_encoder_string s)
      | `Labelled (s, t) -> `Labelled (to_encoder_string s, to_term t)
      | `Encoder e -> `Encoder (to_encoder e))
    l

and to_encoder (lbl, params) = (lbl, to_encoder_params params)

and to_term (tm : Parsed_term.t) : Term.t =
  match tm.term with
    | `Seq (({ term = `Include _ } as t), t')
    | `Seq (({ term = `If_def _ } as t), t')
    | `Seq (({ term = `If_encoder _ } as t), t')
    | `Seq (({ term = `If_version _ } as t), t') ->
        concat_term (to_term t) (to_term t')
    | `Parenthesis tm | `Block tm -> to_term tm
    | `Methods { base; methods } ->
        let term, base_methods =
          match base with
            | `None -> (`Tuple [], Methods.empty)
            | `Term tm ->
                let { term; methods } = to_term tm in
                (term, methods)
            | `Spread tm ->
                let term = to_term tm in
                (* let _ = tm in let replaces _ = () in _ *)
                let { term; methods } =
                  mk ~pos:tm.pos
                    (`Let
                      {
                        doc = None;
                        replace = false;
                        pat = `PVar ["_"];
                        gen = [];
                        def = term;
                        body =
                          mk ~pos:tm.pos
                            (`Let
                              {
                                doc = None;
                                replace = true;
                                pat = `PVar ["_"];
                                gen = [];
                                def = mk ~pos:tm.pos (`Tuple []);
                                body = mk ~pos:tm.pos (`Var "_");
                              });
                      })
                in
                (term, methods)
        in
        let methods =
          List.fold_left
            (fun methods (k, v) -> Methods.add k (to_term v) methods)
            base_methods methods
        in
        { t = Type.var ~pos:tm.pos (); term; methods }
    | term ->
        let term =
          match (to_ast ~pos:tm.pos term, List.rev tm.comments) with
            | `Let p, (pos, doc) :: _ ->
                `Let
                  { p with doc = Doc.parse_doc ~pos (String.concat "\n#" doc) }
            | ast, _ -> ast
        in
        { t = Type.var ~pos:tm.pos (); term; methods = Methods.empty }
