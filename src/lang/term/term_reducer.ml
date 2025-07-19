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

type processor = Term_preprocessor.processor

open Parsed_term
include Runtime_term

let report_annotations ~throw ~pos annotations =
  List.iter
    (function
      | `Deprecated s ->
          let bt = Printexc.get_callstack 0 in
          throw ~bt (Term.Deprecated (s, Pos.of_lexing_pos pos)))
    annotations

let parse_error ~pos msg = raise (Term_base.Parse_error (pos, msg))
let render_string ~pos ~sep s = Lexer.render_string ~pos ~sep s
let mk ?pos = Term.make ?pos:(Option.map Pos.of_lexing_pos pos)
let mk_ty ?pos = Type.make ?pos:(Option.map Pos.of_lexing_pos pos)
let mk_var ?pos = Type.var ?pos:(Option.map Pos.of_lexing_pos pos)
let mk_parsed = Parsed_term.make

let mk_fun ~pos arguments body =
  mk ~pos (`Fun Term.{ free_vars = None; name = None; arguments; body })

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

let typecheck = ref (fun ?env:_ _ -> assert false)

let rec mk_parsed_ty ?pos ~env ~to_term = function
  | `Named s -> mk_named_ty ?pos s
  | `Nullable t ->
      Type.(
        make
          ?pos:(Option.map Pos.of_lexing_pos pos)
          (Nullable (mk_parsed_ty ?pos ~env ~to_term t)))
  | `List t ->
      Type.(
        make
          ?pos:(Option.map Pos.of_lexing_pos pos)
          (List { t = mk_parsed_ty ?pos ~env ~to_term t; json_repr = `Tuple }))
  | `Json_object t ->
      Type.(
        make
          (List
             {
               t = mk_parsed_ty ?pos ~env ~to_term (`Tuple [`Named "string"; t]);
               json_repr = `Object;
             }))
  | `Tuple l ->
      Type.(
        make
          ?pos:(Option.map Pos.of_lexing_pos pos)
          (Tuple (List.map (mk_parsed_ty ~env ~to_term ?pos) l)))
  | `Arrow (args, t) ->
      Type.(
        make
          (Arrow
             ( List.map
                 (fun (optional, name, t) ->
                   (optional, name, mk_parsed_ty ~env ~to_term ?pos t))
                 args,
               mk_parsed_ty ?pos ~env ~to_term t )))
  | `Record l ->
      List.fold_left (mk_meth_ty ?pos ~env ~to_term) Type.(make (Tuple [])) l
  | `Method (t, l) ->
      List.fold_left
        (mk_meth_ty ?pos ~env ~to_term)
        (mk_parsed_ty ?pos ~env ~to_term t)
        l
  | `Invoke (t, s) -> snd (Type.invoke (mk_parsed_ty ?pos ~env ~to_term t) s)
  | `Source (s, p) -> mk_source_ty ?pos s p

and mk_meth_ty ?pos ~env ~to_term base
    { Parsed_term.name; optional_meth = optional; typ; json_name } =
  Type.(
    make
      (Meth
         ( {
             meth = name;
             optional;
             scheme = ([], mk_parsed_ty ?pos ~env ~to_term typ);
             doc = { meth_descr = ""; category = `Method };
             json_name;
           },
           base )))

let program = Term_preprocessor.program

let mk_expr ?fname processor lexbuf =
  let parsed_term = Term_preprocessor.mk_expr ?fname processor lexbuf in
  Term_preprocessor.expand_term parsed_term

let pp_if_reducer ~env ~pos = function
  | `If_def { if_def_negative; if_def_condition; if_def_then; if_def_else } -> (
      let if_def_else =
        Option.value ~default:(mk_parsed ~pos (`Tuple [])) if_def_else
      in
      match
        ( List.mem_assoc if_def_condition env
          || Environment.has_builtin if_def_condition,
          if_def_negative )
      with
        | true, false | false, true -> if_def_then
        | _ -> if_def_else)
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
        | `Eq, 0 -> if_version_then
        | `Geq, v when v >= 0 -> if_version_then
        | `Leq, v when v <= 0 -> if_version_then
        | `Gt, v when v > 0 -> if_version_then
        | `Lt, v when v < 0 -> if_version_then
        | _ -> if_version_else)
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
          !Hooks.make_encoder ~pos:None (if_encoder_condition, [])
        in
        match (!Hooks.has_encoder encoder, if_encoder_negative) with
          | true, false | false, true -> if_encoder_then
          | _ -> if_encoder_else
      with _ -> if_encoder_else)

let pat_var_name =
  let idx = ref 1 in
  fun () ->
    incr idx;
    Printf.sprintf "_%d_pat" !idx

let rec pattern_reducer (pat : Parsed_term.pattern) =
  let mk = mk ~pos:pat.pat_pos in
  match pat.pat_entry with
    | `PVar _ as pat ->
        fun ?doc ?(replace = false) ~body def ->
          `Let { doc; replace; pat; gen = []; def; body }
    | `PTuple l ->
        fun ?doc ?(replace = false) ~body def ->
          let vars, body =
            List.fold_left
              (fun (vars, body) pat ->
                match pat.pat_entry with
                  | `PVar [var] -> (var :: vars, body)
                  (* let (<var>, pattern, ...) = .. in .. becomes:
                     let (<var>, _1_pat, ...) = .. in
                     let pattern = _1_pat in
                     .. *)
                  | _ ->
                      let var = pat_var_name () in
                      let mk_term = pattern_reducer pat in
                      let body = mk (mk_term ~body (mk (`Var var))) in
                      (var :: vars, body))
              ([], body) l
          in
          `Let
            { doc; replace; pat = `PTuple (List.rev vars); gen = []; def; body }
    (* let [x, y, ...spread, z, t] = ... in ... becomes:
       let _1_pat = ... in
       let _2_pat = list.length(l) in
       if _2_pat < 4 then error.raise(error.register("not_found", ...)) end
       let x = list.nth(0, _1_pat) in
       let y = list.nth(1, _1_pat) in
       let spread = list.slice(offset=2, length=_2_pat-4) in
       let z = list.nth(_2_pat-1, l) in
       let t = list.nth(_2_pat-2, l) in
       ... *)
    | `PList (prefix, spread, suffix) ->
        fun ?doc ?replace ~body def ->
          let list_var_name = pat_var_name () in
          let list_var = mk (`Var list_var_name) in
          let list = mk (`Var "list") in
          let nth_op =
            mk (`Invoke { invoked = list; meth = "nth"; invoke_default = None })
          in
          let len_op =
            mk
              (`Invoke
                 { invoked = list; meth = "length"; invoke_default = None })
          in
          let len_app = mk (`App (len_op, [("", list_var)])) in
          let len_var_name = pat_var_name () in
          let len_var = mk (`Var len_var_name) in
          let minus = mk (`Var "-") in
          let len_minus pos =
            mk (`App (minus, [("", len_var); ("", mk (`Int pos))]))
          in
          let body, suffix_len =
            match suffix with
              | [] -> (body, 0)
              | suffix ->
                  let _, body =
                    List.fold_left
                      (fun (idx, body) pat ->
                        let mk_term = pattern_reducer pat in
                        let body =
                          mk
                            (mk_term ~body
                               (mk
                                  (`App
                                     ( nth_op,
                                       [("", list_var); ("", len_minus idx)] ))))
                        in
                        (idx + 1, body))
                      (1, body) (List.rev suffix)
                  in
                  (body, List.length suffix)
          in
          let body, prefix_len =
            match prefix with
              | [] -> (body, 0)
              | prefix ->
                  let _, body =
                    List.fold_left
                      (fun (idx, body) pat ->
                        let mk_term = pattern_reducer pat in
                        let body =
                          mk
                            (mk_term ~body
                               (Term.make
                                  (`App
                                     ( nth_op,
                                       [("", list_var); ("", mk (`Int idx))] ))))
                        in
                        (idx + 1, body))
                      (0, body) prefix
                  in
                  (body, List.length prefix)
          in
          let body =
            match spread with
              | None -> body
              | Some (pos, var) ->
                  let op =
                    mk
                      (`Invoke
                         {
                           invoked = list;
                           meth = "slice";
                           invoke_default = None;
                         })
                  in
                  let def =
                    mk
                      (`App
                         ( op,
                           [
                             ("offset", mk (`Int prefix_len));
                             ("length", len_minus (prefix_len + suffix_len));
                             ("", list_var);
                           ] ))
                  in
                  let mk_term =
                    pattern_reducer { pat_pos = pos; pat_entry = `PVar [var] }
                  in
                  mk (mk_term ~body def)
          in
          let body =
            let if_op = mk (`Var "if") in
            let lt_op = mk (`Var "<") in
            let condition =
              mk
                (`App
                   ( lt_op,
                     [("", len_var); ("", mk (`Int (prefix_len + suffix_len)))]
                   ))
            in
            let error = mk (`Var "error") in
            let raise =
              mk
                (`Invoke
                   { invoked = error; meth = "raise"; invoke_default = None })
            in
            let register =
              mk
                (`Invoke
                   { invoked = error; meth = "register"; invoke_default = None })
            in
            let not_found =
              mk (`App (register, [("", mk (`String "not_found"))]))
            in
            let _then =
              mk_fun ~pos:pat.pat_pos []
                (mk
                   (`App
                      ( raise,
                        [
                          ("", not_found);
                          ( "",
                            mk
                              (`String
                                 "List value does not have enough elements to \
                                  fit the extraction pattern!") );
                        ] )))
            in
            let check_len =
              mk
                (`App
                   ( if_op,
                     [
                       ("", condition);
                       ("then", _then);
                       ("else", mk_fun ~pos:pat.pat_pos [] (mk (`Tuple [])));
                     ] ))
            in
            mk (`Seq (check_len, body))
          in
          let mk_term =
            pattern_reducer { pat with pat_entry = `PVar [len_var_name] }
          in
          let body = mk (mk_term ~body len_app) in
          let mk_term =
            pattern_reducer { pat with pat_entry = `PVar [list_var_name] }
          in
          mk_term ?doc ?replace ~body def
    (* let <pat>.{m = <pat'>; p?; q } = .. in .. becomes:
       let _1_pat = .. in
       let _2_pat = _.m in
       let <pat'> = _2_pat in
       let p = _1_pat?.p in
       let p = _1_pat.q in
       let <pat> = _1_pat in
       ... *)
    | `PMeth (base, meths) ->
        fun ?doc ?replace ~body def ->
          let base_var_name = pat_var_name () in
          let base_var = mk (`Var base_var_name) in
          let body =
            List.fold_left
              (fun body (name, default) ->
                let invoke invoke_default =
                  mk
                    (`Invoke { invoked = base_var; meth = name; invoke_default })
                in
                match default with
                  | `None ->
                      let mk_term =
                        pattern_reducer { pat with pat_entry = `PVar [name] }
                      in
                      mk (mk_term ~body (invoke None))
                  | `Nullable ->
                      let mk_term =
                        pattern_reducer { pat with pat_entry = `PVar [name] }
                      in
                      mk (mk_term ~body (invoke (Some (mk `Null))))
                  | `Pattern pat ->
                      let mk_term = pattern_reducer pat in
                      mk (mk_term ~body (invoke None)))
              body (List.rev meths)
          in
          let body =
            match base with
              | None -> body
              | Some pat ->
                  let mk_term = pattern_reducer pat in
                  let body_var = mk (`Var base_var_name) in
                  mk
                    (mk_term ~body
                       (mk
                          (`Hide
                             (body_var, List.map (fun (name, _) -> name) meths))))
          in
          let mk_term =
            pattern_reducer { pat with pat_entry = `PVar [base_var_name] }
          in
          mk_term ?doc ?replace ~body def

let pattern_reducer ?doc ?replace ~body ~pat def =
  pattern_reducer ~body pat ?doc ?replace def

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

let rec get_env_args ~pos t args =
  let get_arg_type t name =
    match (Type.deref t).Type.descr with
      | Type.Arrow (l, _) ->
          let _, _, t = List.find (fun (_, n, _) -> n = name) l in
          t
      | _ ->
          parse_error ~pos
            (Printf.sprintf
               "Cannot get argument type of %s, this is not a function, it has \
                type: %s."
               name (Type.to_string t))
  in
  List.map
    (fun (n, n', v) ->
      let t = mk_ty ~pos (get_arg_type t n).Type.descr in
      let as_variable = if n = n' then None else Some n' in
      {
        label = n;
        as_variable;
        typ = t;
        default = Option.map (term_of_value ~pos ~name:n t) v;
        pos = t.pos;
      })
    args

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
    let mk_tm ?(flags = Flags.empty) term =
      mk ~flags ~t:(mk_ty ~pos t.Type.descr) term
    in
    match v with
      | Value.Int { value = i; flags } -> mk_tm ~flags (`Int i)
      | Value.Float { value = f } -> mk_tm (`Float f)
      | Value.Bool { value = b } -> mk_tm (`Bool b)
      | Value.String { value = s } -> mk_tm (`String s)
      | Value.Custom { value = g } -> mk_tm (`Custom g)
      | Value.List { value = l } ->
          mk_tm
            (`List (List.map (term_of_value_base ~pos (get_list_type ())) l))
      | Value.Tuple { value = l } ->
          mk_tm
            (`Tuple
               (List.mapi
                  (fun idx v -> term_of_value_base ~pos (get_tuple_type idx) v)
                  l))
      | Value.Null _ -> mk_tm `Null
      (* Ignoring env is not correct here but this is an internal operator
         so we have to trust that devs using it via %argsof now that they are doing. *)
      | Value.Fun { fun_args = args; fun_body = body } ->
          let body =
            mk
              ~t:(mk_ty ~pos body.t.Type.descr)
              ~methods:body.Term.methods body.Term.term
          in
          mk_tm
            (`Fun
               {
                 Term_base.name = None;
                 arguments = get_env_args ~pos t args;
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
         Value.(methods v))
    tm.Term.term

and term_of_value ~pos ~name t v =
  try term_of_value_base ~pos t v
  with _ ->
    parse_error ~pos
      (Printf.sprintf "Argument %s: value %s cannot be represented as a term"
         name (Value.to_string v))

let builtin_args_of ~only ~except ~pos name =
  match Environment.get_builtin name with
    | Some ((_, t), Value.Fun { fun_args = args })
    | Some ((_, t), Value.FFI { ffi_args = args; _ }) ->
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
        get_env_args ~pos t filtered_args
    | Some _ ->
        parse_error ~pos (Printf.sprintf "Builtin %s is not a function!" name)
    | None ->
        parse_error ~pos (Printf.sprintf "Builtin %s is not registered!" name)

let args_of ~only ~except ~pos ~env name =
  match List.assoc_opt name env with
    | Some { term = `Fun { arguments } } ->
        let filtered_args =
          List.filter (fun { label } -> label <> "") arguments
        in
        let filtered_args =
          if only <> [] then
            List.map
              (fun n ->
                try List.find (fun { label } -> label = n) filtered_args
                with Not_found ->
                  parse_error ~pos
                    (Printf.sprintf "%s does not have an argument named %s" name
                       n))
              only
          else filtered_args
        in
        List.iter
          (fun n ->
            match List.find_opt (fun { label } -> label = n) filtered_args with
              | Some _ -> ()
              | None ->
                  parse_error ~pos
                    (Printf.sprintf "%s does not have an argument named %s" name
                       n))
          except;
        List.filter (fun { label } -> not (List.mem label except)) filtered_args
    | Some _ -> parse_error ~pos (Printf.sprintf "%s is not a function!" name)
    | None -> builtin_args_of ~only ~except ~pos name

let expand_argsof ~pos ~env ~to_term ~throw args =
  let anonymous_var_id = ref 0 in
  let mk_def, args =
    List.fold_left
      (fun (mk_def, args) -> function
        | `Argsof { only; except; source } ->
            (mk_def, List.rev (args_of ~pos ~env ~only ~except source) @ args)
        | `Term { label; as_variable; default; typ; annotations; pos } ->
            report_annotations ~throw ~pos annotations;
            let mk_def, as_variable =
              match as_variable with
                | None -> (mk_def, None)
                | Some { pat_entry = `PVar [v] } -> (mk_def, Some v)
                | Some pat ->
                    incr anonymous_var_id;
                    let v = Printf.sprintf "_ann_%d" !anonymous_var_id in
                    let mk_def def =
                      mk_def (mk (pattern_reducer ~body:def ~pat (mk (`Var v))))
                    in
                    (mk_def, Some v)
            in
            ( mk_def,
              {
                label;
                as_variable;
                typ =
                  (match typ with
                    | None -> mk_var ()
                    | Some typ -> mk_parsed_ty ~env ~to_term typ);
                default = Option.map (to_term ~env) default;
                pos = Some (Pos.of_lexing_pos pos);
              }
              :: args ))
      ((fun b -> b), [])
      args
  in
  (mk_def, List.rev args)

let app_of ~pos ~only ~except ~env source =
  let args = args_of ~pos ~only ~except ~env source in
  List.map (fun { label } -> (label, mk ~t:(mk_var ~pos ()) (`Var label))) args

let expand_appof ~pos ~env ~to_term args =
  List.rev
    (List.fold_left
       (fun args -> function
         | `Argsof { only; except; source } ->
             List.rev (app_of ~pos ~only ~except ~env source) @ args
         | `Term (l, v) -> (l, to_term ~env v) :: args)
       [] args)

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
          Term_base.label;
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
  | `Inline_if { if_condition; if_then; if_elsif; if_else }
  | `If { if_condition; if_then; if_elsif; if_else } ->
      let if_else =
        match if_else with
          | None -> mk ~pos (`Tuple [])
          | Some t -> to_term ~env t
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
          if_else
          (List.rev ((if_condition, if_then) :: if_elsif))
      in
      term.term

let while_reducer ~pos ~env ~to_term = function
  | `While { while_condition; while_loop } ->
      let op = mk ~pos (`Var "while") in
      let while_condition = mk_fun ~pos [] (to_term ~env while_condition) in
      let while_loop = mk_fun ~pos [] (to_term ~env while_loop) in
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
      { iterable_for_variable; iterable_for_iterator; iterable_for_loop } ->
      base_for_reducer ~pos iterable_for_variable
        (to_term ~env iterable_for_iterator)
        (to_term ~env iterable_for_loop)

let for_reducer ~pos ~env ~to_term = function
  | `For { for_variable; for_from; for_to; for_loop } ->
      let to_op = mk ~pos (`Var "iterator") in
      let to_op =
        mk ~pos
          (`Invoke { invoked = to_op; invoke_default = None; meth = "int" })
      in
      let for_condition =
        mk ~pos
          (`App (to_op, [("", to_term ~env for_from); ("", to_term ~env for_to)]))
      in
      base_for_reducer ~pos for_variable for_condition (to_term ~env for_loop)

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
  | `Try { try_body; try_variable; try_errors_list; try_handler; try_finally }
    ->
      let try_body = mk_fun ~pos [] (to_term ~env try_body) in
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
        match try_finally with
          | None -> (pos, mk ~pos (`Tuple []))
          | Some tm -> (tm.pos, to_term ~env tm)
      in
      let finally = mk_fun ~pos:finally_pos [] finally in
      let handler_pos, handler =
        match try_handler with
          | None -> (pos, mk ~pos (`Tuple []))
          | Some tm -> (tm.pos, to_term ~env tm)
      in
      let handler = mk_fun ~pos:handler_pos err_arg handler in
      let error_module = mk ~pos (`Var "error") in
      let try_errors_list =
        match try_errors_list with
          | None -> mk ~pos `Null
          | Some tm -> to_term ~env tm
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

let to_encoder_string = function
  | `Verbatim s -> s
  | `String (pos, (sep, s)) -> render_string ~pos ~sep s

let rec to_encoder_params ~env ~to_term l =
  List.map
    (function
      | `Anonymous s -> `Anonymous (to_encoder_string s)
      | `Labelled (s, t) -> `Labelled (to_encoder_string s, to_term ~env t)
      | `Encoder e -> `Encoder (to_encoder ~to_term ~env e))
    l

and to_encoder ~env ~to_term (lbl, params) =
  (lbl, to_encoder_params ~env ~to_term params)

let rec to_ast ~throw ~env ~pos ~comments ast =
  let to_ast = to_ast ~throw in
  let to_term = to_term ~throw in
  match ast with
    | `Methods _ | `Block _ | `Parenthesis _ | `Eof | `Include _ -> assert false
    | (`If_def _ as ast) | (`If_encoder _ as ast) | (`If_version _ as ast) ->
        (to_term ~env (pp_if_reducer ~pos ~env ast)).term
    | `Get _ as ast -> get_reducer ~pos ~env ~to_term ast
    | `Set _ as ast -> set_reducer ~pos ~env ~to_term ast
    | `Inline_if _ as ast -> if_reducer ~pos ~env ~to_term ast
    | `If _ as ast -> if_reducer ~pos ~env ~to_term ast
    | `While _ as ast -> while_reducer ~pos ~env ~to_term ast
    | `For _ as ast -> for_reducer ~pos ~env ~to_term ast
    | `Iterable_for _ as ast -> iterable_for_reducer ~pos ~env ~to_term ast
    | `Not _ as ast -> not_reducer ~pos ~env ~to_term ast
    | `Negative _ as ast -> negative_reducer ~pos ~env ~to_term ast
    | `Append _ as ast -> append_reducer ~pos ~env ~to_term ast
    | `Assoc _ as ast -> assoc_reducer ~pos ~env ~to_term ast
    | `Infix _ as ast -> infix_reducer ~pos ~env ~to_term ast
    | `Bool _ as ast -> ast
    | `BoolOp _ as ast -> bool_op_reducer ~pos ~env ~to_term ast
    | `Simple_fun _ as ast -> simple_fun_reducer ~pos ~env ~to_term ast
    | `Regexp _ as ast -> regexp_reducer ~pos ~env ~to_term ast
    | `Try _ as ast -> try_reducer ~pos ~env ~to_term ast
    | `String_interpolation (sep, l) ->
        let l =
          List.map
            (function
              | `String s -> `Term (mk_parsed ~pos (`String (sep, s)))
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
        to_ast ~env ~pos ~comments
          (`App (op, [`Term ("", mk_parsed ~pos (`List l))]))
    | `Def p | `Let p | `Binding p ->
        mk_let ~throw ~pos ~env ~to_term ~comments p
    | `Coalesce (t, default) -> mk_coalesce ~pos ~env ~to_term ~default t
    | `At (t, t') -> `App (to_term ~env t', [("", to_term ~env t)])
    | `Time t -> mk_time_pred ~pos (during ~pos t)
    | `Time_interval (t, t') -> mk_time_pred ~pos (between ~pos t t')
    | `Custom _ as ast -> ast
    | `Encoder e -> `Encoder (to_encoder ~to_term ~env e)
    | `List l -> list_reducer ~pos ~env ~to_term (List.rev l)
    | `Tuple l -> `Tuple (List.map (to_term ~env) l)
    | `String (sep, s) -> `String (render_string ~pos ~sep s)
    | `Int i -> `Int (int_of_string i)
    | `Float f -> (
        try `Float (Scanf.sscanf f "%f" (fun v -> v))
        with _ ->
          parse_error ~pos (Printf.sprintf "Invalid float value: %s" f))
    | `Null -> `Null
    | `Cast { cast = t; typ } ->
        `Cast
          { cast = to_term ~env t; typ = mk_parsed_ty ~pos ~env ~to_term typ }
    | `Invoke { invoked; optional; meth } ->
        let default = if optional then Some (mk_parsed ~pos `Null) else None in
        mk_invoke ~pos ~env ?default ~to_term invoked meth
    | `Open (t, t') -> `Open (to_term ~env t, to_term ~env t')
    | `Var s -> `Var s
    | `Seq (t, t') -> `Seq (to_term ~env t, to_term ~env t')
    | `App (t, args) ->
        (match (t, args) with
          | { term = `Var "_null"; pos }, [] ->
              let bt = Printexc.get_callstack 0 in
              throw ~bt (Term.Deprecated ("use `null`", Pos.of_lexing_pos pos))
          | _ -> ());
        let args = expand_appof ~pos ~env ~to_term args in
        `App (to_term ~env t, args)
    | `Fun (args, body) -> `Fun (to_func ~throw ~pos ~env ~to_term args body)
    | `RFun (name, args, body) ->
        `Fun (to_func ~throw ~pos ~env ~to_term ~name args body)

and to_func ~pos ~env ~to_term ~throw ?name arguments body =
  let mk_def, arguments = expand_argsof ~throw ~pos ~env ~to_term arguments in
  { name; arguments; body = mk_def (to_term ~env body); free_vars = None }

and to_term ~throw ~env (tm : Parsed_term.t) : Term.t =
  let to_term = to_term ~throw in
  report_annotations ~throw ~pos:tm.pos tm.annotations;
  match tm.term with
    | `Seq ({ pos; term = `If_def _ as ast }, t')
    | `Seq ({ pos; term = `If_encoder _ as ast }, t')
    | `Seq ({ pos; term = `If_version _ as ast }, t') ->
        let t = pp_if_reducer ~pos ~env ast in
        to_term ~env (Term_preprocessor.concat_term t t')
    | `Block tm -> to_term ~env tm
    | `Parenthesis tm -> to_term ~env tm
    | `Eof -> to_term ~env { tm with term = `Tuple [] }
    | `Methods (base, methods) ->
        (* let _ = src in
           let replaces _ = dst in
           _ *)
        let replace_methods ~src dst =
          mk ~pos:tm.pos
            (`Let
               {
                 doc = None;
                 replace = false;
                 pat = `PVar ["_"];
                 gen = [];
                 def = src;
                 body =
                   mk ~pos:tm.pos
                     (`Let
                        {
                          doc = None;
                          replace = true;
                          pat = `PVar ["_"];
                          gen = [];
                          def = dst;
                          body = mk ~pos:tm.pos (`Var "_");
                        });
               })
        in
        let term =
          match base with
            | None -> mk ~pos:tm.pos (`Tuple [])
            | Some tm -> to_term ~env tm
        in
        List.fold_left
          (fun term -> function
            | `Ellipsis src -> replace_methods ~src:(to_term ~env src) term
            | `Method (name, tm) ->
                {
                  term with
                  methods = Methods.add name (to_term ~env tm) term.methods;
                })
          term methods
    | term ->
        let flags =
          match term with
            | `Int i
              when String.length i >= 2
                   && String.(lowercase_ascii (sub i 0 2)) = "0x" ->
                Flags.(add empty hex_int)
            | `Int i
              when String.length i >= 2
                   && String.(lowercase_ascii (sub i 0 2)) = "0o" ->
                Flags.(add empty octal_int)
            | _ -> Flags.empty
        in
        let term = to_ast ~throw ~env ~pos:tm.pos ~comments:tm.comments term in
        { t = mk_var ~pos:tm.pos (); term; methods = Methods.empty; flags }

let to_encoder_params ~throw =
  let to_term = to_term ~throw in
  to_encoder_params ~env:[] ~to_term

let to_term ~throw tm = to_term ~throw ~env:[] tm
