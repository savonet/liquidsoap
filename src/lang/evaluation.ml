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

(** {1 Evaluation} *)

open Term

(** [remove_first f l] removes the first element [e] of [l] such that [f e], and
    returns [e,l'] where [l'] is the list without [e]. Asserts that there is
    such an element. *)
let remove_first filter =
  let rec aux acc = function
    | [] -> assert false
    | hd :: tl ->
        if filter hd then (hd, List.rev_append acc tl) else aux (hd :: acc) tl
  in
  aux []

let eval_pat pat v =
  let aux env pat v =
    match (pat, v) with
      | `PVar x, v -> (x, v) :: env
      | `PTuple pl, { Value.value = Value.Tuple l } ->
          List.fold_left2 (fun env lbl v -> ([lbl], v) :: env) env pl l
      | _ -> assert false
  in
  aux [] pat v

module Env = struct
  type t = Value.lazy_env

  (** Find the value of a variable in the environment. *)
  let lookup (env : t) var =
    try Lazy.force (List.assoc var env)
    with Not_found ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace
        (Failure
           (Printf.sprintf "Internal error: variable %s not in environment." var))
        bt

  (** Restrict an environment to a given set of variables. *)
  let restrict (env : t) vars =
    let vars = ref vars in
    let mem x =
      if Vars.mem x !vars then (
        vars := Vars.remove x !vars;
        true)
      else false
    in
    List.filter (fun (x, _) -> mem x) env

  (** Bind a variable to a lazy value in an environment. *)
  let add_lazy (env : t) x v : t = (x, v) :: env

  (** Bind a variable to a value in an environment. *)
  let add env x v = add_lazy env x (Lazy.from_val v)

  (** Bind multiple variables in an environment. *)
  let adds env binds = List.fold_right (fun (x, v) env -> add env x v) binds env

  (** Bind multiple variables to lazy values in an environment. *)
  let adds_lazy env bind =
    List.fold_right (fun (x, v) env -> add_lazy env x v) bind env
end

let rec prepare_ast ~(env : Value.lazy_env) ~pos ~flags = function
  | ast when Term_base.is_ground_ast ast ->
      let v =
        eval_ast ~eval_check:(fun ~env:_ ~tm:_ _ -> ()) ~env ~pos ~flags ast
      in
      `Value (Value.term_val_of_val (Lazy.from_val v))
  | `Cache_env _ as v -> v
  | `Var v when List.mem_assoc v env ->
      `Value (Value.term_val_of_val (List.assoc v env))
  | `Hide (tm, l) -> (
      let tm = prepare_term ~env tm in
      tm.methods <- Methods.filter (fun m _ -> not (List.mem m l)) tm.methods;
      match tm with
        | { term = `Value v } ->
            let v = Lazy.force (Value.val_of_term_val v) in
            let v =
              {
                v with
                Value.methods =
                  Methods.filter (fun m _ -> not (List.mem m l)) v.Value.methods;
              }
            in
            `Value (Value.term_val_of_val (Lazy.from_val v))
        | _ -> `Hide (tm, l))
  | `Invoke { invoked; meth; invoke_default } -> (
      match prepare_term ~env invoked with
        | { term = `Value v } -> (
            match
              Methods.find_opt meth
                (Lazy.force (Value.val_of_term_val v)).Value.methods
            with
              | Some v -> `Value (Value.term_val_of_val (Lazy.from_val v))
              | None ->
                  `Invoke
                    {
                      invoked =
                        {
                          invoked with
                          term = `Tuple [];
                          methods = Methods.empty;
                        };
                      meth;
                      invoke_default =
                        Option.map (prepare_term ~env) invoke_default;
                    })
        | invoked ->
            `Invoke
              {
                invoked;
                meth;
                invoke_default = Option.map (prepare_term ~env) invoke_default;
              })
  | `Fun ({ arguments; body } as func) ->
      let arguments =
        List.map
          (fun ({ default } as arg) ->
            { arg with default = Option.map (prepare_term ~env) default })
          arguments
      in
      let fv = Term.free_fun_vars func in
      let env = Env.restrict env fv in
      let body = prepare_term ~env body in
      `Fun { func with arguments; body }
  | `Let { pat = `PVar [] } -> assert false
  | `Let ({ pat = `PVar [var]; replace; def; body } as _let) -> (
      match prepare_term ~env def with
        | { term = `Value v } ->
            let v = Value.val_of_term_val v in
            let v =
              match (replace, List.assoc_opt var env) with
                | true, Some env_v ->
                    let v = Lazy.force v in
                    let env_v = Lazy.force env_v in
                    Lazy.from_val { v with methods = env_v.Value.methods }
                | _ -> v
            in
            let env = (var, v) :: env in
            let body = prepare_term ~env body in
            `Seq (Term.make (`Tuple []), body)
        | def ->
            `Let
              {
                _let with
                def;
                body =
                  prepare_term
                    ~env:(List.filter (fun (lbl, _) -> lbl <> var) env)
                    body;
              })
  | `Let ({ pat = `PVar (var :: path); replace; def; body } as _let) -> (
      let def = prepare_term ~env def in
      match (List.assoc_opt var env, prepare_term ~env def) with
        | Some v, { term = `Value def } ->
            let def = Lazy.force (Value.val_of_term_val def) in
            let rec pop_path ~v = function
              | [] -> assert false
              | meth :: [] ->
                  Lazy.from_val
                    {
                      v with
                      Value.methods =
                        Methods.add meth def
                          (if replace then v.Value.methods else Methods.empty);
                    }
              | meth :: path ->
                  pop_path ~v:(Methods.find meth v.Value.methods) path
            in
            let v = pop_path ~v:(Lazy.force v) path in
            let env = (var, v) :: env in
            let body = prepare_term ~env body in
            `Seq (Term.make (`Tuple []), body)
        | v, def -> (
            let ast =
              `Let
                {
                  _let with
                  def;
                  body =
                    prepare_term
                      ~env:(List.filter (fun (lbl, _) -> lbl <> var) env)
                      body;
                }
            in
            match v with
              | None -> ast
              | Some v ->
                  `Let
                    {
                      replace = false;
                      doc = None;
                      pat = `PVar [var];
                      gen = [];
                      def = Term.make (`Value (Value.term_val_of_val v));
                      body = Term.make ast;
                    }))
  | `Let ({ pat = `PTuple l; def; body } as _let) -> (
      let def = prepare_term ~env def in
      match prepare_term ~env def with
        | { term = `Value v } ->
            let t =
              match Lazy.force (Value.val_of_term_val v) with
                | { Value.value = Tuple t } -> t
                | _ -> assert false
            in
            let env =
              List.fold_left2
                (fun env lbl v -> (lbl, Lazy.from_val v) :: env)
                env l t
            in
            `Seq (Term.make (`Tuple []), prepare_term ~env body)
        | _ ->
            let env = List.filter (fun (lbl, _) -> not (List.mem lbl l)) env in
            Runtime_term.map_ast (prepare_term ~env) (`Let _let))
  | ast -> Runtime_term.map_ast (prepare_term ~env) ast

and prepare_term ~env tm =
  {
    tm with
    term = prepare_ast ~env ~flags:tm.flags ~pos:tm.t.Type.pos tm.term;
    methods = Methods.map (prepare_term ~env) tm.methods;
  }

and prepare_fun ~eval_check ~arguments ~env body =
  (* Unlike OCaml we always evaluate default values, and we do that early. I
     think the only reason is homogeneity with FFI, which are declared with
     values as defaults. *)
  let arguments =
    List.map
      (function
        | { label; as_variable; default = Some v } ->
            ( label,
              Option.value ~default:label as_variable,
              Some (eval ~eval_check env v) )
        | { label; as_variable; default = None } ->
            (label, Option.value ~default:label as_variable, None))
      arguments
  in
  (arguments, prepare_term ~env body)

and apply ?(pos = []) ~eval_check f l =
  let apply_pos = match pos with [] -> None | p :: _ -> Some p in
  (* Extract the components of the function, whether it's explicit or foreign. *)
  let p, f =
    match f.Value.value with
      | Value.Fun { fun_args = p; fun_body = body } ->
          (p, fun env -> eval ~eval_check (Env.adds [] env) body)
      | Value.FFI { ffi_args = p; ffi_fn = f } ->
          (p, fun env -> f (List.rev env))
      | _ -> assert false
  in
  (* Record error positions. *)
  let f pe =
    try f pe with
      | Runtime_error.Runtime_error err ->
          let bt = Printexc.get_raw_backtrace () in
          Runtime_error.raise ~bt
            ~pos:(Option.to_list apply_pos @ err.pos)
            ~message:err.Runtime_error.msg err.Runtime_error.kind
      | Internal_error (poss, e) ->
          let bt = Printexc.get_raw_backtrace () in
          Printexc.raise_with_backtrace
            (Internal_error (Option.to_list apply_pos @ poss, e))
            bt
  in
  (* Provide given arguments. *)
  let pe, p =
    List.fold_left
      (fun (pe, p) (lbl, v) ->
        let (_, var, _), p = remove_first (fun (l, _, _) -> l = lbl) p in
        ((var, v) :: pe, p))
      ([], p) l
  in
  (* Add default values for remaining arguments. *)
  let pe =
    List.fold_left
      (fun pe (_, var, v) ->
        (* Typing should ensure that there are no mandatory arguments remaining. *)
        assert (v <> None);
        ( var,
          (* Set the position information on FFI's default values. Cf. r5008:
             if an Invalid_value is raised on a default value, which happens
             with the mount/name params of output.icecast.*, the printing of
             the error should succeed at getting a position information. *)
          let v = Option.get v in
          { v with Value.pos = apply_pos } )
        :: pe)
      pe p
  in
  (* Add position *)
  let pe = pe @ [(Lang_core.pos_var, Lang_core.Stacktrace.to_value pos)] in
  let v = f pe in
  (* Similarly here, the result of an FFI call should have some position
     information. For example, if we build a fallible source and pass it to an
     operator that expects an infallible one, an error is issued about that
     FFI-made value and a position is needed. *)
  { v with Value.pos = apply_pos }

and eval_ast ~eval_check ~(env : Env.t) ~flags ~pos ast =
  let mk v =
    Value.{ pos; value = v; methods = Methods.empty; flags; id = Value.id () }
  in
  match ast with
    | `Value v -> Lazy.force (Value.val_of_term_val v)
    | `Int i -> mk (Value.Int i)
    | `Float f -> mk (Value.Float f)
    | `Bool b -> mk (Value.Bool b)
    | `String s -> mk (Value.String s)
    | `Custom g -> mk (Value.Custom g)
    | `Cache_env _ -> assert false
    | `Encoder (e, p) ->
        let rec eval_param p =
          List.map
            (fun t ->
              match t with
                | `Anonymous s -> `Anonymous s
                | `Labelled (l, t) -> `Labelled (l, eval ~eval_check env t)
                | `Encoder (l, p) -> `Encoder (l, eval_param p))
            p
        in
        let p = eval_param p in
        !Hooks.make_encoder ~pos (e, p)
    | `List l -> mk (Value.List (List.map (eval ~eval_check env) l))
    | `Tuple l ->
        mk (Value.Tuple (List.map (fun a -> eval ~eval_check env a) l))
    | `Null -> mk Value.Null
    | `Hide (tm, methods) ->
        let v = eval ~eval_check env tm in
        {
          v with
          methods =
            Methods.filter (fun n _ -> not (List.mem n methods)) v.methods;
        }
    | `Cast { cast } -> { (eval ~eval_check env cast) with pos }
    | `Invoke { invoked = t; invoke_default; meth } -> (
        let v = eval ~eval_check env t in
        match (Value.Methods.find_opt meth v.Value.methods, invoke_default) with
          (* If method returns `null` and a default is provided, pick default. *)
          | Some Value.{ value = Null; methods }, Some default
            when Methods.is_empty methods ->
              eval ~eval_check env default
          | Some v, _ -> v
          | None, Some default -> eval ~eval_check env default
          | _ ->
              raise
                (Internal_error
                   ( Option.to_list pos,
                     "invoked method `" ^ meth ^ "` not found" )))
    | `Open (t, u) ->
        let t = eval ~eval_check env t in
        let env =
          Methods.fold
            (fun key meth env -> Env.add env key meth)
            t.Value.methods env
        in
        eval ~eval_check env u
    | `Let { pat; replace; def = v; body = b; _ } ->
        let v = eval ~eval_check env v in
        let penv =
          List.map
            (fun (ll, v) ->
              match ll with
                | [] -> assert false
                | [x] ->
                    let v () =
                      if replace then Value.remeth (Env.lookup env x) v else v
                    in
                    (x, Lazy.from_fun v)
                | l :: ll ->
                    (* Add method ll with value v to t *)
                    let rec meths ll v t =
                      match ll with
                        | [] -> assert false
                        | [l] ->
                            Value.{ t with methods = Methods.add l v t.methods }
                        | l :: ll ->
                            Value.
                              {
                                t with
                                methods =
                                  Methods.add l
                                    (meths ll v (Value.invoke t l))
                                    t.methods;
                              }
                    in
                    let v () =
                      let t = Env.lookup env l in
                      let v =
                        (* When replacing, keep previous methods. *)
                        if replace then Value.remeth (Value.invokes t ll) v
                        else v
                      in
                      meths ll v t
                    in
                    (l, Lazy.from_fun v))
            (eval_pat pat v)
        in
        let env = Env.adds_lazy env penv in
        eval ~eval_check env b
    | `Fun ({ name; arguments; body } as p) ->
        let fv = Term.free_fun_vars p in
        let env = Env.restrict env fv in
        let rec v () =
          let env =
            match name with
              | None -> env
              | Some name -> Env.add_lazy env name (Lazy.from_fun v)
          in
          let p, body = prepare_fun ~eval_check ~arguments ~env body in
          mk (Value.Fun { fun_args = p; fun_body = body })
        in
        v ()
    | `Var var -> Env.lookup env var
    | `Seq (a, b) ->
        ignore (eval ~eval_check env a);
        eval ~eval_check env b
    | `App (f, l) ->
        let ans () =
          let f = eval ~eval_check env f in
          let l = List.map (fun (l, t) -> (l, eval ~eval_check env t)) l in
          let pos =
            match pos with
              | None -> []
              | Some p ->
                  p
                  ::
                  (try
                     List.map Lang_core.Position.of_value
                       (Lang_core.to_list
                          (Lazy.force (List.assoc Lang_core.pos_var env)))
                   with _ -> [])
          in
          apply ~pos ~eval_check f l
        in
        if !profile then (
          match f.term with
            | `Var fname -> Profiler.time fname ans ()
            | _ -> ans ())
        else ans ()

and eval_term ~eval_check env tm =
  let v =
    eval_ast ~eval_check ~env ~flags:tm.flags ~pos:tm.t.Type.pos tm.term
  in
  if Methods.is_empty tm.methods then v
  else
    {
      v with
      methods =
        Methods.fold
          (fun k tm m -> Methods.add k (eval ~eval_check env tm) m)
          tm.methods v.Value.methods;
    }

and eval ~eval_check env tm =
  let v = eval_term ~eval_check env tm in
  eval_check ~env ~tm v;
  v

let apply ?pos t p =
  let eval_check = !Hooks.eval_check in
  apply ?pos ~eval_check t p

let eval ?env tm =
  let env =
    match env with
      | Some env -> env
      | None -> Environment.default_environment ()
  in
  let env = List.map (fun (x, v) -> (x, Lazy.from_val v)) env in
  let eval_check = !Hooks.eval_check in
  eval ~eval_check env tm

(** Add toplevel definitions to [builtins] so they can be looked during the
    evaluation of the next scripts. Also try to generate a structured
    documentation from the source code. *)
let toplevel_add ?doc pat ~t v =
  let generalized, t = t in
  let doc =
    match doc with
      | None -> None
      | Some doc ->
          let doc () =
            (* FIll in types and default values. *)
            let arguments =
              (* Type for parameters. *)
              let rec ptypes t =
                match (Type.deref t).Type.descr with
                  | Type.Arrow (p, _) -> p
                  | Type.Meth (_, t) -> ptypes t
                  | _ -> []
              in
              let ptypes = ref (ptypes t) in
              (* Default values for parameters. *)
              let pvalues v =
                match v.Value.value with
                  | Value.Fun { fun_args = p } ->
                      List.map (fun (l, _, o) -> (l, o)) p
                  | _ -> []
              in
              let pvalues = ref (pvalues v) in
              let doc_arguments = ref doc.Doc.Value.arguments in
              let arguments = ref [] in
              List.iter
                (fun (_, label, t) ->
                  let label = if label = "" then None else Some label in
                  let description =
                    match List.assoc_opt label !doc_arguments with
                      | Some argument ->
                          doc_arguments :=
                            List.remove_assoc label !doc_arguments;
                          argument.arg_description
                      | None -> None
                  in
                  let t = Repr.string_of_type ~generalized t in
                  let default =
                    let label = Option.value ~default:"" label in
                    match List.assoc_opt label !pvalues with
                      | Some value ->
                          pvalues := List.remove_assoc label !pvalues;
                          value
                      | None -> None
                  in
                  let default = Option.map Value.to_string default in
                  arguments :=
                    ( label,
                      Doc.Value.
                        {
                          arg_type = t;
                          arg_default = default;
                          arg_description = description;
                        } )
                    :: !arguments)
                !ptypes;
              (*
          List.iter
            (fun (s, _) ->
               Printf.eprintf "WARNING: Unused @param %S for %s %s\n" s
                 (string_of_pat pat)
                 (Pos.Option.to_string v.Value.pos))
            !doc_arguments;
          *)
              List.rev !arguments
            in
            let methods, t =
              let methods, t =
                let methods, t = Type.split_meths t in
                match (Type.deref t).Type.descr with
                  | Type.Arrow (p, a) ->
                      let methods, a = Type.split_meths a in
                      (* Note that in case we have a function, we drop the methods around,
                         the reason being that we expect that they are registered on their
                         own in the documentation. For instance, we don't want the field
                         recurrent to appear in the doc of thread.run: it is registered as
                         thread.run.recurrent anyways. *)
                      (methods, Type.make ?pos:t.Type.pos (Type.Arrow (p, a)))
                  | _ -> (methods, t)
              in
              let methods =
                List.map
                  (fun m ->
                    let l = m.Type.meth in
                    (* Override description by the one given in comment if it exists. *)
                    let d =
                      match List.assoc_opt l doc.Doc.Value.methods with
                        | Some m -> m.meth_description
                        | None -> Some m.doc
                    in
                    let t = Repr.string_of_scheme m.scheme in
                    (l, Doc.Value.{ meth_type = t; meth_description = d }))
                  methods
              in
              (methods, t)
            in
            let typ = Repr.string_of_type ~generalized t in
            { doc with typ; arguments; methods }
          in
          Some (Lazy.from_fun doc)
  in
  let env, pa = Typechecking.type_of_pat ~level:max_int ~pos:None pat in
  Typing.(t <: pa);
  List.iter
    (fun (x, v) ->
      let t = List.assoc x env in
      Environment.add_builtin ~override:true ?doc x ((generalized, t), v))
    (eval_pat pat v)

let rec eval_toplevel ?(interactive = false) t =
  match t.term with
    | `Let { doc; gen = generalized; replace; pat; def; body } ->
        let def_t, def =
          if not replace then (def.t, eval def)
          else (
            match pat with
              | `PVar [] -> assert false
              | `PVar (x :: l) ->
                  let old_t, old =
                    ( List.assoc x (Environment.default_typing_environment ()),
                      List.assoc x (Environment.default_environment ()) )
                  in
                  let old_t = snd old_t in
                  let old_t = snd (Type.invokes old_t l) in
                  let old = Value.invokes old l in
                  (Type.remeth old_t def.t, Value.remeth old (eval def))
              | `PTuple _ -> assert false)
        in
        toplevel_add ?doc pat ~t:(generalized, def_t) def;
        if Lazy.force debug then
          Printf.eprintf "Added toplevel %s : %s\n%!" (string_of_pat pat)
            (Type.to_string ~generalized def_t);
        let var = string_of_pat pat in
        if
          interactive && var <> "_"
          &&
          try
            ignore (Scanf.sscanf var "_%dpat" (fun v -> v));
            false
          with _ -> true
        then
          Format.printf "@[<2>%s :@ %a =@ %s@]@." var
            (fun f t -> Repr.print_scheme f (generalized, t))
            def_t (Value.to_string def);
        eval_toplevel ~interactive body
    | `Seq (a, b) ->
        ignore
          (let v = eval_toplevel a in
           if v.Value.pos = None then { v with Value.pos = a.t.Type.pos } else v);
        eval_toplevel ~interactive b
    | _ ->
        let v = eval t in
        if interactive && t.term <> unit then
          Format.printf "- : %a = %s@." Repr.print_type t.t (Value.to_string v);
        v
