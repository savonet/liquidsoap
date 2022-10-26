(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

let rec eval_pat ~pos pat v =
  let rec aux env pat v =
    match (pat, v) with
      | PVar x, v -> (x, v) :: env
      | PTuple pl, { Value.value = Value.Tuple l } ->
          List.fold_left2 aux env pl l
      (* The parser parses [x,y,z] as PList ([], None, l) *)
      | PList (([] as l'), (None as spread), l), { Value.value = Value.List lv }
      | PList (l, spread, l'), { Value.value = Value.List lv } ->
          let ln = List.length l in
          let ln' = List.length l' in
          let lvn = List.length lv in
          if lvn < ln + ln' then
            Runtime_error.raise ~pos
              ~message:
                "List value does not have enough elements to fit the \
                 extraction pattern!"
              "not_found";
          let lv =
            List.mapi
              (fun pos v ->
                match pos with
                  | _ when pos < ln -> (`First, v)
                  | _ when lvn - ln' <= pos -> (`Second, v)
                  | _ -> (`Spread, v))
              lv
          in
          let ll =
            List.map snd (List.filter (fun (lbl, _) -> lbl = `First) lv)
          in
          let ls =
            List.map snd (List.filter (fun (lbl, _) -> lbl = `Spread) lv)
          in
          let ll' =
            List.map snd (List.filter (fun (lbl, _) -> lbl = `Second) lv)
          in
          let spread_env =
            match spread with
              | None -> []
              | Some s -> [([s], Value.{ v with value = List ls })]
          in
          List.fold_left2 aux [] l' ll'
          @ spread_env @ env
          @ List.fold_left2 aux [] l ll
          @ env
      | PMeth (pat, l), _ ->
          let m, v = Value.split_meths v in
          let env = match pat with None -> env | Some pat -> aux env pat v in
          List.fold_left
            (fun env (lbl, pat) ->
              let v = List.assoc lbl m in
              (match pat with None -> [] | Some pat -> eval_pat ~pos pat v)
              @ [([lbl], v)]
              @ env)
            env l
      | _ -> assert false
  in
  aux [] pat v

module Env = struct
  type t = Value.lazy_env

  (** Find the value of a variable in the environment. *)
  let lookup (env : t) var =
    try Lazy.force (List.assoc var env)
    with Not_found ->
      failwith
        (Printf.sprintf "Internal error: variable %s not in environment." var)

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

let rec prepare_fun fv p env =
  (* Unlike OCaml we always evaluate default values, and we do that early. I
     think the only reason is homogeneity with FFI, which are declared with
     values as defaults. *)
  let p =
    List.map
      (function
        | lbl, var, _, Some v -> (lbl, var, Some (eval env v))
        | lbl, var, _, None -> (lbl, var, None))
      p
  in
  (* Keep only once the variables we might use in the environment. *)
  let env = Env.restrict env fv in
  (p, env)

and eval (env : Env.t) tm =
  let mk v =
    (* Ensure that the kind computed at runtime for sources will agree with
       the typing. *)
    (match (Type.deref tm.t).Type.descr with
      | Type.Constr
          { Type.constructor = "source"; params = [(Type.Invariant, k)] } -> (
          let frame_content_of_t t =
            match (Type.deref t).Type.descr with
              | Type.Var _ -> `Any
              | Type.Constr { Type.constructor; params = [(_, t)] } -> (
                  match (Type.deref t).Type.descr with
                    | Type.Ground (Type.Format fmt) -> `Format fmt
                    | Type.Var _ -> `Kind (Content.kind_of_string constructor)
                    | _ -> failwith ("Unhandled content: " ^ Type.to_string tm.t)
                  )
              | Type.Constr { Type.constructor = "none" } ->
                  `Kind (Content.kind_of_string "none")
              | _ -> failwith ("Unhandled content: " ^ Type.to_string tm.t)
          in
          let k = of_frame_kind_t k in
          let k =
            Kind.of_kind
              {
                Frame.audio = frame_content_of_t k.Frame.audio;
                video = frame_content_of_t k.Frame.video;
                midi = frame_content_of_t k.Frame.midi;
              }
          in
          let rec demeth = function
            | Value.Meth (_, _, v) -> demeth v.Value.value
            | v -> v
          in
          match demeth v with
            | Value.Source s -> Kind.unify s#kind k
            | _ ->
                raise
                  (Internal_error
                     ( Option.to_list tm.t.Type.pos,
                       "term has type source but is not a source: "
                       ^ Value.to_string
                           { Value.pos = tm.t.Type.pos; Value.value = v } )))
      | _ -> ());
    { Value.pos = tm.t.Type.pos; Value.value = v }
  in
  match tm.term with
    | Ground g -> mk (Value.Ground g)
    | Encoder (e, p) ->
        let pos = tm.t.Type.pos in
        let rec eval_param p =
          List.map
            (fun (l, t) ->
              ( l,
                match t with
                  | `Term t -> `Value (eval env t)
                  | `Encoder (l, p) -> `Encoder (l, eval_param p) ))
            p
        in
        let p = eval_param p in
        let enc : Value.encoder = (e, p) in
        let e = Lang_encoder.make_encoder ~pos tm enc in
        mk (Value.Encoder e)
    | List l -> mk (Value.List (List.map (eval env) l))
    | Tuple l -> mk (Value.Tuple (List.map (fun a -> eval env a) l))
    | Null -> mk Value.Null
    | Cast (e, _) ->
        let e = eval env e in
        mk e.Value.value
    | Meth (l, u, v) -> mk (Value.Meth (l, eval env u, eval env v))
    | Invoke (t, l) ->
        let rec aux t =
          match t.Value.value with
            | Value.Meth (l', t, _) when l = l' -> t
            | Value.Meth (_, _, t) -> aux t
            | _ ->
                raise
                  (Internal_error
                     ( Option.to_list tm.t.Type.pos,
                       "invoked method `" ^ l ^ "` not found" ))
        in
        aux (eval env t)
    | Open (t, u) ->
        let t = eval env t in
        let rec aux env t =
          match t.Value.value with
            | Value.Meth (l, v, t) -> aux (Env.add env l v) t
            | Value.Tuple [] -> env
            | _ -> assert false
        in
        let env = aux env t in
        eval env u
    | Let { pat; replace; def = v; body = b; _ } ->
        let v = eval env v in
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
                      let mk ~pos value = { Value.pos; value } in
                      match ll with
                        | [] -> assert false
                        | [l] -> mk ~pos:tm.t.Type.pos (Value.Meth (l, v, t))
                        | l :: ll ->
                            mk ~pos:t.Value.pos
                              (Value.Meth (l, meths ll v (Value.invoke t l), t))
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
            (eval_pat pat
               ~pos:(match tm.t.Type.pos with None -> [] | Some p -> [p])
               v)
        in
        let env = Env.adds_lazy env penv in
        eval env b
    | Fun (fv, p, body) ->
        let p, env = prepare_fun fv p env in
        mk (Value.Fun (p, env, body))
    | RFun (x, fv, p, body) ->
        let p, env = prepare_fun fv p env in
        let rec v () =
          let env = Env.add_lazy env x (Lazy.from_fun v) in
          { Value.pos = tm.t.Type.pos; value = Value.Fun (p, env, body) }
        in
        v ()
    | Var var -> Env.lookup env var
    | Seq (a, b) ->
        ignore (eval env a);
        eval env b
    | App (f, l) ->
        let ans () =
          let f = eval env f in
          let l = List.map (fun (l, t) -> (l, eval env t)) l in
          apply ?pos:tm.t.Type.pos f l
        in
        if !profile then (
          match f.term with
            | Var fname -> Profiler.time fname ans ()
            | _ -> ans ())
        else ans ()

and apply ?pos f l =
  (* Extract the components of the function, whether it's explicit or foreign. *)
  let p, f =
    match (Value.demeth f).Value.value with
      | Value.Fun (p, e, body) ->
          ( p,
            fun pe ->
              let env = Env.adds e pe in
              eval env body )
      | Value.FFI (p, f) -> (p, fun pe -> f (List.rev pe))
      | _ -> assert false
  in
  (* Record error positions. *)
  let f pe =
    try f pe with
      | Runtime_error.Runtime_error err ->
          let bt = Printexc.get_raw_backtrace () in
          Runtime_error.raise ~bt
            ~pos:(Option.to_list pos @ err.Runtime_error.pos)
            ~message:err.Runtime_error.msg err.Runtime_error.kind
      | Internal_error (poss, e) ->
          let bt = Printexc.get_raw_backtrace () in
          Printexc.raise_with_backtrace
            (Internal_error (Option.to_list pos @ poss, e))
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
          { v with Value.pos } )
        :: pe)
      pe p
  in
  (* Add position *)
  let pe =
    pe
    @ [
        ( Lang_core.pos_var,
          Lang_core.Position.to_value
            (match pos with None -> [] | Some p -> [p]) );
      ]
  in
  let v = f pe in
  (* Similarly here, the result of an FFI call should have some position
     information. For example, if we build a fallible source and pass it to an
     operator that expects an infallible one, an error is issued about that
     FFI-made value and a position is needed. *)
  { v with Value.pos }

let eval ?env tm =
  let env =
    match env with
      | Some env -> env
      | None -> Environment.default_environment ()
  in
  let env = List.map (fun (x, (_, v)) -> (x, Lazy.from_val v)) env in
  eval env tm

(** Add toplevel definitions to [builtins] so they can be looked during the
    evaluation of the next scripts. Also try to generate a structured
    documentation from the source code. *)
let toplevel_add (doc, params, methods) pat ~t v =
  let generalized, t = t in
  let rec ptypes t =
    match (Type.deref t).Type.descr with
      | Type.Arrow (p, _) -> p
      | Type.Meth (_, t) -> ptypes t
      | _ -> []
  in
  let ptypes = ptypes t in
  let rec pvalues v =
    match v.Value.value with
      | Value.Fun (p, _, _) -> List.map (fun (l, _, o) -> (l, o)) p
      | Value.Meth (_, _, v) -> pvalues v
      | _ -> []
  in
  let pvalues = pvalues v in
  let params, _ =
    List.fold_left
      (fun (params, pvalues) (_, label, t) ->
        let descr, params =
          try (List.assoc label params, List.remove_assoc label params)
          with Not_found -> ("", params)
        in
        let default, pvalues =
          try
            (`Known (List.assoc label pvalues), List.remove_assoc label pvalues)
          with Not_found -> (`Unknown, pvalues)
        in
        let item () =
          let item = Doc.trivial (if descr = "" then "(no doc)" else descr) in
          item#add_subsection "type"
            (Lazy.from_fun (fun () -> Repr.doc_of_type ~generalized t));
          item#add_subsection "default"
            (Lazy.from_fun (fun () ->
                 Doc.trivial
                   (match default with
                     | `Unknown -> "???"
                     | `Known (Some v) -> Value.to_string v
                     | `Known None -> "None")));
          item
        in
        doc#add_subsection
          (if label = "" then "(unlabeled)" else label)
          (Lazy.from_fun item);
        (params, pvalues))
      (params, pvalues) ptypes
  in
  List.iter
    (fun (s, _) ->
      Printf.eprintf "WARNING: Unused @param %S for %s %s\n" s
        (string_of_pat pat)
        (Pos.Option.to_string v.Value.pos))
    params;
  (let meths, t =
     let meths, t = Type.split_meths t in
     match (Type.deref t).Type.descr with
       | Type.Arrow (p, a) ->
           let meths, a = Type.split_meths a in
           (* Note that in case we have a function, we drop the methods around,
              the reason being that we expect that they are registered on their
              own in the documentation. For instance, we don't want the field
              recurrent to appear in the doc of thread.run: it is registered as
              thread.run.recurrent anyways. *)
           (meths, { t with Type.descr = Type.Arrow (p, a) })
       | _ -> (meths, t)
   in
   doc#add_subsection "_type"
     (Lazy.from_fun (fun () -> Repr.doc_of_type ~generalized t));
   let meths =
     List.map
       (fun Type.({ meth = l; doc = d } as m) ->
         (* Override description by the one given in comment if it exists. *)
         let d = try List.assoc l methods with Not_found -> d in
         Type.{ m with doc = d })
       meths
   in
   if meths <> [] then
     doc#add_subsection "_methods"
       (Lazy.from_fun (fun () -> Repr.doc_of_meths meths)));
  let env, pa = Typechecking.type_of_pat ~level:max_int ~pos:None pat in
  Typing.(t <: pa);
  List.iter
    (fun (x, v) ->
      let t = List.assoc x env in
      Environment.add_builtin ~override:true ~doc:(Lazy.from_val doc) x
        ((generalized, t), v))
    (eval_pat ~pos:(match t.Type.pos with None -> [] | Some p -> [p]) pat v)

let rec eval_toplevel ?(interactive = false) t =
  match t.term with
    | Let { doc = comment; gen = generalized; replace; pat; def; body } ->
        let def_t, def =
          if not replace then (def.t, eval def)
          else (
            match pat with
              | PVar [] -> assert false
              | PVar (x :: l) ->
                  let old_t, old =
                    List.assoc x (Environment.default_environment ())
                  in
                  let old_t = snd old_t in
                  let old_t = snd (Type.invokes old_t l) in
                  let old = Value.invokes old l in
                  (Type.remeth old_t def.t, Value.remeth old (eval def))
              | PMeth _ | PList _ | PTuple _ ->
                  failwith "TODO: cannot replace toplevel patterns for now")
        in
        toplevel_add comment pat ~t:(generalized, def_t) def;
        if Lazy.force debug then
          Printf.eprintf "Added toplevel %s : %s\n%!" (string_of_pat pat)
            (Type.to_string ~generalized def_t);
        let var = string_of_pat pat in
        if interactive && var <> "_" then
          Format.printf "@[<2>%s :@ %a =@ %s@]@." var
            (fun f t -> Repr.print_scheme f (generalized, t))
            def_t (Value.to_string def);
        eval_toplevel ~interactive body
    | Seq (a, b) ->
        ignore
          (let v = eval_toplevel a in
           if v.Value.pos = None then { v with Value.pos = a.t.Type.pos } else v);
        eval_toplevel ~interactive b
    | _ ->
        let v = eval t in
        if interactive && t.term <> unit then
          Format.printf "- : %a = %s@." Repr.print_type t.t (Value.to_string v);
        v
