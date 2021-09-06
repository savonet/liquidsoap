(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

(** [remove_first f l] removes the first element [e] of [l] such that [f e],
  * and returns [e,l'] where [l'] is the list without [e].
  * Asserts that there is such an element. *)
let remove_first filter =
  let rec aux acc = function
    | [] -> assert false
    | hd :: tl ->
        if filter hd then (hd, List.rev_append acc tl) else aux (hd :: acc) tl
  in
  aux []

let lookup (env : Value.lazy_env) var =
  try Lazy.force (List.assoc var env)
  with Not_found ->
    failwith
      (Printf.sprintf "Internal error: variable %s not in environment." var)

let eval_pat pat v =
  let rec aux env pat v =
    match (pat, v) with
      | PVar x, v -> (x, v) :: env
      | PTuple pl, { Value.value = Value.Tuple l } ->
          List.fold_left2 aux env pl l
      | _ -> assert false
  in
  aux [] pat v

let rec eval ~env tm =
  let env = (env : Value.lazy_env) in
  let prepare_fun fv p env =
    (* Unlike OCaml we always evaluate default values, and we do that early. I
       think the only reason is homogeneity with FFI, which are declared with
       values as defaults. *)
    let p =
      List.map
        (function
          | lbl, var, _, Some v -> (lbl, var, Some (eval ~env v))
          | lbl, var, _, None -> (lbl, var, None))
        p
    in
    (* Keep only once the variables we might use in the environment. *)
    let env =
      let fv = ref fv in
      let mem x =
        if Vars.mem x !fv then (
          fv := Vars.remove x !fv;
          true)
        else false
      in
      List.filter (fun (x, _) -> mem x) env
    in
    (p, env)
  in
  let mk v =
    (* Ensure that the kind computed at runtime for sources will agree with
       the typing. *)
    (match (Type.deref tm.t).Type.descr with
      | Type.Constr { Type.name = "source"; params = [(Type.Invariant, k)] }
        -> (
          let frame_content_of_t t =
            match (Type.deref t).Type.descr with
              | Type.EVar _ -> `Any
              | Type.Constr { Type.name; params = [(_, t)] } -> (
                  match (Type.deref t).Type.descr with
                    | Type.Ground (Type.Format fmt) -> `Format fmt
                    | Type.EVar _ -> `Kind (Frame_content.kind_of_string name)
                    | _ -> failwith ("Unhandled content: " ^ Type.print tm.t))
              | Type.Constr { Type.name = "none" } ->
                  `Kind (Frame_content.kind_of_string "none")
              | _ -> failwith ("Unhandled content: " ^ Type.print tm.t)
          in
          let k = of_frame_kind_t k in
          let k =
            Source.Kind.of_kind
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
            | Value.Source s -> Source.Kind.unify s#kind k
            | _ ->
                raise
                  (Internal_error
                     ( Option.to_list tm.t.Type.pos,
                       "term has type source but is not a source: "
                       ^ Value.print_value
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
                  | `Term t -> `Value (eval ~env t)
                  | `Encoder (l, p) -> `Encoder (l, eval_param p) ))
            p
        in
        let p = eval_param p in
        let enc : Value.encoder = (e, p) in
        let e = Lang_encoder.make_encoder ~pos tm enc in
        mk (Value.Encoder e)
    | List l -> mk (Value.List (List.map (eval ~env) l))
    | Tuple l -> mk (Value.Tuple (List.map (fun a -> eval ~env a) l))
    | Null -> mk Value.Null
    | Cast (e, _) ->
        let e = eval ~env e in
        mk e.Value.value
    | Meth (l, u, v) -> mk (Value.Meth (l, eval ~env u, eval ~env v))
    | Invoke (t, l) ->
        let rec aux t =
          match t.Value.value with
            | Value.Meth (l', t, _) when l = l' -> t
            | Value.Meth (_, _, t) -> aux t
            | _ ->
                raise
                  (Internal_error
                     ( Option.to_list tm.t.Type.pos,
                       "invoked method " ^ l ^ " not found" ))
        in
        aux (eval ~env t)
    | Open (t, u) ->
        let t = eval ~env t in
        let rec aux env t =
          match t.Value.value with
            | Value.Meth (l, v, t) -> aux ((l, Lazy.from_val v) :: env) t
            | Value.Tuple [] -> env
            | _ -> assert false
        in
        let env = aux env t in
        eval ~env u
    | Let { pat; replace; def = v; body = b; _ } ->
        let v = eval ~env v in
        let penv =
          List.map
            (fun (ll, v) ->
              match ll with
                | [] -> assert false
                | [x] ->
                    let v () =
                      if replace then
                        Value.remeth (Lazy.force (List.assoc x env)) v
                      else v
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
                      let t = Lazy.force (List.assoc l env) in
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
        let env = penv @ env in
        eval ~env b
    | Fun (fv, p, body) ->
        let p, env = prepare_fun fv p env in
        mk (Value.Fun (p, [], env, body))
    | RFun (x, fv, p, body) ->
        let p, env = prepare_fun fv p env in
        let rec v () =
          let env = (x, Lazy.from_fun v) :: env in
          { Value.pos = tm.t.Type.pos; value = Value.Fun (p, [], env, body) }
        in
        v ()
    | Var var -> lookup env var
    | Seq (a, b) ->
        ignore (eval ~env a);
        eval ~env b
    | App (f, l) ->
        let ans () =
          apply (eval ~env f) (List.map (fun (l, t) -> (l, eval ~env t)) l)
        in
        if !profile then (
          match f.term with
            | Var fname -> Profiler.time fname ans ()
            | _ -> ans ())
        else ans ()

and apply f l =
  let rec pos = function
    | [(_, v)] -> (
        match (f.Value.pos, v.Value.pos) with
          | Some (p, _), Some (_, q) -> Some (p, q)
          | Some pos, None -> Some pos
          | None, Some pos -> Some pos
          | None, None -> None)
    | _ :: l -> pos l
    | [] -> f.Value.pos
  in
  (* Position of the whole application. *)
  let pos = pos l in
  let pos_f = f.Value.pos in
  let mk ~pos v = { pos; Value.value = v } in
  (* Extract the components of the function, whether it's explicit or foreign,
     together with a rewrapping function for creating a closure in case of
     partial application. *)
  let p, pe, f, rewrap =
    match (Value.demeth f).Value.value with
      | Value.Fun (p, pe, e, body) ->
          ( p,
            pe,
            (fun pe ->
              let pe = List.map (fun (x, gv) -> (x, Lazy.from_val gv)) pe in
              eval ~env:(List.rev_append pe e) body),
            fun p pe -> mk ~pos:pos_f (Value.Fun (p, pe, e, body)) )
      | Value.FFI (p, pe, f) ->
          ( p,
            pe,
            (fun pe -> f (List.rev pe)),
            fun p pe -> mk ~pos:pos_f (Value.FFI (p, pe, f)) )
      | _ -> assert false
  in
  (* Record error positions. *)
  let f pe =
    try f pe with
      | Runtime_error err ->
          let bt = Printexc.get_raw_backtrace () in
          Printexc.raise_with_backtrace
            (Runtime_error { err with pos = Option.to_list pos @ err.pos })
            bt
      | Internal_error (poss, e) ->
          let bt = Printexc.get_raw_backtrace () in
          Printexc.raise_with_backtrace
            (Internal_error (Option.to_list pos @ poss, e))
            bt
  in
  let pe, p =
    List.fold_left
      (fun (pe, p) (lbl, v) ->
        let (_, var, _), p = remove_first (fun (l, _, _) -> l = lbl) p in
        ((var, v) :: pe, p))
      (pe, p) l
  in
  if List.exists (fun (_, _, x) -> x = None) p then
    (* Partial application. *)
    rewrap p pe
  else (
    let pe =
      List.fold_left
        (fun pe (_, var, v) ->
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
    let v = f pe in
    (* Similarly here, the result of an FFI call should have some position
       information. For example, if we build a fallible source and pass it to an
       operator that expects an infallible one, an error is issued about that
       FFI-made value and a position is needed. *)
    { v with Value.pos })

let eval ?env tm =
  let env =
    match env with
      | Some env -> env
      | None -> Environment.default_environment ()
  in
  let env = List.map (fun (x, (_, v)) -> (x, Lazy.from_val v)) env in
  eval ~env tm

(** Add toplevel definitions to [builtins] so they can be looked during the
    evaluation of the next scripts. Also try to generate a structured
    documentation from the source code. *)
let toplevel_add (doc, params, methods) pat ~t v =
  let generalized, t = t in
  let rec ptypes t =
    match (Type.deref t).Type.descr with
      | Type.Arrow (p, _) -> p
      | Type.Meth (_, _, _, t) -> ptypes t
      | _ -> []
  in
  let ptypes = ptypes t in
  let rec pvalues v =
    match v.Value.value with
      | Value.Fun (p, _, _, _) -> List.map (fun (l, _, o) -> (l, o)) p
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
        let item = Doc.trivial (if descr = "" then "(no doc)" else descr) in
        item#add_subsection "type" (Type.doc_of_type ~generalized t);
        item#add_subsection "default"
          (Doc.trivial
             (match default with
               | `Unknown -> "???"
               | `Known (Some v) -> Value.print_value v
               | `Known None -> "None"));
        doc#add_subsection (if label = "" then "(unlabeled)" else label) item;
        (params, pvalues))
      (params, pvalues) ptypes
  in
  List.iter
    (fun (s, _) ->
      Printf.eprintf "WARNING: Unused @param %S for %s %s\n" s
        (string_of_pat pat)
        (Type.print_pos_opt v.Value.pos))
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
   doc#add_subsection "_type" (Type.doc_of_type ~generalized t);
   let meths =
     List.map
       (fun (l, (t, d)) ->
         (* Override description by the one given in comment if it exists. *)
         let d = try List.assoc l methods with Not_found -> d in
         (l, (t, d)))
       meths
   in
   if meths <> [] then doc#add_subsection "_methods" (Type.doc_of_meths meths));
  List.iter
    (fun (x, v) ->
      Environment.add_builtin ~override:true ~doc x ((generalized, t), v))
    (eval_pat pat v)

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
              | PTuple _ ->
                  failwith "TODO: cannot replace toplevel tuples for now")
        in
        toplevel_add comment pat ~t:(generalized, def_t) def;
        if Lazy.force debug then
          Printf.eprintf "Added toplevel %s : %s\n%!" (string_of_pat pat)
            (Type.print ~generalized def_t);
        let var = string_of_pat pat in
        if interactive && var <> "_" then
          Format.printf "@[<2>%s :@ %a =@ %s@]@." var
            (Type.pp_type_generalized generalized)
            def_t (Value.print_value def);
        eval_toplevel ~interactive body
    | Seq (a, b) ->
        ignore
          (let v = eval_toplevel a in
           if v.Value.pos = None then { v with Value.pos = a.t.Type.pos } else v);
        eval_toplevel ~interactive b
    | _ ->
        let v = eval t in
        if interactive && t.term <> unit then
          Format.printf "- : %a = %s@." Type.pp_type t.t (Value.print_value v);
        v
