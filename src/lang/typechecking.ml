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

open Term
open Typing

let debug = ref false

module List = struct
  include List

  (** Index where a predicate is satisfied. *)
  let index p l =
    let rec aux n = function
      | x :: l -> if p x then n else aux (n + 1) l
      | [] -> raise Not_found
    in
    aux 0 l
end

(** {1 Type checking / inference} *)

(** Terms for which generalization is safe. *)
let rec value_restriction t =
  match t.term with
    | Var _ -> true
    | Fun _ -> true
    | RFun _ -> true
    | Null -> true
    | List l | Tuple l -> List.for_all value_restriction l
    | Meth (_, t, u) -> value_restriction t && value_restriction u
    (* | Invoke (t, _) -> value_restriction t *)
    | Ground _ -> true
    | Let l -> value_restriction l.def && value_restriction l.body
    | _ -> false

(** A simple mechanism for delaying printing toplevel tasks as late as possible,
    to avoid seeing too many unknown variables. *)
let add_task, pop_tasks =
  let q = Queue.create () in
  ( (fun f -> Queue.add f q),
    fun () ->
      try
        while true do
          (Queue.take q) ()
        done
      with Queue.Empty -> () )

(** Generate a type with fresh variables for a pattern. *)
let rec type_of_pat ~level ~pos = function
  | PVar x ->
      let a = Type.var ~level ?pos () in
      ([(x, a)], a)
  | PTuple l ->
      let env, l =
        List.fold_left
          (fun (env, l) p ->
            let env', a = type_of_pat ~level ~pos p in
            (env' @ env, a :: l))
          ([], []) l
      in
      let l = List.rev l in
      (env, Type.make ?pos (Type.Tuple l))
  | PList (l, spread, l') ->
      let fold_env l ty =
        List.fold_left
          (fun (env, ty, ety) p ->
            let env', ty' = type_of_pat ~level ~pos p in
            let ty = Typing.sup ~pos ty ty' in
            (env' @ env, ty, ty' :: ety))
          ([], ty, []) l
      in
      let ty = Type.var ~level ?pos () in
      let env, ty, ety = fold_env l ty in
      let env', ty, ety' = fold_env l' ty in
      let spread_env =
        match spread with
          | None -> []
          | Some v ->
              [([v], Type.make ?pos Type.(List { t = ty; json_repr = `Tuple }))]
      in
      List.iter (fun ety -> Typing.(ety <: ty)) (ety @ ety');
      ( env' @ spread_env @ env,
        Type.make ?pos Type.(List { t = ty; json_repr = `Tuple }) )
  | PMeth (pat, l) ->
      let env, ty =
        match pat with
          | None -> ([], Type.make ?pos (Type.Tuple []))
          | Some pat -> type_of_pat ~level ~pos pat
      in
      let env, ty =
        List.fold_left
          (fun (env, ty) (lbl, p) ->
            let env', a =
              match p with
                | None -> ([], Type.var ~level ?pos ())
                | Some pat -> type_of_pat ~level ~pos pat
            in
            let ty =
              Type.make ?pos
                Type.(
                  Meth
                    ( {
                        meth = lbl;
                        scheme = ([], a);
                        doc = "";
                        json_name = None;
                      },
                      ty ))
            in
            (env' @ [([lbl], a)] @ env, ty))
          (env, ty) l
      in
      (env, ty)

(** Type-check an expression. In passing, we also produce an optimized
    representation of the term. *)
let rec check ?(print_toplevel = false) ~throw ~level ~(env : Typing.env) e :
    TermDB.t =
  let check = check ~throw in
  if !debug then Printf.printf "\n# %s : ?\n\n%!" (Term.to_string e);
  let check ?print_toplevel ~level ~env e =
    let e' = check ?print_toplevel ~level ~env e in
    if !debug then
      Printf.printf "\n# %s : %s\n\n%!" (Term.to_string e) (Type.to_string e.t);
    e'
  in
  (* The toplevel position of the (un-dereferenced) type is the actual parsing
     position of the value. When we synthesize a type against which the type of
     the term is unified, we have to set the position information in order not
     to loose it. *)
  let pos = e.t.Type.pos in
  let mk t = Type.make ?pos t in
  let mkg t = mk (Type.Ground t) in
  let check_fun ~proto ~env e body =
    let base_check = check ~level ~env in
    let proto', proto_t, env =
      List.fold_left
        (fun (proto', p, env) -> function
          | lbl, var, kind, None ->
              update_level level kind;
              ( (var, None) :: proto',
                (false, lbl, kind) :: p,
                (var, ([], kind)) :: env )
          | lbl, var, kind, Some v ->
              update_level level kind;
              let v' = base_check v in
              v.t <: kind;
              ( (var, Some v') :: proto',
                (true, lbl, kind) :: p,
                (var, ([], kind)) :: env ))
        ([], [], env) proto
    in
    let proto_t = List.rev proto_t in
    let body' = check ~level ~env body in
    e.t >: mk (Type.Arrow (proto_t, body.t));
    (proto', body')
  in
  match e.term with
    | Ground g ->
        e.t >: mkg (Ground.to_type g);
        Ground g
    | Encoder f ->
        (* Ensure that we only use well-formed terms. *)
        let rec check_enc (enc, p) : TermDB.encoder =
          ( e.t.pos,
            enc,
            List.map
              (function
                | p, `Term t -> (p, `Term (t.t.pos, check ~level ~env t))
                | p, `Encoder e -> (p, `Encoder (check_enc e)))
              p )
        in
        let f' = check_enc f in
        let t =
          try Lang_encoder.type_of_encoder ~pos:e.t.Type.pos f
          with Not_found ->
            let bt = Printexc.get_raw_backtrace () in
            Printexc.raise_with_backtrace
              (Unsupported_format (pos, Term.to_string e))
              bt
        in
        e.t >: t;
        Encoder f'
    | List l ->
        let l' = List.map (fun x -> check ~level ~env x) l in
        let t = Type.var ~level ?pos () in
        List.iter (fun e -> e.t <: t) l;
        e.t >: mk Type.(List { t; json_repr = `Tuple });
        List l'
    | Tuple l ->
        let l' = List.map (fun a -> check ~level ~env a) l in
        e.t >: mk (Type.Tuple (List.map (fun a -> a.t) l));
        Tuple l'
    | Null ->
        e.t >: mk (Type.Nullable (Type.var ~level ?pos ()));
        Null
    | Cast (a, t) ->
        let a' = check ~level ~env a in
        a.t <: t;
        e.t >: t;
        Cast (a', t)
    | Meth (l, a, b) ->
        let a' = check ~level ~env a in
        let b' = check ~level ~env b in
        e.t
        >: mk
             (Type.Meth
                ( {
                    Type.meth = l;
                    scheme = Typing.generalize ~level a.t;
                    doc = "";
                    json_name = None;
                  },
                  b.t ));
        Meth (l, a', b')
    | Invoke (a, l) ->
        let a' = check ~level ~env a in
        let rec aux t =
          match (Type.deref t).Type.descr with
            | Type.(Meth ({ meth = l'; scheme = generalized, b }, c)) ->
                if l = l' then Typing.instantiate ~level ~generalized b
                else aux c
            | _ ->
                (* We did not find the method, the type we will infer is not the
                   most general one (no generalization), but this is safe and
                   enough for records. *)
                let x = Type.var ~level ?pos () in
                let y = Type.var ~level ?pos () in
                a.t
                <: mk
                     Type.(
                       Meth
                         ( {
                             meth = l;
                             scheme = ([], x);
                             doc = "";
                             json_name = None;
                           },
                           y ));
                x
        in
        e.t >: aux a.t;
        Invoke (a', l)
    | Open (a, b) ->
        let a' = check ~level ~env a in
        a.t <: mk Type.unit;
        let rec aux env t =
          match (Type.deref t).Type.descr with
            | Type.(Meth ({ meth = l; scheme = g, u }, t)) ->
                aux ((l, (g, u)) :: env) t
            | _ -> env
        in
        let env = aux env a.t in
        let b' = check ~level ~env b in
        e.t >: b.t;
        Open (a', b')
    | Seq (a, b) ->
        let a' = check ~env ~level a in
        if not (can_ignore a.t) then throw (Ignored a);
        let b' = check ~print_toplevel ~level ~env b in
        e.t >: b.t;
        Seq (a', b')
    | App (a, l) ->
        let a' = check ~level ~env a in
        let l' = List.map (fun (l, b) -> (l, check ~env ~level b)) l in
        (* If [a] is known to have a function type, manually dig through it for
           better error messages. Otherwise generate its type and unify -- in
           that case the optionality can't be guessed and mandatory is the
           default. *)
        (match (Type.demeth a.t).Type.descr with
          | Type.Arrow (ap, t) ->
              (* Find in l the first arg labeled lbl, return it together with the
                 remaining of the list. *)
              let get_arg lbl l =
                let rec aux acc = function
                  | [] -> None
                  | (o, lbl', t) :: l ->
                      if lbl = lbl' then Some (o, t, List.rev_append acc l)
                      else aux ((o, lbl', t) :: acc) l
                in
                aux [] l
              in
              let _, ap =
                (* Remove the applied parameters, check their types on the
                   fly. *)
                List.fold_left
                  (fun (already, ap) (lbl, v) ->
                    match get_arg lbl ap with
                      | None ->
                          let first = not (List.mem lbl already) in
                          raise (No_label (a, lbl, first, v))
                      | Some (_, t, ap') ->
                          v.t <: t;
                          (lbl :: already, ap'))
                  ([], ap) l
              in
              (* See if any mandatory argument is missing. *)
              let mandatory =
                List.filter_map
                  (fun (o, l, t) -> if o then None else Some (l, t))
                  ap
              in
              if mandatory <> [] then
                raise (Term.Missing_arguments (pos, mandatory));
              e.t >: t
          | _ ->
              let p = List.map (fun (lbl, b) -> (false, lbl, b.t)) l in
              a.t <: Type.make (Type.Arrow (p, e.t)));
        App (a', l')
    | Fun (proto, body) ->
        let proto, body = check_fun ~proto ~env e body in
        Fun (proto, body)
    | RFun (x, proto, body) ->
        let env = (x, ([], e.t)) :: env in
        let proto, body = check_fun ~proto ~env e body in
        RFun (proto, body)
    | Var var ->
        let generalized, orig =
          try List.assoc var env
          with Not_found -> raise (Unbound (e.t.Type.pos, var))
        in
        let list_index p l =
          let rec aux i = function
            | x :: l -> if p x then i else aux (i + 1) l
            | [] -> raise Not_found
          in
          aux 0 l
        in
        let i = list_index (fun (x, _) -> x = var) env in
        e.t >: Typing.instantiate ~level ~generalized orig;
        if Lazy.force Term.debug then
          Printf.eprintf "Instantiate %s : %s becomes %s\n" var
            (Type.to_string orig) (Type.to_string e.t);
        Var (i, var)
    | Let ({ pat; replace; def; body; _ } as l) ->
        let def' = check ~level:(level + 1) ~env def in
        let generalized =
          (* Printf.printf "generalize at %d: %B\n\n!" level (value_restriction def); *)
          if value_restriction def then fst (generalize ~level def.t) else []
        in
        let penv, pa = type_of_pat ~level:(level + 1) ~pos pat in
        def.t <: pa;
        let penv =
          List.map
            (fun (ll, a) ->
              match ll with
                | [] -> assert false
                | [x] ->
                    let a =
                      if replace then Type.remeth (snd (List.assoc x env)) a
                      else a
                    in
                    if !debug then
                      Printf.printf "\nLET %s : %s\n%!" x
                        (Repr.string_of_scheme (generalized, a));
                    (x, (generalized, a))
                | l :: ll -> (
                    try
                      let g, t = List.assoc l env in
                      let a =
                        (* If we are replacing the value, we keep the previous methods. *)
                        if replace then Type.remeth (snd (Type.invokes t ll)) a
                        else a
                      in
                      (l, (g, Type.meths ?pos ll (generalized, a) t))
                    with Not_found -> raise (Unbound (pos, l))))
            penv
        in
        (* Pre-compile the pattern. This could be done in type_of_pat, but this
           is safer this way and should not have a big effect on performance. *)
        let rec pat_of_pat : Term.pattern -> TermDB.pattern = function
          | PVar [_] -> PVar
          | PVar (x :: l) -> PField (List.index (fun (y, _) -> y = x) env, l)
          | PVar [] -> assert false
          | PTuple l -> PTuple (List.map pat_of_pat l)
          | PList (l, p, l') ->
              PList (List.map pat_of_pat l, p <> None, List.map pat_of_pat l')
          | PMeth (p, l) ->
              PMeth
                ( Option.map pat_of_pat p,
                  List.map (fun (l, p) -> (l, Option.map pat_of_pat p)) l )
        in
        let pat' = pat_of_pat l.pat in
        let env = penv @ env in
        l.gen <- generalized;
        if print_toplevel then
          add_task (fun () ->
              Format.printf "@[<2>%s :@ %a@]@."
                (let name = string_of_pat pat in
                 let l = String.length name and max = 5 in
                 if l >= max then name else name ^ String.make (max - l) ' ')
                (fun f t -> Repr.print_scheme f (generalized, t))
                def.t);
        let body' = check ~print_toplevel ~level ~env body in
        e.t >: body.t;
        Let { replace = l.replace; pat = pat'; def = def'; body = body' }

(* The simple definition for external use. *)
let check ?(ignored = false) ~throw e =
  let print_toplevel = !Configure.display_types in
  try
    let env = Environment.default_typing_environment () in
    let e' = check ~print_toplevel ~throw ~level:0 ~env e in
    if print_toplevel && (Type.deref e.t).Type.descr <> Type.unit then
      add_task (fun () ->
          Format.printf "@[<2>-     :@ %a@]@." Repr.print_type e.t);
    if ignored && not (can_ignore e.t) then throw (Ignored e);
    pop_tasks ();
    e'
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    pop_tasks ();
    Printexc.raise_with_backtrace e bt
