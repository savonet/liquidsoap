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

(** {1 Type checking / inference} *)

(** Terms for which generalization is safe. *)
let rec value_restriction t =
  match t.term with
    | Var _ -> true
    | Fun _ -> true
    | RFun _ -> true
    | Null -> true
    | List l | Tuple l -> List.for_all value_restriction l
    | Meth ({ meth_value = v }, u) -> value_restriction v && value_restriction u
    | Ground _ -> true
    | Let l -> value_restriction l.def && value_restriction l.body
    | Cast (t, _) -> value_restriction t
    (* | Invoke (t, _) -> value_restriction t *)
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
      Typing.(
        ty
        <: List.fold_left
             (fun ty (label, _) ->
               Type.meth ~optional:true label
                 ([], Type.make ?pos Ground_type.never)
                 ty)
             (Type.var ~level ?pos ()) l);
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
                        optional = false;
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

(* Type-check an expression. *)
let rec check ?(print_toplevel = false) ~throw ~level ~(env : Typing.env) e =
  let check = check ~throw in
  if !debug then Printf.printf "\n# %s : ?\n\n%!" (Term.to_string e);
  let check ?print_toplevel ~level ~env e =
    check ?print_toplevel ~level ~env e;
    if !debug then
      Printf.printf "\n# %s : %s\n\n%!" (Term.to_string e) (Type.to_string e.t)
  in
  (* The toplevel position of the (un-dereferenced) type is the actual parsing
     position of the value. When we synthesize a type against which the type of
     the term is unified, we have to set the position information in order not
     to loose it. *)
  let pos = e.t.Type.pos in
  let mk t = Type.make ?pos t in
  let mkg t = mk t in
  let check_fun ~proto ~env e body =
    let base_check = check ~level ~env in
    let proto_t, env =
      List.fold_left
        (fun (p, env) -> function
          | lbl, var, kind, None ->
              update_level level kind;
              ((false, lbl, kind) :: p, (var, ([], kind)) :: env)
          | lbl, var, kind, Some v ->
              update_level level kind;
              base_check v;
              v.t <: kind;
              ((true, lbl, kind) :: p, (var, ([], kind)) :: env))
        ([], env) proto
    in
    let proto_t = List.rev proto_t in
    check ~level ~env body;
    e.t >: mk (Type.Arrow (proto_t, body.t))
  in
  match e.term with
    | Any -> ()
    | Ground g -> e.t >: mkg (Ground.to_descr g)
    | Encoder f ->
        (* Ensure that we only use well-formed terms. *)
        let rec check_enc (_, p) =
          List.iter
            (function
              | _, `Term t -> check ~level ~env t | _, `Encoder e -> check_enc e)
            p
        in
        check_enc f;
        let t =
          try !Hooks.type_of_encoder ~pos f
          with Not_found ->
            let bt = Printexc.get_raw_backtrace () in
            Printexc.raise_with_backtrace
              (Unsupported_format (pos, Term.to_string e))
              bt
        in
        e.t >: t
    | List l ->
        List.iter (fun x -> check ~level ~env x) l;
        let t = Type.var ~level ?pos () in
        List.iter (fun e -> e.t <: t) l;
        e.t >: mk Type.(List { t; json_repr = `Tuple })
    | Tuple l ->
        List.iter (fun a -> check ~level ~env a) l;
        e.t >: mk (Type.Tuple (List.map (fun a -> a.t) l))
    | Null -> e.t >: mk (Type.Nullable (Type.var ~level ?pos ()))
    | Cast (a, t) ->
        check ~level ~env a;
        a.t <: t;
        e.t >: t
    | Meth ({ name = l; meth_value = a }, b) ->
        check ~level ~env a;
        check ~level ~env b;
        e.t
        >: mk
             (Type.Meth
                ( {
                    Type.meth = l;
                    optional = false;
                    scheme = Typing.generalize ~level a.t;
                    doc = "";
                    json_name = None;
                  },
                  b.t ))
    | Invoke { invoked = a; default; meth = l } ->
        check ~level ~env a;
        let rec aux t =
          match (Type.deref t).Type.descr with
            | Type.(Meth ({ meth = l'; scheme = s }, _)) when l = l' ->
                (fst s, Typing.instantiate ~level s)
            | Type.(Meth (_, c)) -> aux c
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
                             optional = default <> None;
                             scheme = ([], x);
                             doc = "";
                             json_name = None;
                           },
                           y ));
                ([], x)
        in
        let vars, typ = aux a.t in
        let typ =
          match default with
            | None -> typ
            | Some v ->
                check ~level ~env v;
                (* We want to make sure that: x?.foo types as: { foo?: 'a } *)
                let typ =
                  match (Type.deref v.t).descr with
                    | Type.Nullable _ -> mk Type.(Nullable typ)
                    | _ -> typ
                in
                Typing.instantiate ~level (vars, v.t) <: typ;
                typ
        in
        e.t >: typ
    | Open (a, b) ->
        check ~level ~env a;
        a.t <: mk Type.unit;
        let rec aux env t =
          match (Type.deref t).Type.descr with
            | Type.(Meth ({ meth = l; scheme = g, u }, t)) ->
                aux ((l, (g, u)) :: env) t
            | _ -> env
        in
        let env = aux env a.t in
        check ~level ~env b;
        e.t >: b.t
    | Seq (a, b) ->
        check ~env ~level a;
        if not (can_ignore a.t) then throw (Ignored a);
        check ~print_toplevel ~level ~env b;
        e.t >: b.t
    | App (a, l) -> (
        check ~level ~env a;
        List.iter (fun (_, b) -> check ~env ~level b) l;

        (* If [a] is known to have a function type, manually dig through it for
           better error messages. Otherwise generate its type and unify -- in
           that case the optionality can't be guessed and mandatory is the
           default. *)
        match (Type.demeth a.t).Type.descr with
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
                          (match (a.term, lbl) with
                            | Var "if", "then" | Var "if", "else" ->
                                let a = Type.var ?pos:v.t.Type.pos () in
                                let b = Type.var ?pos:t.Type.pos () in
                                v.t <: Type.make (Type.Arrow ([], a));
                                t <: Type.make (Type.Arrow ([], b));
                                a <: b
                            | _ -> v.t <: t);
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
              a.t <: Type.make (Type.Arrow (p, e.t)))
    | Fun (_, proto, body) -> check_fun ~proto ~env e body
    | RFun (x, _, proto, body) ->
        let env = (x, ([], e.t)) :: env in
        check_fun ~proto ~env e body
    | Var var ->
        let s =
          try List.assoc var env with Not_found -> raise (Unbound (pos, var))
        in
        e.t >: Typing.instantiate ~level s;
        if Lazy.force Term.debug then
          Printf.eprintf "Instantiate %s : %s becomes %s\n" var
            (Type.string_of_scheme s) (Type.to_string e.t)
    | Let ({ pat; replace; def; body; _ } as l) ->
        check ~level:(level + 1) ~env def;
        let generalized =
          (* Printf.printf "generalize at %d: %B\n\n!" level (value_restriction def); *)
          if value_restriction def then fst (generalize ~level def.t) else []
        in
        let penv, pa = type_of_pat ~level ~pos pat in
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
        check ~print_toplevel ~level ~env body;
        e.t >: body.t

let display_types = ref false

(* The simple definition for external use. *)
let check ?(ignored = false) ~throw e =
  let print_toplevel = !display_types in
  try
    let env = Environment.default_typing_environment () in
    check ~print_toplevel ~throw ~level:0 ~env e;
    if print_toplevel && (Type.deref e.t).Type.descr <> Type.unit then
      add_task (fun () ->
          Format.printf "@[<2>-     :@ %a@]@." Repr.print_type e.t);
    if ignored && not (can_ignore e.t) then throw (Ignored e);
    pop_tasks ()
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    pop_tasks ();
    Printexc.raise_with_backtrace e bt
