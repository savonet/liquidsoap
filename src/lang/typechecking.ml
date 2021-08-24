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

open Term
open Typing

(** {1 Type checking / inference} *)

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
    | _ -> false

(** [No_label (f,lbl,first,x)] indicates that the parameter [x] could not be
  * passed to the function [f] because the latter has no label [lbl].
  * The [first] information tells whether [lbl=x] is the first parameter with
  * label [lbl] in the considered application, which makes the message a bit
  * more helpful. *)
exception No_label of term * string * bool * term

(** A simple mechanism for delaying printing toplevel tasks
  * as late as possible, to avoid seeing too many unknown variables. *)
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
      let a = T.fresh_evar ~level ~pos in
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
      (env, T.make ~level ~pos (T.Tuple l))

(* Type-check an expression.
 * [level] should be the sum of the lengths of [env] and [builtins],
 * that is the size of the typing context, that is the number of surrounding
 * abstractions. *)
let rec check ?(print_toplevel = false) ~throw ~level ~(env : Typing.env) e =
  (* The role of this function is not only to type-check but also to assign
   * meaningful levels to type variables, and unify the types of
   * all occurrences of the same variable, since the parser does not do it. *)
  assert (e.t.T.level = -1);
  e.t.T.level <- level;

  (* The toplevel position of the (un-dereferenced) type
   * is the actual parsing position of the value.
   * When we synthesize a type against which the type of the term is unified,
   * we have to set the position information in order not to loose it. *)
  let pos = e.t.T.pos in
  let mk t = T.make ~level ~pos t in
  let mkg t = mk (T.Ground t) in
  let check_fun ~proto ~env e body =
    let base_check = check ~throw ~level ~env in
    let proto_t, env, level =
      List.fold_left
        (fun (p, env, level) -> function
          | lbl, var, kind, None ->
              if Lazy.force debug then
                Printf.eprintf "Assigning level %d to %s (%s).\n" level var
                  (T.print kind);
              kind.T.level <- level;
              ((false, lbl, kind) :: p, (var, ([], kind)) :: env, level + 1)
          | lbl, var, kind, Some v ->
              if Lazy.force debug then
                Printf.eprintf "Assigning level %d to %s (%s).\n" level var
                  (T.print kind);
              kind.T.level <- level;
              base_check v;
              v.t <: kind;
              ((true, lbl, kind) :: p, (var, ([], kind)) :: env, level + 1))
        ([], env, level) proto
    in
    let proto_t = List.rev proto_t in
    check ~throw ~level ~env body;
    e.t >: mk (T.Arrow (proto_t, body.t))
  in
  match e.term with
    | Ground g -> e.t >: mkg (Ground.to_type g)
    | Encoder f -> e.t >: type_of_format ~pos:e.t.T.pos ~level f
    | List l ->
        List.iter (fun x -> check ~throw ~level ~env x) l;
        let t =
          List.fold_left
            (fun t e -> Typing.min_type ~pos:e.t.T.pos ~level t e.t)
            (T.fresh_evar ~level ~pos) l
        in
        List.iter (fun e -> e.t <: t) l;
        e.t >: mk (T.List t)
    | Tuple l ->
        List.iter (fun a -> check ~throw ~level ~env a) l;
        e.t >: mk (T.Tuple (List.map (fun a -> a.t) l))
    | Null -> e.t >: mk (T.Nullable (T.fresh_evar ~level ~pos))
    | Cast (a, t) ->
        check ~throw ~level ~env a;
        a.t <: t;
        e.t >: t
    | Meth (l, a, b) ->
        check ~throw ~level ~env a;
        check ~throw ~level ~env b;
        e.t >: mk (T.Meth (l, (Typing.generalizable ~level a.t, a.t), "", b.t))
    | Invoke (a, l) ->
        check ~throw ~level ~env a;
        let rec aux t =
          match (T.deref t).T.descr with
            | T.Meth (l', (generalized, b), _, c) ->
                if l = l' then Typing.instantiate ~level ~generalized b
                else aux c
            | _ ->
                (* We did not find the method, the type we will infer is not the
                   most general one (no generalization), but this is safe and
                   enough for records. *)
                let x = T.fresh_evar ~level ~pos in
                let y = T.fresh_evar ~level ~pos in
                a.t <: mk (T.Meth (l, ([], x), "", y));
                x
        in
        e.t >: aux a.t
    | Open (a, b) ->
        check ~throw ~level ~env a;
        a.t <: mk T.unit;
        let rec aux env t =
          match (T.deref t).T.descr with
            | T.Meth (l, (g, u), _, t) -> aux ((l, (g, u)) :: env) t
            | _ -> env
        in
        let env = aux env a.t in
        check ~throw ~level ~env b;
        e.t >: b.t
    | Seq (a, b) ->
        check ~throw ~env ~level a;
        if not (can_ignore a.t) then throw (Ignored a);
        check ~print_toplevel ~throw ~level ~env b;
        e.t >: b.t
    (* Special case for if branchings in order to backtrack in the case where
       the then branch is nullable but the else branch is not, see #1816. *)
    | App
        ( ({ term = Var "if" } as e_if),
          [("", e_cond); ("then", e_then); ("else", e_else)] ) ->
        check ~throw ~level ~env e_if;
        check ~throw ~level ~env e_cond;
        check ~throw ~level ~env e_then;
        check ~throw ~level ~env e_else;
        let a = T.fresh_evar ~level ~pos in
        e_cond.t <: T.make (T.Ground T.Bool);
        e_else.t <: T.make (T.Arrow ([], a));
        if
          try
            e_then.t <: T.make (T.Arrow ([], a));
            true
          with _ -> false
        then e.t >: a
        else (
          e_then.t <: T.make (T.Arrow ([], T.make (T.Nullable a)));
          e.t >: T.make (T.Nullable a))
    | App (a, l) -> (
        check ~throw ~level ~env a;
        List.iter (fun (_, b) -> check ~throw ~env ~level b) l;

        (* If [a] is known to have a function type, manually dig through
         * it for better error messages. Otherwise generate its type
         * and unify -- in that case the optionality can't be guessed
         * and mandatory is the default. *)
        match (T.demeth a.t).T.descr with
          | T.Arrow (ap, t) ->
              (* Find in l the first arg labeled lbl,
               * return it together with the remaining of the list. *)
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
                (* Remove the applied parameters,
                 * check their types on the fly. *)
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
              (* See if any mandatory argument remains, check the return type. *)
              if List.for_all (fun (o, _, _) -> o) ap then e.t >: t
              else e.t >: T.make ~level ~pos:None (T.Arrow (ap, t))
          | _ ->
              let p = List.map (fun (lbl, b) -> (false, lbl, b.t)) l in
              a.t <: T.make ~level ~pos:None (T.Arrow (p, e.t)))
    | Fun (_, proto, body) -> check_fun ~proto ~env e body
    | RFun (x, _, proto, body) ->
        let env = (x, ([], e.t)) :: env in
        check_fun ~proto ~env e body
    | Var var ->
        let generalized, orig =
          try List.assoc var env
          with Not_found -> raise (Unbound (e.t.T.pos, var))
        in
        e.t >: Typing.instantiate ~level ~generalized orig;
        if Lazy.force debug then
          Printf.eprintf "Instantiate %s[%d] : %s becomes %s\n" var
            (T.deref e.t).T.level (T.print orig) (T.print e.t)
    | Let ({ pat; replace; def; body; _ } as l) ->
        check ~throw ~level ~env def;
        let generalized =
          if value_restriction def then (
            let f x t =
              let x' =
                Type.filter_vars
                  (function
                    | { T.descr = T.EVar (i, _); level = l; _ } ->
                        (not (List.mem_assoc i x)) && l >= level
                    | _ -> assert false)
                  t
              in
              x' @ x
            in
            f [] def.t)
          else []
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
                      if replace then T.remeth (snd (List.assoc x env)) a else a
                    in
                    (x, (generalized, a))
                | l :: ll -> (
                    try
                      let g, t = List.assoc l env in
                      let a =
                        (* If we are replacing the value, we keep the previous methods. *)
                        if replace then T.remeth (snd (T.invokes t ll)) a else a
                      in
                      (l, (g, T.meths ~pos ~level ll (generalized, a) t))
                    with Not_found ->
                      raise (Unbound (pos, String.concat "." (l :: ll)))))
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
                (T.pp_type_generalized generalized)
                def.t);
        check ~print_toplevel ~throw ~level:(level + 1) ~env body;
        e.t >: body.t

(* The simple definition for external use. *)
let check ?(ignored = false) ~throw e =
  let print_toplevel = !Configure.display_types in
  try
    let env = default_typing_environment () in
    check ~print_toplevel ~throw ~level:(List.length env) ~env e;
    if print_toplevel && (T.deref e.t).T.descr <> T.unit then
      add_task (fun () -> Format.printf "@[<2>-     :@ %a@]@." T.pp_type e.t);
    if ignored && not (can_ignore e.t) then throw (Ignored e);
    pop_tasks ()
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    pop_tasks ();
    Printexc.raise_with_backtrace e bt
