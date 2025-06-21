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

open Term
open Typing

exception No_method of string * Type.t
exception Top_level_override of string * Pos.t option

let debug = ref false

(** {1 Type checking / inference} *)

(** Can a function return type be generalized after being applied? Current
    implementation considers it safe to generalize when all universal types in
    the function return type are specified by a non-optional argument.

    For instance,
    - this can be generalized:
      [def f() = fun (x) -> x end; fn = f() : fun ('a) -> 'a ]
    - but this cannot: [s = single() : source('A) ]
    - and this cannot either:
      [def f() = fun (x=null) -> ref(x) end; fn = f() : (?'A?) -> ref('A) ]

    When all universal types in the return are specified by a non-optional
    argument, we are assured that anything that would be generalized is later
    specified when the required argument is passed. If this argument is still of
    a universal type, then the argument's universal type takes over the type
    that was generalized. *)

let function_app_value_restriction fn =
  let rec filter_app_vars l t =
    let t = Type.deref t in
    match t.descr with
      | Int | Float | String | Bool | Never -> l
      | Custom c -> c.filter_vars filter_app_vars l c.typ
      | Getter t -> filter_app_vars l t
      | List { t } | Nullable t -> filter_app_vars l t
      | Tuple aa -> List.fold_left filter_app_vars l aa
      | Meth ({ scheme = g, t }, u) ->
          let l =
            List.filter (fun v -> not (List.mem v g)) (filter_app_vars l t)
          in
          filter_app_vars l u
      | Constr c ->
          List.fold_left (fun l (_, t) -> filter_app_vars l t) l c.params
      | Var { contents = Free var } -> var :: l
      | Var { contents = Link _ } -> assert false
      | Arrow (p, t) ->
          let l = filter_app_vars l t in
          let pl =
            List.fold_left
              (fun pl -> function
                | true, _, _ -> pl | false, _, t -> filter_app_vars pl t)
              [] p
          in
          List.filter (fun v -> not (List.memq v pl)) l
  in
  match Type.demeth fn.Term.t with
    | { Type.descr = Arrow (_, t) } -> filter_app_vars [] t = []
    | _ -> false

(** Terms for which generalization is safe. *)
let value_restriction t =
  let rec value_restriction t =
    match t.term with
      | `Var _ -> true
      | `Fun _ -> true
      | `Null -> true
      | `App (fn, _) -> function_app_value_restriction fn
      | `List l | `Tuple l -> List.for_all value_restriction l
      | `Int _ | `Float _ | `String _ | `Bool _ | `Custom _ -> true
      | `Let l -> value_restriction l.def && value_restriction l.body
      | `Cast { cast = t } -> value_restriction t
      (* | Invoke (t, _) -> value_restriction t *)
      | _ -> false
  in
  value_restriction t
  && Methods.for_all (fun _ meth_term -> value_restriction meth_term) t.methods

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
let type_of_pat ~level ~pos = function
  | `PVar x ->
      let a = Type.var ~level ?pos () in
      ([(x, a)], a)
  | `PTuple l ->
      let env, l =
        List.fold_left
          (fun (env, l) var ->
            let a = Type.var ~level ?pos () in
            (([var], a) :: env, a :: l))
          ([], []) l
      in
      let l = List.rev l in
      (env, Type.make ?pos (Type.Tuple l))

(* Type-check an expression. *)
let rec check ?(print_toplevel = false) ~throw ~level ~env e =
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
  let check_fun ~env e { arguments; body } =
    let base_check = check ~level ~env in
    let proto_t, env =
      List.fold_left
        (fun (p, env) -> function
          | { label; as_variable; typ; default = None } ->
              update_level level typ;
              ( (false, label, typ) :: p,
                env#add ~pos (Option.value ~default:label as_variable) ([], typ)
              )
          | { label; as_variable; typ; default = Some v } ->
              update_level level typ;
              base_check v;
              v.t <: typ;
              ( (true, label, typ) :: p,
                env#add ~pos (Option.value ~default:label as_variable) ([], typ)
              ))
        ([], env) arguments
    in
    let proto_t = List.rev proto_t in
    (* Ensure that we don't have the same label twice. *)
    List.fold_left
      (fun labels (_, l, _) ->
        if l = "" then labels
        else (
          if List.mem l labels then raise (Duplicate_label (e.t.Type.pos, l));
          l :: labels))
      [] proto_t
    |> ignore;
    check ~level ~env body;
    e.t >: mk (Type.Arrow (proto_t, body.t))
  in
  let base_type = Type.var () in
  let () =
    match e.term with
      | `Cache_env r ->
          r :=
            {
              var_name = Atomic.get Type_base.var_name_atom;
              var_id = Atomic.get Type_base.var_id_atom;
              env = env#current;
            };
          base_type >: mk (Tuple [])
      | `Int _ -> base_type >: mk Int
      | `Float _ -> base_type >: mk Float
      | `String _ -> base_type >: mk String
      | `Bool _ -> base_type >: mk Bool
      | `Custom h -> base_type >: mk h.handler.typ.Type.descr
      | `Encoder f ->
          (* Ensure that we only use well-formed terms. *)
          let rec check_enc (_, p) =
            List.iter
              (function
                | `Labelled (_, t) -> check ~level ~env t
                | `Anonymous _ -> ()
                | `Encoder e -> check_enc e)
              p
          in
          check_enc f;
          let t =
            try !Hooks.type_of_encoder ~pos f
            with Not_found ->
              let bt = Printexc.get_raw_backtrace () in
              Printexc.raise_with_backtrace
                (Unsupported_encoder (pos, Term.to_string e))
                bt
          in
          base_type >: t
      | `List l ->
          List.iter (fun x -> check ~level ~env x) l;
          let t = Type.var ~level ?pos () in
          List.iter (fun e -> e.t <: t) l;
          base_type >: mk Type.(List { t; json_repr = `Tuple })
      | `Tuple l ->
          List.iter (fun a -> check ~level ~env a) l;
          base_type >: mk (Type.Tuple (List.map (fun a -> a.t) l))
      | `Null -> base_type >: mk (Type.Nullable (Type.var ~level ?pos ()))
      | `Cast { cast = a; typ = t } ->
          check ~level ~env a;
          a.t <: t;
          base_type >: t
      | `Hide (a, methods) ->
          check ~level ~env a;
          let ty =
            List.fold_left
              (fun ty name ->
                Type.make ?pos
                  Type.(
                    Meth
                      ( {
                          meth = name;
                          optional = true;
                          scheme = ([], Type.make ?pos Type.Never);
                          doc = "";
                          json_name = None;
                        },
                        ty )))
              a.t methods
          in
          base_type >: ty
      | `Invoke { invoked = a; invoke_default; meth = l } ->
          check ~level ~env a;
          let rec aux t =
            match (Type.deref t).Type.descr with
              | Type.(
                  Meth ({ meth = l'; scheme = (_, { descr }) as s; optional }, _))
                when l = l' && (optional = false || descr = Never) ->
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
                               optional = invoke_default <> None;
                               scheme = ([], x);
                               doc = "";
                               json_name = None;
                             },
                             y ));
                  ([], x)
          in
          let vars, typ = aux a.t in
          let typ =
            match (invoke_default, Type.deref typ) with
              | None, { descr = Never } -> raise (No_method (l, a.t))
              | None, _ -> typ
              | Some v, _ -> (
                  check ~level ~env v;
                  let v_t = Typing.instantiate ~level (vars, v.t) in
                  match typ.Type.descr with
                    | Never -> v_t
                    | _ ->
                        (* We want to make sure that: x?.foo types as: { foo?: 'a } *)
                        let typ =
                          match (Type.deref v.t).descr with
                            | Type.Nullable _ -> mk Type.(Nullable typ)
                            | _ -> typ
                        in
                        v_t <: typ;
                        typ)
          in
          base_type >: typ
      | `Open (a, b) ->
          check ~level ~env a;
          a.t <: mk Type.unit;
          let rec aux env t =
            match (Type.deref t).Type.descr with
              | Type.(Meth ({ meth = l; scheme = g, u }, t)) ->
                  aux (env#add ~pos l (g, u)) t
              | _ -> env
          in
          let env = aux env a.t in
          check ~level ~env b;
          base_type >: b.t
      | `Seq (a, b) ->
          check ~env ~level a;
          if not (can_ignore a.t) then (
            let bt = Printexc.get_callstack 0 in
            throw ~bt (Ignored a));
          check ~print_toplevel ~level ~env b;
          base_type >: b.t
      | `App (a, l) -> (
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
                              | `Var "if", "then" | `Var "if", "else" -> (
                                  match
                                    ( (Type.deref v.t).descr,
                                      (Type.deref t).descr )
                                  with
                                    | Type.Arrow ([], vt), Type.Arrow ([], t) ->
                                        vt <: t
                                    | _ -> assert false)
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
                base_type >: t
            | _ ->
                let p = List.map (fun (lbl, b) -> (false, lbl, b.t)) l in
                a.t <: Type.make (Type.Arrow (p, base_type)))
      | `Fun p ->
          let env =
            match p.name with
              | None -> env
              | Some name -> env#add ~pos name ([], base_type)
          in
          check_fun ~env e p
      | `Var var ->
          let s = env#get ~pos var in
          base_type >: Typing.instantiate ~level s;
          if Lazy.force Term.debug then
            Printf.eprintf "Instantiate %s : %s becomes %s\n" var
              (Type.string_of_scheme s) (Type.to_string base_type)
      | `Let ({ pat; replace; def; body; _ } as l) ->
          check ~level:(level + 1) ~env def;
          let generalized =
            if value_restriction def then fst (generalize ~level def.t) else []
          in
          let penv, pa = type_of_pat ~level ~pos pat in
          def.t <: pa;
          let env =
            List.fold_left
              (fun env (ll, a) ->
                match ll with
                  | [] -> assert false
                  | [x] ->
                      let a =
                        if replace then Type.remeth (snd (env#get ~pos x)) a
                        else a
                      in
                      if !debug then
                        Printf.printf "\nLET %s : %s\n%!" x
                          (Repr.string_of_scheme (generalized, a));
                      env#add ~pos x (generalized, a)
                  | l :: ll ->
                      let g, t = env#get ~pos l in
                      let a =
                        (* If we are replacing the value, we keep the previous methods. *)
                        if replace then Type.remeth (snd (Type.invokes t ll)) a
                        else a
                      in
                      env#override l (g, Type.meths ?pos ll (generalized, a) t))
              env penv
          in
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
          base_type >: body.t
  in
  e.t
  >: Methods.fold
       (fun meth meth_term t ->
         check ~level ~env meth_term;
         Type.make ?pos
           (Type.Meth
              ( {
                  Type.meth;
                  optional = false;
                  scheme = Typing.generalize ~level meth_term.t;
                  doc = "";
                  json_name = None;
                },
                t )))
       e.methods base_type

let display_types = ref false

(* The simple definition for external use. *)
let check ?env ~check_top_level_override ~throw e =
  let print_toplevel = !display_types in
  try
    let env =
      match env with
        | Some env -> env
        | None -> Environment.default_typing_environment ()
    in
    let top_level_variables =
      if check_top_level_override then List.map fst env else []
    in
    let env =
      object (self)
        val env = env
        val top_level_variables = top_level_variables
        method current = env

        method check_top_level_override ~pos var =
          if List.mem var top_level_variables then (
            let bt = Printexc.get_callstack 1 in
            throw ~bt (Top_level_override (var, pos)))

        method add ~pos var v =
          self#check_top_level_override ~pos var;
          self#override var v

        method override var v =
          {<env = (var, v) :: env
           ; top_level_variables = List.filter
                                     (fun v -> v <> var)
                                     top_level_variables>}

        method get ~pos var =
          try List.assoc var env with Not_found -> raise (Unbound (pos, var))
      end
    in
    check ~print_toplevel ~throw ~level:0 ~env e;
    if print_toplevel && (Type.deref e.t).Type.descr <> Type.unit then
      add_task (fun () ->
          Format.printf "@[<2>-     :@ %a@]@." Repr.print_type e.t);
    pop_tasks ()
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    pop_tasks ();
    Printexc.raise_with_backtrace e bt
