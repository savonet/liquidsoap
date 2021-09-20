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

(** Typing. *)

open Type

let () = Type.debug := true
let debug_subtyping = ref true

type env = (string * scheme) list

(** Do we have a method. *)
let has_meth a = match (deref a).descr with Meth _ -> true | _ -> false

let rec hide_meth l a =
  match (deref a).descr with
    | Meth (l', _, _, u) when l' = l -> hide_meth l u
    | Meth (l', t, d, u) -> { a with descr = Meth (l', t, d, hide_meth l u) }
    | _ -> a

(** {1 Type generalization and instantiation}
  *
  * We don't have type schemes per se, but we compute generalizable variables
  * and keep track of them in the AST.
  * This is simple and useful because in any case we need to distinguish
  * two 'a variables bound at different places. Indeed, we might instantiate
  * one in a term where the second is bound, and we don't want to
  * merge the two when going under the binder.
  *
  * When generalizing we need to know what can be generalized in the outermost
  * type but also in the inner types of the term forming a let-definition.
  * Indeed those variables will have to be instantiated by fresh ones for
  * every instance.
  *
  * If the value restriction applies, then we have some (fun (...) -> ...)
  * and any type variable of higher level can be generalized, whether it's
  * in the outermost type or not. *)

(** Return a list of generalizable variables in a type.
  * This is performed after type inference on the left-hand side
  * of a let-in, with [level] being the level of that let-in.
  * Uses the simple method of ML, to be associated with a value restriction. *)
let generalizable ~level t = filter_vars (fun v -> v.level > level) t

(* TODO: we should keep only bound with no type variable and fix the type otherwise. *)
let generalize ~level t : Type.scheme =
  let g = generalizable ~level t in
  (g, t)

(** Substitutions. *)
module Subst = struct
  module M = Map.Make (struct
    type t = var

    (* We can compare variables with their indices. *)
    let compare (x : var) (y : var) = compare x.name y.name
  end)

  type subst = t M.t
  type t = subst

  let of_list l : t = M.of_seq (List.to_seq l)

  (** Retrieve the value of a variable. *)
  let value (s : t) (i : var) = M.find i s

  (* let filter f (s : t) = M.filter (fun i t -> f i t) s *)

  (** Whether we have the identity substitution. *)
  let is_identity (s : t) = M.is_empty s
end

(** Copy a term, substituting some EVars as indicated by a list of
    associations. *)
let copy_with (subst : Subst.t) t =
  let rec aux t =
    let cp x = { t with descr = x } in
    match t.descr with
      | Var { contents = Free v } -> (
          try Subst.value subst v with Not_found -> t)
      | Var { contents = Link t } ->
          (* Keep links to preserve rich position information, and to make it
             possible to check if the application left the type unchanged. *)
          cp (Var (ref (Link (aux t))))
      | Constr c ->
          let params = List.map (fun (v, t) -> (v, aux t)) c.params in
          cp (Constr { c with params })
      | Ground _ -> cp t.descr
      | Bot -> cp Bot
      | Getter t -> cp (Getter (aux t))
      | List t -> cp (List (aux t))
      | Nullable t -> cp (Nullable (aux t))
      | Tuple l -> cp (Tuple (List.map aux l))
      | Meth (l, (g, t), d, u) ->
          (* We assume that we don't substitute generalized variables. *)
          if !debug then
            assert (Subst.M.for_all (fun v _ -> not (List.mem v g)) subst);
          cp (Meth (l, (g, aux t), d, aux u))
      | Arrow (p, t) ->
          cp (Arrow (List.map (fun (o, l, t) -> (o, l, aux t)) p, aux t))
  in
  if Subst.is_identity subst then t else aux t

(** Instantiate a type scheme, given as a type together with a list of
   generalized variables. Fresh variables are created with the given (current)
   level, and attached to the appropriate constraints.  This erases position
   information, since they usually become irrelevant. *)
let instantiate ~level ~generalized =
  let subst = List.map (fun v -> (v, Type.var ~level ())) generalized in
  let subst = Subst.of_list subst in
  fun t -> copy_with subst t

(** {1 The type lattice} *)

exception Incompatible

(** Approximated supremum of two types. We grow the second argument so that it
    has a chance be be greater than the first. No binding is performed by this
    function so that it should always be followed by a subtyping. *)
let rec sup ~pos a b =
  let sup = sup ~pos in
  let mk descr = { pos; descr } in
  let scheme_sup t t' =
    match (t, t') with ([], t), ([], t') -> ([], sup t t') | _ -> t'
  in
  let rec meth_type l a =
    match (deref a).descr with
      | Meth (l', t, _, _) when l = l' -> Some t
      | Meth (_, _, _, a) -> meth_type l a
      | Var { contents = Free _ } -> Some ([], Type.var ~pos ())
      | _ -> None
  in
  match ((deref a).descr, (deref b).descr) with
    | Bot, _ -> b
    | _, Bot -> a
    | Var _, _ -> b
    | _, Var _ -> a
    | Nullable a, Nullable b -> mk (Nullable (sup a b))
    | Nullable a, _ -> mk (Nullable (sup a b))
    | _, Nullable b -> mk (Nullable (sup a b))
    | List a, List b -> mk (List (sup a b))
    | Arrow (p, a), Arrow (q, b) ->
        if List.length p <> List.length q then raise Incompatible;
        mk (Arrow (q, sup a b))
    | Tuple l, Tuple m ->
        if List.length l <> List.length m then raise Incompatible;
        mk (Tuple (List.map2 sup l m))
    | Ground g, Ground g' ->
        (* It might happen that we compare abstract values. *)
        (try if g <> g' then raise Incompatible with _ -> ());
        mk (Ground g')
    | Meth (l, t, _, a), Meth (l', t', d', b) when l = l' ->
        mk (Meth (l, scheme_sup t t', d', sup a b))
    | Meth (l, t, d, a), _ -> (
        match meth_type l b with
          (* TODO: I guess that we should hide other methods named l *)
          | Some t' -> mk (Meth (l, scheme_sup t' t, d, sup a b))
          | None -> sup a b)
    | _, Meth (l, t, d, b) -> (
        match meth_type l a with
          (* TODO: I guess that we should hide other methods named l *)
          | Some t' -> mk (Meth (l, scheme_sup t' t, d, sup a b))
          | None -> sup a b)
    | ( Constr { constructor = c; params = a },
        Constr { constructor = d; params = b } ) ->
        if c <> d || List.length a <> List.length b then raise Incompatible;
        let params =
          List.map2
            (fun (v, a) (v', b) ->
              if v <> v' then raise Incompatible;
              (v, sup a b))
            a b
        in
        mk (Constr { constructor = c; params })
    | Getter a, Getter b -> mk (Getter (sup a b))
    | Getter a, Arrow ([], b) -> mk (Getter (sup a b))
    | Getter a, _ -> mk (Getter (sup a b))
    | Arrow ([], a), Getter b -> mk (Getter (sup a b))
    | _, Getter b -> mk (Getter (sup a b))
    | _, _ ->
        if !debug_subtyping then
          failwith
            (Printf.sprintf "\nFailed sup: %s \\/ %s\n\n%!" (Type.print a)
               (Type.print b))
        else raise Incompatible

let sup ~pos a b =
  let b' = sup ~pos a b in
  if !debug_subtyping && b' != b then
    Printf.printf "sup: %s \\/ %s = %s\n%! " (Type.print a) (Type.print b)
      (Type.print b');
  b'

exception Occur_check of var * t

(** Check that [a] (a dereferenced type variable) does not occur in [b], and
    prepare the instantiation [a<-b] by adjusting the levels. *)
let rec occur_check (a : var) b =
  let b0 = b in
  let b = deref b in
  match b.descr with
    | Constr c -> List.iter (fun (_, x) -> occur_check a x) c.params
    | Tuple l -> List.iter (occur_check a) l
    | Getter t -> occur_check a t
    | List t -> occur_check a t
    | Nullable t -> occur_check a t
    | Meth (_, (_, t), _, u) ->
        (* We assume that a is not a generalized variable of t. *)
        occur_check a t;
        occur_check a u
    | Arrow (p, t) ->
        List.iter (fun (_, _, t) -> occur_check a t) p;
        occur_check a t
    | Ground _ | Bot -> ()
    | Var { contents = Free b } ->
        if a.name = b.name then raise (Occur_check (a, b0));
        b.level <- min a.level b.level
    | Var { contents = Link _ } -> assert false

(** {1 Constraints} *)

exception Unsatisfied_constraint of constr * t

(** Ensure that a type satisfies a given constraint, i.e. morally that b <: c. *)
let satisfies_constraint b = function
  | Ord ->
      let rec check b =
        let m, b = split_meths b in
        match b.descr with
          | Ground _ -> ()
          | Var { contents = Free v } ->
              if not (List.mem Ord v.constraints) then
                v.constraints <- Ord :: v.constraints
          | Tuple [] ->
              (* For records, we want to ensure that all fields are ordered. *)
              List.iter
                (fun (_, ((v, a), _)) ->
                  if v <> [] then raise (Unsatisfied_constraint (Ord, a));
                  check a)
                m
          | Tuple l -> List.iter (fun b -> check b) l
          | List b -> check b
          | Nullable b -> check b
          | _ -> raise (Unsatisfied_constraint (Ord, b))
      in
      check b
  | Dtools -> (
      match b.descr with
        | Ground g ->
            if not (List.mem g [Bool; Int; Float; String]) then
              raise (Unsatisfied_constraint (Dtools, b))
        | Tuple [] -> ()
        | List b' -> (
            match (deref b').descr with
              | Ground g ->
                  if g <> String then
                    raise (Unsatisfied_constraint (Dtools, b'))
              | Var ({ contents = Free _ } as v) ->
                  (* TODO: we should check the constraints and the lower bound *)
                  v := Link (make ~pos:b.pos (Ground String))
              | _ -> raise (Unsatisfied_constraint (Dtools, b')))
        | Var { contents = Free v } ->
            if not (List.mem Dtools v.constraints) then
              v.constraints <- Dtools :: v.constraints
        | _ -> raise (Unsatisfied_constraint (Dtools, b)))
  | InternalMedia -> (
      let is_internal name =
        try
          let kind = Frame_content.kind_of_string name in
          Frame_content.is_internal kind
        with Frame_content.Invalid -> false
      in
      match b.descr with
        | Constr { constructor } when is_internal constructor -> ()
        | Ground (Format f) when Frame_content.(is_internal (kind f)) -> ()
        | Var { contents = Free v } ->
            if not (List.mem InternalMedia v.constraints) then
              v.constraints <- InternalMedia :: v.constraints
        | _ -> raise (Unsatisfied_constraint (InternalMedia, b)))
  | Num -> (
      match (demeth b).descr with
        | Ground g ->
            if g <> Int && g <> Float then
              raise (Unsatisfied_constraint (Num, b))
        | Var { contents = Free v } ->
            if not (List.mem Num v.constraints) then
              v.constraints <- Num :: v.constraints
        | _ -> raise (Unsatisfied_constraint (Num, b)))

let satisfies_constraints b c = List.iter (satisfies_constraint b) c

(** {1 Type checking/inference} *)

exception Error of (repr * repr)

(** Make a variable link to given type *)
let rec bind a b =
  let a0 = a in
  let v, a =
    match a.descr with
      | Var ({ contents = Free a } as v) -> (v, a)
      | _ -> assert false
  in
  (* if !debug then Printf.eprintf "%s := %s\n%!" (print a0) (print b); *)
  let b = deref b in
  occur_check a b;
  satisfies_constraints b a.constraints;
  a.lower_bound <: b;
  let b = if b.pos = None then { b with pos = a0.pos } else b in
  v := Link b

(* I'd like to add subtyping on unions of scalar types, but for now the only
 * non-trivial thing is the arrow.
 * We allow
 *  (L1@L2)->T <: (L1)->T        if L2 is purely optional
 *  (L1@L2)->T <: (L1)->(L2)->T  otherwise (at least one mandatory param in L2)
 *
 * Memo: A <: B means that any value of type A can be passed where a value
 * of type B can. Indeed, if you can pass a function, you can also pass the same
 * one with extra optional parameters.
 *
 * This relation must be transitive. Note that it is not safe to allow the
 * promotion of optional parameters into mandatory ones, because the function
 * with the optional parameter, when fully applied, applies implicitly its
 * optional argument; whereas with a mandatory argument it is expected to wait
 * for it. *)

(** Ensure that a<:b, perform unification if needed. In case of error, generate
    an explanation. *)
and ( <: ) a b =
  if !debug || !debug_subtyping then
    Printf.eprintf "\n%s <: %s\n%!" (print a) (print b);
  let a = deref a in
  let b = deref b in
  match (a.descr, b.descr) with
    | Bot, _ -> ()
    | Constr c1, Constr c2 when c1.constructor = c2.constructor ->
        let rec aux pre p1 p2 =
          match (p1, p2) with
            | (v1, h1) :: t1, (v2, h2) :: t2 ->
                begin
                  try
                    let v = if v1 = v2 then v1 else Invariant in
                    match v with
                      | Covariant -> h1 <: h2
                      | Contravariant -> h2 <: h1
                      | Invariant ->
                          h1 <: h2;
                          h2 <: h1
                  with Error (a, b) ->
                    let bt = Printexc.get_raw_backtrace () in
                    let post = List.map (fun (v, _) -> (v, `Ellipsis)) t1 in
                    Printexc.raise_with_backtrace
                      (Error
                         ( `Constr (c1.constructor, pre @ [(v1, a)] @ post),
                           `Constr (c1.constructor, pre @ [(v2, b)] @ post) ))
                      bt
                end;
                aux ((v1, `Ellipsis) :: pre) t1 t2
            | [], [] -> ()
            | _ -> assert false
          (* same name => same arity *)
        in
        aux [] c1.params c2.params
    | List t1, List t2 -> (
        try t1 <: t2 with Error (a, b) -> raise (Error (`List a, `List b)))
    | Nullable t1, Nullable t2 -> (
        try t1 <: t2
        with Error (a, b) -> raise (Error (`Nullable a, `Nullable b)))
    | Tuple l, Tuple m ->
        if List.length l <> List.length m then (
          let l = List.map (fun _ -> `Ellipsis) l in
          let m = List.map (fun _ -> `Ellipsis) m in
          raise (Error (`Tuple l, `Tuple m)));
        let n = ref 0 in
        List.iter2
          (fun a b ->
            incr n;
            try a <: b
            with Error (a, b) ->
              let bt = Printexc.get_raw_backtrace () in
              let l = List.init (!n - 1) (fun _ -> `Ellipsis) in
              let l' = List.init (List.length m - !n) (fun _ -> `Ellipsis) in
              Printexc.raise_with_backtrace
                (Error (`Tuple (l @ [a] @ l'), `Tuple (l @ [b] @ l')))
                bt)
          l m
    | Arrow (l12, t), Arrow (l, t') ->
        (* Here, it must be that l12 = l1@l2 where l1 is essentially l modulo
           order and either l2 is erasable and t<:t' or (l2)->t <: t'. *)
        let ellipsis = (false, "", `Range_Ellipsis) in
        let elide (o, l, _) = (o, l, `Ellipsis) in
        let l1, l2 =
          List.fold_left
            (* Start with [l2:=l12], [l1:=[]] and move each param [o,lbl]
               required by [l] from [l2] to [l1]. *)
              (fun (l1, l2) (o, lbl, t) ->
              (* Search for a param with optionality o and label lbl. Returns
                 the first matching parameter and the list without it. *)
              let rec get_param acc = function
                | [] ->
                    raise
                      (Error
                         ( `Arrow
                             (List.rev_append l1 (List.map elide l2), `Ellipsis),
                           `Arrow
                             ( List.rev (ellipsis :: (o, lbl, `Ellipsis) :: l1),
                               `Ellipsis ) ))
                | (o', lbl', t') :: tl ->
                    if o = o' && lbl = lbl' then
                      ((o', lbl', t'), List.rev_append acc tl)
                    else get_param ((o', lbl', t') :: acc) tl
              in
              let (o, lbl, t'), l2' = get_param [] l2 in
              (* Check on-the-fly that the types match. *)
              begin
                try t <: t'
                with Error (t, t') ->
                  let bt = Printexc.get_raw_backtrace () in
                  let make t =
                    `Arrow (List.rev (ellipsis :: (o, lbl, t) :: l1), `Ellipsis)
                  in
                  Printexc.raise_with_backtrace (Error (make t', make t)) bt
              end;
              ((o, lbl, `Ellipsis) :: l1, l2'))
            ([], l12) l
        in
        let l1 = List.rev l1 in
        if List.for_all (fun (o, _, _) -> o) l2 then (
          try t <: t'
          with Error (t, t') ->
            let bt = Printexc.get_raw_backtrace () in
            Printexc.raise_with_backtrace
              (Error (`Arrow ([ellipsis], t), `Arrow ([ellipsis], t')))
              bt)
        else (
          try { a with descr = Arrow (l2, t) } <: t' with
            | Error (`Arrow (p, t), t') ->
                raise (Error (`Arrow (l1 @ p, t), `Arrow (l1, t')))
            | Error _ -> assert false)
    | Ground (Format k), Ground (Format k') -> (
        try Frame_content.merge k k' with _ -> raise (Error (repr a, repr b)))
    | Ground x, Ground y -> if x <> y then raise (Error (repr a, repr b))
    | Getter t1, Getter t2 -> (
        try t1 <: t2 with Error (a, b) -> raise (Error (`Getter a, `Getter b)))
    | Arrow ([], t1), Getter t2 -> (
        try t1 <: t2
        with Error (a, b) -> raise (Error (`Arrow ([], a), `Getter b)))
    | Var { contents = Free v }, Var { contents = Free v' } when var_eq v v' ->
        ()
    | Var { contents = Free v }, _ -> (
        (* TODO: we could add an upper bound, but this is left for later. *)
        try bind a b
        with Occur_check _ | Unsatisfied_constraint _ ->
          (* Can't do more concise than a full representation, as the problem
             isn't local. *)
          raise (Error (repr a, repr b)))
    | _, Var { contents = Free v } ->
        (*
        occur_check v a;
        satisfies_constraints a v.constraints;
        let lb = v.lower_bound in
        v.lower_bound <- sup ~pos:b.pos v.lower_bound a;
        try
          lb <: v.lower_bound;
          a <: v.lower_bound
        with _ ->
          failwith
            (Printf.sprintf "sup did to increase: %s sup %s not <: %s"
               (Type.print lb) (Type.print a) (Type.print v.lower_bound))
         *)
        (* TODO: restore subtyping... *)
        bind b a
    (*
    | Var _, _ -> (
        try bind a b
        with Occur_check _ | Unsatisfied_constraint _ ->
          (* Can't do more concise than a full representation, as the problem
             isn't local. *)
          raise (Error (repr a, repr b)))
    | _, Var { contents = Free v }
    (* Force dropping the methods when we have constraints (see #1496) unless
       we are comparing records (see #1930). *)
      when (not (has_meth a)) || v.constraints = [] || (demeth a).descr = unit -> (
        try bind b a
        with Occur_check _ | Unsatisfied_constraint _ ->
          raise (Error (repr a, repr b)))
    *)
    | _, Nullable t2 -> (
        try a <: t2 with Error (a, b) -> raise (Error (a, `Nullable b)))
    | Meth (l, (g1, t1), _, u1), Meth (l', (g2, t2), _, u2) when l = l' -> (
        (* Handle explicitly this case in order to avoid #1842. *)
        try
          (* TODO: we should perform proper type scheme subtyping, but this
                is a good approximation for now... *)
          instantiate ~level:max_int ~generalized:g1 t1
          <: instantiate ~level:max_int ~generalized:g2 t2;
          u1 <: u2
        with Error (a, b) ->
          let bt = Printexc.get_raw_backtrace () in
          Printexc.raise_with_backtrace
            (Error (`Meth (l, ([], a), `Ellipsis), `Meth (l, ([], b), `Ellipsis)))
            bt)
    | _, Meth (l, (g2, t2), _, u2) -> (
        try
          let g1, t1 = invoke a l in
          (try
             (* TODO: we should perform proper type scheme subtyping, but this
                is a good approximation for now... *)
             instantiate ~level:max_int ~generalized:g1 t1
             <: instantiate ~level:max_int ~generalized:g2 t2
           with Error (a, b) ->
             let bt = Printexc.get_raw_backtrace () in
             Printexc.raise_with_backtrace
               (Error
                  (`Meth (l, ([], a), `Ellipsis), `Meth (l, ([], b), `Ellipsis)))
               bt);
          try a <: hide_meth l u2
          with Error (a, b) ->
            let bt = Printexc.get_raw_backtrace () in
            Printexc.raise_with_backtrace
              (Error (a, `Meth (l, ([], `Ellipsis), b)))
              bt
        with Not_found -> (
          let a' = demeth a in
          match a'.descr with
            | Var _ ->
                a' <: make (Meth (l, (g2, t2), "", Type.var ()));
                a <: b
            | _ -> raise (Error (repr a, `Meth (l, ([], `Ellipsis), `Ellipsis))))
        )
    | Meth (l, _, _, u1), _ -> hide_meth l u1 <: b
    | _, Getter t2 -> (
        try a <: t2 with Error (a, b) -> raise (Error (a, `Getter b)))
    | _, _ ->
        (* The superficial representation is enough for explaining the
           mismatch. *)
        let filter () =
          let already = ref false in
          function
          | { descr = Var { contents = Link _ }; _ } -> false
          | _ ->
              let x = !already in
              already := true;
              x
        in
        let a = repr ~filter_out:(filter ()) a in
        let b = repr ~filter_out:(filter ()) b in
        raise (Error (a, b))

let ( >: ) a b =
  try b <: a
  with Error (y, x) ->
    let bt = Printexc.get_raw_backtrace () in
    Printexc.raise_with_backtrace (Type_error (true, b, a, y, x)) bt

let ( <: ) a b =
  try a <: b
  with Error (x, y) ->
    let bt = Printexc.get_raw_backtrace () in
    Printexc.raise_with_backtrace (Type_error (false, a, b, x, y)) bt
