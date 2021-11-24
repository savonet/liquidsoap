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

let () = Type.debug := false
let () = Type.debug_levels := false
let () = Type.debug_variance := false
let debug_subtyping = ref false

type env = (string * scheme) list

(*
(** Do we have a method. *)
let has_meth a = match (deref a).descr with Meth _ -> true | _ -> false

let rec hide_meth l a =
  match (deref a).descr with
    | Meth ({ meth = l' }, u) when l' = l -> hide_meth l u
    | Meth (m, u) -> { a with descr = Meth (m, hide_meth l u) }
    | _ -> a
*)

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

let generalize ~level t : scheme = (generalizable ~level t, t)

(** Substitutions. *)
module Subst = struct
  module M = Map.Make (struct
    type t = var

    (* We can compare variables with their indices. *)
    let compare (v : var) (v' : var) = compare v.name v'.name
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

(** Instantiate a type scheme, given as a type together with a list of
    generalized variables. Fresh variables are created with the given (current)
    level, and attached to the appropriate constraints. This erases position
    information, since they usually become irrelevant. *)
let instantiate ~level ~generalized =
  let memo = ref ([] : (Type.var * Type.descr) list) in
  let rec subst_var v =
    try List.assoc v !memo
    with Not_found ->
      if List.mem v generalized then (
        let x = var ~level ~constraints:v.constraints () in
        memo := (v, x.descr) :: !memo;
        let v' = match x.descr with Var v' -> v' | _ -> assert false in
        v'.lower <- copy v.lower;
        v'.upper <- copy v.upper;
        x.descr)
      else Var v
  and copy t =
    let descr =
      match t.descr with
        | Var v -> subst_var v
        | Constr c ->
            let params = List.map (fun (v, t) -> (v, copy t)) c.params in
            Constr { c with params }
        | Ground _ as g -> g
        | Getter t -> Getter (copy t)
        | List { t; json_repr } -> List { t = copy t; json_repr }
        | Nullable t -> Nullable (copy t)
        | Tuple l -> Tuple (List.map copy l)
        | Meth (({ scheme = g, t } as m), u) ->
            (* We assume that we don't substitute generalized variables. *)
            if !debug then
              assert (List.for_all (fun (v, _) -> not (List.mem v g)) !memo);
            Meth ({ m with scheme = (g, copy t) }, copy u)
        | Arrow (p, t) ->
            Arrow (List.map (fun (o, l, t) -> (o, l, copy t)) p, copy t)
    in
    { t with descr }
  in
  copy

(** {1 Assignation} *)

(** These two exceptions can be raised when attempting to assign a variable. *)
exception Occur_check of var * t

exception Unsatisfied_constraint of constr * t

(*
(** Check that [a] (a dereferenced type variable) does not occur in [b],
    and prepare the instantiation [a<-b] by adjusting the levels. *)
let rec occur_check (a : var) b =
  let b0 = b in
  let b = deref b in
  match b.descr with
    | Constr c -> List.iter (fun (_, x) -> occur_check a x) c.params
    | Tuple l -> List.iter (occur_check a) l
    | Getter t -> occur_check a t
    | List { t } -> occur_check a t
    | Nullable t -> occur_check a t
    | Meth ({ scheme = _, t }, u) ->
        (* We assume that a is not a generalized variable of t. *)
        occur_check a t;
        occur_check a u
    | Arrow (p, t) ->
        List.iter (fun (_, _, t) -> occur_check a t) p;
        occur_check a t
    | Ground _ -> ()
    | Var { contents = Free b } ->
        if a.name = b.name then raise (Occur_check (a, b0));
        b.level <- min a.level b.level
    | Var { contents = Link _ } -> assert false
*)

(* TODO: restore this *)
(*
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
                (fun Type.{ scheme = v, a } ->
                  if v <> [] then raise (Unsatisfied_constraint (Ord, a));
                  check a)
                m
          | Tuple l -> List.iter (fun b -> check b) l
          | List { t = b } -> check b
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
        | List { t = b' } -> (
            match (deref b').descr with
              | Ground g ->
                  if g <> String then
                    raise (Unsatisfied_constraint (Dtools, b'))
              | Var ({ contents = Free _ } as v) ->
                  (* TODO: we should check the constraints *)
                  v := Link (Invariant, make ?pos:b.pos (Ground String))
              | _ -> raise (Unsatisfied_constraint (Dtools, b')))
        | Var { contents = Free v } ->
            if not (List.mem Dtools v.constraints) then
              v.constraints <- Dtools :: v.constraints
        | _ -> raise (Unsatisfied_constraint (Dtools, b)))
  | InternalMedia -> (
      let is_internal name =
        try
          let kind = Content.kind_of_string name in
          Content.is_internal kind
        with Content.Invalid -> false
      in
      match b.descr with
        | Constr { constructor } when is_internal constructor -> ()
        | Ground (Format f) when Content.(is_internal (kind f)) -> ()
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
*)

(*
(** Make a variable link to given type *)
let bind ?(variance = Invariant) a b =
  let a0 = a in
  let v, a =
    match a.descr with
      | Var ({ contents = Free a } as v) -> (v, a)
      | _ -> assert false
  in
  if !debug then
    Printf.printf "\n%s := %s\n%!" (Type.to_string a0) (Type.to_string b);
  let b = deref b in
  occur_check a b;
  satisfies_constraints b a.constraints;
  let b = if b.pos = None then { b with pos = a0.pos } else b in
  (* We do not want to check the constraints when we increase the types (for
     now). *)
  let variance = if a.constraints <> [] then Invariant else variance in
  v := Link (variance, b)
*)

(*
(** Lower all type variables to given level. *)
let update_level ~level a =
  let x = Type.var ~level () in
  let x =
    match x.descr with Var { contents = Free x } -> x | _ -> assert false
  in
  occur_check x a
*)

(** {1 Subtype checking/inference} *)

exception Incompatible

(*
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
      | Meth ({ meth = l'; scheme = t }, _) when l = l' -> Some t
      | Meth (_, a) -> meth_type l a
      | Var { contents = Free _ } -> Some ([], var ?pos ())
      | _ -> None
  in
  match ((deref a).descr, (deref b).descr) with
    | Var { contents = Free _ }, _ -> b
    | _, Var { contents = Free _ } -> a
    | Nullable a, Nullable b -> mk (Nullable (sup a b))
    | Nullable a, _ -> mk (Nullable (sup a b))
    | _, Nullable b -> mk (Nullable (sup a b))
    | List { t = a }, List { t = b } ->
        mk (List { t = sup a b; json_repr = `Tuple })
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
    | Meth (m, a), Meth (m', b) when m.meth = m'.meth ->
        mk (Meth ({ m' with scheme = scheme_sup m.scheme m'.scheme }, sup a b))
    | Meth (m, a), _ -> (
        match meth_type m.meth b with
          (* TODO: I guess that we should hide other methods named l *)
          | Some t' ->
              mk (Meth ({ m with scheme = scheme_sup t' m.scheme }, sup a b))
          | None -> sup a b)
    | _, Meth (m, b) -> (
        match meth_type m.meth a with
          (* TODO: I guess that we should hide other methods named l *)
          | Some t' ->
              mk (Meth ({ m with scheme = scheme_sup t' m.scheme }, sup a b))
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
            (Printf.sprintf "\nFailed sup: %s \\/ %s\n\n%!" (Type.to_string a)
               (Type.to_string b))
        else raise Incompatible
*)

let sup a b = failwith "TODO"

let sup ~pos a b =
  let b' = sup ~pos a b in
  if !debug_subtyping && b' != b then
    Printf.printf "sup: %s \\/ %s = %s\n%! " (Type.to_string a)
      (Type.to_string b) (Type.to_string b');
  b'

exception Error of (Repr.t * Repr.t)

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

(** Ensure that a<:b, perform unification if needed.
  * In case of error, generate an explanation. *)
let rec ( <: ) a b =
  if !debug || !debug_subtyping then
    Printf.printf "\n%s <: %s\n%!" (Type.to_string a) (Type.to_string b);
  match (a.descr, b.descr) with
    | Var { contents = Free v }, Var { contents = Free v' } when var_eq v v' ->
        ()
    | _, Var ({ contents = Link (Covariant, b') } as var) ->
        (* When the variable is covariant, we take the opportunity here to correct
           bad choices. For instance, if we took int, but then have a 'a?, we
           change our mind and use int? instead. *)
        let b'' = try sup ~pos:b'.pos a b' with Incompatible -> b' in
        (try b' <: b''
         with _ ->
           failwith
             (Printf.sprintf "sup did to increase: %s !< %s" (Type.to_string b')
                (Type.to_string b'')));
        if b'' != b' then var := Link (Covariant, b'');
        a <: b''
    | Var ({ contents = Link (Covariant, a') } as var), _ ->
        var := Link (Invariant, a');
        a <: b
    | _, Var { contents = Link (_, b) } -> a <: b
    | Var { contents = Link (_, a) }, _ -> a <: b
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
    | List { t = t1; json_repr = repr1 }, List { t = t2; json_repr = repr2 }
      -> (
        try t1 <: t2
        with Error (a, b) ->
          raise (Error (`List (a, repr1), `List (b, repr2))))
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
        try Content.merge k k'
        with _ -> raise (Error (Repr.make a, Repr.make b)))
    | Ground x, Ground y ->
        if x <> y then raise (Error (Repr.make a, Repr.make b))
    | Getter t1, Getter t2 -> (
        try t1 <: t2 with Error (a, b) -> raise (Error (`Getter a, `Getter b)))
    | Arrow ([], t1), Getter t2 -> (
        try t1 <: t2
        with Error (a, b) -> raise (Error (`Arrow ([], a), `Getter b)))
    | Var { contents = Free _ }, _ -> (
        try bind a b
        with Occur_check _ | Unsatisfied_constraint _ ->
          (* Can't do more concise than a full representation, as the problem
             isn't local. *)
          raise (Error (Repr.make a, Repr.make b)))
    | _, Var { contents = Free v }
    (* Force dropping the methods when we have constraints (see #1496) unless
       we are comparing records (see #1930). *)
      when (not (has_meth a)) || v.constraints = [] || (demeth a).descr = unit
      -> (
        try bind ~variance:Covariant b a
        with Occur_check _ | Unsatisfied_constraint _ ->
          raise (Error (Repr.make a, Repr.make b)))
    | _, Nullable t2 -> (
        try a <: t2 with Error (a, b) -> raise (Error (a, `Nullable b)))
    | ( Meth ({ meth = l; scheme = g1, t1; json_name = json_name1 }, u1),
        Meth ({ meth = l'; scheme = g2, t2; json_name = json_name2 }, u2) )
      when l = l' -> (
        (* Handle explicitly this case in order to avoid #1842. *)
        (try
           (* TODO: we should perform proper type scheme subtyping, but this
                 is a good approximation for now... *)
           instantiate ~level:(-1) ~generalized:g1 t1
           <: instantiate ~level:(-1) ~generalized:g2 t2
         with Error (a, b) ->
           let bt = Printexc.get_raw_backtrace () in
           Printexc.raise_with_backtrace
             (Error
                ( `Meth (l, ([], a), json_name1, `Ellipsis),
                  `Meth (l, ([], b), json_name2, `Ellipsis) ))
             bt);
        try u1 <: u2
        with Error (a, b) ->
          let bt = Printexc.get_raw_backtrace () in
          Printexc.raise_with_backtrace
            (Error
               ( `Meth (l, ([], `Ellipsis), json_name1, a),
                 `Meth (l, ([], `Ellipsis), json_name2, b) ))
            bt)
    | _, Meth ({ meth = l; scheme = g2, t2; json_name }, u2) -> (
        try
          let g1, t1 = invoke a l in
          (try
             (* TODO: we should perform proper type scheme subtyping, but this
                is a good approximation for now... *)
             instantiate ~level:(-1) ~generalized:g1 t1
             <: instantiate ~level:(-1) ~generalized:g2 t2
           with Error (a, b) ->
             let bt = Printexc.get_raw_backtrace () in
             Printexc.raise_with_backtrace
               (Error
                  ( `Meth (l, ([], a), None, `Ellipsis),
                    `Meth (l, ([], b), json_name, `Ellipsis) ))
               bt);
          try a <: hide_meth l u2
          with Error (a, b) ->
            let bt = Printexc.get_raw_backtrace () in
            Printexc.raise_with_backtrace
              (Error (a, `Meth (l, ([], `Ellipsis), json_name, b)))
              bt
        with Not_found -> (
          let a' = demeth a in
          match a'.descr with
            | Var { contents = Free _ } ->
                a'
                <: make
                     (Meth
                        ( {
                            meth = l;
                            scheme = (g2, t2);
                            doc = "";
                            json_name = None;
                          },
                          var () ));
                a <: b
            | _ ->
                raise
                  (Error
                     ( Repr.make a,
                       `Meth (l, ([], `Ellipsis), json_name, `Ellipsis) ))))
    | Meth (m, u1), _ -> hide_meth m.meth u1 <: b
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
        let a = Repr.make ~filter_out:(filter ()) a in
        let b = Repr.make ~filter_out:(filter ()) b in
        raise (Error (a, b))

let ( >: ) a b =
  try b <: a
  with Error (y, x) ->
    let bt = Printexc.get_raw_backtrace () in
    Printexc.raise_with_backtrace (Repr.Type_error (true, b, a, y, x)) bt

let ( <: ) a b =
  try a <: b
  with Error (x, y) ->
    let bt = Printexc.get_raw_backtrace () in
    Printexc.raise_with_backtrace (Repr.Type_error (false, a, b, x, y)) bt
