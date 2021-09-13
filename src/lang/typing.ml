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

(* let () = Type.debug := true *)

let debug_subtyping = ref true
let () = Type.debug_subtyping := !debug_subtyping

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
let generalizable ~level t = filter_vars (fun t -> t.level >= level) t

(** Substitutions. *)
module Subst = struct
  module M = Map.Make (struct
    type t = var

    (* We can compare variables with their indices. *)
    let compare (x : var) (y : var) = compare (fst x) (fst y)
  end)

  type subst = t M.t
  type t = subst

  let of_seq seq : t = M.of_seq seq

  (** Retrieve the value of a variable. *)
  let value (s : t) (i : var) = M.find i s

  (* let filter f (s : t) = M.filter (fun i t -> f i t) s *)

  (** Whether we have the identity substitution. *)
  let is_identity (s : t) = M.is_empty s
end

(** Copy a term, substituting some EVars as indicated by a list
  * of associations. Other EVars are not copied, so sharing is
  * preserved. *)
let copy_with (subst : Subst.t) t =
  let rec aux t =
    let cp x = { t with descr = x } in
    match t.descr with
      | EVar v -> ( try Subst.value subst v with Not_found -> t)
      | Constr c ->
          let params = List.map (fun (v, t) -> (v, aux t)) c.params in
          cp (Constr { c with params })
      | Ground _ -> cp t.descr
      | AString s -> cp (AString s)
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
      | Union (t, u) -> cp (Union (aux t, aux u))
      | Link (v, t) ->
          (* Keep links to preserve rich position information,
           * and to make it possible to check if the application left
           * the type unchanged. *)
          cp (Link (v, aux t))
  in
  if Subst.is_identity subst then t else aux t

(** Instantiate a type scheme, given as a type together with a list
  * of generalized variables.
  * Fresh variables are created with the given (current) level,
  * and attached to the appropriate constraints.
  * This erases position information, since they usually become
  * irrelevant. *)
let instantiate ~level ~generalized =
  let subst =
    Seq.map
      (fun ic -> (ic, fresh ~level ~constraints:(snd ic) ~pos:None))
      (List.to_seq generalized)
  in
  let subst = Subst.of_seq subst in
  fun t -> copy_with subst t

(** {1 Assignation} *)

(** These two exceptions can be raised when attempting to assign a variable. *)
exception Occur_check of t * t

exception Unsatisfied_constraint of constr * t

(** Check that [a] (a dereferenced type variable) does not occur in [b],
  * and prepare the instantiation [a<-b] by adjusting the levels. *)
let rec occur_check a b =
  let b = deref b in
  if a == b then raise (Occur_check (a, b));
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
    | Union (t, u) ->
        occur_check a t;
        occur_check a u
    | EVar _ ->
        (* In normal type inference level -1 should never arise.
         * Unfortunately we can't check it strictly because this code
         * is also used to process type annotations, which make use
         * of unknown levels. Also note that >=0 levels can arise
         * when processing type annotations, because of builtins. *)
        if b.level = -1 then b.level <- a.level
        else if a.level <> -1 then b.level <- min b.level a.level
    | Ground _ -> ()
    | AString _ -> ()
    | Link _ -> assert false

(* Perform [a := b] where [a] is an EVar, check that [type(a)<:type(b)]. *)
let rec bind ?(variance = Invariant) a0 b0 =
  if !debug || true then
    Printf.eprintf "%s := %s%s\n%!" (print a0) (print b0)
      (if variance = Covariant then " (covariant)" else "");
  let a = deref a0 in
  let b = deref b0 in
  if b == a then ()
  else (
    occur_check a b;
    let constraints =
      match a.descr with
        | EVar (_, constraints) ->
            List.iter
              (function
                | Ord ->
                    let rec check b =
                      let m, b = split_meths b in
                      match b.descr with
                        | Ground _ -> ()
                        | AString _ -> ()
                        | EVar (j, c) ->
                            if List.mem Ord c then ()
                            else b.descr <- EVar (j, Ord :: c)
                        | Tuple [] ->
                            (* For records, we want to ensure that all fields are ordered. *)
                            List.iter
                              (fun (_, ((v, a), _)) ->
                                if v <> [] then
                                  raise (Unsatisfied_constraint (Ord, a));
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
                            | EVar (_, _) -> bind b' (make (Ground String))
                            | _ -> raise (Unsatisfied_constraint (Dtools, b')))
                      | EVar (j, c) ->
                          if not (List.mem Dtools c) then
                            b.descr <- EVar (j, Dtools :: c)
                      | _ -> raise (Unsatisfied_constraint (Dtools, b)))
                | InternalMedia -> (
                    let is_internal name =
                      try
                        let kind = Frame_content.kind_of_string name in
                        Frame_content.is_internal kind
                      with Frame_content.Invalid -> false
                    in
                    match b.descr with
                      | Constr { name } when is_internal name -> ()
                      | Ground (Format f)
                        when Frame_content.(is_internal (kind f)) ->
                          ()
                      | EVar (j, c) ->
                          if List.mem InternalMedia c then ()
                          else b.descr <- EVar (j, InternalMedia :: c)
                      | _ -> raise (Unsatisfied_constraint (InternalMedia, b)))
                | Num -> (
                    match (demeth b).descr with
                      | Ground g ->
                          if g <> Int && g <> Float then
                            raise (Unsatisfied_constraint (Num, b))
                      | EVar (j, c) ->
                          if List.mem Num c then ()
                          else b.descr <- EVar (j, Num :: c)
                      | _ -> raise (Unsatisfied_constraint (Num, b))))
              constraints;
            constraints
        | _ -> assert false
      (* only EVars are bindable *)
    in

    (* We do not want to check the constraints when we increase the types (for
       now). *)
    (* let variance = if constraints <> [] then Invariant else variance in *)
    (* TODO: this is actually quite ennoying, better be slightly unsafe *)
    ignore constraints;

    (* This is a shaky hack...
     * When a value is passed to a FFI, its type is bound to a type without
     * any location.
     * If it doesn't break sharing, we set the parsing position of
     * that variable occurrence to the position of the inferred type. *)
    if b.pos = None && match b.descr with EVar _ -> false | _ -> true then
      a.descr <- Link (variance, { a0 with descr = b.descr })
    else a.descr <- Link (variance, b))

(** {1 Subtype checking/inference} *)

exception Incompatible

(** Approximated supremum of two types. We grow the second argument so that it
    has a chance be be greater than the first. No binding is performed by this
    function so that it should always be followed by a subtyping. *)
let rec sup ~pos a b =
  (* Printf.printf "sup %s // %s\n%!" (Type.print a) (Type.print b); *)
  let sup = sup ~pos in
  let mk descr = { level = -1; pos; descr } in
  let scheme_sup t t' =
    match (t, t') with ([], t), ([], t') -> ([], sup t t') | _ -> t'
  in
  let rec meth_type l a =
    match (deref a).descr with
      | Meth (l', t, _, _) when l = l' -> Some t
      | Meth (_, _, _, a) -> meth_type l a
      | EVar _ -> Some ([], fresh_evar ~level:max_int ~pos)
      | _ -> None
  in
  match ((deref a).descr, (deref b).descr) with
    | EVar _, _ -> b
    | _, EVar _ -> a
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
    | AString s, AString t ->
        if s <> t then mk (Ground String) else mk (AString t)
    | AString _, Ground String -> mk (Ground String)
    | Ground String, AString _ -> mk (Ground String)
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
    | Constr { name = c; params = a }, Constr { name = d; params = b } ->
        if c <> d || List.length a <> List.length b then raise Incompatible;
        let params =
          List.map2
            (fun (v, a) (v', b) ->
              if v <> v' then raise Incompatible;
              (v, sup a b))
            a b
        in
        mk (Constr { name = c; params })
    | Getter a, Getter b -> mk (Getter (sup a b))
    | Getter a, Arrow ([], b) -> mk (Getter (sup a b))
    | Getter a, _ -> mk (Getter (sup a b))
    | Arrow ([], a), Getter b -> mk (Getter (sup a b))
    | _, Getter b -> mk (Getter (sup a b))
    | _, _ -> raise Incompatible

let sup ~pos a b =
  let b' =
    try sup ~pos a b
    with Incompatible as e ->
      if !debug_subtyping then
        failwith
          (Printf.sprintf "\nFailed sup: %s \\/ %s\n\n%!" (Type.print a)
             (Type.print b))
      else raise e
  in
  if !debug_subtyping && b' != b then
    Printf.printf "sup: %s \\/ %s = %s\n%! " (Type.print a) (Type.print b)
      (Type.print b');
  b'

exception Error of (repr * repr)

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

(** The subtyping function. We ensure that a<:b, and perform unification if
    needed. In case of error, we generate an explanation. *)
let rec ( <: ) a b =
  if !debug || !debug_subtyping then
    Printf.eprintf "\n%s <: %s\n%!" (print a) (print b);
  match (a.descr, b.descr) with
    | _, Link (Covariant, b') ->
        (* When the variable is covariant, we take the opportunity here to correct
           bad choices. For instance, if we took int, but then have a 'a?, we
           change our mind and use int? instead. *)
        let b'' = try sup ~pos:b.pos a b' with Incompatible -> b' in
        (* Printf.printf "the sup of %s and %s is %s\n%!" (Type.print a) *)
        (* (Type.print b') (Type.print b''); *)
        (try b' <: b''
         with _ ->
           failwith
             (Printf.sprintf "sup did to increase: %s !< %s" (Type.print b')
                (Type.print b'')));
        if b'' != b' then
          (* Printf.printf "%s becomes %s\n%!" (Type.print b) (Type.print b''); *)
          b.descr <- Link (Covariant, b'');
        a <: b''
    | Link (Covariant, a'), _ ->
        a.descr <- Link (Invariant, a');
        a' <: a
    | _, Link (_, b) -> a <: b
    | Link (_, a), _ -> a <: b
    | Constr c1, Constr c2 when c1.name = c2.name ->
        let rec aux pre p1 p2 =
          match (p1, p2) with
            | (v, h1) :: t1, (_, h2) :: t2 ->
                begin
                  try (* TODO use variance info *)
                      h1 <: h2
                  with Error (a, b) ->
                    let bt = Printexc.get_raw_backtrace () in
                    let post = List.map (fun (v, _) -> (v, `Ellipsis)) t1 in
                    Printexc.raise_with_backtrace
                      (Error
                         ( `Constr (c1.name, pre @ [(v, a)] @ post),
                           `Constr (c1.name, pre @ [(v, b)] @ post) ))
                      bt
                end;
                aux ((v, `Ellipsis) :: pre) t1 t2
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
                try t' <: t
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
    | AString s, AString s' -> if s <> s' then raise (Error (repr a, repr b))
    | AString _, Ground Type.String -> ()
    | Getter t1, Getter t2 -> (
        try t1 <: t2 with Error (a, b) -> raise (Error (`Getter a, `Getter b)))
    | Arrow ([], t1), Getter t2 -> (
        try t1 <: t2
        with Error (a, b) -> raise (Error (`Arrow ([], a), `Getter b)))
    | EVar _, _ -> (
        (* The variance might be surprising here: we suppose that universally
           quantified variables are always covariant and change the variance
           later on if they are used as lower bound. *)
        try bind ~variance:Covariant a b
        with Occur_check _ | Unsatisfied_constraint _ ->
          (* Can't do more concise than a full representation, as the problem
             isn't local. *)
          raise (Error (repr a, repr b)))
    | _, EVar (_, c)
    (* Force dropping the methods when we have constraints, see #1496. *)
      when not (has_meth a && c <> []) -> (
        try bind ~variance:Covariant b a
        with Occur_check _ | Unsatisfied_constraint _ ->
          raise (Error (repr a, repr b)))
    | Union (u, v), _ ->
        u <: b;
        v <: v
    | _, Union (u, v) -> (
        try a <: u
        with Error _ -> (
          try a <: v with Error _ -> raise (Error (repr a, repr b))))
    | _, Nullable t2 -> (
        try a <: t2 with Error (a, b) -> raise (Error (a, `Nullable b)))
    | Meth (l, (g1, t1), _, u1), Meth (l', (g2, t2), _, u2) when l = l' -> (
        (* Handle explicitly this case in order to avoid #1842. *)
        try
          (* TODO: we should perform proper type scheme subtyping, but this
                is a good approximation for now... *)
          instantiate ~level:(-1) ~generalized:g1 t1
          <: instantiate ~level:(-1) ~generalized:g2 t2;
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
             instantiate ~level:(-1) ~generalized:g1 t1
             <: instantiate ~level:(-1) ~generalized:g2 t2
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
            | EVar _ ->
                a'
                <: make
                     (Meth
                        ( l,
                          (g2, t2),
                          "",
                          fresh ~level:(-1) ~constraints:[] ~pos:None ));
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
          | { descr = Link _; _ } -> false
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
