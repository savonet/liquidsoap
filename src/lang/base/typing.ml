(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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
let () = Repr.global_evar_names := false
let debug_subtyping = ref false

(** Allow functions to forget arguments during subtyping. This would not be a
    good idea if we had de Bruijn indices for instance. *)
let forget_arguments = true

type env = (string * scheme) list

(** {1 Type generalization and instantiation}

    We don't have type schemes per se, but we compute generalizable variables
    and keep track of them in the AST. This is simple and useful because in any
    case we need to distinguish two 'a variables bound at different places.
    Indeed, we might instantiate one in a term where the second is bound, and we
    don't want to merge the two when going under the binder.

    When generalizing we need to know what can be generalized in the outermost
    type but also in the inner types of the term forming a let-definition.
    Indeed those variables will have to be instantiated by fresh ones for every
    instance.

    If the value restriction applies, then we have some (fun (...) -> ...) and
    any type variable of higher level can be generalized, whether it's in the
    outermost type or not. *)

(** Find all the free variables satisfying a predicate. *)
let filter_vars f t =
  let rec aux l t =
    let t = deref t in
    match t.descr with
      | Int | Float | String | Bool | Never -> l
      | Custom c -> c.filter_vars aux l c.typ
      | Getter t -> aux l t
      | List { t } | Nullable t -> aux l t
      | Tuple aa -> List.fold_left aux l aa
      | Meth ({ scheme = g, t }, u) ->
          let l = List.filter (fun v -> not (List.mem v g)) (aux l t) in
          aux l u
      | Constr c -> List.fold_left (fun l (_, t) -> aux l t) l c.params
      | Arrow (p, t) -> aux (List.fold_left (fun l (_, _, t) -> aux l t) l p) t
      | Var { contents = Free var } ->
          if f var && not (List.exists (Var.eq var) l) then var :: l else l
      | Var { contents = Link _ } -> assert false
  in
  aux [] t

(** Return a list of generalizable variables in a type. This is performed after
    type inference on the left-hand side of a let-in, with [level] being the
    level of that let-in. Uses the simple method of ML, to be associated with a
    value restriction. *)
let generalizable ~level t = filter_vars (fun v -> v.level > level) t

let generalize ~level t : scheme = (generalizable ~level t, t)

(** Instantiate a type scheme, given as a type together with a list of
    generalized variables. This erases position information, since they usually
    become irrelevant. *)
let instantiate ~level ((generalized, t) : scheme) =
  if generalized = [] then t
  else (
    let mapper =
      Type.Fresh.init ~selector:(fun v -> List.memq v generalized) ~level ()
    in
    Type.Fresh.make mapper t)

(** {1 Assignation} *)

(** This exception can be raised when attempting to assign a variable. *)
exception Occur_check of var * t

(** Check that [a] (a dereferenced type variable) does not occur in [b] and
    prepare the instantiation [a<-b] by adjusting the levels. *)
let occur_check (a : var) =
  let rec occur_check = function
    | { descr = Int }
    | { descr = Float }
    | { descr = String }
    | { descr = Bool }
    | { descr = Never } ->
        ()
    | { descr = Constr c } -> List.iter (fun (_, x) -> occur_check x) c.params
    | { descr = Tuple l } -> List.iter occur_check l
    | { descr = Getter t } -> occur_check t
    | { descr = List { t } } -> occur_check t
    | { descr = Nullable t } -> occur_check t
    | { descr = Meth ({ scheme = g, t }, u) } ->
        (* We assume that a is not a generalized variable of t. *)
        (* TODO: we should not lower the level of bound variables, but this
           complicates the code and has little effect. *)
        assert (not (List.exists (Var.eq a) g));
        occur_check t;
        occur_check u
    | { descr = Arrow (p, t) } ->
        List.iter (fun (_, _, t) -> occur_check t) p;
        occur_check t
    | { descr = Custom c } -> c.occur_check occur_check c.typ
    | { descr = Var { contents = Free x } } as b ->
        if Type.Var.eq a x then raise (Occur_check (a, b));
        x.level <- min a.level x.level
    | { descr = Var { contents = Link (_, b) } } -> occur_check b
  in
  occur_check

let do_occur_check = ref true
let occur_check a t = if !do_occur_check then occur_check a t

(** Lower all type variables to given level. *)
let update_level level a =
  let x = Type.var ~level () in
  let x =
    match x.descr with Var { contents = Free x } -> x | _ -> assert false
  in
  occur_check x a

(** {1 Subtype checking/inference} *)

exception Incompatible

(** Approximated supremum of two types. We grow the second argument so that it
    has a chance be be greater than the first. No binding is performed by this
    function so that it should always be followed by a subtyping. *)
let rec sup ~pos a b =
  (* Printf.printf "  sup: %s \\/ %s\n%!" (Type.to_string a) (Type.to_string b); *)
  let sup = sup ~pos in
  let mk descr = Type.make ?pos descr in
  let scheme_sup t t' =
    match (t, t') with ([], t), ([], t') -> ([], sup t t') | _ -> t'
  in
  let rec meth_type l a =
    match (deref a).descr with
      | Meth ({ meth = l'; optional; scheme = t }, _) when l = l' ->
          Some (t, optional)
      | Meth (_, a) -> meth_type l a
      | _ -> None
  in
  let meth_sup m a b =
    let a = hide_meth m.meth a in
    let mb = meth_type m.meth b in
    let b = hide_meth m.meth b in
    match mb with
      | Some (t', optional) -> (
          try
            mk
              (Meth
                 ( {
                     m with
                     optional = m.optional || optional;
                     scheme = scheme_sup t' m.scheme;
                   },
                   sup a b ))
          with Incompatible -> sup a b)
      | None -> mk (Meth ({ m with optional = true }, sup a b))
  in
  let a = deref a in
  let b = deref b in
  if a == b then a
  else (
    match (a.descr, b.descr) with
      | v, v' when v == v' -> a
      | Var { contents = Free _ }, _ -> b
      | _, Var { contents = Free _ } -> a
      | Nullable a, Nullable b -> mk (Nullable (sup a b))
      | Nullable a, _ -> mk (Nullable (sup a b))
      | _, Nullable b -> mk (Nullable (sup a b))
      (* We implicitly convert it to float so we need to make sure to
         be more restrictive when mixing the two types and use int
         everywhere. *)
      | Int, Float | Float, Int -> mk Int
      | List { t = a }, List { t = b } ->
          mk (List { t = sup a b; json_repr = `Tuple })
      | Arrow (p, a), Arrow (q, b) ->
          if List.length p <> List.length q then raise Incompatible;
          mk (Arrow (q, sup a b))
      | Tuple l, Tuple m ->
          if List.length l <> List.length m then raise Incompatible;
          mk (Tuple (List.map2 sup l m))
      | Custom c, Custom c' -> (
          try mk (Custom { c with typ = c.sup sup c.typ c'.typ })
          with _ -> raise Incompatible)
      | Meth (m, a), _ -> meth_sup m a b
      | _, Meth (m, b) -> meth_sup m b a
      | Constr { constructor = "source" }, _
      | _, Constr { constructor = "source" }
      | Constr { constructor = "format" }, _
      | _, Constr { constructor = "format" } ->
          b
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
          else raise Incompatible)

let sup ~pos a b =
  let b' = sup ~pos a b in
  if !debug_subtyping && b' != b then
    Printf.printf "sup: %s \\/ %s = %s\n%! " (Type.to_string a)
      (Type.to_string b) (Type.to_string b');
  b'

exception Error of (Repr.t * Repr.t)

let () =
  Printexc.register_printer (function
    | Error (a, b) ->
        Some
          (Printf.sprintf "Typing error: %s vs %s" (Repr.to_string a)
             (Repr.to_string b))
    | _ -> None)

(** Ensure that a type satisfies a given constraint, i.e. morally that b <: c.
*)
let rec satisfies_constraint b c =
  match (deref b).descr with
    | Var { contents = Free v } ->
        v.constraints <- Constraints.add c v.constraints
    | _ ->
        c.satisfied ~subtype:( <: )
          ~satisfies:(fun b -> satisfies_constraint b c)
          b

and satisfies_constraints b = List.iter (satisfies_constraint b)

(** Make a variable link to given type. *)
and bind ?(variance = `Invariant) a b =
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
  (* update_level a.level b; *)
  satisfies_constraints b (Constraints.elements a.constraints);
  let b = if b.pos = None then Type.make ?pos:a0.pos b.Type.descr else b in
  v.contents <- Link (variance, b)

(** Ensure that the type for the method [l] in [a] is a subtype of the one for
    the same method in [b]. *)
and unify_meth a b l =
  let {
    optional = optional1;
    meth = meth1;
    scheme = s1;
    json_name = json_name1;
  } =
    get_meth l a
  in
  let {
    optional = optional2;
    meth = meth2;
    scheme = s2;
    json_name = json_name2;
  } =
    get_meth l b
  in
  assert (meth1 = l && meth2 = l);
  (* Handle explicitly this case in order to avoid #1842. *)
  ((* We want to allow:
      - {foo:int?} <: {foo?:int}
      - {foo?:int?} <: {foo?:int}
      - {foo?:never} <: {foo?:int}
      and prohibit:
       - {foo?:int} <: {foo:int?} *)
   let s1 =
     match (optional1, optional2, (deref (snd s1)).descr) with
       | true, true, Never -> s2
       | _, true, Nullable t -> (fst s1, t)
       | true, false, _ -> raise (Error (Repr.make a, Repr.make b))
       | _ -> s1
   in
   (* TODO: we should perform proper type scheme subtyping, but this
         is a good approximation for now... *)
   try instantiate ~level:(-1) s1 <: instantiate ~level:(-1) s2
   with Error (a, b) ->
     let bt = Printexc.get_raw_backtrace () in
     Printexc.raise_with_backtrace
       (Error
          ( `Meth
              ( R.
                  {
                    name = l;
                    optional = optional1;
                    scheme = ([], a);
                    json_name = json_name1;
                  },
                `Ellipsis ),
            `Meth
              ( R.
                  {
                    name = l;
                    optional = optional2;
                    scheme = ([], b);
                    json_name = json_name2;
                  },
                `Ellipsis ) ))
       bt);
  try hide_meth l a <: hide_meth l b
  with Error (a, b) ->
    let bt = Printexc.get_raw_backtrace () in
    Printexc.raise_with_backtrace
      (Error
         ( `Meth
             ( R.
                 {
                   name = l;
                   optional = optional1;
                   scheme = ([], `Ellipsis);
                   json_name = json_name1;
                 },
               a ),
           `Meth
             ( R.
                 {
                   name = l;
                   optional = optional2;
                   scheme = ([], `Ellipsis);
                   json_name = json_name2;
                 },
               b ) ))
      bt

(** Ensure that a<:b, perform unification if needed. In case of error, generate
    an explanation. We recall that A <: B means that any value of type A can be
    passed where a value of type B can. This relation must be transitive. *)
and ( <: ) a b =
  if !debug || !debug_subtyping then
    Printf.printf "\n%s <: %s\n%!" (Type.to_string a) (Type.to_string b);
  if a != b then (
    match (a.descr, b.descr) with
      | a, b when a == b -> ()
      (* a <: b means that [a] can be used every time [b] type is expected.
         We do implicit int to float conversions when needed so this is acceptable. *)
      | Int, Float -> ()
      | Var { contents = Free v }, Var { contents = Free v' } when Var.eq v v'
        ->
          ()
      | _, Var ({ contents = Link (`Covariant, b') } as var) ->
          (* When the variable is covariant, we take the opportunity here to correct
             bad choices. For instance, if we took int, but then have a 'a?, we
             change our mind and use int? instead. *)
          let b'' = try sup ~pos:b'.pos a b' with Incompatible -> b' in
          (try
             b' <: b''
             (* The sup is allowed to return something invalid. See: https://github.com/savonet/liquidsoap/pull/3472 *)
           with e when !debug ->
             failwith
               (Printf.sprintf "invalid sup: %s !< %s (%s)" (Type.to_string b')
                  (Type.to_string b'') (Printexc.to_string e)));
          if b'' != b' then var.contents <- Link (`Covariant, b'');
          a <: b''
      | Var ({ contents = Link (`Covariant, a') } as var), _ ->
          var.contents <- Link (`Invariant, a');
          a <: b
      | _, Var { contents = Link (_, b) } -> a <: b
      | Var { contents = Link (_, a) }, _ -> a <: b
      | Constr c1, Constr c2 when c1.constructor = c2.constructor ->
          let rec aux pre p1 p2 =
            match (p1, p2) with
              | (v1, h1) :: t1, (v2, h2) :: t2 ->
                  begin try
                    let v = if v1 = v2 then v1 else `Invariant in
                    match v with
                      | `Covariant -> h1 <: h2
                      | `Invariant ->
                          mk_invariant h2;
                          h1 <: h2;
                          mk_invariant h2
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
             order and either l2 is erasable and t<:t'. *)
          let ellipsis = (false, "", `Range_Ellipsis) in
          let elide (o, l, _) = (o, l, `Ellipsis) in
          let l1, l2 =
            List.fold_left
              (* Start with [l2:=l12], [l1:=[]] and move each param [o,lbl]
                 required by [l] from [l2] to [l1]. *)
              (fun (l1, l2) (o, lbl, t) ->
                (* Search for a param with label lbl. Returns the first
                   matching parameter and the list without it. *)
                let rec get_param acc = function
                  | [] ->
                      raise
                        (Error
                           ( `Arrow
                               ( List.rev_append l1 (List.map elide l2),
                                 `Ellipsis ),
                             `Arrow
                               ( List.rev (ellipsis :: (o, lbl, `Ellipsis) :: l1),
                                 `Ellipsis ) ))
                  | (o', lbl', t') :: tl ->
                      if lbl = lbl' then ((o', lbl', t'), List.rev_append acc tl)
                      else get_param ((o', lbl', t') :: acc) tl
                in
                let (o', lbl, t'), l2' = get_param [] l2 in
                (* Check on-the-fly that the types match. *)
                begin try
                  if (not o') && o then raise (Error (`Ellipsis, `Ellipsis));
                  t <: t'
                with Error (t, t') ->
                  let bt = Printexc.get_raw_backtrace () in
                  let make o t =
                    `Arrow (List.rev (ellipsis :: (o, lbl, t) :: l1), `Ellipsis)
                  in
                  Printexc.raise_with_backtrace
                    (Error (make o' t', make o t))
                    bt
                end;
                ((o, lbl, `Ellipsis) :: l1, l2'))
              ([], l12) l
          in
          let l1 = List.rev l1 in
          ignore l1;
          if
            l2 = [] || (forget_arguments && List.for_all (fun (o, _, _) -> o) l2)
          then (
            try t <: t'
            with Error (t, t') ->
              let bt = Printexc.get_raw_backtrace () in
              Printexc.raise_with_backtrace
                (Error (`Arrow ([ellipsis], t), `Arrow ([ellipsis], t')))
                bt)
          else (
            let l2 = List.map (fun (o, l, t) -> (o, l, Repr.make t)) l2 in
            raise
              (Error
                 ( `Arrow (l2 @ [ellipsis], `Ellipsis),
                   `Arrow ([ellipsis], `Ellipsis) )))
      | Custom c, Custom c' -> (
          try c.subtype ( <: ) c.typ c'.typ
          with _ -> raise (Error (Repr.make a, Repr.make b)))
      | Getter t1, Getter t2 -> (
          try t1 <: t2
          with Error (a, b) -> raise (Error (`Getter a, `Getter b)))
      | Arrow ([], t1), Getter t2 -> (
          try t1 <: t2
          with Error (a, b) -> raise (Error (`Arrow ([], a), `Getter b)))
      | Never, Var { contents = Free _ } | Var { contents = Free _ }, Never ->
          raise (Error (Repr.make a, Repr.make b))
      | Var { contents = Free _ }, _ -> (
          try bind a b
          with Occur_check _ | Unsatisfied_constraint ->
            (* Can't do more concise than a full representation, as the problem
               isn't local. *)
            raise (Error (Repr.make a, Repr.make b)))
      | _, Var { contents = Free _ } -> (
          try bind ~variance:`Covariant b a
          with Occur_check _ | Unsatisfied_constraint ->
            let bt = Printexc.get_raw_backtrace () in
            Printexc.raise_with_backtrace (Error (Repr.make a, Repr.make b)) bt)
      | _, Nullable t2 -> (
          try a <: t2 with Error (a, b) -> raise (Error (a, `Nullable b)))
      | Meth ({ meth = l }, _), _ when Type.has_meth b l -> unify_meth a b l
      | _, Meth ({ meth = l }, _) when Type.has_meth a l -> unify_meth a b l
      | _, Meth ({ meth = l; optional; scheme = g2, t2; json_name }, c) -> (
          let a' = demeth a in
          match a'.descr with
            | Var { contents = Free _ } ->
                let optional, t2 =
                  match (deref t2).descr with
                    | Type.(Nullable t) -> (true, t)
                    | _ -> (optional, t2)
                in
                a'
                <: make
                     (Meth
                        ( {
                            meth = l;
                            optional;
                            scheme = (g2, t2);
                            doc = { meth_descr = ""; category = `Method };
                            json_name = None;
                          },
                          var () ));
                a <: b
            | _ when optional || (deref t2).descr = Never -> a <: hide_meth l c
            | _ ->
                raise
                  (Error
                     ( Repr.make a,
                       `Meth
                         ( R.
                             {
                               name = l;
                               optional;
                               scheme = ([], `Ellipsis);
                               json_name;
                             },
                           `Ellipsis ) )))
      | Meth (m, u1), _ -> opt_meth m.meth u1 <: b
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
          raise (Error (a, b)))

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
