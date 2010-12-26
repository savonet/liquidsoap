(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

let debug =
  try
    ignore (Sys.getenv "LIQUIDSOAP_DEBUG_LANG") ;
    true
  with
    | Not_found -> false

(* Type information comes attached to the AST from the parsing,
 * with appropriate sharing of the type variables. Then the type inference
 * performs in-place unification.
 *
 * In order to report precise type error messages, we put very dense
 * parsing location information in the type. Every layer of it can have
 * a location. Destructive unification introduces links in such a way
 * that the old location is still accessible.
 *
 * The level annotation represents the number of abstractions which surround
 * the type in the AST -- function arguments and let-in definitions.
 * It is used to safely generalize types.
 *
 * Finally, constraints can be attached to existential (unknown, '_a)
 * and universal ('a) type variables. *)

(** Positions *)

type pos = (Lexing.position*Lexing.position)

let print_single_pos (l,_) =
  let file = if l.Lexing.pos_fname="" then "" else l.Lexing.pos_fname^"/" in
  let line,col = l.Lexing.pos_lnum, (l.Lexing.pos_cnum-l.Lexing.pos_bol) in
    Printf.sprintf "%sL%dC%d" file line (col+1)

let print_pos ?(prefix="At ") (start,stop) =
  let prefix =
    match start.Lexing.pos_fname with
      | "" -> prefix
      | file -> prefix ^ file ^ ", "
  in
  let f l = l.Lexing.pos_lnum, (l.Lexing.pos_cnum-l.Lexing.pos_bol) in
  let lstart,cstart = f start in
  let lstop,cstop = f stop in
  let cstart = 1+cstart in
    if lstart = lstop then
      if cstart = cstop then
        Printf.sprintf "%sline %d, char %d" prefix lstart cstart
      else
        Printf.sprintf "%sline %d, char %d-%d" prefix lstart cstart cstop
    else
      Printf.sprintf "%sline %d char %d - line %d char %d"
        prefix lstart cstart lstop cstop

(** Ground types *)

type mul = Frame.multiplicity
type ground = Unit | Bool | Int | String | Float

let rec print_ground = function
  | Unit    -> "unit"
  | String  -> "string"
  | Bool    -> "bool"
  | Int     -> "int"
  | Float   -> "float"

(** Type constraints *)

type constr = Num | Ord | Getter of ground | Dtools | Arity_fixed | Arity_any
type constraints = constr list

let print_symconstr = function
  | Arity_any -> Some "*"
  | Arity_fixed -> Some "#"
  | _ -> None

let print_constr = function
  | Num -> "a number type"
  | Ord -> "an orderable type"
  | Getter t ->
      let t = print_ground t in
        Printf.sprintf "either %s or ()->%s" t t
  | Dtools -> "bool, int, float, string or [string]"
  | Arity_any -> "an arity"
  | Arity_fixed -> "a fixed arity"

(** Types *)

type variance = Covariant | Contravariant | Invariant

(** Every type gets a level annotation.
  * This is useful in order to know what can or cannot be generalized:
  * you need to compare the level of an abstraction and those of a ref or
  * source. *)
type t = { pos : pos option ;
           mutable level : int ;
           mutable descr : descr }
and constructed = { name : string ; params : (variance*t) list }
and descr =
  | Constr  of constructed
  | Ground  of ground
  | List    of t
  | Product of t * t
  | Zero | Succ of t | Variable
  | Arrow     of (bool*string*t) list * t
  | EVar      of int*constraints (* existential/meta variable *)
  | Link      of t

let make ?(pos=None) ?(level=(-1)) d =
  { pos = pos ; level = level ; descr = d }

let dummy = make ~pos:None (EVar (-1,[]))

(** Dereferencing gives you the meaning of a term,
  * going through links created by instantiations.
  * One should (almost) never work on a non-dereferenced type. *)
let rec deref t = match t.descr with
  | Link x -> deref x
  | _ -> t

(** Given a strictly positive integer, generate a name in [a-z]+:
  * a, b, ... z, aa, ab, ... az, ba, ... *)
let name =
  let base = 26 in
  let c i = char_of_int (int_of_char 'a' + i - 1) in
  let add i suffix = Printf.sprintf "%c%s" (c i) suffix in
  let rec n suffix i =
    if i<=base then add i suffix else
      let head = i mod base in
      let head = if head = 0 then base else head in
        n (add head suffix) ((i-head)/base)
  in
    n ""

(** Compute the structure that a term [repr]esents,
  * given the list of universally quantified variables.
  * Also takes care of computing the printing name of variables,
  * including constraint symbols, which are removed from constraint lists.
  * It supports a mechanism for filtering out parts of the type,
  * which are then translated as `Ellipsis. *)
let repr filter_out ?(generalized=[]) =
  let split_constr c =
    List.fold_left
      (fun (s,constraints) c ->
         match print_symconstr c with
           | None -> s,c::constraints
           | Some sym -> s^sym,constraints)
      ("",[]) c
  in
  let uvar i c =
    let constr_symbols,c = split_constr c in
    let rec index n = function
      | v::tl ->
          if fst v = i then
            Printf.sprintf "'%s%s" constr_symbols (name n)
          else
            index (n+1) tl
      | [] -> assert false
    in
      `UVar (index 1 (List.rev generalized), c)
  in
  let counter = let c = ref 0 in fun () -> incr c ; !c in
  let evars = Hashtbl.create 10 in
  let evar i c =
    let constr_symbols,c = split_constr c in
      if debug then
        `EVar (Printf.sprintf "?%s%d" constr_symbols i, c)
      else
        let s =
          try
            Hashtbl.find evars i
          with Not_found ->
            let name = String.uppercase (name (counter ())) in
              Hashtbl.add evars i name ;
              name
        in
          `EVar (Printf.sprintf "?%s%s" constr_symbols s, c)
  in
  let generalized i = List.exists (fun (j,_) -> j=i) generalized in
  let rec repr t =
    if filter_out t then `Ellipsis else
      match t.descr with
        | Ground g -> `Ground g
        | List t -> `List (repr t)
        | Product (a,b) -> `Product (repr a, repr b)
        | Zero -> `Zero
        | Variable -> `Variable
        | Succ t -> `Succ (repr t)
        | Constr { name=name; params=params } ->
            `Constr (name,
                     List.map (fun (l,t) -> l, repr t) params)
        | Arrow (args,t) ->
            `Arrow (List.map (fun (opt,lbl,t) -> opt,lbl,repr t) args,
                    repr t)
        | EVar (id,c) ->
            if generalized id then
              uvar id c
            else
              evar id c
        | Link t -> repr t
  in
    repr

(** Sets of type descriptions. *)
module DS =
  Set.Make(struct type t = (string*constraints) let compare = compare end)

(** Convert a type to a string.
  * Unless in debug mode, variable identifiers are not shown,
  * and variable names are generated.
  * Names are only meaningful over one printing, as they are re-used. *)
let print_repr t =
  (** Compute the string representation of a type.
    * Attaches the list of variables that occur in the type.
    * The [par] params tells whether (..)->.. should be surrounded by
    * parenthesis or not. *)
  let rec print ~par vars = function
    | `Constr (name,params) ->
        (* Let's assume that stream_kind occurs only inside a source
         * or format type -- this should be pretty much true with the
         * current API -- and simplify the printing by labeling its
         * parameters and omitting the stream_kind(...) to avoid
         * source(stream_kind(2,0,0)). *)
        if name = "stream_kind" then
          match params with
            | [_,a;_,v;_,m] ->
                let ellipsis,vars = print ~par:false vars `Ellipsis in
                let has_ellipsis,l,vars =
                  List.fold_left
                    (fun (has_ellipsis,l,vars) (lbl,t) ->
                       if t=`Ellipsis then true,l,vars else
                         let t,vars = print ~par:false vars t in
                           has_ellipsis,
                           (lbl ^ "=" ^ t) :: l,
                           vars)
                    (false,[],vars)
                    ["audio",a;"video",v;"midi",m]
                in
                let l = if has_ellipsis then ellipsis :: l else l in
                let l = List.rev l in
                  String.concat "," l,
                  vars
            | _ -> assert false
        else
          let l,vars = print_list vars [] params in
            Printf.sprintf "%s(%s)" name (String.concat "," l),
            vars
    | `Ground g -> print_ground g, vars
    | `Product (a,b) ->
        let a,vars = print ~par:true vars a in
        let b,vars = print ~par:true vars b in
          Printf.sprintf "(%s*%s)" a b,
          vars
    | `List t ->
        let t,vars = print ~par:false vars t in
          Printf.sprintf "[%s]" t,
          vars
    | `Variable -> "*", vars
    | `Zero | `Succ _ as t ->
        let rec aux n = function
          | `Succ t -> aux (n+1) t
          | `Zero -> string_of_int n, vars
          | t ->
              let s,vars = print ~par vars t in
                Printf.sprintf "%s+%d" s n, vars
        in
          aux 0 t
    | `EVar (name,c) | `UVar (name,c) ->
        name,
        (if c<>[] then DS.add (name,c) vars else vars)
    | `Arrow (p,t) ->
        let _,params,vars =
          List.fold_left
            (fun (in_ellipsis,params,vars) (opt,lbl,kind) ->
               if kind = `Ellipsis then
                 if in_ellipsis then
                   true, params, vars
                 else
                   true, "..."::params, vars
               else
                 let kind,vars = print ~par:true vars kind in
                 let full =
                   (if opt then "?" else "") ^
                   (if lbl <> "" then lbl ^ ":" ^ kind else kind)
                 in
                   false, full::params, vars)
            (false,[],vars)
            p
        in
        let params = List.rev params in
        let t,vars = print ~par:false vars t in
        let print =
          if par then
            Printf.sprintf "((%s)->%s)"
          else
            Printf.sprintf "(%s)->%s"
        in
          print
            (String.concat ", " params)
            t,
          vars
    | `Ellipsis -> "...", vars
  and print_list vars acc = function
    | [] -> List.rev acc, vars
    | (_,x)::l ->
        let x,vars = print ~par:false vars x in
          print_list vars (x::acc) l
  in
    match t with
      (* We're only printing a variable: ignore its [repr]esentation. *)
      | `EVar (i,c) when c <> [] ->
          Printf.sprintf "something that is %s"
            (String.concat " and " (List.map print_constr c))
      | `UVar (i,c) when c <> [] ->
          Printf.sprintf "anything that is %s"
            (String.concat " and " (List.map print_constr c))
      (* Print the full thing, then display constraints *)
      | _ ->
          let repr,constraints = print ~par:false DS.empty t in
          let constraints = DS.elements constraints in
            if constraints = [] then repr else
              let constraints =
                List.map
                  (fun (name,c) ->
                     name,
                     String.concat " and " (List.map print_constr c))
                  constraints
              in
              let constraints =
                List.stable_sort (fun (_,a) (_,b) -> compare a b) constraints
              in
              let group : ('a*'b) list -> ('a list * 'b) list = function
                | [] -> []
                | (i,c)::l ->
                    let rec group prev acc = function
                      | [] -> [List.rev acc,prev]
                      | (i,c)::l ->
                          if prev = c then group c (i::acc) l else
                            (List.rev acc, prev) :: group c [i] l
                    in
                      group c [i] l
              in
              let constraints = group constraints in
              let constraints =
                List.map
                  (fun (ids,c) ->
                     Printf.sprintf "%s is %s" (String.concat ", " ids) c)
                  constraints
              in
                repr ^ " where " ^
                (String.concat ", " constraints)

let fresh_evar =
  let fresh_id =
    let c = ref 0 in
      fun () -> incr c ; !c
  in
  let f ~constraints ~level ~pos =
    { pos = pos ; level = level ; descr = EVar (fresh_id (),constraints) }
  in
    f

(** {1 Assignation} *)

(** These two exceptions can be raised when attempting to assign a variable. *)
exception Occur_check of t*t
exception Unsatisfied_constraint of constr*t

(** Check that [a] (a dereferenced type variable) does not occur in [b],
  * and prepare the instantiation [a<-b] by adjusting the levels. *)
let rec occur_check a b =
  let b = deref b in
    if a == b then raise (Occur_check (a,b)) ;
    match b.descr with
      | Constr c -> List.iter (fun (_,x) -> occur_check a x) c.params
      | Product (t1,t2) -> occur_check a t1 ; occur_check a t2
      | List t -> occur_check a t
      | Succ t -> occur_check a t
      | Zero | Variable -> ()
      | Arrow (p,t) ->
          List.iter
            (fun (o,l,t) -> occur_check a t)
            p ;
          occur_check a t
      | EVar _ ->
          (* In normal type inference level -1 should never arise.
           * Unfortunately we can't check it strictly because this code 
           * is also used to process type annotations, which make use
           * of unknown levels. Also note that >=0 levels can arise
           * when processing type annotations, because of builtins. *)
          if b.level = -1 then
            b.level <- a.level
          else if a.level <> -1 then
            b.level <- min b.level a.level
      | Ground _ -> ()
      | Link _ -> assert false

(* Perform [a := b] where [a] is an EVar, check that [type(a)<:type(b)]. *)
let rec bind a0 b =
  let a = deref a0 in
  let b = deref b in
    if b==a then () else begin
      occur_check a b ;
      begin match a.descr with
        | EVar (i,constraints) ->
            List.iter
              (function
                 | Getter g ->
                     let error = Unsatisfied_constraint (Getter g, b) in
                       begin match b.descr with
                         | Ground g' -> if g<>g' then raise error
                         | Arrow([],t) ->
                             begin match (deref t).descr with
                               | Ground g' -> if g<>g' then raise error
                               | EVar (j,c) ->
                                   (* This is almost wrong as it flips <: into
                                    * >:, but that's OK for a ground type. *)
                                   bind t (make (Ground g))
                               | _ -> raise error
                             end
                         | EVar (j,c) ->
                             if List.mem (Getter g) c then () else
                               b.descr <- EVar (j,(Getter g)::c)
                         | _ -> raise error
                       end
                 | Ord ->
                     (** In check, [b] is assumed to be dereferenced *)
                     let rec check b =
                       match b.descr with
                         | Ground g -> ()
                         | EVar (j,c) ->
                             if List.mem Ord c then () else
                               b.descr <- EVar (j,Ord::c)
                         | Product (b1,b2) ->
                             check (deref b1) ; check (deref b2)
                         | List b -> check (deref b)
                         | _ -> raise (Unsatisfied_constraint (Ord,b))
                     in
                       check b
                 | Dtools ->
                     begin match b.descr with
                       | Ground g ->
                           if not (List.mem g [Bool;Int;Float;String]) then
                             raise (Unsatisfied_constraint (Dtools,b))
                       | List b' ->
                           begin match (deref b').descr with
                             | Ground g ->
                                 if g <> String then
                                   raise (Unsatisfied_constraint (Dtools,b'))
                             | EVar (j,c) ->
                                 bind b' (make (Ground String))
                             | _ -> raise (Unsatisfied_constraint (Dtools,b'))
                           end
                       | EVar (j,c) ->
                           if not (List.mem Dtools c) then
                             b.descr <- EVar (j,Dtools::c)
                       | _ -> raise (Unsatisfied_constraint (Dtools,b))
                     end
                 | Num ->
                     begin match b.descr with
                       | Ground g ->
                           if g<>Int && g<>Float then
                             raise (Unsatisfied_constraint (Num,b))
                       | EVar (j,c) ->
                           if List.mem Num c then () else
                             b.descr <- EVar (j,Num::c)
                       | _ -> raise (Unsatisfied_constraint (Num,b))
                     end
                 | Arity_any ->
                     let rec check b = match b.descr with
                       | Zero | Variable -> ()
                       | Succ b -> check (deref b)
                       | EVar (j,c) ->
                           if List.mem Arity_any c then () else
                             b.descr <- EVar (j,Arity_any::c)
                       | _ -> raise (Unsatisfied_constraint (Arity_any,b))
                     in check b
                 | Arity_fixed ->
                     let rec check b = match b.descr with
                       | Zero -> ()
                       | Succ b -> check (deref b)
                       | EVar (j,c) ->
                           if List.mem Arity_fixed c then () else
                             b.descr <- EVar (j,Arity_fixed::c)
                       | _ -> raise (Unsatisfied_constraint (Arity_fixed,b))
                     in check b)
              constraints
        | _ -> assert false (* only EVars are bindable *)
      end ;
      (** This is a shaky hack...
        * When a value is passed to a FFI, its type is bound to a type without
        * any location.
        * If it doesn't break sharing, we set the parsing position of
        * that occurence of the variable as the position of the infered type. *)
      if b.pos = None && match b.descr with EVar _ -> false | _ -> true
      then
        a.descr <- Link { a0 with descr = b.descr }
      else
        a.descr <- Link b
    end

(* {1 Subtype checking/inference} *)

(** Subtype checking raises a traced error. Each item (ta,tb) in the trace tells
  * that ta<:tb failed. The next item refines the error, telling which sub-call
  * failed. Not all types have a defined position, but in practical cases at
  * least one of the outermost types should have one -- it came from the AST.
  *
  * From the user point of view this is not enough. The message has to take into
  * account which is the already inferred type (this ...) and which is the
  * expected generated type (... but should ...).
  * This "focus" get changed when going through the left of an arrow.
  * The first call by Lang_values.check knows where the focus initially is.
  *
  * Checking f(x), the type of f is required to be a supertype of (tx)->...
  * If x doesn't fit, the complete trace should look like that:
  *
  * At POS(f), this expr. has type (t)->...
  *   but should be a subtype of (tx)->..., inferred at <?>.
  * At POS(x) this expr. has type tx
  *   but should be a supertype of A, inferred at <?>.
  *
  * In both items, it is likely that <?> will be undefined. *)
type trace_item = Item of t*t | Flip
exception Error of trace_item list

let print_type_error trace =
  let rec strip = function
    | Flip :: tl -> strip tl
    | Item (a,b) :: tl -> (a,b) :: strip tl
    | [] -> []
  in
  let trace = strip trace in
  let tr_a,tr_b = List.split trace in
  let a = List.hd tr_a and b = List.hd tr_b in
  let filter_out x =
    let eq y = deref x == deref y in
      not (List.exists eq tr_a || List.exists eq tr_b)
  in
  let print t = print_repr (repr filter_out t) in
  let infered_pos a =
    let dpos = (deref a).pos in
      if a.pos = dpos then "" else
        match dpos with
          | None -> ""
          | Some p -> "\n    (infered at " ^ print_pos ~prefix:"" p ^ ")"
  in
    Printf.printf
      "\n%s:\n  this value has type\n    %s%s\n"
      (match a.pos with
         | None -> "At unknown position"
         | Some p -> print_pos p)
      (print a)
      (infered_pos a) ;
    Printf.printf
      "  but it should be a subtype of%s\n    %s%s\n%!"
      (match b.pos with
         | None -> ""
         | Some p ->
             Printf.sprintf " (the type of the value at %s)"
               (print_pos ~prefix:"" p))
      (print b)
      (infered_pos b)

let print ?generalized t =
  print_repr (repr ?generalized (fun _ -> false) t)

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
 * This relation should be transitive. Note that it is not safe to allow the
 * promotion of optional parameters into mandatory ones, because the function
 * with the optional parameter, when fully applied, applies implicitely its
 * optional argument; whereas with a mandatory argument it is expected to wait
 * for it. *)
let rec (<:) a b =
  let (>:) b' a' =
    try a' <: b' with Error trace -> raise (Error ((Item(a,b))::Flip::trace))
  in
  let (<:) a' b' =
    try a' <: b' with Error trace -> raise (Error ((Item(a,b))::trace))
  in

  if debug then Printf.eprintf "%s <: %s\n" (print a) (print b) ;
  match (deref a).descr, (deref b).descr with
    | Constr c1, Constr c2 when c1.name=c2.name ->
        List.iter2 (fun (_,x) (_,y) -> x<:y) c1.params c2.params
    | List t1, List t2 -> t1 <: t2
    | Product (a,b), Product (aa,bb) -> a <: aa ; b <: bb
    | Zero, Zero -> ()
    | Zero, Variable -> ()
    | Succ t1, Succ t2 -> t1 <: t2
    | Succ t1, Variable -> t1 <: b
    | Variable, Variable -> ()
    | Arrow (p,t), Arrow (p',t') ->
        (* Takes [l] and [l12] and returns [l1,l2] where:
         * [l2] is the list of parameters from [l12] unmatched in [l];
         * [l1] is the list of pairs [t,t12] of matched parameters. *)
        let remove_params l l12 =
          (* Takes a list of parameters, a label and an optionality.
           * Returns the first matching parameter and the list without it. *)
          let get_param o lbl l =
            let rec aux acc = function
              | [] ->
                  (* TODO One could use some extra error explaination here. *)
                  raise (Error [Item (a,b)])
              | (o',lbl',t')::tl ->
                  if o=o' && lbl=lbl' then
                    (o,lbl,t'), List.rev_append acc tl
                  else
                    aux ((o',lbl',t')::acc) tl
            in
              aux [] l
          in
          let l1,l2 =
            List.fold_left
              (* Move param [lbl] required by [l] from [l2] to [l1]. *)
              (fun (l1,l2) (o,lbl,t) ->
                 let ((o,lbl,t'),l2') = get_param o lbl l2 in
                   ((t,t')::l1),l2')
              ([],l12)
              l
          in
            List.rev l1, l2
        in
        let p1,p2 = remove_params p' p in
          List.iter (fun (t',t) -> t >: t') p1 ;
          if List.for_all (fun (o,_,_) -> o) p2 then
            t <: t'
          else
            { a with descr = Arrow (p2,t) } <: t'
    (* The two EVar cases are abusive because of subtyping. We should add a
     * subtyping constraint instead of unifying. Nevermind...
     * It's a pain for arrow types, and forgetting about it doesn't hurt. *)
    | EVar (_,c), Variable when List.mem Arity_fixed c -> ()
    | EVar (_,c), Variable when List.mem Arity_any c -> ()
    | EVar _, _ ->
        begin try bind a b with
          | Occur_check _ | Unsatisfied_constraint _ ->
              raise (Error [Item (a,b)]) end
    | _, EVar _ ->
        begin try bind b a with
          | Occur_check _ | Unsatisfied_constraint _ ->
              raise (Error [Item (a,b)]) end
    | Link _,_ | _,Link _ -> assert false (* thanks to deref *)
    | Ground x,Ground y ->
        (* The remaining cases are the base types, thanks to deref. *)
        if x <> y then raise (Error [Item (a,b)])
    | _,_ -> raise (Error [Item (a,b)])

let (>:) a b =
  try b <: a with Error l -> raise (Error (Flip::l))

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

let filter_vars f t =
  let rec aux l t = let t = deref t in match t.descr with
    | Ground _ | Zero | Variable -> l
    | Succ t | List t -> aux l t
    | Product (a,b) -> aux (aux l a) b
    | Constr c ->
        List.fold_left (fun l (_,t) -> aux l t) l c.params
    | Arrow (p,t) ->
        aux (List.fold_left (fun l (_,_,t) -> aux l t) l p) t
    | EVar (i,constraints) ->
        if f t then (i,constraints)::l else l
    | Link _ -> assert false
  in
    aux [] t

(** Return a list of generalizable variables in a type.
  * This is performed after type inference on the left-hand side
  * of a let-in, with [level] being the level of that let-in.
  * Uses the simple method of ML, to be associated with a value restriction. *)
let generalizable ~level t =
  filter_vars (fun t -> t.level >= level) t

(** Copy a term, substituting some EVars as indicated by a list
  * of associations. Other EVars are not copied, so sharing is
  * preserved. *)
let copy_with subst t =
  let rec aux t =
    let cp x = { t with descr = x } in
      match t.descr with
        | EVar (i,c) ->
            begin try
              snd (List.find (fun ((j,_),_) -> i=j) subst)
            with
              | Not_found -> t
            end
        | Constr c ->
            let params = List.map (fun (v,t) -> v, aux t) c.params in
              cp (Constr { c with params = params })
        | Ground _ -> cp t.descr
        | List t -> cp (List (aux t))
        | Product (a,b) -> cp (Product (aux a, aux b))
        | Zero | Variable -> cp t.descr
        | Succ t -> cp (Succ (aux t))
        | Arrow (p,t) ->
            cp (Arrow (List.map (fun (o,l,t) -> (o,l,aux t)) p, aux t))
        | Link t ->
            (* Keep links to preserve rich position information,
             * and to make it possible to check if the application left
             * the type unchanged. *)
            cp (Link (aux t))
  in
    aux t

module M = Map.Make(struct type t = int let compare = compare end)

(** Instantiate a type scheme, given as a type together with a list
  * of generalized variables.
  * Fresh variables are created with the given (current) level,
  * and attached to the appropriate constraints.
  * This erases position information, since they usually become
  * irrelevant. *)
let instantiate ~level ~generalized =
  let subst =
    List.map
      (fun (i,c) -> (i,c), fresh_evar ~level ~constraints:c ~pos:None)
      generalized
  in
    fun t -> copy_with subst t

(** Simplified version of existential variable generation,
  * without constraints. This is used when parsing to annotate
  * the AST. *)
let fresh = fresh_evar
let fresh_evar = fresh_evar ~constraints:[]
