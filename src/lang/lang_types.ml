(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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

(* The type information is supposed to be attached to the AST from the parsing,
 * with appropriate sharing of the type variables. Then the type inference
 * performs in-place unification.
 *
 * In order to report precise type error messages, we put very dense
 * parsing location information in the type. Every layer of it can have
 * a location.
 * The level annotation represents the number of abstractions which surround
 * the type in the AST -- abstractions are lambda but also let-in. It is used
 * to safely generalize types.
 * We attach constraints to type variables. 
 *)

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

type ground = Unit | Bool | Int | String | Float | Source | Request

let print_ground = function
  | Unit    -> "unit"
  | String  -> "string"
  | Bool    -> "bool"
  | Int     -> "int"
  | Float   -> "float"
  | Source  -> "source"
  | Request -> "request"

(** Type constraints *)

type constr = Num | Ord | Getter of ground | Dtools
type constraints = constr list
let print_constr = function
  | Num -> "a number type"
  | Ord -> "an orderable type"
  | Getter t ->
      let t = print_ground t in
        Printf.sprintf "either %s or ()->%s" t t
  | Dtools -> "bool, int, float, string or [string]"

(** Types *)

(** Every type gets a level annotation.
  * This is useful in order to know what can or cannot be generalized:
  * you need to compare the level of an abstraction and those of a ref or
  * source. *)
type t = { pos : pos option ;
           mutable level : int ;
           mutable descr : descr }
and descr =
  | Ground  of ground
  | List    of t
  | Product of t * t
  | Arrow     of (bool*string*t) list * t
  | EVar      of int*constraints (* existential variable *)
  | UVar      of int*constraints (* universal variable, implicitely bound *)
  | Link      of t
and ty_range =
  | String_r of string
  | Int_r    of int*int
  | Float_r  of float*float

let make ?(pos=None) ?(level=(-1)) d =
  { pos = pos ; level = level ; descr = d }

let dummy = make ~pos:None (EVar (-1,[]))

(** Sets of type descriptions. *)
module DS = Set.Make(struct type t = descr let compare = compare end)

(** Dereferencing gives you the meaning of a term,
  * taking instantiations into account;
  * one should never work on a non-dereferenced type. *)
let rec deref t = match t.descr with
  | Link x -> deref x
  | _ -> t

(** Given a strictly positive integer, generate a name in [a-z]+. *)
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

let print t =
  let unknowns = Hashtbl.create 10 in
  let counter = let c = ref 0 in fun () -> incr c ; !c in
  let evar_name i =
    if debug then Printf.sprintf "?%d" i else
      let s =
        try
          Hashtbl.find unknowns i
        with Not_found ->
          let name = String.uppercase (name (counter ())) in
            Hashtbl.add unknowns i name ;
            name
      in
        Printf.sprintf "?%s" s
  in
  let uvar_name i = Printf.sprintf "'%s" (name i) in
  (** Compute the string representation of a type, dereferencing it on-the-fly.
    * Attaches the list of variables that occur in the type.
    * The [par] params tells whether (..)->.. should be surrounded by
    * parenthesis or not. *)
  let rec print ~par vars t = match t.descr with
    | Ground g -> print_ground g, vars
    | Product (a,b) ->
        let a,vars = print ~par:true vars a in
        let b,vars = print ~par:true vars b in
          Printf.sprintf "(%s*%s)" a b,
          vars
    | List t ->
        let t,vars = print ~par:false vars t in
          Printf.sprintf "[%s]" t,
          vars
    | EVar (i,c) as d ->
        (evar_name i),
        (if c<>[] then DS.add d vars else vars)
    | UVar (i,c) as d ->
        (uvar_name i),
        (if c<>[] then DS.add d vars else vars)
    | Arrow (p,t) ->
        let params,vars =
           List.fold_left
             (fun (params,vars) (opt,lbl,kind) ->
                let kind,vars = print ~par:true vars kind in
                 let prefix =
                   if lbl <> "" then
                     (if opt then "?" else "~") ^ lbl ^ ":"
                   else
                     (if opt then "?" else "")
                 in
                   (prefix^kind)::params,vars)
             ([],vars)
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
    | Link x -> assert (x!=t) ; print ~par vars x
  in
  let repr,constraints = print ~par:false DS.empty t in
    if DS.is_empty constraints then repr else
      match (deref t).descr, DS.elements constraints with
        | UVar _, [UVar(i,c)] ->
            Printf.sprintf "anything that is %s"
              (String.concat " and " (List.map print_constr c))
        | EVar _, [EVar(i,c)] when not debug ->
            Printf.sprintf "something that is %s"
              (String.concat " and " (List.map print_constr c))
        | _, constraints ->
            let constraints =
              List.map
                (fun x ->
                   let i,c =
                     match x with
                       | EVar (i,c) -> evar_name i, c
                       | UVar (i,c) -> uvar_name i, c
                       | _ -> assert false
                   in
                   let c = String.concat " and " (List.map print_constr c) in
                     i,c)
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
      | Product (t1,t2) -> occur_check a t1 ; occur_check a t2
      | List t -> occur_check a t
      | Arrow (p,t) ->
          List.iter
            (fun (o,l,t) -> occur_check a t)
            p ;
          occur_check a t
      | EVar _ ->
          assert (a.level <> -1 && b.level <> -1) ;
          b.level <- min b.level a.level
      | UVar _ -> assert false
      | Link _ | Ground _ -> ()

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
                         | Ground g ->
                             if g=Source || g=Request then
                               raise (Unsatisfied_constraint (Ord,b))
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
                     end)
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
    | List t1, List t2 -> t1 <: t2
    | Product (a,b), Product (aa,bb) -> a <: aa ; b <: bb
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
    | EVar _, _ ->
        begin try bind a b with
          | Occur_check _ | Unsatisfied_constraint _ ->
              raise (Error [Item (a,b)]) end
    | _, EVar _ ->
        begin try bind b a with
          | Occur_check _ | Unsatisfied_constraint _ ->
              raise (Error [Item (a,b)]) end
    | UVar _,_ | _,UVar _ -> assert false (* no type schemes here *)
    | Link _,_ | _,Link _ -> assert false (* thanks to deref *)
    | Ground x,Ground y ->
        (* The remaining cases are the base types, thanks to deref. *)
        if x <> y then raise (Error [Item (a,b)])
    | _,_ -> raise (Error [Item (a,b)])

let (>:) a b =
  try b <: a with Error l -> raise (Error (Flip::l))

(** {1 Handling of type schemata} *)

module M = Map.Make(struct type t = int let compare = compare end)

(** Return an instantiated copy, where sharing is preserved.
  * The position of the copy will be [pos], the internal positions will be at
  * [dummy_pos], the fresh EVars have level [level].
  * Whenever we use a value, its type schema has to be instantiated. *)
let instantiate ~level t =
  let evars = ref M.empty in
  let rec aux t =
    let t = deref t in
    let cp x = { t with pos = None ; descr = x } in
      match t.descr with
        | UVar (i,c) ->
            begin try
              M.find i !evars
            with
              | Not_found ->
                  let v = fresh_evar ~constraints:c ~level ~pos:None in
                    evars := M.add i v !evars ;
                    v
            end
        | EVar _ -> t (* Copying EVars would break sharing. *)
        | Ground _ -> cp t.descr
        | List t -> cp (List (aux t))
        | Product (a,b) -> cp (Product (aux a, aux b))
        | Arrow (p,t) ->
            cp (Arrow (List.map (fun (o,l,t) -> (o,l,aux t)) p, aux t))
        | Link _ -> assert false
  in
  let t' = aux t in
    (* If no UVar has been found, keep the old t, without erased positions.
     * Otherwise add the outer position information, which is the only one
     * that makes sense after an effective instantiation. In that case,
     * the superficial copying cannot break sharing. *)
    if M.is_empty !evars then t else { level = 0 ; pos = t.pos ;
                                       descr =
                                         Link { t' with pos = (deref t).pos } }

(** In-place generalization.
  * This should be done after the inference of the type of the left hand-side
  * of a let-in, with [level] being the level of that let-in. *)
let generalize ~level t =
  let c = let r = ref 0 in fun () -> incr r ; !r in
  let rec aux t =
    let t = deref t in
      match t.descr with
       | Ground _ -> ()
       | UVar _ -> () (* this EVar has already been generalized *)
       | EVar (i,constraints) ->
           if t.level >= level then
             (* Using bind would yield un-necessary, and even failed checks. *)
             t.descr <- Link { t with descr = UVar (c(),constraints) }
       | List t -> aux t
       | Product (a,b) -> aux a ; aux b
       | Arrow (p,t) -> List.iter (fun (_,_,t) -> aux t) p ; aux t
       | Link _ -> assert false
  in
    aux t

(** Specialization for external uses. *)

let fresh_evar = fresh_evar ~constraints:[]
