(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2020 Savonet team

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

let debug = ref (Utils.getenv_opt "LIQUIDSOAP_DEBUG_LANG" <> None)
let debug_levels = false

(** Pretty-print getters as {t}. *)
let pretty_getters = ref true

(** Show generalized variables in records. *)
let show_record_schemes = ref true

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

type pos = Lexing.position * Lexing.position

let print_single_pos l =
  let file =
    if l.Lexing.pos_fname = "" then ""
    else Printf.sprintf "file %s, " l.Lexing.pos_fname
  in
  let line, col = (l.Lexing.pos_lnum, l.Lexing.pos_cnum - l.Lexing.pos_bol) in
  Printf.sprintf "%sline %d, char %d" file line col

let print_pos ?(prefix = "at ") (start, stop) =
  let prefix =
    match start.Lexing.pos_fname with
      | "" -> prefix
      | file -> prefix ^ file ^ ", "
  in
  let f l = (l.Lexing.pos_lnum, l.Lexing.pos_cnum - l.Lexing.pos_bol) in
  let lstart, cstart = f start in
  let lstop, cstop = f stop in
  if lstart = lstop then
    if cstop = cstart + 1 then
      Printf.sprintf "%sline %d, char %d" prefix lstart cstart
    else Printf.sprintf "%sline %d, char %d-%d" prefix lstart cstart cstop
  else
    Printf.sprintf "%sline %d char %d - line %d char %d" prefix lstart cstart
      lstop cstop

let print_pos_opt ?prefix = function
  | Some pos -> print_pos ?prefix pos
  | None -> "unknown position"

let rec print_pos_list ?prefix = function
  | [] -> "unknown position"
  | [pos] -> print_pos ?prefix pos
  | pos :: l -> print_pos_list ?prefix l ^ ", " ^ print_pos ?prefix pos

(** Ground types *)

type ground = ..

type ground +=
  | Bool
  | Int
  | String
  | Float
  | Request
  | Format of Frame_content.format

let ground_printers = Queue.create ()
let register_ground_printer fn = Queue.add fn ground_printers

exception Found of string

let () =
  register_ground_printer (function
    | String -> Some "string"
    | Bool -> Some "bool"
    | Int -> Some "int"
    | Float -> Some "float"
    | Request -> Some "request"
    | Format p -> Some (Frame_content.string_of_format p)
    | _ -> None)

let print_ground v =
  try
    Queue.iter
      (fun fn -> match fn v with Some s -> raise (Found s) | None -> ())
      ground_printers;
    assert false
  with Found s -> s

(** Type constraints *)

type constr = Num | Ord | Getter of ground | Dtools | InternalMedia
type constraints = constr list

let print_constr = function
  | Num -> "a number type"
  | Ord -> "an orderable type"
  | Getter t ->
      let t = print_ground t in
      if !pretty_getters then Printf.sprintf "{%s}" t
      else Printf.sprintf "either %s or ()->%s" t t
  | Dtools -> "unit, bool, int, float, string or [string]"
  | InternalMedia -> "an internal media type (none, pcm, yuv420p or midi)"

(** Types *)

type variance = Covariant | Contravariant | Invariant

(** Every type gets a level annotation.
  * This is useful in order to know what can or cannot be generalized:
  * you need to compare the level of an abstraction and those of a ref or
  * source. *)
type t = { pos : pos option; mutable level : int; mutable descr : descr }

and constructed = { name : string; params : (variance * t) list }

and descr =
  | Constr of constructed
  | Ground of ground
  | List of t
  | Tuple of t list
  | Nullable of t
  | Meth of string * scheme * t
  | Arrow of (bool * string * t) list * t
  | EVar of var (* type variable *)
  | Link of t

and var = int * constraints

and scheme = var list * t

type env = (string * scheme) list

let unit = Tuple []

type repr =
  [ `Constr of string * (variance * repr) list
  | `Ground of ground
  | `List of repr
  | `Tuple of repr list
  | `Nullable of repr
  | `Meth of string * ((string * constraints) list * repr) * repr
  | `Arrow of (bool * string * repr) list * repr
  | `EVar of string * constraints (* existential variable *)
  | `UVar of string * constraints (* universal variable *)
  | `Ellipsis (* omitted sub-term *)
  | `Range_Ellipsis (* omitted sub-terms (in a list, e.g. list of args) *) ]

let make ?(pos = None) ?(level = -1) d = { pos; level; descr = d }
let dummy = make ~pos:None (EVar (-1, []))

(** Dereferencing gives you the meaning of a term, going through links created
    by instantiations. One should (almost) never work on a non-dereferenced
    type. *)
let rec deref t = match t.descr with Link x -> deref x | _ -> t

(** Remove methods. This function also removes links. *)
let rec demeth t =
  let t = deref t in
  match t.descr with Meth (_, _, t) -> demeth t | _ -> t

let rec remeth t u =
  let t = deref t in
  match t.descr with
    | Meth (l, v, t) -> { t with descr = Meth (l, v, remeth t u) }
    | _ -> u

let rec hide_meth l a =
  match (deref a).descr with
    | Meth (l', _, u) when l' = l -> hide_meth l u
    | Meth (l', t, u) -> { a with descr = Meth (l', t, hide_meth l u) }
    | _ -> a

let rec invoke t l =
  match (deref t).descr with
    | Meth (l', t, _) when l = l' -> t
    | Meth (_, _, t) -> invoke t l
    | _ -> raise Not_found

let meth ?pos ?level l v t = make ?pos ?level (Meth (l, v, t))

let rec meths ?pos ?level l v t =
  match l with
    | [] -> assert false
    | [l] -> meth ?pos ?level l v t
    | l :: ll ->
        let g, tl = invoke t l in
        let v = meths ?pos ?level ll v tl in
        meth ?pos ?level l (g, v) t

(** Given a strictly positive integer, generate a name in [a-z]+:
  * a, b, ... z, aa, ab, ... az, ba, ... *)
let name =
  let base = 26 in
  let c i = char_of_int (int_of_char 'a' + i - 1) in
  let add i suffix = Printf.sprintf "%c%s" (c i) suffix in
  let rec n suffix i =
    if i <= base then add i suffix
    else (
      let head = i mod base in
      let head = if head = 0 then base else head in
      n (add head suffix) ((i - head) / base) )
  in
  n ""

(** Compute the structure that a term [repr]esents,
  * given the list of universally quantified variables.
  * Also takes care of computing the printing name of variables,
  * including constraint symbols, which are removed from constraint lists.
  * It supports a mechanism for filtering out parts of the type,
  * which are then translated as `Ellipsis. *)
let repr ?(filter_out = fun _ -> false) ?(generalized = []) t : repr =
  let split_constr c =
    List.fold_left (fun (s, constraints) c -> (s, c :: constraints)) ("", []) c
  in
  let uvar g level (i, c) =
    let constr_symbols, c = split_constr c in
    let rec index n = function
      | v :: tl ->
          if fst v = i then Printf.sprintf "'%s%s" constr_symbols (name n)
          else index (n + 1) tl
      | [] -> assert false
    in
    let v = index 1 (List.rev g) in
    (* let v = Printf.sprintf "'%d" i in *)
    let v = if debug_levels then Printf.sprintf "%s[%d]" v level else v in
    `UVar (v, c)
  in
  let counter =
    let c = ref 0 in
    fun () ->
      incr c;
      !c
  in
  let evars = Hashtbl.create 10 in
  let evar level i c =
    let constr_symbols, c = split_constr c in
    if !debug then (
      let v = Printf.sprintf "?%s%d" constr_symbols i in
      let v = if debug_levels then Printf.sprintf "%s[%d]" v level else v in
      `EVar (v, c) )
    else (
      let s =
        try Hashtbl.find evars i
        with Not_found ->
          let name = String.uppercase_ascii (name (counter ())) in
          Hashtbl.add evars i name;
          name
      in
      `EVar (Printf.sprintf "?%s%s" constr_symbols s, c) )
  in
  let rec repr g t =
    if filter_out t then `Ellipsis
    else (
      match t.descr with
        | Ground g -> `Ground g
        | List t -> `List (repr g t)
        | Tuple l -> `Tuple (List.map (repr g) l)
        | Nullable t -> `Nullable (repr g t)
        | Meth (l, (g', u), v) ->
            let gen =
              List.map
                (fun ic -> match uvar (g' @ g) t.level ic with `UVar ic -> ic)
                (List.sort_uniq compare g')
            in
            `Meth (l, (gen, repr (g' @ g) u), repr g v)
        | Constr { name; params } ->
            `Constr (name, List.map (fun (l, t) -> (l, repr g t)) params)
        | Arrow (args, t) ->
            `Arrow
              ( List.map (fun (opt, lbl, t) -> (opt, lbl, repr g t)) args,
                repr g t )
        | EVar (i, c) ->
            if List.exists (fun (j, _) -> j = i) g then uvar g t.level (i, c)
            else evar t.level i c
        | Link t -> repr g t )
  in
  repr generalized t

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

  let filter f (s : t) = M.filter (fun i t -> f i t) s

  (** Whether we have the identity substitution. *)
  let is_identity (s : t) = M.is_empty s
end

(** Sets of type descriptions. *)
module DS = Set.Make (struct
  type t = string * constraints

  let compare = compare
end)

(** Print a type representation.
  * Unless in debug mode, variable identifiers are not shown,
  * and variable names are generated.
  * Names are only meaningful over one printing, as they are re-used. *)
let print_repr f t =
  (* Display the type and return the list of variables that occur in it.
   * The [par] params tells whether (..)->.. should be surrounded by
   * parenthesis or not. *)
  let rec print ~par vars : repr -> DS.t = function
    | `Constr ("stream_kind", params) -> (
        (* Let's assume that stream_kind occurs only inside a source
         * or format type -- this should be pretty much true with the
         * current API -- and simplify the printing by labeling its
         * parameters and omitting the stream_kind(...) to avoid
         * source(stream_kind(pcm(stereo),none,none)). *)
          match params with
          | [(_, a); (_, v); (_, m)] ->
              let first, has_ellipsis, vars =
                List.fold_left
                  (fun (first, has_ellipsis, vars) (lbl, t) ->
                    if t = `Ellipsis then (false, true, vars)
                    else (
                      if not first then Format.fprintf f ",@ ";
                      Format.fprintf f "%s=" lbl;
                      let vars = print ~par:false vars t in
                      (false, has_ellipsis, vars) ))
                  (true, false, vars)
                  [("audio", a); ("video", v); ("midi", m)]
              in
              if not has_ellipsis then vars
              else (
                if not first then Format.fprintf f ",@,";
                print ~par:false vars `Range_Ellipsis )
          | _ -> assert false )
    | `Constr ("none", _) ->
        Format.fprintf f "none";
        vars
    | `Constr (_, [(_, `Ground (Format format))]) ->
        Format.fprintf f "%s" (Frame_content.string_of_format format);
        vars
    | `Constr (name, params) ->
        Format.open_box (1 + String.length name);
        Format.fprintf f "%s(" name;
        let vars = print_list vars params in
        Format.fprintf f ")";
        Format.close_box ();
        vars
    | `Ground g ->
        Format.fprintf f "%s" (print_ground g);
        vars
    | `Tuple [] ->
        Format.fprintf f "unit";
        vars
    | `Tuple l ->
        if par then Format.fprintf f "@[<1>(" else Format.fprintf f "@[<0>";
        let rec aux vars = function
          | [a] -> print ~par:true vars a
          | a :: l ->
              let vars = print ~par:true vars a in
              Format.fprintf f " *@ ";
              aux vars l
          | [] -> assert false
        in
        let vars = aux vars l in
        if par then Format.fprintf f ")@]" else Format.fprintf f "@]";
        vars
    | `Nullable t ->
        let vars = print ~par:true vars t in
        Format.fprintf f "?";
        vars
    | `Meth (l, (_, a), b) as t ->
        if not !debug then (
          (* Find all methods. *)
          let rec aux = function
            | `Meth (l, t, u) ->
                let m, u = aux u in
                ((l, t) :: m, u)
            | u -> ([], u)
          in
          let m, t = aux t in
          (* Filter out duplicates. *)
          let rec aux = function
            | (l, t) :: m ->
                (l, t) :: aux (List.filter (fun (l', _) -> l <> l') m)
            | [] -> []
          in
          let m = aux m in
          (* Put latest addition last. *)
          let m = List.rev m in
          (* First print the main value. *)
          let vars =
            if t = `Tuple [] then (
              Format.fprintf f "@,@[<hv 2>{@,";
              vars )
            else (
              let vars = print ~par:true vars t in
              Format.fprintf f "@,@[<hv 2>.{@,";
              vars )
          in
          let vars =
            if m = [] then vars
            else (
              let rec gen = function
                | (x, _) :: g -> x ^ "." ^ gen g
                | [] -> ""
              in
              let gen g =
                if !show_record_schemes then gen (List.sort compare g) else ""
              in
              let rec aux vars = function
                | [(l, (g, t))] ->
                    Format.fprintf f "%s : %s" l (gen g);
                    print ~par:true vars t
                | (l, (g, t)) :: m ->
                    Format.fprintf f "%s : %s" l (gen g);
                    let vars = print ~par:false vars t in
                    Format.fprintf f ",@ ";
                    aux vars m
                | [] -> assert false
              in
              aux vars m )
          in
          Format.fprintf f "@]@,}";
          vars )
        else (
          let vars = print ~par:true vars b in
          Format.fprintf f ".{%s = " l;
          let vars = print ~par:false vars a in
          Format.fprintf f "}";
          vars )
    | `List t ->
        Format.fprintf f "@[<1>[";
        let vars = print ~par:false vars t in
        Format.fprintf f "]@]";
        vars
    | `EVar (_, [Getter a]) when !pretty_getters ->
        Format.fprintf f "?{%s}" (print_ground a);
        vars
    | `UVar (_, [Getter a]) when !pretty_getters ->
        Format.fprintf f "{%s}" (print_ground a);
        vars
    | `EVar (name, c) | `UVar (name, c) ->
        Format.fprintf f "%s" name;
        if c <> [] then DS.add (name, c) vars else vars
    | `Arrow (p, t) ->
        if par then Format.fprintf f "@[<hov 1>("
        else Format.fprintf f "@[<hov 0>";
        Format.fprintf f "@[<1>(";
        let _, vars =
          List.fold_left
            (fun (first, vars) (opt, lbl, kind) ->
              if not first then Format.fprintf f ",@ ";
              if opt then Format.fprintf f "?";
              if lbl <> "" then Format.fprintf f "%s : " lbl;
              let vars = print ~par:true vars kind in
              (false, vars))
            (true, vars) p
        in
        Format.fprintf f ")@] ->@ ";
        let vars = print ~par:false vars t in
        if par then Format.fprintf f ")@]" else Format.fprintf f "@]";
        vars
    | `Ellipsis ->
        Format.fprintf f "_";
        vars
    | `Range_Ellipsis ->
        Format.fprintf f "...";
        vars
  and print_list ?(first = true) ?(acc = []) vars = function
    | [] -> vars
    | (_, x) :: l ->
        if not first then Format.fprintf f ",";
        let vars = print ~par:false vars x in
        print_list ~first:false ~acc:(x :: acc) vars l
  in
  Format.fprintf f "@[";
  begin
    match t with
    (* We're only printing a variable: ignore its [repr]esentation. *)
    | (`EVar (_, [Getter a]) | `UVar (_, [Getter a])) when !pretty_getters ->
        let t = print_ground a in
        Format.fprintf f "{%s}" t
    | `EVar (_, c) when c <> [] ->
        Format.fprintf f "something that is %s"
          (String.concat " and " (List.map print_constr c))
    | `UVar (_, c) when c <> [] ->
        Format.fprintf f "anything that is %s"
          (String.concat " and " (List.map print_constr c))
    (* Print the full thing, then display constraints *)
    | _ ->
        let constraints = print ~par:false DS.empty t in
        let constraints = DS.elements constraints in
        if constraints <> [] then (
          let constraints =
            List.map
              (fun (name, c) ->
                (name, String.concat " and " (List.map print_constr c)))
              constraints
          in
          let constraints =
            List.stable_sort (fun (_, a) (_, b) -> compare a b) constraints
          in
          let group : ('a * 'b) list -> ('a list * 'b) list = function
            | [] -> []
            | (i, c) :: l ->
                let rec group prev acc = function
                  | [] -> [(List.rev acc, prev)]
                  | (i, c) :: l ->
                      if prev = c then group c (i :: acc) l
                      else (List.rev acc, prev) :: group c [i] l
                in
                group c [i] l
          in
          let constraints = group constraints in
          let constraints =
            List.map
              (fun (ids, c) -> String.concat ", " ids ^ " is " ^ c)
              constraints
          in
          Format.fprintf f "@ @[<2>where@ ";
          Format.fprintf f "%s" (List.hd constraints);
          List.iter (fun s -> Format.fprintf f ",@ %s" s) (List.tl constraints);
          Format.fprintf f "@]" )
  end;
  Format.fprintf f "@]"

let pp_type f t = print_repr f (repr t)

let pp_type_generalized generalized f t =
  if !debug then
    List.iter
      (fun v ->
        print_repr f (repr ~generalized (make (EVar v)));
        Format.fprintf f ".")
      generalized;
  print_repr f (repr ~generalized t)

let print ?generalized t : string =
  print_repr Format.str_formatter (repr ?generalized t);
  Format.fprintf Format.str_formatter "@?";
  Format.flush_str_formatter ()

let print_scheme (g, t) = print ~generalized:g t

let fresh_evar =
  let fresh_id =
    let c = ref 0 in
    fun () ->
      incr c;
      !c
  in
  let f ~constraints ~level ~pos =
    { pos; level; descr = EVar (fresh_id (), constraints) }
  in
  f

let rec invokes t = function
  | l :: ll ->
      let g, t = invoke t l in
      if ll = [] then (g, t) else invokes t ll
  | [] -> ([], t)

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
    | List t -> occur_check a t
    | Nullable t -> occur_check a t
    | Meth (_, (_, t), u) ->
        (* We assume that a is not a generalized variable of t. *)
        occur_check a t;
        occur_check a u
    | Arrow (p, t) ->
        List.iter (fun (_, _, t) -> occur_check a t) p;
        occur_check a t
    | EVar _ ->
        (* In normal type inference level -1 should never arise.
         * Unfortunately we can't check it strictly because this code 
         * is also used to process type annotations, which make use
         * of unknown levels. Also note that >=0 levels can arise
         * when processing type annotations, because of builtins. *)
        if b.level = -1 then b.level <- a.level
        else if a.level <> -1 then b.level <- min b.level a.level
    | Ground _ -> ()
    | Link _ -> assert false

(* Perform [a := b] where [a] is an EVar, check that [type(a)<:type(b)]. *)
let rec bind a0 b =
  (* if !debug then Printf.eprintf "%s := %s\n%!" (print a0) (print b); *)
  let a = deref a0 in
  let b = deref b in
  if b == a then ()
  else (
    occur_check a b;
    begin
      match a.descr with
      | EVar (_, constraints) ->
          List.iter
            (function
              | Getter g -> (
                  let error = Unsatisfied_constraint (Getter g, b) in
                  match b.descr with
                    | Ground g' -> if g <> g' then raise error
                    | Arrow ([], t) -> (
                        match (deref t).descr with
                          | Ground g' -> if g <> g' then raise error
                          | EVar (_, _) ->
                              (* This is almost wrong as it flips <: into
                               * >:, but that's OK for a ground type. *)
                              bind t (make (Ground g))
                          | _ -> raise error )
                    | EVar (j, c) ->
                        if List.mem (Getter g) c then ()
                        else b.descr <- EVar (j, Getter g :: c)
                    | _ -> raise error )
              | Ord ->
                  let rec check b =
                    let b = demeth b in
                    match b.descr with
                      | Ground _ -> ()
                      | EVar (j, c) ->
                          if List.mem Ord c then ()
                          else b.descr <- EVar (j, Ord :: c)
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
                          | _ -> raise (Unsatisfied_constraint (Dtools, b')) )
                    | EVar (j, c) ->
                        if not (List.mem Dtools c) then
                          b.descr <- EVar (j, Dtools :: c)
                    | _ -> raise (Unsatisfied_constraint (Dtools, b)) )
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
                    | _ -> raise (Unsatisfied_constraint (InternalMedia, b)) )
              | Num -> (
                  match (demeth b).descr with
                    | Ground g ->
                        if g <> Int && g <> Float then
                          raise (Unsatisfied_constraint (Num, b))
                    | EVar (j, c) ->
                        if List.mem Num c then ()
                        else b.descr <- EVar (j, Num :: c)
                    | _ -> raise (Unsatisfied_constraint (Num, b)) ))
            constraints
      | _ -> assert false (* only EVars are bindable *)
    end;

    (* This is a shaky hack...
     * When a value is passed to a FFI, its type is bound to a type without
     * any location.
     * If it doesn't break sharing, we set the parsing position of
     * that variable occurrence to the position of the inferred type. *)
    if b.pos = None && match b.descr with EVar _ -> false | _ -> true then
      a.descr <- Link { a0 with descr = b.descr }
    else a.descr <- Link b )

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

(** Find all the free variables satisfying a predicate. *)
let filter_vars f t =
  let rec aux l t =
    let t = deref t in
    match t.descr with
      | Ground _ -> l
      | List t | Nullable t -> aux l t
      | Tuple aa -> List.fold_left aux l aa
      | Meth (_, (g, t), u) ->
          let l = List.filter (fun v -> not (List.mem v g)) (aux l t) in
          aux l u
      | Constr c -> List.fold_left (fun l (_, t) -> aux l t) l c.params
      | Arrow (p, t) -> aux (List.fold_left (fun l (_, _, t) -> aux l t) l p) t
      | EVar (i, constraints) -> if f t then (i, constraints) :: l else l
      | Link _ -> assert false
  in
  aux [] t

(** Return a list of generalizable variables in a type.
  * This is performed after type inference on the left-hand side
  * of a let-in, with [level] being the level of that let-in.
  * Uses the simple method of ML, to be associated with a value restriction. *)
let generalizable ~level t = filter_vars (fun t -> t.level >= level) t

(** Copy a term, substituting some EVars as indicated by a list
  * of associations. Other EVars are not copied, so sharing is
  * preserved. *)
let copy_with (subst : Subst.t) t =
  let rec aux t =
    let cp x = { t with descr = x } in
    match t.descr with
      | EVar v -> ( try Subst.value subst v with Not_found -> t )
      | Constr c ->
          let params = List.map (fun (v, t) -> (v, aux t)) c.params in
          cp (Constr { c with params })
      | Ground _ -> cp t.descr
      | List t -> cp (List (aux t))
      | Nullable t -> cp (Nullable (aux t))
      | Tuple l -> cp (Tuple (List.map aux l))
      | Meth (l, (g, t), u) ->
          (* We assume that we don't substitute generalized variables. *)
          if !debug then
            assert (Subst.M.for_all (fun v _ -> not (List.mem v g)) subst);
          cp (Meth (l, (g, aux t), aux u))
      | Arrow (p, t) ->
          cp (Arrow (List.map (fun (o, l, t) -> (o, l, aux t)) p, aux t))
      | Link t ->
          (* Keep links to preserve rich position information,
           * and to make it possible to check if the application left
           * the type unchanged. *)
          cp (Link (aux t))
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
      (fun ic -> (ic, fresh_evar ~level ~constraints:(snd ic) ~pos:None))
      (List.to_seq generalized)
  in
  let subst = Subst.of_seq subst in
  fun t -> copy_with subst t

(** Simplified version of existential variable generation,
  * without constraints. This is used when parsing to annotate
  * the AST. *)
let fresh = fresh_evar

let fresh_evar = fresh_evar ~constraints:[]

(** {1 Subtype checking/inference} *)

exception Error of (repr * repr)

type explanation = bool * t * t * repr * repr

exception Type_Error of explanation

let print_type_error error_header ((flipped, ta, tb, a, b) : explanation) =
  error_header (print_pos_opt ta.pos);
  match b with
    | `Meth (l, ([], `Ellipsis), `Ellipsis) ->
        Format.printf "this value does not have a field %s.@]@." l
    | _ ->
        let inferred_pos a =
          let dpos = (deref a).pos in
          if a.pos = dpos then ""
          else (
            match dpos with
              | None -> ""
              | Some p -> " (inferred at " ^ print_pos ~prefix:"" p ^ ")" )
        in
        let ta, tb, a, b = if flipped then (tb, ta, b, a) else (ta, tb, a, b) in
        Format.printf "this value has type@.@[<2>  %a@]%s@ " print_repr a
          (inferred_pos ta);
        Format.printf "but it should be a %stype of%s@.@[<2>  %a@]%s@]@."
          (if flipped then "super" else "sub")
          ( match tb.pos with
            | None -> ""
            | Some p ->
                Printf.sprintf " the type of the value at %s"
                  (print_pos ~prefix:"" p) )
          print_repr b (inferred_pos tb)

let doc_of_type ~generalized t =
  let margin = Format.pp_get_margin Format.str_formatter () in
  Format.pp_set_margin Format.str_formatter 58;
  Format.fprintf Format.str_formatter "%a@?" (pp_type_generalized generalized) t;
  Format.pp_set_margin Format.str_formatter margin;
  Doc.trivial (Format.flush_str_formatter ())

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
 * with the optional parameter, when fully applied, applies implicitely its
 * optional argument; whereas with a mandatory argument it is expected to wait
 * for it. *)

let constr_sub x y =
  match (x, y) with
    | _, _ when x = y -> true
    | "active_source", "source" -> true
    | _ -> false

(** Ensure that a<:b, perform unification if needed.
  * In case of error, generate an explaination. *)
let rec ( <: ) a b =
  if !debug then Printf.eprintf "%s <: %s\n%!" (print a) (print b);
  match ((deref a).descr, (deref b).descr) with
    | Constr c1, Constr c2 when constr_sub c1.name c2.name ->
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
        try t1 <: t2 with Error (a, b) -> raise (Error (`List a, `List b)) )
    | Nullable t1, Nullable t2 -> (
        try t1 <: t2
        with Error (a, b) -> raise (Error (`Nullable a, `Nullable b)) )
    | Tuple l, Tuple m ->
        if List.length l <> List.length m then (
          let l = List.map (fun _ -> `Ellipsis) l in
          let m = List.map (fun _ -> `Ellipsis) m in
          raise (Error (`Tuple l, `Tuple m)) );
        let n = ref 0 in
        List.iter2
          (fun a b ->
            incr n;
            try a <: b
            with Error (a, b) ->
              let l = List.init (!n - 1) (fun _ -> `Ellipsis) in
              let l' = List.init (List.length m - !n) (fun _ -> `Ellipsis) in
              raise (Error (`Tuple (l @ [a] @ l'), `Tuple (l @ [b] @ l'))))
          l m
    | Arrow (l12, t), Arrow (l, t') ->
        (* Here, it must be that l12 = l1@l2
         * where l1 is essentially l modulo order
         * and either l2 is erasable and t<:t'
         *        or (l2)->t <: t'. *)
        let ellipsis = (false, "", `Range_Ellipsis) in
        let elide (o, l, _) = (o, l, `Ellipsis) in
        let l1, l2 =
          List.fold_left
            (* Start with [l2:=l12], [l1:=[]] and
             * move each param [o,lbl] required by [l] from [l2] to [l1]. *)
              (fun (l1, l2) (o, lbl, t) ->
              (* Search for a param with optionality o and label lbl.
               * Returns the first matching parameter
               * and the list without it. *)
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
                      ((o, lbl, t'), List.rev_append acc tl)
                    else get_param ((o', lbl', t') :: acc) tl
              in
              let (o, lbl, t'), l2' = get_param [] l2 in
              (* Check on-the-fly that the types match. *)
              begin
                try t <: t'
                with Error (t, t') ->
                  let make t =
                    `Arrow (List.rev (ellipsis :: (o, lbl, t) :: l1), `Ellipsis)
                  in
                  raise (Error (make t', make t))
              end;
              ((o, lbl, `Ellipsis) :: l1, l2'))
            ([], l12) l
        in
        let l1 = List.rev l1 in
        if List.for_all (fun (o, _, _) -> o) l2 then (
          try t <: t'
          with Error (t, t') ->
            raise (Error (`Arrow ([ellipsis], t), `Arrow ([ellipsis], t'))) )
        else (
          try { a with descr = Arrow (l2, t) } <: t' with
            | Error (`Arrow (p, t), t') ->
                raise (Error (`Arrow (l1 @ p, t), `Arrow (l1, t')))
            | Error _ -> assert false )
    | Ground (Format k), Ground (Format k') -> (
        try Frame_content.merge k k' with _ -> raise (Error (repr a, repr b)) )
    | Ground x, Ground y -> if x <> y then raise (Error (repr a, repr b))
    | EVar _, _ -> (
        try bind a b
        with Occur_check _ | Unsatisfied_constraint _ ->
          (* Can't do more concise than a full representation, as the problem
             isn't local. *)
          raise (Error (repr a, repr b)) )
    | _, EVar _ -> (
        try bind b a
        with Occur_check _ | Unsatisfied_constraint _ ->
          raise (Error (repr a, repr b)) )
    | _, Nullable t2 -> (
        try a <: t2 with Error (a, b) -> raise (Error (a, `Nullable b)) )
    | _, Meth (l, (g2, t2), u2) -> (
        try
          let g1, t1 = invoke a l in
          try
            (* TODO: we should perform proper type scheme subtyping, but this is
               a good approximation for now... *)
            instantiate ~level:(-1) ~generalized:g1 t1
            <: instantiate ~level:(-1) ~generalized:g2 t2;
            a <: hide_meth l u2
          with Error (a, b) ->
            (* TODO: it would be better to keep generalized variables here and
               below *)
            raise
              (Error
                 (`Meth (l, ([], a), `Ellipsis), `Meth (l, ([], b), `Ellipsis)))
        with Not_found -> (
          let a' = demeth a in
          match a'.descr with
            | EVar _ ->
                a'
                <: make
                     (Meth
                        ( l,
                          (g2, t2),
                          fresh ~level:(-1) ~constraints:[] ~pos:None ));
                a <: b
            | _ -> raise (Error (repr a, `Meth (l, ([], `Ellipsis), `Ellipsis))) )
        )
    | Meth (l, _, u1), _ -> hide_meth l u1 <: b
    | Link _, _ | _, Link _ -> assert false (* thanks to deref *)
    | _, _ ->
        (* The superficial representation is enough for explaining
         * the mismatch. *)
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
    Printexc.raise_with_backtrace (Type_Error (true, b, a, y, x)) bt

let ( <: ) a b =
  try a <: b
  with Error (x, y) ->
    let bt = Printexc.get_raw_backtrace () in
    Printexc.raise_with_backtrace (Type_Error (false, a, b, x, y)) bt

let rec duplicate ?pos ?level t =
  make ?pos ?level
    ( match (deref t).descr with
      | Constr c ->
          Constr
            {
              c with
              params =
                List.map
                  (fun (vars, t) -> (vars, duplicate ?pos ?level t))
                  c.params;
            }
      | Ground (Format k) -> Ground (Format (Frame_content.duplicate k))
      | Ground g -> Ground g
      | List t -> List (duplicate ?pos ?level t)
      | Tuple l -> Tuple (List.map (duplicate ?pos ?level) l)
      | Nullable t -> Nullable (duplicate ?pos ?level t)
      | Meth (name, (v, g), t) ->
          Meth (name, (v, duplicate ?pos ?level g), duplicate ?pos ?level t)
      | Arrow (args, t) ->
          Arrow
            ( List.map (fun (b, n, t) -> (b, n, duplicate ?pos ?level t)) args,
              duplicate ?pos ?level t )
      | EVar v -> EVar v
      | Link t -> Link (duplicate ?pos ?level t) )

(** Find the minimal type that is safe to use instead of both
    left and right hand types. *)
let rec min_type ?(pos = None) ?(level = -1) a b =
  try
    let min a b =
      try
        let t = fresh_evar ~level ~pos in
        duplicate ~pos ~level a <: t;
        duplicate ~pos ~level b <: t;
        deref t
      with _ ->
        let t = fresh_evar ~level ~pos in
        duplicate ~pos ~level b <: t;
        duplicate ~pos ~level a <: t;
        deref t
    in
    match ((deref a).descr, (deref b).descr) with
      | (Meth _ as da), (Meth _ as db) ->
          let rec methods m = function
            | Meth (name, scheme, t) when not (List.mem_assoc name m) ->
                methods ((name, scheme) :: m) (deref t).descr
            | _ -> m
          in
          let meths_a = methods [] da in
          let meths_b = methods [] db in
          let meths =
            List.fold_left
              (fun cur (name, (g, t)) ->
                match List.assoc_opt name meths_b with
                  | None -> cur
                  | Some (g', t') -> (
                      try (name, (g @ g', min_type t t')) :: cur with _ -> cur ))
              [] meths_a
          in
          let t = min (demeth a) (demeth b) in
          let t =
            List.fold_left
              (fun cur (name, s) -> make ~pos ~level (Meth (name, s, cur)))
              t meths
          in
          duplicate ~pos ~level a <: t;
          duplicate ~pos ~level b <: t;
          deref t
      | _ -> min a b
  with Error (x, y) ->
    let bt = Printexc.get_raw_backtrace () in
    Printexc.raise_with_backtrace (Type_Error (false, a, b, x, y)) bt
