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

let debug = ref (Utils.getenv_opt "LIQUIDSOAP_DEBUG_LANG" <> None)
let debug_levels = false

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

type pos = Runtime_error.pos

let print_single_pos = Runtime_error.print_single_pos
let print_pos = Runtime_error.print_pos
let print_pos_opt = Runtime_error.print_pos_opt
let print_pos_list = Runtime_error.print_pos_list

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

type constr = Num | Ord | Dtools | InternalMedia
type constraints = constr list

let print_constr = function
  | Num -> "a number type"
  | Ord -> "an orderable type"
  | Dtools -> "unit, bool, int, float, string or [string]"
  | InternalMedia -> "an internal media type (none, pcm, yuva420p or midi)"

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
  | Getter of t
  | List of t
  | Tuple of t list
  | Nullable of t
  | Meth of string * scheme * string * t (* label, type, documentation, type to decorate *)
  | Arrow of (bool * string * t) list * t
  | Union of t * t
  | EVar of var (* type variable *)
  | Link of t

and var = int * constraints

and scheme = var list * t

let unit = Tuple []

type repr =
  [ `Constr of string * (variance * repr) list
  | `Ground of ground
  | `List of repr
  | `Tuple of repr list
  | `Nullable of repr
  | `Meth of string * ((string * constraints) list * repr) * repr
  | `Arrow of (bool * string * repr) list * repr
  | `Getter of repr
  | `Union of repr * repr
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
  match t.descr with Meth (_, _, _, t) -> demeth t | _ -> t

let rec remeth t u =
  let t = deref t in
  match t.descr with
    | Meth (l, v, d, t) -> { t with descr = Meth (l, v, d, remeth t u) }
    | _ -> u

let rec invoke t l =
  match (deref t).descr with
    | Meth (l', t, _, _) when l = l' -> t
    | Meth (_, _, _, t) -> invoke t l
    | _ -> raise Not_found

(** Add a method. *)
let meth ?pos ?level l v ?(doc = "") t = make ?pos ?level (Meth (l, v, doc, t))

(** Add methods. *)
let rec meths ?pos ?level l v t =
  match l with
    | [] -> assert false
    | [l] -> meth ?pos ?level l v t
    | l :: ll ->
        let g, tl = invoke t l in
        let v = meths ?pos ?level ll v tl in
        meth ?pos ?level l (g, v) t

(** Split the methods from the type. *)
let split_meths t =
  let rec aux hide t =
    let t = deref t in
    match t.descr with
      | Meth (l, v, d, t) ->
          let m, t = aux (l :: hide) t in
          let m = if List.mem l hide then m else (l, (v, d)) :: m in
          (m, t)
      | _ -> ([], t)
  in
  aux [] t

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
      n (add head suffix) ((i - head) / base))
  in
  n ""

(** Compute the structure that a term [repr]esents, given the list of
   universally quantified variables. Also takes care of computing the printing
   name of variables, including constraint symbols, which are removed from
   constraint lists. It supports a mechanism for filtering out parts of the
   type, which are then translated as `Ellipsis. *)
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
      `EVar (v, c))
    else (
      let s =
        try Hashtbl.find evars i
        with Not_found ->
          let name = String.uppercase_ascii (name (counter ())) in
          Hashtbl.add evars i name;
          name
      in
      `EVar (Printf.sprintf "?%s%s" constr_symbols s, c))
  in
  let rec repr g t =
    if filter_out t then `Ellipsis
    else (
      match t.descr with
        | Ground g -> `Ground g
        | Getter t -> `Getter (repr g t)
        | List t -> `List (repr g t)
        | Tuple l -> `Tuple (List.map (repr g) l)
        | Nullable t -> `Nullable (repr g t)
        | Meth (l, (g', u), _, v) ->
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
        | Union (u, v) -> `Union (repr g u, repr g v)
        | EVar (i, c) ->
            if List.exists (fun (j, _) -> j = i) g then uvar g t.level (i, c)
            else evar t.level i c
        | Link t -> repr g t)
  in
  repr generalized t

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
                      (false, has_ellipsis, vars)))
                  (true, false, vars)
                  [("audio", a); ("video", v); ("midi", m)]
              in
              if not has_ellipsis then vars
              else (
                if not first then Format.fprintf f ",@,";
                print ~par:false vars `Range_Ellipsis)
          | _ -> assert false)
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
              vars)
            else (
              let vars = print ~par:true vars t in
              Format.fprintf f "@,@[<hv 2>.{@,";
              vars)
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
              aux vars m)
          in
          Format.fprintf f "@]@,}";
          vars)
        else (
          let vars = print ~par:true vars b in
          Format.fprintf f ".{%s = " l;
          let vars = print ~par:false vars a in
          Format.fprintf f "}";
          vars)
    | `List t ->
        Format.fprintf f "@[<1>[";
        let vars = print ~par:false vars t in
        Format.fprintf f "]@]";
        vars
    | `Getter t ->
        Format.fprintf f "{";
        let vars = print ~par:false vars t in
        Format.fprintf f "}";
        vars
    | `Union (t, u) ->
        Format.fprintf f "@[<1>(";
        let vars = print ~par:false vars t in
        Format.fprintf f " ∪ ";
        let vars = print ~par:false vars u in
        Format.fprintf f ")@]";
        vars
    | `EVar (a, [InternalMedia]) ->
        Format.fprintf f "?internal(%s)" a;
        vars
    | `UVar (a, [InternalMedia]) ->
        Format.fprintf f "internal(%s)" a;
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
          Format.fprintf f "@]")
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

type explanation = bool * t * t * repr * repr

exception Type_error of explanation

let print_type_error error_header ((flipped, ta, tb, a, b) : explanation) =
  error_header (print_pos_opt ta.pos);
  match b with
    | `Meth (l, ([], `Ellipsis), `Ellipsis) when not flipped ->
        Format.printf "this value has no method %s@." l
    | _ ->
        let inferred_pos a =
          let dpos = (deref a).pos in
          if a.pos = dpos then ""
          else (
            match dpos with
              | None -> ""
              | Some p -> " (inferred at " ^ print_pos ~prefix:"" p ^ ")")
        in
        let ta, tb, a, b = if flipped then (tb, ta, b, a) else (ta, tb, a, b) in
        Format.printf "this value has type@.@[<2>  %a@]%s@ " print_repr a
          (inferred_pos ta);
        Format.printf "but it should be a %stype of%s@.@[<2>  %a@]%s@]@."
          (if flipped then "super" else "sub")
          (match tb.pos with
            | None -> ""
            | Some p ->
                Printf.sprintf " the type of the value at %s"
                  (print_pos ~prefix:"" p))
          print_repr b (inferred_pos tb)

let fresh =
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

(** Simplified version of existential variable generation, without
    constraints. This is used when parsing to annotate the AST. *)
let fresh_evar = fresh ~constraints:[]

(** Find all the free variables satisfying a predicate. *)
let filter_vars f t =
  let rec aux l t =
    let t = deref t in
    match t.descr with
      | Ground _ -> l
      | Getter t -> aux l t
      | List t | Nullable t -> aux l t
      | Tuple aa -> List.fold_left aux l aa
      | Meth (_, (g, t), _, u) ->
          let l = List.filter (fun v -> not (List.mem v g)) (aux l t) in
          aux l u
      | Constr c -> List.fold_left (fun l (_, t) -> aux l t) l c.params
      | Arrow (p, t) -> aux (List.fold_left (fun l (_, _, t) -> aux l t) l p) t
      | Union (t, u) -> aux (aux l t) u
      | EVar (i, constraints) -> if f t then (i, constraints) :: l else l
      | Link _ -> assert false
  in
  aux [] t

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
              | Ord ->
                  let rec check b =
                    let m, b = split_meths b in
                    match b.descr with
                      | Ground _ -> ()
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
    else a.descr <- Link b)

(** {1 Documentation} *)

let doc_of_type ~generalized t =
  let margin = Format.pp_get_margin Format.str_formatter () in
  Format.pp_set_margin Format.str_formatter 58;
  Format.fprintf Format.str_formatter "%a@?" (pp_type_generalized generalized) t;
  Format.pp_set_margin Format.str_formatter margin;
  Doc.trivial (Format.flush_str_formatter ())

let doc_of_meths m =
  let items = new Doc.item "" in
  List.iter
    (fun (m, ((generalized, t), doc)) ->
      let i = new Doc.item ~sort:false doc in
      i#add_subsection "type" (doc_of_type ~generalized t);
      items#add_subsection m i)
    m;
  items
