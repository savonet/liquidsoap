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
let debug_levels = ref false
let debug_variance = ref false

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

(** Ground types *)

type ground = ..
type ground += Bool | Int | String | Float | Format of Content.format

let ground_printers = Queue.create ()
let register_ground_printer fn = Queue.add fn ground_printers
let ground_resolvers = Queue.create ()
let register_ground_resolver fn = Queue.add fn ground_resolvers

exception FoundName of string
exception FoundGround of ground

let () =
  register_ground_printer (function
    | String -> Some "string"
    | Bool -> Some "bool"
    | Int -> Some "int"
    | Float -> Some "float"
    | Format p -> Some (Content.string_of_format p)
    | _ -> None);
  register_ground_resolver (function
    | "string" -> Some String
    | "bool" -> Some Bool
    | "int" -> Some Int
    | "float" -> Some Float
    | _ -> None)

let string_of_ground v =
  try
    Queue.iter
      (fun fn -> match fn v with Some s -> raise (FoundName s) | None -> ())
      ground_printers;
    assert false
  with FoundName s -> s

let resolve_ground name =
  try
    Queue.iter
      (fun fn ->
        match fn name with Some g -> raise (FoundGround g) | None -> ())
      ground_resolvers;
    raise Not_found
  with FoundGround g -> g

let resolve_ground_opt name =
  try
    Queue.iter
      (fun fn ->
        match fn name with Some g -> raise (FoundGround g) | None -> ())
      ground_resolvers;
    None
  with FoundGround g -> Some g

(** Type constraints *)

type constr = Num | Ord | Dtools | InternalMedia
type constraints = constr list

let string_of_constr = function
  | Num -> "a number type"
  | Ord -> "an orderable type"
  | Dtools -> "unit, bool, int, float, string or [string]"
  | InternalMedia -> "an internal media type (none, pcm, yuva420p or midi)"

(** Types *)

type variance = Covariant | Contravariant | Invariant

type t = { pos : pos option; descr : descr }

and constructed = { constructor : string; params : (variance * t) list }

and meth = {
  meth : string;
  scheme : scheme;
  doc : string;
  json_name : string option;
}

and repr_t = { t : t; json_repr : [ `Tuple | `Object ] }

and descr =
  | Constr of constructed
  | Ground of ground
  | Getter of t
  | List of repr_t
  | Tuple of t list
  | Nullable of t
  | Meth of meth * t
  | Arrow of (bool * string * t) list * t
  | Var of invar ref

and invar = Free of var | Link of variance * t

and var = { name : int; mutable level : int; mutable constraints : constraints }

and scheme = var list * t

let unit = Tuple []
let var_eq v v' = v.name = v'.name
let make ?pos d = { pos; descr = d }

(** Dereferencing gives you the meaning of a term, going through links created
    by instantiations. One should (almost) never work on a non-dereferenced
    type. *)
let rec deref t =
  match t.descr with Var { contents = Link (_, t) } -> deref t | _ -> t

(** Remove methods. This function also removes links. *)
let rec demeth t =
  let t = deref t in
  match t.descr with Meth (_, t) -> demeth t | _ -> t

let rec remeth t u =
  let t = deref t in
  match t.descr with
    | Meth (m, t) -> { t with descr = Meth (m, remeth t u) }
    | _ -> u

let rec invoke t l =
  match (deref t).descr with
    | Meth (m, _) when m.meth = l -> m.scheme
    | Meth (_, t) -> invoke t l
    | _ -> raise Not_found

(** Add a method. *)
let meth ?pos ?json_name meth scheme ?(doc = "") t =
  make ?pos (Meth ({ meth; scheme; doc; json_name }, t))

(** Add methods. *)
let rec meths ?pos l v t =
  match l with
    | [] -> assert false
    | [l] -> meth ?pos l v t
    | l :: ll ->
        let g, tl = invoke t l in
        let v = meths ?pos ll v tl in
        meth ?pos l (g, v) t

(** Split the methods from the type. *)
let split_meths t =
  let rec aux hide t =
    let t = deref t in
    match t.descr with
      | Meth (m, t) ->
          let meth, t = aux (m.meth :: hide) t in
          let meth = if List.mem m.meth hide then meth else m :: meth in
          (meth, t)
      | _ -> ([], t)
  in
  aux [] t

let var =
  let name =
    let c = ref (-1) in
    fun () ->
      incr c;
      !c
  in
  let f ?(constraints = []) ?(level = max_int) ?pos () =
    let name = name () in
    make ?pos (Var (ref (Free { name; level; constraints })))
  in
  f

(** Find all the free variables satisfying a predicate. *)
let filter_vars f t =
  let rec aux l t =
    let t = deref t in
    match t.descr with
      | Ground _ -> l
      | Getter t -> aux l t
      | List { t } | Nullable t -> aux l t
      | Tuple aa -> List.fold_left aux l aa
      | Meth ({ scheme = g, t }, u) ->
          let l = List.filter (fun v -> not (List.mem v g)) (aux l t) in
          aux l u
      | Constr c -> List.fold_left (fun l (_, t) -> aux l t) l c.params
      | Arrow (p, t) -> aux (List.fold_left (fun l (_, _, t) -> aux l t) l p) t
      | Var { contents = Free var } ->
          if f var && not (List.exists (var_eq var) l) then var :: l else l
      | Var { contents = Link _ } -> assert false
  in
  aux [] t

let rec invokes t = function
  | l :: ll ->
      let g, t = invoke t l in
      if ll = [] then (g, t) else invokes t ll
  | [] -> ([], t)

let to_string_fun =
  ref (fun ?generalized:_ _ -> failwith "Type.to_string not defined yet")

let to_string ?generalized (t : t) : string = !to_string_fun ?generalized t
