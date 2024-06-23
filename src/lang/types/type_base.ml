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

(** Show debugging information. *)
let debug = ref (Sys.getenv_opt "LIQUIDSOAP_DEBUG_LANG" <> None)

(** Show variables levels. *)
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

(** {2 Types} *)

(** Type description *)

type variance = [ `Covariant | `Invariant ]
type 'a argument = bool * string * 'a

module R = struct
  type 'a meth = {
    name : string;
    optional : bool;
    scheme : 'a var list * 'a t;
    json_name : string option;
  }

  and 'a t =
    [ `Constr of string * (variance * 'a t) list
    | `List of 'a t * [ `Object | `Tuple ]
    | `Tuple of 'a t list
    | `Nullable of 'a t
    | `Meth of 'a meth * 'a t (* label, type scheme, JSON name, base type *)
    | `Arrow of 'a t argument list * 'a t
    | `Getter of 'a t
    | `EVar of 'a var (* existential variable *)
    | `UVar of 'a var (* universal variable *)
    | `Ellipsis (* omitted sub-term *)
    | `Range_Ellipsis (* omitted sub-terms (in a list, e.g. list of args) *)
    | `Debug of
      string * 'a t * string
      (* add annotations before / after, mostly used for debugging *) ]

  and 'a var = string * 'a Type_constraints.t
end

type custom

(** Contents of a variable. *)
type var = {
  name : int;
  mutable pos : Pos.Option.t;
  mutable level : int;
  mutable constraints : constr Type_constraints.t;
}

and invar =
  | Free of var
  | Link of variance * t  (** the variable has bee substituted *)

and var_t = { id : int; mutable contents : invar }

and constr = {
  constr_descr : string;
  univ_descr : string option;
  satisfied : subtype:(t -> t -> unit) -> satisfies:(t -> unit) -> t -> unit;
}

(** A type scheme (i.e. a type with universally quantified variables). *)
and scheme = var list * t

and t =
  | String of Pos.Option.t
  | Int of Pos.Option.t
  | Float of Pos.Option.t
  | Bool of Pos.Option.t
  | Never of Pos.Option.t
  | Custom of {
      typ : custom;
      custom_name : string;
      copy_with : (t -> t) -> custom -> custom;
      occur_check : (t -> unit) -> custom -> unit;
      filter_vars :
        (var list -> t -> var list) -> var list -> custom -> var list;
      repr : (var list -> t -> constr R.t) -> var list -> custom -> constr R.t;
      subtype : (t -> t -> unit) -> custom -> custom -> unit;
      sup : (t -> t -> t) -> custom -> custom -> custom;
      to_string : custom -> string;
      pos : Pos.Option.t;
    }
  | Constr of {
      constructor : string;
      params : (variance * t) list;
      pos : Pos.Option.t;
    }
  | Getter of { t : t; pos : Pos.Option.t }
      (** a getter: something that is either a t or () -> t *)
  | List of { t : t; json_repr : [ `Tuple | `Object ]; pos : Pos.Option.t }
  | Tuple of { t : t list; pos : Pos.Option.t }
  | Nullable of { t : t; pos : Pos.Option.t }
      (** something that is either t or null *)
  | Meth of {
      meth : string;  (** name of the method *)
      optional : bool;  (** is the method optional? *)
      scheme : scheme;  (** type scheme *)
      doc : string;  (** documentation *)
      json_name : string option;  (** name when represented as JSON *)
      t : t;
      pos : Pos.Option.t;
    }
  | Arrow of { args : t argument list; t : t; pos : Pos.Option.t }
      (** a function *)
  | Var of var_t  (** a type variable *)

type custom_handler = {
  typ : custom;
  custom_name : string;
  copy_with : (t -> t) -> custom -> custom;
  occur_check : (t -> unit) -> custom -> unit;
  filter_vars : (var list -> t -> var list) -> var list -> custom -> var list;
  repr : (var list -> t -> constr R.t) -> var list -> custom -> constr R.t;
  subtype : (t -> t -> unit) -> custom -> custom -> unit;
  sup : (t -> t -> t) -> custom -> custom -> custom;
  to_string : custom -> string;
}

type constructed = { constructor : string; params : (variance * t) list }
type repr_t = { t : t; json_repr : [ `Tuple | `Object ] }

type meth = {
  meth : string;  (** name of the method *)
  optional : bool;  (** is the method optional? *)
  scheme : scheme;  (** type scheme *)
  doc : string;  (** documentation *)
  json_name : string option;  (** name when represented as JSON *)
}

type descr =
  [ `String
  | `Int
  | `Float
  | `Bool
  | `Never
  | `Custom of custom_handler
  | `Constr of constructed
  | `Getter of t
  | `List of repr_t
  | `Tuple of t list
  | `Nullable of t
  | `Meth of meth * t
  | `Arrow of t argument list * t
  | `Var of var_t ]

module Constraints = struct
  include Type_constraints

  type nonrec t = constr Type_constraints.t
end

module DS = Set.Make (struct
  type nonrec t = string * Constraints.t

  let compare (s, v) (s', v') =
    match Stdlib.compare s s' with 0 -> Constraints.compare v v' | x -> x
end)

let string_of_constr c = c.constr_descr

exception NotImplemented
exception Exists of Pos.Option.t * string
exception Unsatisfied_constraint

let unit = `Tuple []

let rec is_unit = function
  | Tuple { t = [] } -> true
  | Var { contents = Link (_, t) } -> is_unit t
  | _ -> false

(** Operations on variables. *)
module Var = struct
  type t = var

  (** Compare two variables for equality. This comparison should always be used
      to compare variables (as opposed to =). *)
  let eq v v' = v.name = v'.name

  let compare v v' = compare v.name v'.name
end

(** Sets of variables. *)
module Vars = struct
  include Set.Make (Var)

  let add_list l v = add_seq (List.to_seq l) v
end

(** Create a type from its value. *)
let make ?pos : descr -> t = function
  | `String -> String pos
  | `Int -> Int pos
  | `Float -> Float pos
  | `Bool -> Bool pos
  | `Never -> Never pos
  | `Custom
      {
        typ;
        custom_name;
        copy_with;
        occur_check;
        filter_vars;
        repr;
        subtype;
        sup;
        to_string;
      } ->
      Custom
        {
          typ;
          custom_name;
          copy_with;
          occur_check;
          filter_vars;
          repr;
          subtype;
          sup;
          to_string;
          pos;
        }
  | `Constr { constructor; params } -> Constr { constructor; params; pos }
  | `Getter t -> Getter { t; pos }
  | `List { t; json_repr } -> List { t; json_repr; pos }
  | `Tuple t -> Tuple { t; pos }
  | `Nullable t -> Nullable { t; pos }
  | `Meth ({ meth; optional; scheme; doc; json_name }, t) ->
      Meth { meth; optional; scheme; doc; json_name; t; pos }
  | `Arrow (args, t) -> Arrow { args; t; pos }
  | `Var { id; contents } -> Var { id; contents }

let descr : t -> descr = function
  | String _ -> `String
  | Int _ -> `Int
  | Float _ -> `Float
  | Bool _ -> `Bool
  | Never _ -> `Never
  | Custom
      {
        typ;
        custom_name;
        copy_with;
        occur_check;
        filter_vars;
        repr;
        subtype;
        sup;
        to_string;
      } ->
      `Custom
        {
          typ;
          custom_name;
          copy_with;
          occur_check;
          filter_vars;
          repr;
          subtype;
          sup;
          to_string;
        }
  | Constr { constructor; params } -> `Constr { constructor; params }
  | Getter { t } -> `Getter t
  | List { t; json_repr } -> `List { t; json_repr }
  | Tuple { t } -> `Tuple t
  | Nullable { t } -> `Nullable t
  | Meth { meth; optional; scheme; doc; json_name; t } ->
      `Meth ({ meth; optional; scheme; doc; json_name }, t)
  | Arrow { args; t } -> `Arrow (args, t)
  | Var c -> `Var c

let get_meth = function
  | Meth { meth; optional; scheme; doc; json_name } ->
      { meth; optional; scheme; doc; json_name }
  | _ -> assert false

let rec pos = function
  | String pos
  | Int pos
  | Float pos
  | Bool pos
  | Never pos
  | Custom { pos }
  | Constr { pos }
  | Getter { pos }
  | List { pos }
  | Tuple { pos }
  | Nullable { pos }
  | Meth { pos }
  | Arrow { pos }
  | Var { contents = Free { pos } } ->
      pos
  | Var { contents = Link (_, t) } -> pos t

(** Dereferencing gives you the meaning of a term, going through links created
    by instantiations. One should (almost) never work on a non-dereferenced
    type. *)
let rec deref t =
  match t with Var { contents = Link (_, t) } -> deref t | _ -> t

(** Remove methods. This function also removes links. *)
let rec demeth t =
  let t = deref t in
  match t with Meth { t } -> demeth t | _ -> t
  [@@inline always]

let rec filter_meths t fn =
  let t = deref t in
  match t with
    | Meth { meth; optional; scheme; doc; json_name; t }
      when not (fn { meth; optional; scheme; doc; json_name }) ->
        filter_meths t fn
    | Meth ({ t } as m) -> Meth { m with t = filter_meths t fn }
    | _ -> t
  [@@inline always]

(** Put the methods of the first type around the second type. *)
let rec remeth t u =
  let t = deref t in
  match t with Meth ({ t } as m) -> Meth { m with t = remeth t u } | _ -> u
  [@@inline always]

(** Type of a method in a type. *)
let rec invoke t l =
  match deref t with
    | Meth { meth; scheme } when meth = l -> scheme
    | Meth { t } -> invoke t l
    | _ -> raise Not_found
  [@@inline always]

(** Do we have a method with given label? *)
let has_meth t l =
  try
    ignore (invoke t l);
    true
  with Not_found -> false

(** Type of a submethod in a type. *)
let rec invokes t = function
  | l :: ll ->
      let g, t = invoke t l in
      if ll = [] then (g, t) else invokes t ll
  | [] -> ([], t)

(** Add a method to a type. *)
let meth ?pos ?json_name ?(optional = false) meth scheme ?(doc = "") t =
  Meth { meth; optional; scheme; doc; json_name; t; pos }

(** Add a submethod to a type. *)
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
    match t with
      | Meth { meth; optional; scheme; doc; json_name; t } ->
          let l, t = aux (meth :: hide) t in
          let l =
            if List.mem meth hide then l
            else { meth; optional; scheme; doc; json_name } :: l
          in
          (l, t)
      | _ -> ([], t)
  in
  aux [] t

(** Create a fresh variable. *)
let var_name_atom = Atomic.make (-1)

let var_name () = Atomic.fetch_and_add var_name_atom 1
let var_id_atom = Atomic.make (-1)
let var_id () = Atomic.fetch_and_add var_id_atom 1

let var ?(constraints = []) ?(level = max_int) ?pos () =
  let constraints = Constraints.of_list constraints in
  let name = var_name () in
  make
    (`Var { id = var_id (); contents = Free { pos; name; level; constraints } })

module Fresh = struct
  type mapper = {
    level : int option;
    selector : var -> bool;
    var_maps : (var, var) Hashtbl.t;
    link_maps : (int, var_t) Hashtbl.t;
  }

  let init ?(selector = fun _ -> true) ?level () =
    {
      level;
      selector;
      var_maps = Hashtbl.create 10;
      link_maps = Hashtbl.create 10;
    }

  let make_var { level; selector; var_maps } var =
    if not (selector var) then var
    else (
      try Hashtbl.find var_maps var
      with Not_found ->
        let level = Option.value ~default:var.level level in
        let new_var = { var with name = var_name (); level } in
        Hashtbl.replace var_maps var new_var;
        new_var)

  let make ({ selector; link_maps } as h) t =
    let map_var = make_var h in
    let rec map v =
      match v with
        | Int _ | Float _ | String _ | Bool _ | Never _ -> v
        | Custom c -> Custom { c with typ = c.copy_with map c.typ }
        | Constr { constructor; params; pos } ->
            Constr
              {
                constructor;
                params = List.map (fun (v, t) -> (v, map t)) params;
                pos;
              }
        | Getter { t; pos } -> Getter { t = map t; pos }
        | List { t; json_repr; pos } -> List { t = map t; json_repr; pos }
        | Tuple { t; pos } -> Tuple { t = List.map map t; pos }
        | Nullable { t; pos } -> Nullable { t = map t; pos }
        | Meth ({ scheme = vars, t'; t; pos } as m) ->
            Meth
              {
                m with
                scheme = (List.map map_var vars, map t');
                t = map t;
                pos;
              }
        | Arrow { args; t; pos } ->
            Arrow
              {
                args = List.map (fun (b, s, t) -> (b, s, map t)) args;
                t = map t;
                pos;
              }
        (* Here we keep all links. While it could be tempting to deref,
           we are using links to compute type supremum in type unification
           so we are better off keeping them. Also, we need to create fresh
           links to make sure that a suppremum computation in the refreshed
           type does not impact the original type. *)
        | Var { id; contents = Link (v, t) } ->
            Var
              (try Hashtbl.find link_maps id
               with Not_found ->
                 let new_link =
                   { id = var_id (); contents = Link (v, map t) }
                 in
                 Hashtbl.replace link_maps id new_link;
                 new_link)
        | Var { id; contents = Free var } as descr ->
            if not (selector var) then descr
            else
              Var
                (try Hashtbl.find link_maps id
                 with Not_found ->
                   let new_link =
                     { id = var_id (); contents = Free (map_var var) }
                   in
                   Hashtbl.replace link_maps id new_link;
                   new_link)
    in
    map t
end

let fresh t = Fresh.make (Fresh.init ()) t

let to_string_fun =
  ref (fun ?(generalized : var list option) _ ->
      ignore generalized;
      failwith "Type.to_string not defined yet")

(** String representation of a type. *)
let to_string ?generalized (t : t) : string = !to_string_fun ?generalized t

let string_of_scheme (g, t) = to_string ~generalized:g t
let is_fun t = match demeth t with Arrow _ -> true | _ -> false

let is_source t =
  match demeth t with
    | Constr { constructor = "source"; _ } -> true
    | _ -> false

let custom_types : (string, unit -> t) Hashtbl.t = Hashtbl.create 10

let register_type name custom =
  let mk_typ =
    match Hashtbl.find_opt custom_types name with
      | Some mk_typ -> fun () -> remeth (mk_typ ()) (custom ())
      | None -> custom
  in
  Hashtbl.replace custom_types name mk_typ

let register_type name custom =
  match String.split_on_char '.' name with
    | [] -> assert false
    | name :: [] -> register_type name custom
    | root :: names ->
        let default_mk_typ () = make unit in
        let root_mk_typ =
          Option.value ~default:default_mk_typ
            (Hashtbl.find_opt custom_types root)
        in
        let rec f root_typ = function
          | [] -> assert false
          | name :: [] -> meth name ([], custom ()) root_typ
          | name :: names ->
              let typ =
                try snd (invoke root_typ name) with _ -> default_mk_typ ()
              in
              meth name ([], f typ names) root_typ
        in
        Hashtbl.replace custom_types root (fun () -> f (root_mk_typ ()) names)

let find_opt_typ = Hashtbl.find_opt custom_types

let rec mk_invariant t =
  match t with
    | Var ({ contents = Link (_, t) } as c) ->
        c.contents <- Link (`Invariant, t);
        mk_invariant t
    | _ -> ()
