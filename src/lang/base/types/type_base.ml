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

(** Show debugging information. *)
let debug = ref (Sys.getenv_opt "LIQUIDSOAP_DEBUG_LANG" <> None)

(** Show variables levels. *)
let debug_levels = ref false

let debug_variance = ref false

(** Type information comes attached to the AST from the parsing, with
    appropriate sharing of the type variables. Then the type inference performs
    in-place unification.

    In order to report precise type error messages, we put very dense parsing
    location information in the type. Every layer of it can have a location.
    Destructive unification introduces links in such a way that the old location
    is still accessible.

    The level annotation represents the number of abstractions which surround
    the type in the AST -- function arguments and let-in definitions. It is used
    to safely generalize types.

    Finally, constraints can be attached to existential (unknown, '_a) and
    universal ('a) type variables. *)

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
    | `Debug of string * 'a t * string
      (* add annotations before / after, mostly used for debugging *) ]

  and 'a var = string * 'a Type_constraints.t
end

type custom
type meth_doc = { meth_descr : string; category : [ `Method | `Callback ] }

type t = { pos : Pos.Option.t; descr : descr }

and constr = {
  constr_descr : string;
  univ_descr : string option;
  satisfied : subtype:(t -> t -> unit) -> satisfies:(t -> unit) -> t -> unit;
}

(** A type constructor applied to arguments (e.g. source). *)
and constructed = { constructor : string; params : (variance * t) list }

(** Contents of a variable. *)
and var = {
  name : int;
  mutable level : int;
  mutable constraints : constr Type_constraints.t;
}

and invar =
  | Free of var  (** the variable is free *)
  | Link of variance * t  (** the variable has bee substituted *)

(** A type scheme (i.e. a type with universally quantified variables). *)
and scheme = var list * t

(** A method. *)
and meth = {
  meth : string;  (** name of the method *)
  optional : bool;  (** is the method optional? *)
  scheme : scheme;  (** type scheme *)
  doc : meth_doc;  (** documentation *)
  json_name : string option;  (** name when represented as JSON *)
}

and repr_t = { t : t; json_repr : [ `Tuple | `Object ] }

and custom_handler = {
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

and var_t = { id : int; mutable contents : invar }

and descr =
  | String
  | Int
  | Float
  | Bool
  | Never
  | Custom of custom_handler
  | Constr of constructed
  | Getter of t  (** a getter: something that is either a t or () -> t *)
  | List of repr_t
  | Tuple of t list
  | Nullable of t  (** something that is either t or null *)
  | Meth of meth * t  (** t with a method added *)
  | Arrow of t argument list * t  (** a function *)
  | Var of var_t  (** a type variable *)

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

let unit = Tuple []

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

let var_id_atom = Atomic.make (-1)
let var_id () = Atomic.fetch_and_add var_id_atom 1

(** Dereferencing gives you the meaning of a term, going through links created
    by instantiations. One should (almost) never work on a non-dereferenced
    type. *)
let rec deref t =
  match t.descr with Var { contents = Link (_, t) } -> deref t | _ -> t

(** Create a type from its value. *)
let make ?pos d =
  match d with
    | Nullable t -> (
        let t' = deref t in
        match t'.descr with
          (* Avoid double nullable by returning link to inner nullable *)
          | Nullable _ ->
              {
                pos;
                descr = Var { id = var_id (); contents = Link (`Covariant, t') };
              }
          | _ -> { pos; descr = d })
    | _ -> { pos; descr = d }

(** Remove methods. This function also removes links. *)
let rec demeth t =
  let t = deref t in
  match t.descr with Meth (_, t) -> demeth t | _ -> t

let rec filter_meths t fn =
  let t = deref t in
  match t.descr with
    | Meth (m, t) when not (fn m) -> filter_meths t fn
    | Meth (m, t) -> { t with descr = Meth (m, filter_meths t fn) }
    | _ -> t

let rec map_meths t fn =
  let t = deref t in
  match t.descr with
    | Meth (m, t) -> { t with descr = Meth (fn m, map_meths t fn) }
    | _ -> t

(** Put the methods of the first type around the second type. *)
let rec remeth t u =
  let t = deref t in
  match t.descr with
    | Meth (m, t) -> { t with descr = Meth (m, remeth t u) }
    | _ -> u

(** Type of a method in a type. *)
let rec invoke t l =
  match (deref t).descr with
    | Meth (m, _) when m.meth = l -> m.scheme
    | Meth (_, t) -> invoke t l
    | _ -> raise Not_found

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
let meth ?pos ?json_name ?(category = `Method) ?(optional = false) meth scheme
    ?(doc = "") t =
  make ?pos
    (Meth
       ( {
           meth;
           optional;
           scheme;
           doc = { meth_descr = doc; category };
           json_name;
         },
         t ))

(** Add a submethod to a type. *)
let rec meths ?(invoke = invoke) ?pos l v t =
  match l with
    | [] -> assert false
    | [l] -> meth ?pos l v t
    | l :: ll ->
        let g, tl = invoke t l in
        let v = meths ?pos ~invoke ll v tl in
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

(** Create a fresh variable. *)
let var_name_atom = Atomic.make (-1)

let var_name () = Atomic.fetch_and_add var_name_atom 1

let var ?(constraints = []) ?(level = max_int) ?pos () =
  let constraints = Constraints.of_list constraints in
  let name = var_name () in
  make ?pos
    (Var { id = var_id (); contents = Free { name; level; constraints } })

module Fresh = struct
  type mapper = {
    level : int option;
    preserve_positions : bool;
    selector : var -> bool;
    var_maps : (var, var) Hashtbl.t;
    link_maps : (int, var_t) Hashtbl.t;
  }

  let init ?(preserve_positions = false) ?(selector = fun _ -> true) ?level () =
    {
      level;
      preserve_positions;
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

  let make ({ preserve_positions; selector; link_maps } as h) t =
    let map_var = make_var h in
    let map_descr map = function
      | Int -> Int
      | Float -> Float
      | String -> String
      | Bool -> Bool
      | Never -> Never
      | Custom c -> Custom { c with typ = c.copy_with map c.typ }
      | Constr { constructor; params } ->
          Constr
            { constructor; params = List.map (fun (v, t) -> (v, map t)) params }
      | Getter t -> Getter (map t)
      | List { t; json_repr } -> List { t = map t; json_repr }
      | Tuple l -> Tuple (List.map map l)
      | Nullable t -> Nullable (map t)
      | Meth ({ meth; optional; scheme = vars, t; doc; json_name }, t') ->
          Meth
            ( {
                meth;
                optional;
                scheme = (List.map map_var vars, map t);
                doc;
                json_name;
              },
              map t' )
      | Arrow (args, t) ->
          Arrow (List.map (fun (b, s, t) -> (b, s, map t)) args, map t)
      (* Here we keep all links. While it could be tempting to deref,
         we are using links to compute type supremum in type unification
         so we are better off keeping them. Also, we need to create fresh
         links to make sure that a suppremum computation in the refreshed
         type does not impact the original type. *)
      | Var { id; contents = Link (v, t) } ->
          Var
            (try Hashtbl.find link_maps id
             with Not_found ->
               let new_link = { id = var_id (); contents = Link (v, map t) } in
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
    let rec map { pos; descr } =
      {
        pos = (if preserve_positions then pos else None);
        descr = map_descr map descr;
      }
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
let is_fun t = match (demeth t).descr with Arrow _ -> true | _ -> false

let is_source t =
  match (demeth t).descr with
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
    | { descr = Var ({ contents = Link (_, t) } as c) } ->
        c.contents <- Link (`Invariant, t);
        mk_invariant t
    | _ -> ()

let rec hide_meth l a =
  match (deref a).descr with
    | Meth ({ meth = l' }, u) when l' = l -> hide_meth l u
    | Meth (m, u) -> make ?pos:a.pos (Meth (m, hide_meth l u))
    | _ -> a

let rec opt_meth l a =
  match (deref a).descr with
    | Meth (({ meth = l' } as m), u) when l' = l ->
        make ?pos:a.pos (Meth ({ m with optional = true }, u))
    | Meth (m, u) -> make ?pos:a.pos (Meth (m, opt_meth l u))
    | _ -> a

let rec get_meth l a =
  match (deref a).descr with
    | Meth (({ meth = l' } as meth), _) when l = l' -> meth
    | Meth (_, a) -> get_meth l a
    | _ -> assert false
