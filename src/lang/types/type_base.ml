(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

type variance = [ `Covariant | `Invariant ]

(** Type description *)
type descr = ..

(** A type *)
type t = { pos : Pos.Option.t; descr : descr }

(** Constraint type *)
type constr_t = ..

type constr_t += Num | Ord

type constr = {
  t : constr_t;
  constr_descr : string;
  satisfied : subtype:(t -> t -> unit) -> satisfies:(t -> unit) -> t -> unit;
}

let string_of_constraint c =
  match c.t with Num -> "num" | Ord -> "ord" | _ -> "?"

module Constraints = Set.Make (struct
  type t = constr

  let compare { t } { t = t' } = Stdlib.compare t t'
end)

(** A type constructor applied to arguments (e.g. source). *)
type constructed = { constructor : string; params : (variance * t) list }

(** Contents of a variable. *)
type var = {
  name : int;
  mutable level : int;
  mutable constraints : Constraints.t;
}

type invar =
  | Free of var  (** the variable is free *)
  | Link of variance * t  (** the variable has bee substituted *)

(** A type scheme (i.e. a type with universally quantified variables). *)
type scheme = var list * t

(** A method. *)
type meth = {
  meth : string;  (** name of the method *)
  optional : bool;  (** is the method optional? *)
  scheme : scheme;  (** type scheme *)
  doc : string;  (** documentation *)
  json_name : string option;  (** name when represented as JSON *)
}

type repr_t = { t : t; json_repr : [ `Tuple | `Object ] }

(** Sets of type descriptions. *)
module DS = Set.Make (struct
  type t = string * Constraints.t

  let compare (s, v) (s', v') =
    match Stdlib.compare s s' with 0 -> Constraints.compare v v' | x -> x
end)

let string_of_constr c = c.constr_descr

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

module R = struct
  type meth = {
    name : string;
    optional : bool;
    scheme : var list * t;
    json_name : string option;
  }

  and t =
    [ `Constr of string * (variance * t) list
    | `List of t * [ `Object | `Tuple ]
    | `Tuple of t list
    | `Nullable of t
    | `Meth of meth * t (* label, type scheme, JSON name, base type *)
    | `Arrow of (bool * string * t) list * t
    | `Getter of t
    | `EVar of var (* existential variable *)
    | `UVar of var (* universal variable *)
    | `Ellipsis (* omitted sub-term *)
    | `Range_Ellipsis (* omitted sub-terms (in a list, e.g. list of args) *)
    | `Debug of
      string * t * string
      (* add annotations before / after, mostly used for debugging *) ]

  and var = string * Constraints.t
end

type custom = ..

type custom_handler = {
  typ : custom;
  copy_with : (t -> t) -> custom -> custom;
  occur_check : (t -> unit) -> custom -> unit;
  filter_vars : (var list -> t -> var list) -> var list -> custom -> var list;
  repr : (var list -> t -> R.t) -> var list -> custom -> R.t;
  subtype : (t -> t -> unit) -> custom -> custom -> unit;
  sup : (t -> t -> t) -> custom -> custom -> custom;
  to_string : custom -> string;
  to_json : custom -> Json.t;
}

type descr +=
  | Custom of custom_handler
  | Constr of constructed
  | Getter of t  (** a getter: something that is either a t or () -> t *)
  | List of repr_t
  | Tuple of t list
  | Nullable of t  (** something that is either t or null *)
  | Meth of meth * t  (** t with a method added *)
  | Arrow of (bool * string * t) list * t  (** a function *)
  | Var of invar ref  (** a type variable *)

(** Create a type from its value. *)
let make ?pos d = { pos; descr = d }

(** The unit type. *)
let unit = Tuple []

(** Dereferencing gives you the meaning of a term, going through links created
    by instantiations. One should (almost) never work on a non-dereferenced
    type. *)
let rec deref t =
  match t.descr with Var { contents = Link (_, t) } -> deref t | _ -> t

(** Remove methods. This function also removes links. *)
let rec demeth t =
  let t = deref t in
  match t.descr with Meth (_, t) -> demeth t | _ -> t

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
let meth ?pos ?json_name ?(optional = false) meth scheme ?(doc = "") t =
  make ?pos (Meth ({ meth; optional; scheme; doc; json_name }, t))

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
    match t.descr with
      | Meth (m, t) ->
          let meth, t = aux (m.meth :: hide) t in
          let meth = if List.mem m.meth hide then meth else m :: meth in
          (meth, t)
      | _ -> ([], t)
  in
  aux [] t

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

let find_type_opt = Hashtbl.find_opt custom_types

let json_of_variance (v : variance) =
  let v =
    match v with `Invariant -> "invariant" | `Covariant -> "covariant"
  in
  `String v

let json_of_constraint c =
  `Assoc
    [
      ("constraint", `String (string_of_constraint c));
      ("description", `String c.constr_descr);
    ]

let rec to_json (a : t) : Json.t =
  match a.descr with
    | Custom c -> c.to_json c.typ
    | List a ->
        `Assoc
          [
            ("kind", `String "list");
            ("of", to_json a.t);
            ( "json_repr",
              match a.json_repr with
                | `Tuple -> `String "tuple"
                | `Object -> `String "object" );
          ]
    | Tuple l ->
        `Assoc [("kind", `String "tuple"); ("of", `Tuple (List.map to_json l))]
    | Getter a -> `Assoc [("kind", `String "getter"); ("of", to_json a)]
    | Nullable a -> `Assoc [("kind", `String "nullable"); ("of", to_json a)]
    | Meth (m, a) ->
        `Assoc
          [
            ("kind", `String "method");
            ("name", `String m.meth);
            ("optional", `Bool m.optional);
            ("scheme", json_of_scheme m.scheme);
            ("doc", `String m.doc);
            ( "json_name",
              Option.fold ~none:`Null ~some:(fun s -> `String s) m.json_name );
            ("of", to_json a);
          ]
    | Var { contents = Free x } ->
        `Assoc
          [
            ("kind", `String "var");
            ("name", `Int x.name);
            ("level", `Int x.level);
            ( "constraints",
              x.constraints |> Constraints.to_seq |> Seq.map json_of_constraint
              |> List.of_seq
              |> fun l -> `Tuple l );
          ]
    | Var { contents = Link (_, a) } -> to_json a
    | Arrow (l, a) ->
        let l =
          List.map (fun (o, l, a) -> `Tuple [`Bool o; `String l; to_json a]) l
        in
        `Assoc
          [
            ("kind", `String "arrow"); ("arguments", `Tuple l); ("to", to_json a);
          ]
    | Constr c ->
        let params =
          List.map
            (fun (v, a) -> `Tuple [json_of_variance v; to_json a])
            c.params
        in
        `Assoc
          [
            ("kind", `String "constructed");
            ("constructor", `String c.constructor);
            ("params", `Tuple params);
          ]
    | _ -> assert false

and json_of_var x = to_json (make (Var (ref (Free x))))

and json_of_scheme (g, a) =
  let g = List.map json_of_var g in
  `Tuple [`Tuple g; to_json a]

(* TODO: use something more efficient that association lists *)

(** Variables already serialized. *)
let json_variables = ref []

module Json = struct
  include Json

  let get_list = get_tuple

  let get_pair j =
    match get_tuple j with [x; y] -> (x, y) | _ -> raise Not_found

  let get_triple j =
    match get_tuple j with [x; y; z] -> (x, y, z) | _ -> raise Not_found
end

let rec of_json (j : Json.t) : t =
  match j with
    | `Assoc l -> (
        match List.assoc "kind" l |> Json.get_string with
          | "custom" ->
              List.assoc "name" l |> Json.get_string |> find_type_opt
              |> Option.get
              |> fun f -> f ()
          | "getter" -> List.assoc "of" l |> of_json |> fun a -> make (Getter a)
          | "list" ->
              let a = List.assoc "of" l |> of_json in
              let a =
                {
                  t = a;
                  json_repr =
                    (match List.assoc "json_repr" l with
                      | `String "tuple" -> `Tuple
                      | `String "object" -> `Object
                      | _ -> assert false);
                }
              in
              make (List a)
          | "tuple" ->
              List.assoc "of" l |> Json.get_tuple |> List.map of_json
              |> fun l -> make (Tuple l)
          | "arrow" ->
              let args =
                List.assoc "arguments" l |> Json.get_tuple
                |> List.map Json.get_triple
                |> List.map (function o, l, a ->
                       (Json.get_bool o, Json.get_string l, of_json a))
              in
              let a = List.assoc "to" l |> of_json in
              make (Arrow (args, a))
          | "constructed" ->
              let constructor = List.assoc "constructor" l |> Json.get_string in
              let variance_of_string = function
                | "covariant" -> `Covariant
                | "invariant" -> `Invariant
                | _ -> assert false
              in
              let params =
                List.assoc "params" l |> Json.get_tuple
                |> List.map Json.get_tuple
                |> List.map (function
                     | [v; a] ->
                         (variance_of_string (Json.get_string v), of_json a)
                     | _ -> assert false)
              in
              make (Constr { constructor; params })
          | "var" ->
              let name = List.assoc "name" l |> Json.get_int in
              let level = List.assoc "level" l |> Json.get_int in
              let constraint_of_json j =
                match Json.get_string j with
                  | "num" -> Num
                  | "ord" -> Ord
                  | _ -> assert false
              in
              let constraints =
                List.assoc "constraints" l |> Json.get_list
                |> List.map constraint_of_json
              in
              (* TODO: handle constraints. The difficulty is that they are
                 declared after for now... *)
              assert (constraints = []);
              let constraints = Constraints.empty in
              let var = { name; level; constraints } in
              if not (List.mem_assoc name !json_variables) then (
                let a = make (Var (ref (Free var))) in
                json_variables := (name, a) :: !json_variables);
              List.assoc name !json_variables
          | "method" ->
              let name = List.assoc "name" l |> Json.get_string in
              let optional = List.assoc "optional" l |> Json.get_bool in
              let scheme =
                List.assoc "scheme" l |> Json.get_pair |> function
                | g, a ->
                    ( List.map
                        (fun x ->
                          match (of_json x).descr with
                            | Var { contents = Free x } -> x
                            | _ -> assert false)
                        (Json.get_list g),
                      of_json a )
              in
              let doc = List.assoc "doc" l |> Json.get_string in
              let json_name =
                List.assoc "json_name" l |> Json.get_null
                |> Option.map Json.get_string
              in
              let a = List.assoc "of" l |> of_json in
              make (Meth ({ meth = name; optional; scheme; doc; json_name }, a))
          | _ -> failwith "TODO")
    | _ -> assert false

exception NotImplemented
exception Exists of Pos.Option.t * string
exception Unsatisfied_constraint

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

(** Create a fresh variable. *)
let var =
  let name =
    let c = ref (-1) in
    fun () ->
      incr c;
      !c
  in
  let f ?(constraints = []) ?(level = max_int) ?pos () =
    let constraints = Constraints.of_list constraints in
    let name = name () in
    make ?pos (Var (ref (Free { name; level; constraints })))
  in
  f

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

let rec mk_invariant t =
  match t with
    | { descr = Var ({ contents = Link (_, t) } as c) } ->
        c := Link (`Invariant, t);
        mk_invariant t
    | _ -> ()
