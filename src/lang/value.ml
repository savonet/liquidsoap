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

(** Values are untyped normal forms of terms. *)

open Term_hash
module Custom = Term.Custom
module Methods = Runtime_term.Methods

(** We derive a hash of the environment to invalidate the cache when the builtin
    env change. We mostly keep name and methods. *)
type env = (string * t) list

and dynamic_methods = {
  hidden_methods : string list;
  methods : string -> t option; [@hash.ignore]
}

and t =
  | Int of {
      pos : Pos.Option.t; [@hash.ignore]
      value : int;
      methods : t Methods.t;
      mutable flags : Flags.flags; [@hash.ignore]
    }
  | Float of {
      pos : Pos.Option.t; [@hash.ignore]
      value : float;
      methods : t Methods.t;
    }
  | String of {
      pos : Pos.Option.t; [@hash.ignore]
      value : string;
      methods : t Methods.t;
    }
  | Bool of {
      pos : Pos.Option.t; [@hash.ignore]
      value : bool;
      methods : t Methods.t;
    }
  | Null of { pos : Pos.Option.t; [@hash.ignore] methods : t Methods.t }
  | Custom of {
      pos : Pos.Option.t; [@hash.ignore]
      value : Custom.t; [@hash.ignore]
      methods : t Methods.t;
      dynamic_methods : dynamic_methods option;
      mutable flags : Flags.flags; [@hash.ignore]
    }
  | List of {
      pos : Pos.Option.t; [@hash.ignore]
      value : t list;
      methods : t Methods.t;
      mutable flags : Flags.flags; [@hash.ignore]
    }
  | Tuple of {
      pos : Pos.Option.t; [@hash.ignore]
      value : t list;
      methods : t Methods.t;
      mutable flags : Flags.flags; [@hash.ignore]
    }
  | (* Function with given list of argument name, argument variable and default
       value, the (relevant part of the) closure, and the body. *)
    Fun of {
      id : int;
      pos : Pos.Option.t; [@hash.ignore]
      fun_args : (string * string * t option) list;
      fun_env : env; [@hash.ignore]
      fun_body : Term.t; [@hash.ignore]
      methods : t Methods.t;
      mutable flags : Flags.flags; [@hash.ignore]
    }
  | (* For a foreign function only the arguments are visible, the closure
       doesn't capture anything in the environment. *)
    FFI of {
      id : int;
      pos : Pos.Option.t; [@hash.ignore]
      ffi_args : (string * string * t option) list;
      mutable ffi_fn : env -> t; [@hash.ignore]
      methods : t Methods.t;
      mutable flags : Flags.flags; [@hash.ignore]
    }
[@@deriving hash]

type fun_v = {
  fun_args : (string * string * t option) list;
  fun_env : env;
  fun_body : Term.t;
}

type ffi = { ffi_args : (string * string * t option) list; ffi_fn : env -> t }

type in_value =
  [ `Int of int
  | `Float of float
  | `String of string
  | `Bool of bool
  | `Null
  | `Custom of Custom.t
  | `List of t list
  | `Tuple of t list
  | `Fun of fun_v
  | `FFI of ffi ]

let methods = function
  | Int { methods }
  | Float { methods }
  | String { methods }
  | Bool { methods }
  | Custom { methods }
  | Null { methods }
  | Tuple { methods }
  | List { methods }
  | Fun { methods }
  | FFI { methods } ->
      methods
[@@inline always]

let map_methods v fn =
  match v with
    | Int ({ methods } as p) -> Int { p with methods = fn methods }
    | Float ({ methods } as p) -> Float { p with methods = fn methods }
    | String ({ methods } as p) -> String { p with methods = fn methods }
    | Bool ({ methods } as p) -> Bool { p with methods = fn methods }
    | Custom ({ methods } as p) -> Custom { p with methods = fn methods }
    | Null ({ methods } as p) -> Null { p with methods = fn methods }
    | Tuple ({ methods } as p) -> Tuple { p with methods = fn methods }
    | List ({ methods } as p) -> List { p with methods = fn methods }
    | Fun ({ methods } as p) -> Fun { p with methods = fn methods }
    | FFI ({ methods } as p) -> FFI { p with methods = fn methods }
[@@inline always]

let pos = function
  | Int { pos }
  | Float { pos }
  | String { pos }
  | Bool { pos }
  | Custom { pos }
  | Null { pos }
  | Tuple { pos }
  | List { pos }
  | Fun { pos }
  | FFI { pos } ->
      pos
[@@inline always]

let set_pos v pos =
  match v with
    | Int p -> Int { p with pos }
    | Float p -> Float { p with pos }
    | String p -> String { p with pos }
    | Bool p -> Bool { p with pos }
    | Custom p -> Custom { p with pos }
    | Null p -> Null { p with pos }
    | Tuple p -> Tuple { p with pos }
    | List p -> List { p with pos }
    | Fun p -> Fun { p with pos }
    | FFI p -> FFI { p with pos }
[@@inline always]

let has_flag v flag =
  match v with
    | Float _ | String _ | Bool _ | Null _ -> false
    | Int { flags }
    | Custom { flags }
    | Tuple { flags }
    | List { flags }
    | Fun { flags }
    | FFI { flags } ->
        Flags.has flags flag
[@@inline always]

let add_flag v flag =
  match v with
    | Float _ | String _ | Bool _ | Null _ -> assert false
    | Int p -> p.flags <- Flags.add p.flags flag
    | Custom p -> p.flags <- Flags.add p.flags flag
    | Tuple p -> p.flags <- Flags.add p.flags flag
    | List p -> p.flags <- Flags.add p.flags flag
    | Fun p -> p.flags <- Flags.add p.flags flag
    | FFI p -> p.flags <- Flags.add p.flags flag
[@@inline always]

let unit = `Tuple []
let is_unit = function Tuple { value = [] } -> true | _ -> false
let fun_id = Atomic.make 0

let make ?pos ?(methods = Methods.empty) ?(flags = Flags.empty) : in_value -> t
    = function
  | `Int i -> Int { pos; methods; flags; value = i }
  | `Float f -> Float { pos; methods; value = f }
  | `String s -> String { pos; methods; value = s }
  | `Bool b -> Bool { pos; methods; value = b }
  | `Custom c ->
      Custom { pos; methods; flags; dynamic_methods = None; value = c }
  | `Null -> Null { pos; methods }
  | `Tuple l -> Tuple { pos; methods; flags; value = l }
  | `List l -> List { pos; methods; flags; value = l }
  | `Fun { fun_args; fun_env; fun_body } ->
      Fun
        {
          id = Atomic.fetch_and_add fun_id 1;
          pos;
          methods;
          flags;
          fun_args;
          fun_env;
          fun_body;
        }
  | `FFI { ffi_args; ffi_fn } ->
      FFI
        {
          id = Atomic.fetch_and_add fun_id 1;
          pos;
          methods;
          flags;
          ffi_args;
          ffi_fn;
        }

let string_of_int_value ~flags i =
  if Flags.has flags Flags.octal_int then Printf.sprintf "0o%o" i
  else if Flags.has flags Flags.hex_int then Printf.sprintf "0x%x" i
  else string_of_int i

let rec to_string v =
  let base_string v =
    match v with
      | Int { value = i; flags } -> string_of_int_value ~flags i
      | Float { value = f } -> Utils.string_of_float f
      | Bool { value = b } -> string_of_bool b
      | String { value = s } -> Lang_string.quote_string s
      | Custom { value = c } -> Custom.to_string c
      | List { value = l } ->
          "[" ^ String.concat ", " (List.map to_string l) ^ "]"
      | Tuple { value = l } ->
          "(" ^ String.concat ", " (List.map to_string l) ^ ")"
      | Null _ -> "null"
      | Fun { fun_args = []; fun_body = x } when Term.is_ground x ->
          "{" ^ Term.to_string x ^ "}"
      | Fun { fun_args = l; fun_body = x } when Term.is_ground x ->
          let f (label, _, value) =
            match (label, value) with
              | "", None -> "_"
              | "", Some v -> Printf.sprintf "_=%s" (to_string v)
              | label, Some v -> Printf.sprintf "~%s=%s" label (to_string v)
              | label, None -> Printf.sprintf "~%s=_" label
          in
          let args = List.map f l in
          Printf.sprintf "fun (%s) -> %s" (String.concat "," args)
            (Term.to_string x)
      | Fun _ | FFI _ -> "<fun>"
  in
  let s = base_string v in
  let methods = methods v in
  if Methods.is_empty methods then s
  else (
    let methods = Methods.bindings methods in
    (if is_unit v then "" else s ^ ".")
    ^ "{"
    ^ String.concat ", "
        (List.map (fun (l, meth_term) -> l ^ "=" ^ to_string meth_term) methods)
    ^ "}")

(** Find a method in a value. *)
let invoke x l =
  try
    match (Methods.find_opt l (methods x), x) with
      | Some v, _ -> v
      | None, Custom { dynamic_methods = Some { hidden_methods; methods } }
        when not (List.mem l hidden_methods) ->
          Option.get (methods l)
      | _ -> raise Not_found
  with _ -> failwith ("Could not find method " ^ l ^ " of " ^ to_string x)

(** Perform a sequence of invokes: invokes x [l1;l2;l3;...] is x.l1.l2.l3... *)
let rec invokes x = function l :: ll -> invokes (invoke x l) ll | [] -> x

let demeth e =
  map_methods
    (match e with
      | Custom p ->
          Custom { p with methods = Methods.empty; dynamic_methods = None }
      | _ -> e)
    (fun _ -> Methods.empty)

let remeth t u =
  match t with
    | Custom { dynamic_methods = Some _ } ->
        Runtime_error.raise
          ~pos:(match pos u with None -> [] | Some p -> [p])
          ~message:
            "Spread and method replacements are not supported for values with \
             dynamic methods. Most likely, you are trying to define new source \
             tracks as: `{...source.tracks(s), video=..}`. Please use: \
             `source.tracks(s).{ video=.. }` instead."
          "invalid"
    | _ ->
        let t_methods = methods t in
        map_methods u (fun u_methods ->
            Methods.fold Methods.add t_methods u_methods)

let split_meths e = (Methods.bindings (methods e), demeth e)

let compare a b =
  let rec aux = function
    | Int { value = i }, Int { value = i' } -> Stdlib.compare i i'
    | Float { value = f }, Float { value = f' } -> Stdlib.compare f f'
    | Bool { value = b }, Bool { value = b' } -> Stdlib.compare b b'
    | String { value = s }, String { value = s' } -> Stdlib.compare s s'
    | Custom { value = a }, Custom { value = b } -> Custom.compare a b
    | Tuple { value = l }, Tuple { value = m } ->
        List.fold_left2
          (fun cmp a b -> if cmp <> 0 then cmp else compare a b)
          0 l m
    | List { value = l1 }, List { value = l2 } ->
        let rec cmp = function
          | [], [] -> 0
          | [], _ -> -1
          | _, [] -> 1
          | h1 :: l1, h2 :: l2 ->
              let c = compare h1 h2 in
              if c = 0 then cmp (l1, l2) else c
        in
        cmp (l1, l2)
    | Fun { id = i }, Fun { id = i' }
    | FFI { id = i }, FFI { id = i' }
    | Fun { id = i }, FFI { id = i' }
    | FFI { id = i }, Fun { id = i' } ->
        Stdlib.compare i i'
    | Null _, Null _ -> 0
    | Null _, _ -> -1
    | _, Null _ -> 1
    | v, v' ->
        failwith
          (Printf.sprintf "Cannot compare %s and %s" (to_string v)
             (to_string v'))
  and compare a b =
    (* For records, we compare the list ["label", field; ..] of common fields. *)
    if is_unit a && is_unit b then (
      let r a =
        let m, _ = split_meths a in
        m
      in
      let a = r a in
      let b = r b in
      (* Keep only common fields: with subtyping it might happen that some fields are ignored. *)
      let a =
        List.filter (fun (l, _) -> List.exists (fun (l', _) -> l = l') b) a
      in
      let b =
        List.filter (fun (l, _) -> List.exists (fun (l', _) -> l = l') a) b
      in
      let a = List.sort (fun x x' -> Stdlib.compare (fst x) (fst x')) a in
      let b = List.sort (fun x x' -> Stdlib.compare (fst x) (fst x')) b in
      let a =
        make
          (`Tuple
             (List.map
                (fun (lbl, v) -> make (`Tuple [make (`String lbl); v]))
                a))
      in
      let b =
        make
          (`Tuple
             (List.map
                (fun (lbl, v) -> make (`Tuple [make (`String lbl); v]))
                b))
      in
      aux (a, b))
    else aux (a, b)
  in
  compare a b

(* Custom values. *)

module type Custom = sig
  include Term.Custom

  val to_value : ?pos:Pos.t -> content -> t
  val of_value : t -> content
  val is_value : t -> bool
end

module type CustomDef = Term.CustomDef

module MkCustomFromTerm (Term : Term.Custom) = struct
  include Term

  let to_value ?pos c = make ?pos (`Custom (to_custom c))

  let of_value v =
    match v with Custom { value = c } -> of_custom c | _ -> assert false

  let is_value v =
    match v with Custom { value = c } -> is_custom c | _ -> false
end

module MkCustom (Def : CustomDef) = struct
  module Term = Term.MkCustom (Def)
  include MkCustomFromTerm (Term)
end

module RuntimeType = MkCustom (struct
  type content = Type.t

  let name = "type"
  let to_string _ = "type"

  let to_json ~pos _ =
    Runtime_error.raise ~pos ~message:"Types cannot be represented as json"
      "json"

  let compare = Stdlib.compare
end)
