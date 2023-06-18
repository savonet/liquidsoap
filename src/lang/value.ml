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

(** Values are untyped normal forms of terms. *)

(** Ground values. *)
module Ground = Term.Ground

module Methods = Term.Methods

type t = { pos : Pos.Option.t; value : in_value; methods : t Methods.t }
and env = (string * t) list

(* Some values have to be lazy in the environment because of recursive functions. *)
and lazy_env = (string * t Lazy.t) list

and in_value =
  | Ground of Ground.t
  | List of t list
  | Tuple of t list
  | Null
  (* Function with given list of argument name, argument variable and default
     value, the (relevant part of the) closure, and the body. *)
  | Fun of (string * string * t option) list * lazy_env * Term.t
  (* For a foreign function only the arguments are visible, the closure
     doesn't capture anything in the environment. *)
  | FFI of (string * string * t option) list * (env -> t)

let unit : in_value = Tuple []

let string_of_float f =
  let s = string_of_float f in
  if s.[String.length s - 1] = '.' then s ^ "0" else s

let rec to_string v =
  let base_string v =
    match v.value with
      | Ground g -> Ground.to_string g
      | List l -> "[" ^ String.concat ", " (List.map to_string l) ^ "]"
      | Tuple l -> "(" ^ String.concat ", " (List.map to_string l) ^ ")"
      | Null -> "null"
      | Fun ([], _, x) when Term.is_ground x -> "{" ^ Term.to_string x ^ "}"
      | Fun (l, _, x) when Term.is_ground x ->
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
  if Methods.is_empty v.methods then s
  else (
    let methods = Methods.bindings v.methods in
    (if v.value = Tuple [] then "" else s ^ ".")
    ^ "{"
    ^ String.concat ", "
        (List.map (fun (l, meth_term) -> l ^ "=" ^ to_string meth_term) methods)
    ^ "}")

(** Find a method in a value. *)
let invoke x l =
  try Methods.find l x.methods
  with Not_found ->
    failwith ("Could not find method " ^ l ^ " of " ^ to_string x)

(** Perform a sequence of invokes: invokes x [l1;l2;l3;...] is x.l1.l2.l3... *)
let rec invokes x = function l :: ll -> invokes (invoke x l) ll | [] -> x

let demeth e = { e with methods = Methods.empty }

let remeth t u =
  { u with methods = Methods.fold Methods.add t.methods u.methods }

let split_meths e = (Methods.bindings e.methods, demeth e)

let compare a b =
  let rec aux = function
    | Ground a, Ground b -> Ground.compare a b
    | Tuple l, Tuple m ->
        List.fold_left2
          (fun cmp a b -> if cmp <> 0 then cmp else compare a b)
          0 l m
    | List l1, List l2 ->
        let rec cmp = function
          | [], [] -> 0
          | [], _ -> -1
          | _, [] -> 1
          | h1 :: l1, h2 :: l2 ->
              let c = compare h1 h2 in
              if c = 0 then cmp (l1, l2) else c
        in
        cmp (l1, l2)
    | Null, Null -> 0
    | Null, _ -> -1
    | _, Null -> 1
    | _ -> assert false
  and compare a b =
    (* For records, we compare the list ["label", field; ..] of common fields. *)
    if a.value = Tuple [] && b.value = Tuple [] then (
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
        Tuple
          (List.map
             (fun (lbl, v) ->
               {
                 pos = None;
                 value =
                   Tuple
                     [
                       {
                         pos = None;
                         value = Ground (Ground.String lbl);
                         methods = Methods.empty;
                       };
                       v;
                     ];
                 methods = Methods.empty;
               })
             a)
      in
      let b =
        Tuple
          (List.map
             (fun (lbl, v) ->
               {
                 pos = None;
                 value =
                   Tuple
                     [
                       {
                         pos = None;
                         value = Ground (Ground.String lbl);
                         methods = Methods.empty;
                       };
                       v;
                     ];
                 methods = Methods.empty;
               })
             b)
      in
      aux (a, b))
    else aux (a.value, b.value)
  in
  compare a b

(* Abstract values. *)

module type Abstract = sig
  include Term.Abstract

  val to_value : ?pos:Pos.t -> content -> t
  val of_value : t -> content
  val is_value : t -> bool
end

module type AbstractDef = Term.AbstractDef

module MkAbstractFromTerm (Term : Term.Abstract) = struct
  include Term

  let to_value ?pos c =
    { pos; value = Ground (to_ground c); methods = Methods.empty }

  let of_value t =
    match t.value with
      | Ground g when is_ground g -> of_ground g
      | _ -> assert false

  let is_value t = match t.value with Ground g -> is_ground g | _ -> false
end

module MkAbstract (Def : AbstractDef) = struct
  module Term = Term.MkAbstract (Def)
  include MkAbstractFromTerm (Term)
end

module RuntimeType = MkAbstract (struct
  type content = Type.t

  let name = "type"
  let descr _ = "type"

  let to_json ~pos _ =
    Runtime_error.raise ~pos ~message:"Types cannot be represented as json"
      "json"

  let compare = Stdlib.compare
end)
