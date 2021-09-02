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

(** Values are untyped normal forms of terms. *)

(** Ground values. *)
module Ground = Term.Ground

type t = { pos : Type.pos option; value : in_value }

and env = (string * t) list

(* Some values have to be lazy in the environment because of recursive functions. *)
and lazy_env = (string * t Lazy.t) list

and in_value =
  | Ground of Ground.t
  | Source of Source.source
  | Encoder of Encoder.format
  | List of t list
  | Tuple of t list
  | Null
  (* TODO: It would be better to have a list of methods associated to each
     value than a constructor here. However, I am keeping as is for now because
     implementation is safer this way. *)
  | Meth of string * t * t
  | Ref of t ref
  (* The first environment contains the parameters already passed to the
     function. Next parameters will be inserted between that and the second
     env which is part of the closure. *)
  | Fun of (string * string * t option) list * env * lazy_env * Term.t
  (* A matcher. Some day, Fun might be merged with this. *)
  | Match of lazy_env * (Term.pattern * Term.t) list
  (* For a foreign function only the arguments are visible, the closure
     doesn't capture anything in the environment. *)
  | FFI of (string * string * t option) list * env * (env -> t)

let unit : in_value = Tuple []

let string_of_float f =
  let s = string_of_float f in
  if s.[String.length s - 1] = '.' then s ^ "0" else s

let rec print_value v =
  match v.value with
    | Ground g -> Ground.to_string g
    | Source _ -> "<source>"
    | Encoder e -> Encoder.string_of_format e
    | List l -> "[" ^ String.concat ", " (List.map print_value l) ^ "]"
    | Ref a -> Printf.sprintf "ref(%s)" (print_value !a)
    | Tuple l -> "(" ^ String.concat ", " (List.map print_value l) ^ ")"
    | Null -> "null"
    | Meth (l, v, e) when Lazy.force Term.debug ->
        print_value e ^ ".{" ^ l ^ "=" ^ print_value v ^ "}"
    | Meth _ ->
        let rec split e =
          match e.value with
            | Meth (l, v, e) ->
                let m, e = split e in
                ((l, v) :: m, e)
            | _ -> ([], e)
        in
        let m, e = split v in
        let m =
          List.rev m
          |> List.map (fun (l, v) -> l ^ " = " ^ print_value v)
          |> String.concat ", "
        in
        let e =
          match e.value with Tuple [] -> "" | _ -> print_value e ^ "."
        in
        e ^ "{" ^ m ^ "}"
    | Fun ([], _, _, x) when Term.is_ground x -> "{" ^ Term.print_term x ^ "}"
    | Fun (l, _, _, x) when Term.is_ground x ->
        let f (label, _, value) =
          match (label, value) with
            | "", None -> "_"
            | "", Some v -> Printf.sprintf "_=%s" (print_value v)
            | label, Some v -> Printf.sprintf "~%s=%s" label (print_value v)
            | label, None -> Printf.sprintf "~%s=_" label
        in
        let args = List.map f l in
        Printf.sprintf "fun (%s) -> %s" (String.concat "," args)
          (Term.print_term x)
    | Fun _ | FFI _ -> "<fun>"
    | Match _ -> "<match>"

(** Find a method in a value. *)
let rec invoke x l =
  match x.value with
    | Meth (l', y, _) when l' = l -> y
    | Meth (_, _, x) -> invoke x l
    | _ -> failwith ("Could not find method " ^ l ^ " of " ^ print_value x)

(** Perform a sequence of invokes: invokes x [l1;l2;l3;...] is x.l1.l2.l3... *)
let rec invokes x = function l :: ll -> invokes (invoke x l) ll | [] -> x

let split_meths e =
  let rec aux hide e =
    match e.value with
      | Meth (l, v, e) ->
          if List.mem l hide then aux hide e
          else (
            let m, e = aux (l :: hide) e in
            ((l, v) :: m, e))
      | _ -> ([], e)
  in
  aux [] e

let rec demeth v = match v.value with Meth (_, _, v) -> demeth v | _ -> v

let rec remeth t u =
  match t.value with
    | Meth (l, v, t) -> { t with value = Meth (l, v, remeth t u) }
    | _ -> u
