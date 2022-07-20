(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

exception Conflict of string * string

let string_of_kind = function
  | `Any -> "any"
  | `Internal -> "internal"
  | `Kind k -> Content.string_of_kind k
  | `Format f -> Content.string_of_format f

(* v <: v' means that v can be used whenever v'
   is expected. *)
let ( <: ) v v' =
  match (Unifier.deref v, Unifier.deref v') with
    (* Any *)
    | `Any, `Any -> ()
    | `Any, _ -> Unifier.(v <-- v')
    (* Internal *)
    | `Internal, `Any | `Internal, `Internal -> ()
    | `Internal, `Kind k when Content.is_internal_kind k -> Unifier.(v <-- v')
    | `Internal, `Format f when Content.is_internal_format f ->
        Unifier.(v <-- v')
    (* Kind *)
    | `Kind _, `Any -> ()
    | `Kind k, `Internal when Content.is_internal_kind k -> ()
    | `Kind k, `Kind k' when k = k' -> ()
    | `Kind k, `Format f when Content.kind f = k -> Unifier.(v <-- v')
    (* Format *)
    | `Format _, `Any -> ()
    | `Format f, `Internal when Content.is_internal_format f -> ()
    | `Format f, `Kind k when Content.kind f = k -> ()
    | `Format f, `Format f' when Content.compatible f f' -> Content.merge f f'
    | _ ->
        raise
          (Conflict
             ( string_of_kind (Unifier.deref v),
               string_of_kind (Unifier.deref v') ))

type content = Frame.kind Unifier.t
type spec = { sealed : bool; fields : content Frame.Fields.t }
type t = spec Unifier.t

let make ?(fields = Frame.Fields.empty) ~sealed () =
  Unifier.make { sealed; fields }

let make_content = Unifier.make
let sealed t = (Unifier.deref t).sealed
let fields t = (Unifier.deref t).fields
let content = Unifier.deref

let to_string t =
  let t = Unifier.deref t in
  String.concat ", "
    (Frame.Fields.fold
       (fun k v cur ->
         cur
         @ [
             Printf.sprintf "%s=%s" (Frame.string_of_field k)
               (string_of_kind (Unifier.deref v));
           ])
       t.fields
       (if t.sealed then [] else ["..."]))

let unify_content v v' = v <: v'

(* k <: k' means that k can be used whenever k'
   is expected. *)
let ( <: ) k k' =
  let dk = Unifier.deref k in
  let dk' = Unifier.deref k' in
  let sealed = dk.sealed || dk'.sealed in
  let f =
    Frame.Fields.Set.of_list (List.map fst (Frame.Fields.bindings dk.fields))
  in
  let f' =
    Frame.Fields.Set.of_list (List.map fst (Frame.Fields.bindings dk'.fields))
  in
  (match (sealed, Frame.Fields.Set.subset f' f) with
    | _, true | false, false -> ()
    | _, false -> raise (Conflict (to_string k, to_string k')));
  let fields =
    Frame.Fields.Set.fold
      (fun field fields ->
        match
          ( Frame.Fields.find_opt field dk.fields,
            Frame.Fields.find_opt field dk'.fields )
        with
          | None, None -> assert false
          | Some v, None | None, Some v -> Frame.Fields.add field v fields
          | Some v, Some v' -> (
              try
                v <: v';
                Frame.Fields.add field v fields
              with Conflict _ -> raise (Conflict (to_string k, to_string k'))))
      f' Frame.Fields.empty
  in
  let nk = Unifier.make { sealed; fields } in
  Unifier.(k <-- nk)

let copy_content c = make_content (content c)

let copy t =
  let fields = Frame.Fields.map copy_content (fields t) in
  make ~fields ~sealed:(sealed t) ()

let sup t t' =
  let t = Unifier.deref t in
  let t' = Unifier.deref t' in
  let sealed = t.sealed || t'.sealed in
  let f =
    Frame.Fields.Set.of_list (List.map fst (Frame.Fields.bindings t.fields))
  in
  let f' =
    Frame.Fields.Set.of_list (List.map fst (Frame.Fields.bindings t'.fields))
  in
  let fields =
    if sealed then Frame.Fields.Set.inter f f' else Frame.Fields.Set.union f f'
  in
  let fields =
    Frame.Fields.Set.fold
      (fun field fields ->
        match
          ( Frame.Fields.find_opt field t.fields,
            Frame.Fields.find_opt field t'.fields )
        with
          | None, None -> assert false
          | Some v, None | None, Some v ->
              Frame.Fields.add field (copy_content v) fields
          | Some v, Some v' -> (
              let v = copy_content v in
              let v' = copy_content v' in
              try
                unify_content v v';
                unify_content v' v';
                Frame.Fields.add field v fields
              with Conflict _ -> fields))
      fields Frame.Fields.empty
  in
  make ~sealed ~fields ()
