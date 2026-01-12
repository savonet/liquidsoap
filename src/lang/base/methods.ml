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

(* Immutable fast hash *)

open Term_hash

type ('a, 'b) t = ('a * 'b) list [@@deriving hash]

let is_empty h = h = []
let bindings h = h
let empty = []
let cardinal = List.length
let fold fn h r = List.fold_left (fun r (k, v) -> fn k v r) r h
let find = List.assoc
let find_opt = List.assoc_opt
let mem = List.mem_assoc
let mapi fn = List.map (fun (k, v) -> (k, fn k v))
let map fn = List.map (fun (k, v) -> (k, fn v))
let filter fn = List.filter (fun (k, v) -> fn k v)
let remove k h = List.filter (fun (k', _) -> k <> k') h
let add k v h = (k, v) :: remove k h
let append l l' = List.fold_left (fun m (k, v) -> add k v m) l l'
let from_list l = append [] l
let iter fn = List.iter (fun (k, v) -> fn k v)
let for_all fn = List.for_all (fun (k, v) -> fn k v)
let exists fn = List.exists (fun (k, v) -> fn k v)
