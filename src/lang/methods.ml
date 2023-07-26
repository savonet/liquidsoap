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

(* Immutable fast hash *)

type ('a, 'b) t = ('a * 'b) array

let is_empty h = Array.length h = 0
let bindings = Array.to_list
let empty = [||]
let cardinal = Array.length
let fold fn h r = Array.fold_left (fun r (k, v) -> fn k v r) r h

exception Found of int

let find k h =
  try
    Array.iteri (fun pos (k', _) -> if k = k' then raise (Found pos)) h;
    raise Not_found
  with Found pos -> snd h.(pos)

let find_opt k h = try Some (find k h) with Not_found -> None

let mem k h =
  try
    ignore (find k h);
    true
  with Not_found -> false

let mapi fn = Array.map (fun (k, v) -> (k, fn k v))
let map fn = Array.map (fun (k, v) -> (k, fn v))

let filter fn h =
  Array.of_list
    (Array.fold_left (fun h (k, v) -> if fn k v then (k, v) :: h else h) [] h)

let add k v h =
  Array.of_list
    (Array.fold_left
       (fun h (k', v) -> if k = k' then h else (k', v) :: h)
       [(k, v)]
       h)

let iter fn = Array.iter (fun (k, v) -> fn k v)
let for_all fn = Array.for_all (fun (k, v) -> fn k v)
let exists fn = Array.exists (fun (k, v) -> fn k v)
