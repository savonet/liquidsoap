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

(* Immutable fast hash using hashtbl. *)

type ('a, 'b) t = ('a, 'b) Hashtbl.t

let is_empty h = Hashtbl.length h = 0
let bindings h = Hashtbl.fold (fun k v l -> (k, v) :: l) h []
let empty = Obj.magic (Hashtbl.create 0)
let fold = Hashtbl.fold
let find k h = Hashtbl.find h k
let find_opt k h = Hashtbl.find_opt h k
let mem k h = Hashtbl.mem h k

let mapi fn h =
  let h' = Hashtbl.create (Hashtbl.length h) in
  Hashtbl.iter (fun k v -> Hashtbl.replace h' k (fn k v)) h;
  h'

let map fn h =
  let h' = Hashtbl.create (Hashtbl.length h) in
  Hashtbl.iter (fun k v -> Hashtbl.replace h' k (fn v)) h;
  h'

let filter fn h =
  let h' = Hashtbl.create (Hashtbl.length h) in
  Hashtbl.iter (fun k v -> if fn k v then Hashtbl.replace h' k v) h;
  h'

let add k v h =
  let h = Hashtbl.copy h in
  Hashtbl.replace h k v;
  h

let iter = Hashtbl.iter

exception Terminate

let for_all fn h =
  try
    Hashtbl.iter (fun k v -> if not (fn k v) then raise Terminate) h;
    true
  with Terminate -> false

let exists fn h = not (for_all (fun k v -> not (fn k v)) h)
