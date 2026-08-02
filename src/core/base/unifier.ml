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

(* Simple unification module for variables with no unknown value *)

type 'a t = [ `Value of 'a | `Link of 'a t Atomic.t ]

let make v = `Link (Atomic.make (`Value v))

(* Return the terminal atom of a chain (the one holding `Value), compressing
   the path behind it: every link visited is re-pointed directly at the
   terminal atom, so subsequent walks are O(1) instead of O(chain length).
   Without this, chains only ever grow (each [<--] appends one), and code
   that derefs in a hot loop pays for the full history of unifications. *)
let find_root a =
  let rec find a =
    match Atomic.get a with `Value _ -> a | `Link a' -> find a'
  in
  let root = find a in
  let rec compress a =
    match Atomic.get a with
      | `Value _ -> ()
      | `Link a' ->
          if a' != root then Atomic.set a (`Link root);
          compress a'
  in
  compress a;
  root

let rec deref x =
  match x with
    | `Value v -> v
    | `Link a -> (
        match Atomic.get (find_root a) with
          | `Value v -> v
          (* A concurrent [<--] may have linked our root onward; retry. *)
          | `Link _ as l -> deref l)

let set x v =
  match x with
    | `Value _ -> assert false
    | `Link a -> Atomic.set (find_root a) (`Value v)

let ( <-- ) x x' =
  match (x, x') with
    | `Value _, _ | _, `Value _ -> assert false
    | `Link a, `Link a' ->
        let r = find_root a in
        let r' = find_root a' in
        if r != r' then Atomic.set r (`Link r')
