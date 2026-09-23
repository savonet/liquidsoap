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

(* A node retains whatever it links to, and [find_root] only compresses paths
   that are actually walked. Ranks let [<--] attach the shallower tree under
   the deeper one, so a node never ends up pointing at one created after it. *)
type 'a t = [ `Value of 'a | `Link of 'a link ]
and 'a link = { atom : 'a t Atomic.t; mutable rank : int }

let make v = `Link { atom = Atomic.make (`Value v); rank = 0 }

(* Only writers take this lock. Readers never store, so a writer is the only
   one that can re-point a link and no interleaving can close a cycle. *)
let writers = Mutex_utils.mk_state ()

(* Return the terminal atom of a chain (the one holding `Value), compressing
   the path behind it so later walks are O(1). Caller holds [writers]. *)
let find_root l =
  let rec find l =
    match Atomic.get l.atom with `Value _ -> l | `Link l' -> find l'
  in
  let root = find l in
  let rec compress l =
    match Atomic.get l.atom with
      | `Value _ -> ()
      | `Link l' ->
          if l' != root then Atomic.set l.atom (`Link root);
          compress l'
  in
  compress l;
  root

(* Lock-free: a concurrent [<--] may link a root onward mid-walk, which only
   lengthens the walk. Ranks bound its length to log2 of the tree size. *)
let deref x =
  let rec walk l =
    match Atomic.get l.atom with `Value v -> v | `Link l' -> walk l'
  in
  match x with `Value v -> v | `Link l -> walk l

let set x v =
  match x with
    | `Value _ -> assert false
    | `Link l ->
        Mutex_utils.mutable_lock ~state:writers
          (fun () -> Atomic.set (find_root l).atom (`Value v))
          ()

let ( <-- ) x x' =
  match (x, x') with
    | `Value _, _ | _, `Value _ -> assert false
    | `Link l, `Link l' ->
        Mutex_utils.mutable_lock ~state:writers
          (fun () ->
            let r = find_root l in
            let r' = find_root l' in
            if r != r' then
              if r.rank <= r'.rank then (
                Atomic.set r.atom (`Link r');
                if r.rank = r'.rank then r'.rank <- r'.rank + 1)
              else (
                (* [r'] holds the value that must survive, so move it into [r]
                   before pointing [r'] there. *)
                let surviving = Atomic.get r'.atom in
                Atomic.set r.atom surviving;
                Atomic.set r'.atom (`Link r)))
          ()
