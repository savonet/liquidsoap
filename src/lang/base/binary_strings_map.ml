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

(** Tracks strings marked as binary by their content digest. An entry is kept
    alive as long as at least one registered string with matching content is
    live, then collected automatically. *)

type entry = string

module Table = Weak.Make (struct
  type t = entry

  let equal = String.equal
  let hash = Hashtbl.hash
end)

let table = Table.create 16
let lock = Atomic.make false

let rec atomic_wait () =
  if not (Atomic.compare_and_set lock false true) then (
    Domain.cpu_relax ();
    atomic_wait ())

let atomic_lock f =
  atomic_wait ();
  match f () with
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        Atomic.set lock false;
        Printexc.raise_with_backtrace exn bt
    | v ->
        Atomic.set lock false;
        v

exception Found of entry

let find value =
  try
    List.fold_left
      (fun cur entry ->
        match (cur, entry) with
          | None, _ -> Some entry
          | Some _, v' when v' == value -> raise (Found entry)
          | Some _, _ -> cur)
      None
      (Table.find_all table value)
  with Found entry -> Some entry

(* Strong references for entries anchored to non-heap strings (e.g. literals),
   which cannot have finalizers attached. Such strings are permanent anyway. *)
let permanent = ref []

let register_closure ~entry value =
  try Gc.finalise_last (fun () -> ignore (Sys.opaque_identity entry)) value
  with Invalid_argument _ -> permanent := entry :: !permanent

let add value =
  let entry = Table.merge table value in
  if entry != value then register_closure ~entry value

let register value =
  atomic_lock (fun () ->
      match find value with
        | Some entry when entry == value -> ()
        | _ -> add value)

let remove value =
  atomic_lock (fun () ->
      let n = List.length (Table.find_all table value) in
      for _ = 1 to n do
        Table.remove table value
      done)

let is_binary value =
  atomic_lock (fun () ->
      match find value with
        | Some entry ->
            if entry != value then register_closure ~entry value;
            true
        | None -> false)
