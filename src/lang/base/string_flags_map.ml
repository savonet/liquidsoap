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

(** Tracks binary flags for string values by content digest, so that flags
    survive string re-creation (e.g. cover art metadata passed through charset
    conversion or other processing paths). *)

type entry = { digest : string; mutable flags : Flags.flags }

module Map = Weak.Make (struct
  type t = entry

  let equal e e' = e.digest = e'.digest
  let hash e = Hashtbl.hash e.digest
end)

let table = Map.create 16
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

let digest s = Digest.(to_hex (string s))

let register s flag =
  let d = digest s in
  atomic_lock (fun () ->
      let entry = Map.merge table { digest = d; flags = Flags.empty } in
      entry.flags <- Flags.add entry.flags flag;
      Gc.finalise_last (fun () -> ignore (Sys.opaque_identity entry)) s)

let merge_flags s flags =
  let d = digest s in
  atomic_lock (fun () ->
      match Map.find_opt table { digest = d; flags = Flags.empty } with
        | Some entry ->
            (* Each call adds a finalizer closure to s. We expect few repeated
               calls on the same string, if any, so this is acceptable. *)
            Gc.finalise_last (fun () -> ignore (Sys.opaque_identity entry)) s;
            Flags.merge flags entry.flags
        | None -> flags)
