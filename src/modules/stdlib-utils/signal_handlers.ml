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

module Int_map = Map.Make (Int)

type entry = { id : int; handler : int -> unit }

let handlers : entry list Int_map.t Atomic.t = Atomic.make Int_map.empty
let next_id = Atomic.make 0

(* Runs in signal context: one atomic read, no lock. A failing handler must not
   keep the others from running, nor raise in the interrupted code. *)
let dispatch signal =
  List.iter
    (fun { handler } -> try handler signal with _ -> ())
    (Option.value ~default:[] (Int_map.find_opt signal (Atomic.get handlers)))

let rec update fn =
  let current = Atomic.get handlers in
  if not (Atomic.compare_and_set handlers current (fn current)) then update fn

let add signal handler =
  let id = Atomic.fetch_and_add next_id 1 in
  update (fun handlers ->
      let entries =
        Option.value ~default:[] (Int_map.find_opt signal handlers)
      in
      Int_map.add signal (entries @ [{ id; handler }]) handlers);
  Sys.set_signal signal (Sys.Signal_handle dispatch);
  fun () ->
    update (fun handlers ->
        match Int_map.find_opt signal handlers with
          | None -> handlers
          | Some entries ->
              Int_map.add signal
                (List.filter (fun entry -> entry.id <> id) entries)
                handlers)
