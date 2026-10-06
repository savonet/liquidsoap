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

(* A failing handler does not keep later ones from running, a removed one does
   not run, and handlers run in registration order. *)
let () =
  let calls = ref [] in
  let record name _ = calls := name :: !calls in
  let (_ : unit -> unit) =
    Signal_handlers.add Sys.sigusr2 (fun _ -> failwith "handler failed")
  in
  let (_ : unit -> unit) = Signal_handlers.add Sys.sigusr2 (record "first") in
  let remove = Signal_handlers.add Sys.sigusr2 (record "removed") in
  let (_ : unit -> unit) = Signal_handlers.add Sys.sigusr2 (record "last") in
  remove ();
  Unix.kill (Unix.getpid ()) Sys.sigusr2;
  let deadline = Unix.gettimeofday () +. 5. in
  while List.length !calls < 2 && Unix.gettimeofday () < deadline do
    Unix.sleepf 0.01
  done;
  assert (List.rev !calls = ["first"; "last"]);
  print_endline "Signal handlers: ok"
