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

(** EINTR-safe wrappers around Thread functions. *)

let delay d =
  let until = Unix.gettimeofday () +. d in
  let rec loop () =
    let remaining = until -. Unix.gettimeofday () in
    if remaining > 0. then (
      try Thread.delay remaining
      with Unix.Unix_error (Unix.EINTR, _, _) -> loop ())
  in
  loop ()

let rec wait_signal signals =
  try ignore (Thread.wait_signal signals) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> ()
    | Sys_error s when s = "Thread.wait_signal: Interrupted system call" ->
        wait_signal signals
