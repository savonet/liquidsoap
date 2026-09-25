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

let log = Log.make ["signal"]

(* Callbacks queued by signal handlers, newest first. *)
let pending = Atomic.make []

let rec queue fn =
  let current = Atomic.get pending in
  if not (Atomic.compare_and_set pending current (fn :: current)) then queue fn

let run fn =
  try fn ()
  with exn ->
    let bt = Printexc.get_backtrace () in
    Utils.log_exception ~log ~bt
      (Printf.sprintf "Error in signal callback: %s" (Printexc.to_string exn))

(* Handlers only queue their callback and write to a never-closed pipe:
   running the callback, adding a task or waking a [Duppy.Async] takes a lock
   the interrupted code may hold. *)
let wake_task =
  Lazy.Mutexed.from_fun (fun () ->
      let read_fd, write_fd = Unix.pipe ~cloexec:true () in
      Unix.set_nonblock read_fd;
      Unix.set_nonblock write_fd;
      let buffer = Bytes.create 256 in
      let rec task () =
        {
          Duppy.Task.priority = `Threaded;
          events = [`Read read_fd];
          handler =
            (fun _ ->
              (try ignore (Unix.read read_fd buffer 0 (Bytes.length buffer))
               with
               | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
                 ());
              List.iter run (List.rev (Atomic.exchange pending []));
              [task ()]);
        }
      in
      Duppy.Task.add Tutils.scheduler (task ());
      let byte = Bytes.make 1 '\000' in
      fun () ->
        try ignore (Unix.single_write write_fd byte 0 1)
        with Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ())

let add signal fn =
  let wake = Lazy.Mutexed.force wake_task in
  let active = Atomic.make true in
  let remove =
    Signal_handlers.add signal (fun _ ->
        queue (fun () -> if Atomic.get active then fn ());
        wake ())
  in
  fun () ->
    Atomic.set active false;
    remove ()
