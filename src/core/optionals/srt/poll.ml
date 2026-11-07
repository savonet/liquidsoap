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

let log = Library.log

type state = {
  poll : Srt.Poll.t;
  handlers : (Srt.socket, Srt.socket -> unit) Hashtbl.t;
}

let m = Mutex.create ()
let registered = Condition.create ()
let state = ref None
let stopped = ref false
let loop_thread = ref None
let on_loop () = !loop_thread = Some (Thread.id (Thread.self ()))

let wait_ready { poll; handlers } =
  Mutex.protect m (fun () ->
      while Hashtbl.length handlers = 0 && not !stopped do
        Condition.wait registered m
      done);
  match
    Srt.Poll.wait poll ~timeout:(Library.ms_of_seconds Library.conf_timeout#get)
  with
    | read, write -> List.sort_uniq compare (read @ write)
    | exception Srt.Error (`Etimeout, _, _) -> []
    (* The next iteration waits under the lock while nothing is registered; a
       socket libsrt already dropped is retried after the poll timeout. *)
    | exception Srt.Error (`Epollempty, _, _) ->
        Thread.delay Library.conf_timeout#get;
        []
    | exception exn ->
        log#severe "Error while processing SRT sockets: %s"
          (Printexc.to_string exn);
        []

let handle { poll; handlers } socket =
  Mutex.protect m (fun () ->
      match Hashtbl.find_opt handlers socket with
        | Some handler -> (
            try handler socket
            with exn ->
              let bt = Printexc.get_backtrace () in
              Utils.log_exception ~log ~bt
                (Printf.sprintf "Error while processing SRT socket: %s"
                   (Printexc.to_string exn)))
        | None -> ( try Srt.Poll.remove_usock poll socket with _ -> ()))

let rec iterate state =
  if Mutex.protect m (fun () -> !stopped) then Srt.Poll.release state.poll
  else (
    List.iter (handle state) (wait_ready state);
    iterate state)

let loop state =
  loop_thread := Some (Thread.id (Thread.self ()));
  iterate state

let get () =
  match !state with
    | Some s -> s
    | None ->
        let s = { poll = Srt.Poll.create (); handlers = Hashtbl.create 4 } in
        state := Some s;
        Library.spawn (fun () -> loop s);
        s

let register socket handler =
  Srt.setsockflag socket Srt.rcvsyn false;
  Srt.setsockflag socket Srt.sndsyn false;
  Mutex.protect m (fun () ->
      let { poll; handlers } = get () in
      Hashtbl.replace handlers socket handler;
      Srt.Poll.add_usock ~flags:[`Read; `Error] poll socket;
      Condition.broadcast registered)

let remove socket =
  let remove () =
    match !state with
      | Some { poll; handlers } -> (
          Hashtbl.remove handlers socket;
          try Srt.Poll.remove_usock poll socket with _ -> ())
      | None -> ()
  in
  if on_loop () then remove () else Mutex.protect m remove

(* The loop releases its own set on its way out, and cleanup waits for its
   thread like any other. *)
let () =
  Lifecycle.before_final_cleanup ~name:"srt readiness loop" (fun () ->
      Mutex.protect m (fun () ->
          stopped := true;
          Condition.broadcast registered))
