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

type exit_status =
  [ `None | `Exit of int | `Error of Printexc.raw_backtrace * exn ]

type state = [ `Idle | `Starting | `Running | `Done of exit_status ]

let internal_error_code = 128
let state : state Atomic.t = Atomic.make `Idle
let running () = match Atomic.get state with `Running -> true | _ -> false
let finished () = match Atomic.get state with `Done _ -> true | _ -> false

let exit_code () =
  match Atomic.get state with
    | `Done (`Exit code) -> code
    | `Done (`Error _) -> 1
    | `Idle -> 0
    | _ -> internal_error_code

let _exit code =
  Dtools.Init.exec Dtools.Log.stop;
  exit code

let exit () =
  match Atomic.get state with
    | `Done (`Error (bt, err)) -> Printexc.raise_with_backtrace err bt
    | _ -> exit (exit_code ())

let seems_locked =
  if Sys.win32 then fun _ -> true
  else fun m ->
    if Mutex.try_lock m then (
      Mutex.unlock m;
      false)
    else true

let log = Log.make ["threads"]

(** Manage a set of threads and make sure they terminate correctly, i.e. not by
    raising an exception. *)

let lock = Mutex.create ()

module Set = Set.Make (struct
  type t = string * Condition.t

  let compare = compare
end)

let all = ref Set.empty

let join_all ~set () =
  let rec f () =
    try
      Mutex_utils.mutexify lock
        (fun () ->
          let name, c = Set.choose !set in
          log#info "Waiting for thread %s to shutdown" name;
          Condition.wait c lock)
        ();
      f ()
    with Not_found -> ()
  in
  f ()

let set_done, wait_done =
  let read_done, write_done = Unix.pipe ~cloexec:true () in
  let set_done () = ignore (Unix_utils.write write_done (Bytes.create 1) 0 1) in
  let wait_done () =
    let r, _, _ = Utils.select [read_done] [] [] (-1.) in
    assert (r = [read_done])
  in
  (set_done, wait_done)

exception Exit

let create f x s =
  let c = Condition.create () in
  let set = all in
  Mutex_utils.mutexify lock
    (fun () ->
      let id =
        let process x =
          Utils.Thread.set_current_thread_name s;
          try
            Script_callback.uncollected (fun () -> f x);
            Mutex_utils.mutexify lock
              (fun () ->
                set := Set.remove (s, c) !set;
                log#info "Thread %S terminated (%d remaining)." s
                  (Set.cardinal !set);
                Condition.signal c)
              ()
          with e -> (
            let raw_bt = Printexc.get_raw_backtrace () in
            let bt = Printexc.get_backtrace () in
            try
              match e with
                | Exit -> log#info "Thread %S exited." s
                | Failure e as exn ->
                    log#important "Thread %S failed: %s!" s e;
                    Printexc.raise_with_backtrace exn raw_bt
                | e ->
                    log#important "Thread %S aborts with exception %s!" s
                      (Printexc.to_string e);
                    Printexc.raise_with_backtrace e raw_bt
            with e ->
              let l = String.split_on_char '\n' bt in
              List.iter (log#info "%s") l;
              Mutex_utils.mutexify lock
                (fun () ->
                  set := Set.remove (s, c) !set;
                  if
                    Atomic.compare_and_set state `Running
                      (`Done (`Error (raw_bt, e)))
                  then set_done ();
                  Condition.signal c)
                ();
              Printexc.raise_with_backtrace e raw_bt)
        in
        Thread.create process x
      in
      set := Set.add (s, c) !set;
      log#info "Created thread %S (%d total)." s (Set.cardinal !set);
      id)
    ()

let error_handlers = Stack.create ()

exception Error_processed

let rec error_handler ~bt exn =
  try
    Stack.iter
      (fun handler -> if handler ~bt exn then raise Error_processed)
      error_handlers;
    false
  with
    | Error_processed -> true
    | exn ->
        let bt = Printexc.get_backtrace () in
        error_handler ~bt exn

let join_all () = join_all ~set:all ()
let start () = Atomic.compare_and_set state `Idle `Starting

(** Waits for [f()] to become true on condition [c]. *)
let wait c m f =
  Mutex_utils.mutexify m
    (fun () ->
      while not (f ()) do
        Condition.wait c m
      done)
    ()

exception Timeout of float

let error_translator = function
  | Timeout f ->
      Some (Printf.sprintf "Timed out after waiting for %.02f sec." f)
  | _ -> None

let () = Printexc.register_printer error_translator

type event =
  [ `Read of Unix.file_descr
  | `Write of Unix.file_descr
  | `Both of Unix.file_descr ]

(* Wait for [`Read socket], [`Write socket] or [`Both socket] for at most
   [timeout] seconds on the given [socket]. Raises [Timeout elapsed_time]
   if timeout is reached. *)
let wait_for =
  let end_r, end_w = Unix.pipe ~cloexec:true () in
  Lifecycle.before_core_shutdown ~name:"wait_for shutdown" (fun () ->
      try ignore (Unix_utils.write end_w (Bytes.create 1) 0 1) with _ -> ());
  fun ?(log = fun _ -> ()) event timeout ->
    let start_time = Unix.gettimeofday () in
    let max_time = start_time +. timeout in
    let r, w =
      match event with
        | `Read socket -> ([socket], [])
        | `Write socket -> ([], [socket])
        | `Both socket -> ([socket], [socket])
    in
    let rec wait t =
      let r, w, _ = Utils.select (end_r :: r) w [] t in
      if List.mem end_r r then raise Exit;
      if r = [] && w = [] then (
        let current_time = Unix.gettimeofday () in
        if current_time >= max_time then (
          log "Timeout reached!";
          raise (Timeout (current_time -. start_time)))
        else wait (min 1. (max_time -. current_time)))
    in
    wait (min 1. timeout)

let main () =
  if Atomic.compare_and_set state `Starting `Running then wait_done ();
  log#important "Main loop exited";
  match Atomic.get state with
    | `Done _ -> ()
    | _ ->
        log#critical "Internal state error!";
        _exit internal_error_code

let shutdown code =
  let new_state = `Done (`Exit code) in
  if Atomic.compare_and_set state `Idle new_state then _exit code
  else if Atomic.compare_and_set state `Starting new_state then (
    log#critical "Shutdown called while starting!";
    set_done ())
  else if Atomic.compare_and_set state `Running new_state then set_done ()
  else (
    log#critical
      "Shutdown called twice with different exit conditions! Last call takes \
       precedence.";
    Atomic.set state new_state)

let cleanup () =
  log#important "Waiting for main threads to terminate...";
  join_all ();
  log#important "Main threads terminated."

let write_all ?timeout fd b =
  let rec f ofs len =
    (match timeout with
      | None -> ()
      | Some timeout -> wait_for (`Write fd) timeout);
    match Unix_utils.write fd b ofs len with
      | 0 -> raise End_of_file
      | n when n = len -> ()
      | n -> f (ofs + n) (len - n)
  in
  let len = Bytes.length b in
  if len > 0 then f 0 len
