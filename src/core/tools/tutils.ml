(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

let conf_scheduler =
  Dtools.Conf.void
    ~p:(Configure.conf#plug "scheduler")
    "Internal scheduler"
    ~comments:
      [
        "The scheduler is used to process various tasks in liquidsoap.";
        "There are three kinds of tasks:";
        "\"Non-blocking\" ones are instantaneous to process, these are only";
        "internal processes of liquidsoap like its server.";
        "\"Fast\" tasks are those that can be long but are often not,";
        "such as request resolution (audio file downloading and checking).";
        "Finally, \"slow\" tasks are those that are always taking a long time,";
        "like last.fm submission, or user-defined tasks register via";
        "`thread.run`.";
        "The scheduler consists in a number of queues that process incoming";
        "tasks. Some queues might only process some kinds of tasks so that";
        "they are more responsive.";
        "Having more queues often do not make the program faster in average,";
        "but affect mostly the order in which tasks are processed.";
      ]

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

let generic_queues =
  Dtools.Conf.int
    ~p:(conf_scheduler#plug "generic_queues")
    ~d:5 "Generic queues"
    ~comments:
      [
        "Number of event queues accepting any kind of task.";
        "There should at least be one. Having more can be useful to make sure";
        "that trivial request resolutions (local files) are not delayed";
        "because of a stalled download. But N stalled download can block";
        "N queues anyway.";
      ]

let fast_queues =
  Dtools.Conf.int
    ~p:(conf_scheduler#plug "fast_queues")
    ~d:0 "Fast queues"
    ~comments:
      [
        "Number of queues that are dedicated to fast tasks.";
        "It might be useful to create some if your request resolutions,";
        "or some user defined tasks (cf `thread.run`), are";
        "delayed too much because of slow tasks blocking the generic queues,";
        "such as last.fm submissions or slow `thread.run` handlers.";
      ]

let non_blocking_queues =
  Dtools.Conf.int
    ~p:(conf_scheduler#plug "non_blocking_queues")
    ~d:2 "Non-blocking queues"
    ~comments:
      [
        "Number of queues dedicated to internal non-blocking tasks.";
        "These are only started if such tasks are needed.";
        "There should be at least one.";
      ]

let scheduler_log =
  Dtools.Conf.bool
    ~p:(conf_scheduler#plug "log")
    ~d:false "Log scheduler messages"

let seems_locked =
  if Sys.win32 then fun _ -> true
  else fun m ->
    if Mutex.try_lock m then (
      Mutex.unlock m;
      false)
    else true

let log = Log.make ["threads"]

(** Manage a set of threads and make sure they terminate correctly,
  * i.e. not by raising an exception. *)

let lock = Mutex.create ()

module Set = Set.Make (struct
  type t = string * Condition.t

  let compare = compare
end)

let all = ref Set.empty
let queues = ref Set.empty

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
  let set_done () = ignore (Unix.write write_done (Bytes.create 1) 0 1) in
  let wait_done () =
    let rec wait_for_done () =
      try Utils.select [read_done] [] [] (-1.)
      with Unix.Unix_error (Unix.EINTR, _, _) -> wait_for_done ()
    in
    let r, _, _ = wait_for_done () in
    assert (r = [read_done])
  in
  (set_done, wait_done)

exception Exit

let create ~queue f x s =
  let c = Condition.create () in
  let set = if queue then queues else all in
  Mutex_utils.mutexify lock
    (fun () ->
      let id =
        let process x =
          Utils.Thread.set_current_thread_name s;
          try
            f x;
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
                | e when queue ->
                    Dtools.Init.exec Dtools.Log.stop;
                    Printf.printf "Queue %s crashed with exception %s\n%s" s
                      (Printexc.to_string e) bt;
                    Printf.printf
                      "PANIC: Liquidsoap has crashed, exiting.,\n\
                       Please report at: https://github.com/savonet/liquidsoap";
                    Printf.printf "Queue %s crashed with exception %s\n%s" s
                      (Printexc.to_string e) bt;
                    flush_all ();
                    _exit 1
                | e ->
                    log#important "Thread %S aborts with exception %s!" s
                      (Printexc.to_string e);
                    Printexc.raise_with_backtrace e raw_bt
            with e ->
              let l = Re.Pcre.split ~rex:(Re.Pcre.regexp "\n") bt in
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

type priority =
  [ `Blocking  (** For example a last.fm submission. *)
  | `Maybe_blocking  (** Request resolutions vary a lot. *)
  | `Non_blocking  (** Non-blocking tasks like the server. *) ]

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

let scheduler : priority Duppy.scheduler =
  Duppy.create
    ~on_error:(fun exn raw_bt ->
      let bt = Printexc.raw_backtrace_to_string raw_bt in
      if not (error_handler ~bt exn) then
        Printexc.raise_with_backtrace exn raw_bt)
    ()

let () =
  Lifecycle.on_scheduler_shutdown ~name:"scheduler shutdown" (fun () ->
      log#important "Shutting down scheduler...";
      Duppy.stop scheduler;
      log#important "Scheduler shut down.")

let scheduler_log n =
  if scheduler_log#get then (
    let log = Log.make [n] in
    fun m -> log#info "%s" m)
  else fun _ -> ()

let new_queue ?priorities ~name () =
  let qlog = scheduler_log name in
  let queue () =
    match priorities with
      | None -> Duppy.queue scheduler ~log:qlog name
      | Some priorities -> Duppy.queue scheduler ~log:qlog ~priorities name
  in
  ignore (create ~queue:true queue () name)

let create f x name = create ~queue:false f x name
let join_all () = join_all ~set:all ()

let start () =
  if Atomic.compare_and_set state `Idle `Starting then (
    for i = 1 to generic_queues#get do
      let name = Printf.sprintf "generic queue #%d" i in
      new_queue ~name ()
    done;
    for i = 1 to fast_queues#get do
      let name = Printf.sprintf "fast queue #%d" i in
      new_queue ~name ~priorities:(fun x -> x = `Maybe_blocking) ()
    done;
    for i = 1 to non_blocking_queues#get do
      let name = Printf.sprintf "non-blocking queue #%d" i in
      new_queue ~priorities:(fun x -> x = `Non_blocking) ~name ()
    done)

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
 * [timeout] seconds on the given [socket]. Raises [Timeout elapsed_time]
 * if timeout is reached. *)
let wait_for =
  let end_r, end_w = Unix.pipe ~cloexec:true () in
  Lifecycle.before_core_shutdown ~name:"wait_for shutdown" (fun () ->
      try ignore (Unix.write end_w (Bytes.create 1) 0 1) with _ -> ());
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
      let r, w, _ =
        try Utils.select (end_r :: r) w [] t
        with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
      in
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
    match Unix.write fd b ofs len with
      | 0 -> raise End_of_file
      | n when n = len -> ()
      | n -> f (ofs + n) (len - n)
  in
  let len = Bytes.length b in
  if len > 0 then f 0 len
