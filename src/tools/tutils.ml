(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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
    ~p:(Utils.conf#plug "scheduler")
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

let generic_queues =
  Dtools.Conf.int
    ~p:(conf_scheduler#plug "generic_queues")
    ~d:2 "Generic queues"
    ~comments:
      [
        "Number of event queues accepting any kind of task.";
        "There should at least be one. Having more can be useful to avoid that";
        "trivial request resolutions (local files) are not delayed because of";
        "a stalled download. But N stalled download can block N queues anyway.";
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

let mutexify lock f x =
  Mutex.lock lock;
  try
    let ans = f x in
    Mutex.unlock lock;
    ans
  with e ->
    Mutex.unlock lock;
    raise e

let finalize ~k f =
  try
    let x = f () in
    k ();
    x
  with e ->
    k ();
    raise e

let seems_locked =
  if Sys.win32 then fun _ -> true
  else fun m ->
    if Mutex.try_lock m then (
      Mutex.unlock m;
      false )
    else true

let log = Log.make ["threads"]

(** Manage a set of threads and make sure they terminate correctly,
  * i.e. not by raising an exception. *)

let lock = Mutex.create ()
let uncaught = ref None

module Set = Set.Make (struct
  type t = string * Condition.t

  let compare = compare
end)

let all = ref Set.empty
let queues = ref Set.empty

let join_all ~set () =
  let rec f () =
    try
      mutexify lock
        (fun () ->
          let name, c = Set.choose !set in
          log#info "Waiting for thread %s to shutdown" name;
          Condition.wait c lock)
        ();
      f ()
    with Not_found -> ()
  in
  f ()

let no_problem = Condition.create ()

exception Exit

let create ~queue f x s =
  let c = Condition.create () in
  let set = if queue then queues else all in
  mutexify lock
    (fun () ->
      let id =
        Thread.create
          (fun x ->
            try
              f x;
              mutexify lock
                (fun () ->
                  set := Set.remove (s, c) !set;
                  log#info "Thread %S terminated (%d remaining)." s
                    (Set.cardinal !set);
                  Condition.signal c)
                ()
            with e ->
              let backtrace = Printexc.get_backtrace () in
              begin
                match e with
                | Exit -> log#info "Thread %S exited." s
                | Failure e -> log#important "Thread %S failed: %s!" s e
                | e ->
                    log#important "Thread %S aborts with exception %s!" s
                      (Printexc.to_string e)
              end;
              if e <> Exit then (
                let l = Pcre.split ~pat:"\n" backtrace in
                List.iter (log#info "%s") l );
              mutexify lock
                (fun () ->
                  set := Set.remove (s, c) !set;
                  uncaught := Some e;
                  Condition.signal no_problem;
                  Condition.signal c)
                ();
              if e <> Exit then raise e)
          x
      in
      set := Set.add (s, c) !set;
      log#info "Created thread %S (%d total)." s (Set.cardinal !set);
      id)
    ()

type priority =
  | Blocking  (** For example a last.fm submission. *)
  | Maybe_blocking  (** Request resolutions vary a lot. *)
  | Non_blocking  (** Non-blocking tasks like the server. *)

let scheduler = Duppy.create ()
let started = ref false
let started_m = Mutex.create ()
let has_started = mutexify started_m (fun () -> !started)

let scheduler_shutdown_atom =
  Dtools.Init.at_stop ~name:"Scheduler shutdown" (fun () ->
      log#important "Shutting down scheduler...";
      Duppy.stop scheduler;
      log#important "Scheduler shut down.";
      log#important "Waiting for queue threads to terminate...";
      join_all ~set:queues ();
      log#important "Queues shut down")

let scheduler_log n =
  if scheduler_log#get then (
    let log = Log.make [n] in
    fun m -> log#info "%s" m )
  else fun _ -> ()

let new_queue ?priorities ~name () =
  let qlog = scheduler_log name in
  let queue () =
    try
      match priorities with
        | None -> Duppy.queue scheduler ~log:qlog name
        | Some priorities -> Duppy.queue scheduler ~log:qlog ~priorities name
    with e ->
      log#severe "Queue %s crashed with exception %s\n%s" name
        (Printexc.to_string e)
        (Printexc.get_backtrace ());
      log#critical
        "PANIC: Liquidsoap has crashed, exiting.,\n\
         Please report at: savonet-users@lists.sf.net";
      exit 1
  in
  ignore (create ~queue:true queue () name)

let create f x name = create ~queue:false f x name
let join_all () = join_all ~set:all ()

let () =
  (* A dtool atom to start
   * tasks *)
  ignore
    (Dtools.Init.make ~name:"init-queues-start" ~after:[Dtools.Init.start]
       (fun () ->
         for i = 1 to generic_queues#get do
           let name = Printf.sprintf "generic queue #%d" i in
           new_queue ~name ()
         done;
         for i = 1 to fast_queues#get do
           let name = Printf.sprintf "fast queue #%d" i in
           new_queue ~name ~priorities:(fun x -> x = Maybe_blocking) ()
         done;
         for i = 1 to non_blocking_queues#get do
           let name = Printf.sprintf "non-blocking queue #%d" i in
           new_queue ~priorities:(fun x -> x = Non_blocking) ~name ()
         done;
         mutexify started_m (fun () -> started := true) ()))

(** Replace stdout/err by a pipe, and install a Duppy task that pulls data
  * out of that pipe and logs it.
  * Never use that when logging to stdout: it would just loop! *)
let start_forwarding () =
  let reopen fd =
    let i, o = Unix.pipe () in
    Unix.dup2 o fd;
    Unix.close o;
    Unix.set_close_on_exec i;
    i
  in
  let in_stdout = reopen Unix.stdout in
  let in_stderr = reopen Unix.stderr in
  (* Without the eta-expansion, the timestamp is computed once for all. *)
  let log_stdout =
    let log = Log.make ["stdout"] in
    fun s -> log#important "%s" s
  in
  let log_stderr =
    let log = Log.make ["stderr"] in
    fun s -> log#important "%s" s
  in
  let forward fd log =
    let task ~priority f =
      { Duppy.Task.priority; events = [`Read fd]; handler = f }
    in
    let len = Utils.pagesize in
    let buffer = Bytes.create len in
    let rec f (acc : string list) _ =
      let n = Unix.read fd buffer 0 len in
      let buffer = Bytes.unsafe_to_string buffer in
      let rec split acc i =
        match
          try Some (String.index_from buffer i '\n') with Not_found -> None
        with
          | Some j when j < n ->
              let line =
                List.fold_left
                  (fun s l -> l ^ s)
                  (String.sub buffer i (j - i))
                  acc
              in
              (* This _could_ be blocking! *)
              log line;
              split [] (j + 1)
          | _ -> String.sub buffer i (n - i) :: acc
      in
      [task ~priority:Non_blocking (f (split acc 0))]
    in
    Duppy.Task.add scheduler (task ~priority:Maybe_blocking (f []))
  in
  forward in_stdout log_stdout;
  forward in_stderr log_stderr

let () =
  ignore
    (Dtools.Init.at_start (fun () ->
         if Dtools.Init.conf_daemon#get then (
           Dtools.Log.conf_stdout#set false;
           start_forwarding () )))

(** Waits for [f()] to become true on condition [c]. *)
let wait c m f =
  mutexify m
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

(* Wait for [`Read socker], [`Write socket] or [`Both socket] for at most
 * [timeout] seconds on the given [socket]. Raises [Timeout elapsed_time]
 * if timeout is reached. *)
let wait_for ?(log = fun _ -> ()) event timeout =
  let start_time = Unix.gettimeofday () in
  let max_time = start_time +. timeout in
  let r, w =
    match event with
      | `Read socket -> ([socket], [])
      | `Write socket -> ([], [socket])
      | `Both socket -> ([socket], [socket])
  in
  let rec wait t =
    let l, l', _ = Unix.select r w [] t in
    if l = [] && l' = [] then (
      let current_time = Unix.gettimeofday () in
      if current_time >= max_time then (
        log "Timeout reached!";
        raise (Timeout (current_time -. start_time)) )
      else wait (min 1. (max_time -. current_time)) )
  in
  wait (min 1. timeout)

(** Wait for some thread to crash *)
let run = ref true

let main () = wait no_problem lock (fun () -> not (!run && !uncaught = None))

let shutdown () =
  run := false;
  Condition.signal no_problem

(** Thread-safe lazy cell. *)
let lazy_cell f =
  let lock = Mutex.create () in
  let c = ref None in
  mutexify lock (fun () ->
      match !c with
        | Some v -> v
        | None ->
            let v = f () in
            c := Some v;
            v)

(* Thread with preemptive kill/wait mechanism, see mli for details. *)
let stoppable_thread f name =
  let cond = Condition.create () in
  let lock = Mutex.create () in
  let should_stop = ref false in
  let has_stopped = ref false in
  let kill = mutexify lock (fun () -> should_stop := true) in
  let wait () = wait cond lock (fun () -> !has_stopped) in
  let should_stop = mutexify lock (fun () -> !should_stop) in
  let has_stopped =
    mutexify lock (fun () ->
        has_stopped := true;
        Condition.signal cond)
  in
  let _ = create f (should_stop, has_stopped) name in
  (kill, wait)
