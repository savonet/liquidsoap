(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2013 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Dtools

let conf_scheduler =
  Conf.void ~p:(Utils.conf#plug "scheduler") "Internal scheduler"
    ~comments:[
      "The scheduler is used to process various tasks in liquidsoap." ;
      "There are three kinds of tasks:" ;
      "\"Non-blocking\" ones are instantaneous to process, these are only" ;
      "internal processes of liquidsoap like its server." ;
      "\"Fast\" tasks are those that can be long but are often not," ;
      "such as request resolution (audio file downloading and checking)." ;
      "Finally, \"slow\" tasks are those that are always taking a long time," ;
      "like last.fm submission, or user-defined tasks register via" ;
      "<code>add_timeout()</code>." ;
      "The scheduler consists in a number of queues that process incoming" ;
      "tasks. Some queues might only process some kinds of tasks so that" ;
      "they are more responsive." ;
      "Having more queues often do not make the program faster in average," ;
      "but affect mostly the order in which tasks are processed."
    ]

let generic_queues =
  Conf.int ~p:(conf_scheduler#plug "generic_queues") ~d:2
    "Generic queues"
    ~comments:[
      "Number of event queues accepting any kind of task." ;
      "There should at least be one. Having more can be useful to avoid that" ;
      "trivial request resolutions (local files) are not delayed because of" ;
      "a stalled download. But N stalled download can block N queues anyway."
    ]
let fast_queues =
  Conf.int ~p:(conf_scheduler#plug "fast_queues") ~d:0
     "Fast queues"
     ~comments:[
       "Number of queues that are dedicated to fast tasks." ;
       "It might be useful to create some if your request resolutions," ;
       "or some user defined tasks (cf. <code>add_timeout()</code>), are" ;
       "delayed too much because of slow tasks blocking the generic queues," ;
       "such as last.fm submissions or slow <code>add_timeout</code> handlers."
     ]
let non_blocking_queues =
  Conf.int ~p:(conf_scheduler#plug "non_blocking_queues") ~d:2
     "Non-blocking queues"
     ~comments:[
       "Number of queues dedicated to internal non-blocking tasks." ;
       "These are only started if such tasks are needed." ;
       "There should be at least one."
     ]

let scheduler_log =
  Conf.bool ~p:(conf_scheduler#plug "log") ~d:false
    "Log scheduler messages"

let mutexify lock f =
  fun x ->
    Mutex.lock lock ;
    try
      let ans = f x in Mutex.unlock lock ; ans
    with
      | e -> Mutex.unlock lock ; raise e

let finalize ~k f =
  try let x = f () in k () ; x with e -> k () ; raise e

let seems_locked =
  if Sys.os_type = "Win32" then (fun _ -> true) else
    fun m ->
      if Mutex.try_lock m then begin
        Mutex.unlock m ;
        false
      end else
        true

(** Manage a set of threads and make sure they terminate correctly,
  * i.e. not by raising an exception. *)

let lock = Mutex.create ()
let uncaught = ref None
module Set = Set.Make (struct
                         type t = (string*Thread.t)
                         let compare = compare
                       end)
let all = ref (Set.empty)

(** Check that thread [s] is still running. *)
let running s id = Set.mem (s,id) !all

let join_all () =
  try
    while true do
      let id =
        Mutex.lock lock ;
        snd (Set.choose !all)
      in
        Mutex.unlock lock ;
        Thread.join id
    done
  with
    | Not_found -> Mutex.unlock lock

let no_problem = Condition.create ()

let log = Log.make ["threads"]

exception Exit

let create ~wait f x s =
  mutexify lock (
    fun () ->
      let id =
        Thread.create
          (fun x ->
             try
               f x ;
               if wait then begin
                 Mutex.lock lock ;
                 all := Set.remove (s,(Thread.self ())) !all ;
                 log#f 3
                   "Thread %S terminated (%d remaining)."
                   s (Set.cardinal !all) ;
                 Mutex.unlock lock
               end else
                 log#f 3 "Thread %S terminated." s
             with e ->
               Mutex.lock lock ;
               let backtrace = Utils.get_backtrace () in
               begin match e with
                 | Exit ->
                     log#f 3 "Thread %S exited." s
                 | Failure e ->
                     log#f 1 "Thread %S failed: %s!" s e
                 | e ->
                     log#f 1 "Thread %S aborts with exception %s!"
                              s (Utils.error_message e)
               end ;
               if e <> Exit then
                begin
                 let l = Pcre.split ~pat:"\n" backtrace in
                 List.iter (log#f 3 "%s") l 
                end ;
               if wait then all := Set.remove (s,(Thread.self ())) !all ;
               uncaught := Some e ;
               Condition.signal no_problem ;
               Mutex.unlock lock ;
               if e <> Exit then raise e)
          x
      in
        if wait then begin
          all := Set.add (s,id) !all ;
          log#f 3 "Created thread %S (%d total)." s (Set.cardinal !all)
        end else
          log#f 3 "Created thread %S." s ;
        id
  ) ()

type priority =
  | Blocking       (** For example a last.fm submission. *)
  | Maybe_blocking (** Request resolutions vary a lot. *)
  | Non_blocking   (** Non-blocking tasks like the server. *)

let scheduler = Duppy.create ()

let () =
  let name = "Duppy scheduler shutdown" in
  let f () =
    log#f 3 "Shutting down scheduler...";
    (* TODO: Duppy.stop uses Thread.kill, which is not implemented... *)
    (* Duppy.stop scheduler; *)
    log#f 3 "Scheduler shut down."
  in
  Shutdown.duppy_atom := Some (Dtools.Init.at_stop ~name f)

let scheduler_log n =
  if scheduler_log#get then
    let log = Log.make [n] in
    fun m -> log#f 4 "%s" m
  else
    fun _ -> ()

let new_queue ?priorities ~name () =
   let qlog = scheduler_log name in
   let queue () =
     try
       match priorities with
         | None -> Duppy.queue scheduler ~log:qlog name
         | Some priorities ->
             Duppy.queue scheduler ~log:qlog ~priorities name
     with e ->
       log#f 2 "Queue %s crashed with exception %s" name (Utils.error_message e) ;
       log#f 2 "%s" (Utils.get_backtrace());
       log#f 1 "PANIC: Liquidsoap has crashed, exiting.." ;
       log#f 1 "Please report at: savonet-users@lists.sf.net" ;
       exit 1
   in
   ignore (create ~wait:false queue () name)

let create f x name = create ~wait:true f x name

let start_non_blocking = ref false
let need_non_blocking_queue () = start_non_blocking := true

let () =
  (* A dtool atom to start
   * tasks *)
   ignore(Init.make 
     ~name:"init-queues-start" ~after:[Init.start] (fun () ->
      for i = 1 to generic_queues#get do
        let name = Printf.sprintf "generic queue #%d" i in
          new_queue ~name ()
      done ;
      for i = 1 to fast_queues#get do
        let name = Printf.sprintf "fast queue #%d" i in
          new_queue ~name ~priorities:(fun x -> x = Maybe_blocking) ()
      done;
      if !start_non_blocking then
        for i = 1 to non_blocking_queues#get do
          let name = Printf.sprintf "non-blocking queue #%d" i in
          new_queue
            ~priorities:(fun x -> x = Non_blocking)
            ~name ()
        done))

(** Replace stdout/err by a pipe, and install a Duppy task that pulls data
  * out of that pipe and logs it.
  * Never use that when logging to stdout: it would just loop! *)
let start_forwarding () =
  let reopen fd =
    let i,o = Unix.pipe () in
      Unix.dup2 o fd ;
      Unix.close o ;
      i
  in
  let in_stdout = reopen Unix.stdout in
  let in_stderr = reopen Unix.stderr in
  (* Without the eta-expansion, the timestamp is computed once for all. *)
  let log_stdout =
    let log = Log.make ["stdout"] in
      fun s -> log#f 3 "%s" s
  in
  let log_stderr =
    let log = Log.make ["stderr"] in
      fun s -> log#f 3 "%s" s
  in
  let forward fd log =
    let task ~priority f =
      { Duppy.Task.
         priority = priority ;
         events   = [`Read fd] ;
         handler  = f }
    in
    let len = 1024 in
    let buffer = String.create len in
    let rec f (acc:string list) _ =
      let n = Unix.read fd buffer 0 len in
      let rec split acc i =
        match
          try Some (String.index_from buffer i '\n') with Not_found -> None
        with
          | Some j when j < n ->
              let line =
                List.fold_left (fun s l -> l^s) (String.sub buffer i (j-i)) acc
              in
                (* This _could_ be blocking! *)
                log line ;
                split [] (j+1)
          | _ ->
              String.sub buffer i (n-i) :: acc
      in
        [ task ~priority:Non_blocking (f (split acc 0)) ]
    in
      Duppy.Task.add scheduler 
        (task ~priority:Maybe_blocking (f []))
  in
    forward in_stdout log_stdout ;
    forward in_stderr log_stderr

let () =
  ignore (Dtools.Init.at_start (fun () ->
    if Dtools.Init.conf_daemon#get then begin
      Dtools.Log.conf_stdout#set false ;
      need_non_blocking_queue () ;
      start_forwarding ()
    end))

(** Waits for [f()] to become true on condition [c]. *)
let wait c m f =
  mutexify m (fun () ->
    while not (f ()) do Condition.wait c m done) ()

exception Timeout

let error_translator =
  function
    | Timeout ->
        Some "Timeout while waiting on socket"
    | _ ->
        None

let () = Utils.register_error_translator error_translator

(* Wait for [`Read], [`Write] or [`Both] for at most
 * [timeout] seconds on the given [socket]. Raises [Timeout]
 * if timeout is reached. *)
let wait_for ?(log=fun _ -> ()) event socket timeout =
  let max_time = Unix.gettimeofday () +. timeout in
  let r, w = 
    match event with
      | `Read -> [socket],[]
      | `Write -> [],[socket]
      | `Both -> [socket],[socket]
  in
  let rec wait t =
    let l,l',_ = Unix.select r w [] t in
    if l=[] && l'=[] then begin
      log (Printf.sprintf "No network activity for %.02f second(s)." t);
      let current_time = Unix.gettimeofday () in
      if current_time >= max_time then
       begin
        log "Network activity timeout!" ;
        raise Timeout 
       end
      else
        wait (min 1. (max_time -. current_time))
    end
  in wait (min 1. timeout)

(** Wait for some thread to crash *)
let run = ref true
let main () =
  wait no_problem lock (fun () -> not (!run && !uncaught=None))
let shutdown () = run := false; Condition.signal no_problem

(** Thread-safe lazy cell. *)
let lazy_cell f =
  let lock = Mutex.create () in
  let c = ref None in
    mutexify lock
      (fun () ->
         match !c with
           | Some v -> v
           | None ->
               let v = f () in
                 c := Some v ;
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
    mutexify lock (fun () -> has_stopped := true ; Condition.signal cond)
  in
  let _ = create f (should_stop,has_stopped) name in
    kill, wait
