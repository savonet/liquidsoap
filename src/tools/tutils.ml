(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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
  Conf.void ~p:(Configure.conf#plug "scheduler") "Internal scheduler"
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
  Conf.int ~p:(conf_scheduler#plug "generic_queues") ~d:1
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
  Conf.int ~p:(conf_scheduler#plug "non_blocking_queues") ~d:1
     "Non-blocking queues"
     ~comments:[
       "Number of queues dedicated to internal non-blocking tasks." ;
       "These are only started if such tasks are needed." ;
       "There should be at least one. Having more is probably useless."
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

let create f x s =
  mutexify lock (
    fun () ->
      let id =
        Thread.create
          (fun x ->
             try
               f x ;
               Mutex.lock lock ;
               all := Set.remove (s,(Thread.self ())) !all ;
               log#f 3 "thread %S exited (%d remaining)" s (Set.cardinal !all) ;
               Mutex.unlock lock
             with e ->
               Mutex.lock lock ;
               begin match e with
                 | Failure e ->
                     log#f 1 "thread %S failed: %s" s e
                 | e ->
                     log#f 1 "thread %S aborts with exception %s !"
                              s (Printexc.to_string e)
               end ;
               all := Set.remove (s,(Thread.self ())) !all ;
               uncaught := Some e ;
               Condition.signal no_problem ;
               Mutex.unlock lock ;
               raise e)
          x
      in
        all := Set.add (s,id) !all ;
        log#f 3 "Created thread %S (%d total)" s (Set.cardinal !all) ;
        id
  ) ()

type priority =
  | Blocking       (** For example a last.fm submission. *)
  | Maybe_blocking (** Request resolutions vary a lot. *)
  | Non_blocking   (** Non-blocking tasks like the server. *)

let scheduler = Duppy.create ()

let scheduler_log n =
  if scheduler_log#get then
    let log = Log.make [n] in
    fun m -> log#f 4 "%s" m
  else
    fun _ -> ()

let new_queue ?priorities ~name () =
   let log = scheduler_log name in
   let queue () =
     match priorities with
       | None -> Duppy.queue scheduler ~log name
       | Some priorities ->
           Duppy.queue scheduler ~log ~priorities name
   in
   ignore (create queue () name)

(** Create a default queue at startup, that will accept any task. *)
let () =
  ignore (Dtools.Init.at_start (fun () ->
    for i = 1 to generic_queues#get do
      let name = Printf.sprintf "generic queue #%d" i in
        new_queue ~name ()
    done ;
    for i = 1 to fast_queues#get do
      let name = Printf.sprintf "fast queue #%d" i in
        new_queue ~name ~priorities:(fun x -> x = Maybe_blocking) ()
    done))

let need_non_blocking_queue =
  let has_one = ref false in
    fun () ->
      if not !has_one then begin
        for i = 1 to non_blocking_queues#get do
          let name = Printf.sprintf "non-blocking queue #%d" i in
            new_queue
              ~priorities:(fun x -> x = Non_blocking)
              ~name ()
        done ;
        has_one := true
      end

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
  let log_stdout s = (Log.make ["stdout"])#f 3 "%s" s in
  let log_stderr s = (Log.make ["stderr"])#f 3 "%s" s in
  let forward fd log =
    let task f =
      { Duppy.Task.
         priority = Non_blocking ;
         events   = [`Read fd] ;
         handler  = f }
    in
    let rec f (acc:string list) _ =
      let len = 10 in
      let buffer = String.create len in
      let n = Unix.read fd buffer 0 len in
      let rec split acc i =
        match
          try Some (String.index_from buffer i '\n') with Not_found -> None
        with
          | Some j when j < n ->
              let line =
                List.fold_left (fun s l -> l^s) (String.sub buffer i (j-i)) acc
              in
                log line ;
                split [] (j+1)
          | _ ->
              String.sub buffer i (n-i) :: acc
      in
        [ task (f (split acc 0)) ]
    in
      Duppy.Task.add scheduler (task (f []))
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

(** Wait for some thread to crash *)
let run = ref true
let main () =
  if !run then
  (* The mutex required by [Condition.wait] is pretty useless in that case. *)
  let m = Mutex.create () in
    Mutex.lock m ;
    Condition.wait no_problem m ;
    Mutex.unlock m
let shutdown () = run := false; Condition.signal no_problem

(** Waits for [f()] to become true on condition [c].
  * The mutex [m] protecting data accessed by [f] is in the same state before
  * and after the call. *)
let wait c m f =
  let l = Mutex.try_lock m in
    while not (f ()) do Condition.wait c m done ;
    if l then Mutex.unlock m
