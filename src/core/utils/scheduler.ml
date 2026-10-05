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

let log = Log.make ["threads"]

let conf_scheduler =
  Dtools.Conf.void
    ~p:(Configure.conf#plug "scheduler")
    "Internal scheduler"
    ~comments:
      [
        "The scheduler is used to process various tasks in liquidsoap.";
        "It runs one domain per core and dispatches ready tasks onto";
        "whichever of them is free. A task is one of three kinds, named after";
        "what it does to the domain running it:";
        "\"Non-blocking\" tasks are instantaneous, such as the server's";
        "internal processes; they run in batches directly on a domain.";
        "\"Blocking\" tasks keep a domain busy until they finish, such as a";
        "clock tick or a listener writer; each one runs on a domain by itself.";
        "\"Threaded\" tasks may wait on a socket or a file, such as request";
        "resolution, last.fm submission, or user-defined tasks registered via";
        "`thread.run`; each one runs on a thread inside a domain, so that";
        "waiting leaves the domain free for other work.";
      ]

let blocking_tasks =
  Dtools.Conf.int
    ~p:(conf_scheduler#plug "blocking_tasks")
    ~d:64 "Threaded tasks"
    ~comments:
      [
        "Maximum number of threaded tasks running at once, spread evenly over";
        "the scheduler's domains. Defaults to 64, whatever the number of cores.";
        "Raising it helps when the tasks truly wait, on a socket or a";
        "slow mount. A task that uses a core instead of waiting on one, such as";
        "probing a file for its decoder, gains nothing from extra slots and";
        "takes cores the streaming threads need.";
      ]

let legacy =
  Dtools.Conf.bool
    ~p:(conf_scheduler#plug "legacy")
    ~d:false "Legacy scheduler"
    ~comments:
      [
        "Run tasks on threads rather than domains, one at a time as before";
        "2.5: no task runs in parallel with another or with the streaming";
        "loop. A fail-safe for a script that concurrent execution breaks,";
        "which will be removed in a later version. The threads are the queues";
        "configured by `generic_queues`, `fast_queues` and";
        "`non_blocking_queues`.";
      ]

let deprecated_queue name ~d descr comments =
  Dtools.Conf.int ~p:(conf_scheduler#plug name) ~d descr
    ~comments:
      (comments
      @ [
          "Deprecated: this only applies when `settings.scheduler.legacy` is";
          "set and goes away with it.";
        ])

let generic_queues =
  deprecated_queue "generic_queues" ~d:5 "Generic queues"
    ["Number of legacy queues accepting any kind of task."]

let fast_queues =
  deprecated_queue "fast_queues" ~d:0 "Fast queues"
    ["Number of legacy queues dedicated to fast tasks."]

let non_blocking_queues =
  deprecated_queue "non_blocking_queues" ~d:2 "Non-blocking queues"
    ["Number of legacy queues dedicated to internal non-blocking tasks."]

let scheduler_log =
  Dtools.Conf.bool
    ~p:(conf_scheduler#plug "log")
    ~d:false "Log scheduler messages"

type priority =
  [ `Clock  (** A clock resuming to produce its next frames. *)
  | `Blocking
    (** Keeps its domain busy until done and never parks, such as a listener
        writer. *)
  | `Threaded
    (** May wait on a socket or a file, such as a request resolution or a
        last.fm submission. *)
  | `Non_blocking  (** Non-blocking tasks like the server. *) ]

(* Polymorphic compare orders these by name hash, which is not the order we
   want: the server must come first, then a clock holding a stream to real
   time, then a writer before a task that may wait. *)
let priority_rank = function
  | `Non_blocking -> 0
  | `Clock -> 1
  | `Blocking -> 2
  | `Threaded -> 3

let raw : priority Duppy.scheduler =
  Duppy.create
    ~on_error:(fun exn raw_bt ->
      let bt = Printexc.raw_backtrace_to_string raw_bt in
      if not (Tutils.error_handler ~bt exn) then
        Printexc.raise_with_backtrace exn raw_bt)
    ~on_fatal:(fun exn bt ->
      Dtools.Init.exec Dtools.Log.stop;
      Printf.printf "Scheduler crashed with exception %s\n%s"
        (Printexc.to_string exn)
        (Printexc.raw_backtrace_to_string bt);
      Printf.printf
        "PANIC: Liquidsoap has crashed, exiting.,\n\
         Please report at: https://github.com/savonet/liquidsoap";
      flush_all ();
      exit 1)
    ~rank:priority_rank
    ~classify:(function
      | `Non_blocking -> `Immediate
      (* A clock tick is long and holds a stream to real time: it runs on the
         domain, alone, so ticks spread rather than queueing behind each
         other. *)
      | `Clock | `Blocking -> `Direct
      | `Threaded -> `Threaded)
    ()

let () =
  Lifecycle.on_scheduler_shutdown ~name:"scheduler shutdown" (fun () ->
      log#important "Shutting down raw...";
      Duppy.stop raw;
      log#important "Scheduler shut down.")

let scheduler_logger () =
  if scheduler_log#get then (
    let log = Log.make ["scheduler"] in
    Some (fun m -> log#info "%s" m))
  else None

let legacy_pool () =
  let queues n accepts = List.init n#get (fun _ -> accepts) in
  `Threads
    (queues generic_queues (fun _ -> true)
    @ queues fast_queues (fun p -> p = `Threaded)
    @ queues non_blocking_queues (fun p -> p = `Non_blocking))

let start () =
  if Tutils.start () then (
    let pool =
      if legacy#get then Some (legacy_pool ())
      else (
        if
          List.exists
            (fun q -> q#is_set)
            [generic_queues; fast_queues; non_blocking_queues]
        then
          log#important
            "settings.scheduler.generic_queues, fast_queues and \
             non_blocking_queues are deprecated and ignored unless \
             settings.scheduler.legacy is set.";
        None)
    in
    Duppy.start ?pool ~max_blocking:blocking_tasks#get
      ?log:(scheduler_logger ()) raw)

(* Script code registers its callbacks through an effect, and a handler stays
   on the thread that installed it: every body the scheduler runs installs
   one. *)
let prepared = Script_callback.uncollected

module Task = struct
  include Duppy.Task

  let rec prepared_task task =
    {
      task with
      handler =
        (fun events ->
          List.map prepared_task (prepared (fun () -> task.handler events)));
    }

  let add ?domain task = Duppy.Task.add ?domain raw (prepared_task task)
end

module Async = struct
  include Duppy.Async

  let add ~priority fn = Duppy.Async.add ~priority raw (fun () -> prepared fn)
end

(* A parked computation resumes in a task of the scheduler's own, with the
   handlers installed inside [run] only. *)
let run fn = Duppy.run (fun () -> prepared fn)
let reschedule ?delay ~priority () = Duppy.reschedule ?delay ~priority raw
let reserve_blocking () = Duppy.reserve_blocking raw
