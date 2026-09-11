(*****************************************************************************

  Duppy, a task scheduler for OCaml.
  Copyright 2003-2010 Savonet team

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

module Pcre = Re.Pcre

type fd = Unix.file_descr
type event = [ `Delay of float | `Write of fd | `Read of fd | `Exception of fd ]

(** A task waiting to run: what it waits on, when its earliest delay elapses and
    what to run with whichever of its events fired. [dispatched] retires the
    entry a task leaves behind in the timer index when a descriptor fired first.
*)
type 'a t = {
  id : int;
  prio : 'a;
  t0 : float;
  events : event list;
  deadline : float;
  fire : event list -> 'a t list;
  mutable dispatched : bool;
  (* The one worker allowed to run it, by domain. *)
  pinned : int option;
}

(** A task whose events fired, waiting for a worker. [target] is the domain of
    the one worker that may take it. *)
type 'a ready = { prio : 'a; run : unit -> 'a t list; target : int option }

(** Waiting tasks ordered by when they expire, the id breaking ties between
    tasks sharing a deadline. *)
module Timers = Map.Make (struct
  type t = float * int

  let compare = compare
end)

let next_id = Atomic.make 0
let time () = Unix.gettimeofday ()
let no_interest = { Pollset.read = false; write = false; except = false }

let fds_of_events events =
  List.sort_uniq compare
    (List.filter_map
       (function
         | `Read fd | `Write fd | `Exception fd -> Some fd | `Delay _ -> None)
       events)

let interest_for fd events =
  List.fold_left
    (fun acc ev ->
      match ev with
        | `Read f when f = fd -> { acc with Pollset.read = true }
        | `Write f when f = fd -> { acc with Pollset.write = true }
        | `Exception f when f = fd -> { acc with Pollset.except = true }
        | _ -> acc)
    no_interest events

(** Which of [t]'s events have fired, given what a wait reported. A descriptor
    in error satisfies whatever it was awaited for, so the task runs and finds
    out. *)
let fired_events t ready =
  let of_fd fd = List.assoc_opt fd ready in
  List.filter
    (fun ev ->
      match ev with
        | `Delay d -> time () >= t.t0 +. d
        | `Read fd -> (
            match of_fd fd with
              | Some i -> i.Pollset.read || i.Pollset.except
              | None -> false)
        | `Write fd -> (
            match of_fd fd with
              | Some i -> i.Pollset.write || i.Pollset.except
              | None -> false)
        | `Exception fd -> (
            match of_fd fd with Some i -> i.Pollset.except | None -> false))
    t.events

type execution_class = [ `Immediate | `Direct | `Blocking ]

(** Wraps every task body. Effect handlers do not cross the thread a task is
    dispatched to, so a caller whose tasks need one installs it here. *)
type wrapper = { wrap : 'a. (unit -> 'a) -> 'a }

(** One domain or thread of the pool. [wake] carries a signal across the window
    between registering as idle and blocking on [worker_c], so a wake-up sent in
    that window is not lost. [blocking] counts the tasks parked on this worker's
    auxiliary threads, which live in [aux_pending] and the fields around it. *)
type 'a worker = {
  worker_m : Mutex.t;
  worker_c : Condition.t;
  mutable wake : bool;
  mutable took_batch : bool;
  blocking : int Atomic.t;
  accepts : 'a -> bool;
  (* Set by [start] from the spawned domain; stays [-1] on a thread pool. *)
  mutable domain : int;
  aux_m : Mutex.t;
  aux_c : Condition.t;
  mutable aux_pending : (unit -> unit) list;
  mutable aux_busy : int;
  mutable aux_total : int;
}

type member = [ `Domain of unit Domain.t | `Thread of Thread.t ]

type 'a scheduler = {
  on_error : exn -> Printexc.raw_backtrace -> unit;
  on_fatal : exn -> Printexc.raw_backtrace -> unit;
  mutable log : (string -> unit) option;
  compare : 'a -> 'a -> int;
  classify : 'a -> execution_class;
  wrapper : wrapper;
  out_pipe : fd;
  in_pipe : fd;
  pollset : Pollset.t;
  by_fd : (fd, 'a t list) Hashtbl.t;
  mutable timers : 'a t Timers.t;
  tasks_m : Mutex.t;
  mutable ready : 'a ready list;
  mutable idle : 'a worker list;
  ready_m : Mutex.t;
  started : bool Atomic.t;
  stopped : bool Atomic.t;
  poller_done : bool Atomic.t;
  mutable blocking_per_worker : int;
  mutable threaded : bool;
  mutable workers : 'a worker list;
  mutable members : member list;
}

(** The interest registered for a descriptor is the union of what the tasks
    waiting on it want, so dropping one task does not stop watching for the
    others. [s.tasks_m] must be held. *)
let rearm s fd =
  match Hashtbl.find_opt s.by_fd fd with
    | None | Some [] ->
        Hashtbl.remove s.by_fd fd;
        Pollset.remove s.pollset fd
    | Some tasks ->
        Pollset.set s.pollset fd
          (List.fold_left
             (fun acc t ->
               let i = interest_for fd t.events in
               {
                 Pollset.read = acc.Pollset.read || i.Pollset.read;
                 write = acc.Pollset.write || i.Pollset.write;
                 except = acc.Pollset.except || i.Pollset.except;
               })
             no_interest tasks)

let register s t =
  List.iter
    (fun fd ->
      Hashtbl.replace s.by_fd fd
        (t :: Option.value ~default:[] (Hashtbl.find_opt s.by_fd fd));
      rearm s fd)
    (fds_of_events t.events);
  if t.deadline < infinity then
    s.timers <- Timers.add (t.deadline, t.id) t s.timers

let unregister s t =
  List.iter
    (fun fd ->
      (match Hashtbl.find_opt s.by_fd fd with
        | None -> ()
        | Some tasks ->
            Hashtbl.replace s.by_fd fd
              (List.filter (fun x -> x.id <> t.id) tasks));
      rearm s fd)
    (fds_of_events t.events);
  if t.deadline < infinity then
    s.timers <- Timers.remove (t.deadline, t.id) s.timers

let clear_tasks s =
  Mutex.lock s.tasks_m;
  Hashtbl.iter (fun fd _ -> Pollset.remove s.pollset fd) s.by_fd;
  Hashtbl.reset s.by_fd;
  s.timers <- Timers.empty;
  Mutex.unlock s.tasks_m

let default_on_fatal exn bt =
  Printf.eprintf "Duppy: event loop crashed with %s\n%s\n%!"
    (Printexc.to_string exn)
    (Printexc.raw_backtrace_to_string bt);
  exit 1

let create ?(on_error = Printexc.raise_with_backtrace)
    ?(on_fatal = default_on_fatal) ?(compare = compare)
    ?(classify : 'a -> execution_class = fun _ -> `Blocking)
    ?(wrapper = { wrap = (fun fn -> fn ()) }) () =
  (* A socket pair rather than a pipe: on Windows only sockets can be made
     non-blocking, and a blocking wake-up write could hang its caller. *)
  let out_pipe, in_pipe = Unix_utils.socketpair () in
  Unix.set_nonblock in_pipe;
  let pollset = Pollset.create () in
  Pollset.set pollset out_pipe
    { Pollset.read = true; write = false; except = false };
  {
    on_error;
    on_fatal;
    log = None;
    compare;
    classify;
    wrapper;
    out_pipe;
    in_pipe;
    pollset;
    by_fd = Hashtbl.create 64;
    timers = Timers.empty;
    tasks_m = Mutex.create ();
    ready = [];
    idle = [];
    ready_m = Mutex.create ();
    started = Atomic.make false;
    stopped = Atomic.make false;
    poller_done = Atomic.make false;
    blocking_per_worker = 1;
    threaded = false;
    workers = [];
    members = [];
  }

let started s = Atomic.get s.started
let log s fn = match s.log with None -> () | Some log -> log (fn ())

let wake_up s =
  try ignore (Unix_utils.write s.in_pipe (Bytes.of_string "x") 0 1)
  with
  | Unix.Unix_error (Unix.EAGAIN, _, _)
  | Unix.Unix_error (Unix.EWOULDBLOCK, _, _)
  ->
    ()

let signal_worker w =
  Mutex.lock w.worker_m;
  w.wake <- true;
  Condition.signal w.worker_c;
  Mutex.unlock w.worker_m

(** Detach up to [n] idle workers. [s.ready_m] must be held.

    Threads each accept a subset of priorities, so a wake-up may land on one
    that has nothing to take: wake them all. *)
let take_idle s n =
  let n = if s.threaded then max_int else n in
  let rec f n acc =
    if n <= 0 then acc
    else (
      match s.idle with
        | [] -> acc
        | w :: l ->
            s.idle <- l;
            f (n - 1) (w :: acc))
  in
  f n []

let wake_idle s n =
  let workers = Mutex.protect s.ready_m (fun () -> take_idle s n) in
  List.iter signal_worker workers

(** Take one worker off the idle list. [s.ready_m] must be held. *)
let claim_worker s w =
  s.idle <- List.filter (fun x -> x != w) s.idle;
  w

let wake_worker s w =
  Mutex.protect s.ready_m (fun () -> ignore (claim_worker s w));
  signal_worker w

let worker_for s domain = List.find_opt (fun w -> w.domain = domain) s.workers

exception Unknown_domain of int

module Task = struct
  (** Events and tasks from the user's point-of-view. *)

  type nonrec event = event

  type ('a, 'b) task = {
    priority : 'a;
    events : 'b list;
    handler : 'b list -> ('a, 'b) task list;
  }

  let rec t_of_task ?domain (task : ('a, [< event ]) task) =
    let t0 = time () in
    let events = (task.events :> event list) in
    {
      id = Atomic.fetch_and_add next_id 1;
      prio = task.priority;
      t0;
      events;
      deadline =
        List.fold_left
          (fun d -> function `Delay s -> min d (t0 +. s) | _ -> d)
          infinity events;
      fire =
        (fun fired ->
          let l =
            List.filter (fun ev -> List.mem (ev :> event) fired) task.events
          in
          List.map (t_of_task ?domain) (task.handler l));
      dispatched = false;
      pinned = domain;
    }

  let add_t s items =
    let ready = ref 0 in
    let pinned = ref [] in
    let f item =
      match fired_events item [] with
        | [] ->
            Mutex.lock s.tasks_m;
            register s item;
            Mutex.unlock s.tasks_m
        | fired ->
            item.dispatched <- true;
            Mutex.lock s.ready_m;
            s.ready <-
              {
                prio = item.prio;
                run = (fun () -> item.fire fired);
                target = item.pinned;
              }
              :: s.ready;
            Mutex.unlock s.ready_m;
            incr ready;
            Option.iter (fun d -> pinned := d :: !pinned) item.pinned
    in
    List.iter f items;
    if 0 < !ready then wake_idle s !ready;
    List.iter (fun d -> Option.iter (wake_worker s) (worker_for s d)) !pinned;
    wake_up s

  (* A pin names a worker that has to exist and accept the task, so a wrong one
     fails here rather than leaving a task nobody will ever take. *)
  let add ?domain s t =
    (match domain with
      | None -> ()
      | Some d -> (
          if (not (Atomic.get s.started)) || s.threaded then
            raise (Unknown_domain d);
          match worker_for s d with
            | Some w when w.accepts t.priority -> ()
            | _ -> raise (Unknown_domain d)));
    add_t s [t_of_task ?domain t]
end

open Task

(** A parked computation and the means to wake it. *)
type suspension = { park : (event list -> unit) -> unit }

type _ Effect.t += Await : suspension -> event list Effect.t

let await ~priority s events =
  let events = (events :> event list) in
  Effect.perform
    (Await
       {
         park =
           (fun resume ->
             Task.add s
               {
                 priority;
                 events;
                 handler =
                   (fun e ->
                     resume e;
                     []);
               });
       })

let reschedule ?(delay = 0.) ~priority s =
  ignore (await ~priority s [`Delay delay])

(* A deep handler is part of the continuation it captures, so resuming
   reinstates it and the computation can park again. Parking registers an
   ordinary task whose handler resumes, which is why the task returns no new
   work of its own. *)
let run fn =
  let open Effect.Deep in
  match_with fn ()
    {
      retc = (fun () -> ());
      exnc = (fun exn -> raise exn);
      effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
            | Await { park } ->
                Some
                  (fun (k : (a, unit) continuation) ->
                    park (fun events -> continue k events))
            | _ -> None);
    }

let tmp = Bytes.create 1024

type 'a work =
  | Batch of (unit -> 'a t list) list
  | Direct of (unit -> 'a t list)
  | One of (unit -> 'a t list)

(** Pick this worker's next unit of work and the idle workers to signal for what
    is left behind. [s.ready_m] must be held.

    Immediate tasks go as one batch: they do not block, so running them in
    sequence on the calling domain costs less than a hand-off each. Direct and
    blocking tasks go one at a time, so they spread over the pool: batching them
    would run several long tasks in sequence on one domain. A direct task holds
    no blocking slot, since it runs on the domain rather than on one of its
    auxiliary threads. *)
let take_work s w =
  let mine, others =
    List.partition
      (fun e ->
        w.accepts e.prio && (e.target = None || e.target = Some w.domain))
      s.ready
  in
  let direct, rest =
    List.partition (fun e -> s.classify e.prio = `Direct) mine
  in
  let immediate, blocking =
    List.partition (fun e -> s.classify e.prio = `Immediate) rest
  in
  let singles =
    if Atomic.get w.blocking < s.blocking_per_worker then direct @ blocking
    else direct
  in
  let can_block = singles <> [] in
  (* A worker alternates between a batch and a single task. Taking every ready
     immediate task on every round starves the rest whenever the ready list refills
     as fast as it drains, which a lone worker cannot escape by leaving the
     rest to someone else. *)
    match immediate with
    | _ :: _ when not (w.took_batch && can_block) ->
        s.ready <- direct @ blocking @ others;
        w.took_batch <- true;
        ( Some (Batch (List.rev_map (fun e -> e.run) immediate)),
          take_idle s (List.length s.ready) )
    | _ when can_block ->
        let best =
          List.fold_left
            (fun best x -> if s.compare x.prio best.prio < 0 then x else best)
            (List.hd singles) singles
        in
        s.ready <- List.filter (fun x -> x != best) s.ready;
        w.took_batch <- false;
        let work =
          if s.classify best.prio = `Direct then Direct best.run
          else One best.run
        in
        (Some work, take_idle s (List.length s.ready))
    (* Blocking work is ready but this worker is at its own capacity for it:
       leaving it there would strand the task until a worker happens to look
       for an unrelated reason, so hand it to the ones that are idle. *)
    | _ when blocking <> [] -> (None, take_idle s (List.length blocking))
    | _ -> (None, [])

let run_task s fn =
  match s.wrapper.wrap fn with
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        s.on_error exn bt;
        []
    | v -> v

(** Auxiliary threads are kept parked between tasks, since a task in this class
    can be shorter than the spawn it would otherwise pay for.

    Finishing a job leads back into the queue check under the same lock, so a
    thread with work waiting never parks and is never counted idle in the window
    where a submission would pick it. *)
let aux_loop s w =
  let rec loop () =
    while w.aux_pending = [] && not (Atomic.get s.stopped) do
      Condition.wait w.aux_c w.aux_m
    done;
    match w.aux_pending with
      | [] ->
          w.aux_total <- w.aux_total - 1;
          Mutex.unlock w.aux_m
      | job :: rest ->
          w.aux_pending <- rest;
          w.aux_busy <- w.aux_busy + 1;
          Mutex.unlock w.aux_m;
          (try job ()
           with exn ->
             let bt = Printexc.get_raw_backtrace () in
             s.on_error exn bt);
          Mutex.lock w.aux_m;
          w.aux_busy <- w.aux_busy - 1;
          loop ()
  in
  Mutex.lock w.aux_m;
  loop ()

(** Blocking tasks run on an auxiliary systhread inside the worker's domain:
    once the task parks in a syscall it releases the runtime lock and the domain
    goes back to dispatching.

    A worker that is itself a thread already releases the lock when it parks, so
    it runs the task in place. *)
let run_blocking s w fn =
  Atomic.incr w.blocking;
  let run () =
    let tasks = run_task s fn in
    Atomic.decr w.blocking;
    add_t s tasks
  in
  if s.threaded then run ()
  else begin
    let job () =
      run ();
      wake_worker s w
    in
    Mutex.lock w.aux_m;
    w.aux_pending <- w.aux_pending @ [job];
    if
      w.aux_total - w.aux_busy < List.length w.aux_pending
      && w.aux_total < s.blocking_per_worker
    then begin
      w.aux_total <- w.aux_total + 1;
      ignore (Thread.create (fun () -> aux_loop s w) ())
    end
    else Condition.signal w.aux_c;
    Mutex.unlock w.aux_m
  end

let wait_for_work s w =
  Mutex.lock w.worker_m;
  while (not w.wake) && not (Atomic.get s.stopped) do
    Condition.wait w.worker_c w.worker_m
  done;
  w.wake <- false;
  Mutex.unlock w.worker_m

(** How long [stop] waits for a parked task before giving up on it. *)
let drain_timeout = 5.

(** Longest the loop parks in one wait. A wake-up is a byte on a socket the
    writer drops when its buffer is full, so waiting on one alone risks never
    looking at [stopped] again. *)
let idle_timeout = 1.

let dispatch s w =
  while not (Atomic.get s.stopped) do
    Mutex.lock s.ready_m;
    let work, wake = take_work s w in
    (match work with None -> s.idle <- w :: s.idle | Some _ -> ());
    Mutex.unlock s.ready_m;
    List.iter signal_worker wake;
    begin match work with
      | Some (Batch fns) -> List.iter (fun fn -> add_t s (run_task s fn)) fns
      | Some (Direct fn) -> add_t s (run_task s fn)
      | Some (One fn) -> run_blocking s w fn
      | None -> wait_for_work s w
    end
  done

(** Wait for events, then move the tasks they woke to the ready list. *)
let poll_once s =
  let timeout =
    Mutex.protect s.tasks_m (fun () ->
        match Timers.min_binding_opt s.timers with
          | None -> idle_timeout
          | Some ((deadline, _), _) ->
              min idle_timeout (max 0. (deadline -. time ())))
  in
  log s (fun () ->
      Printf.sprintf "Waiting on %s at %f, timeout %f."
        (Pollset.backend s.pollset)
        (time ()) timeout);
  let fired =
    try Pollset.wait s.pollset ~timeout
    with exn ->
      (* We do not know which descriptor caused the error, so every task
         currently in the loop is discarded. *)
      clear_tasks s;
      raise exn
  in
  log s (fun () ->
      Printf.sprintf "Woke at %f (%d)." (time ()) (List.length fired));
  (* Absorb more than one write: excessive wake-ups would otherwise fill the
     socket's buffer and make [wake_up] block. *)
  if List.mem_assoc s.out_pipe fired then
    ignore (Unix_utils.read s.out_pipe tmp 0 1024);
  let collected =
    Mutex.protect s.tasks_m (fun () ->
        let collected = ref [] in
        let take t =
          if not t.dispatched then (
            match fired_events t fired with
              | [] -> ()
              | events ->
                  t.dispatched <- true;
                  collected := (t, events) :: !collected)
        in
        List.iter
          (fun (fd, _) ->
            match Hashtbl.find_opt s.by_fd fd with
              | None -> ()
              | Some tasks -> List.iter take tasks)
          fired;
        let now = time () in
        let rec expired () =
          match Timers.min_binding_opt s.timers with
            | Some ((deadline, _), t) when deadline <= now ->
                s.timers <- Timers.remove (deadline, t.id) s.timers;
                take t;
                expired ()
            | _ -> ()
        in
        expired ();
        List.iter (fun (t, _) -> unregister s t) !collected;
        !collected)
  in
  match collected with
    | [] -> ()
    | _ ->
        let wake =
          Mutex.protect s.ready_m (fun () ->
              let targets =
                List.filter_map
                  (fun ((t : _ t), _) -> Option.bind t.pinned (worker_for s))
                  collected
              in
              List.iter
                (fun ((t : _ t), events) ->
                  s.ready <-
                    {
                      prio = t.prio;
                      run = (fun () -> t.fire events);
                      target = t.pinned;
                    }
                    :: s.ready)
                collected;
              List.map (claim_worker s) targets
              @ take_idle s (List.length collected))
        in
        List.iter signal_worker wake

let poller s =
  Fun.protect
    ~finally:(fun () -> Atomic.set s.poller_done true)
    (fun () ->
      while not (Atomic.get s.stopped) do
        poll_once s
      done)

let start ?pool ?(current_domain = false) ?(max_blocking = 64) ?log:logger s =
  if not (Atomic.compare_and_set s.started false true) then
    failwith "Duppy.start: scheduler already started";
  s.log <- logger;
  let accepts =
    match pool with
      | Some (`Threads accepts) -> accepts
      | Some (`Domains n) -> List.init (max 1 n) (fun _ _ -> true)
      | None ->
          List.init
            (max 1 (Domain.recommended_domain_count ()))
            (fun _ _ -> true)
  in
  s.threaded <- (match pool with Some (`Threads _) -> true | _ -> false);
  let make_worker accepts =
    {
      worker_m = Mutex.create ();
      worker_c = Condition.create ();
      wake = false;
      took_batch = false;
      blocking = Atomic.make 0;
      accepts;
      domain = -1;
      aux_m = Mutex.create ();
      aux_c = Condition.create ();
      aux_pending = [];
      aux_busy = 0;
      aux_total = 0;
    }
  in
  let workers = List.map make_worker accepts in
  (* A thread on the calling domain, so the domain that evaluated the script
     takes tasks too and collects what it allocated: a GC only reclaims the
     heap of the domain it runs on. A thread pool already sits there. *)
  let current =
    if current_domain && not s.threaded then (
      let w = make_worker (fun _ -> true) in
      w.domain <- (Domain.self () :> int);
      Some w)
    else None
  in
  let workers = workers @ Option.to_list current in
  let count = List.length workers in
  s.blocking_per_worker <- max 1 ((max_blocking + count - 1) / count);
  s.workers <- workers;
  let guard fn () =
    try fn ()
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      s.on_fatal exn bt
  in
  let spawn fn =
    if s.threaded then `Thread (Thread.create (guard fn) ())
    else `Domain (Domain.spawn (guard fn))
  in
  (* The parent reads the spawned domain's id directly, so a pin can be
     validated before the worker has run a single instruction. *)
  let spawn_worker w =
    let m = spawn (fun () -> dispatch s w) in
    (match m with
      | `Domain d -> w.domain <- (Domain.get_id d :> int)
      | `Thread _ -> ());
    m
  in
  let spawn_worker w =
    match current with
      | Some c when c == w ->
          `Thread (Thread.create (guard (fun () -> dispatch s w)) ())
      | _ -> spawn_worker w
  in
  s.members <- spawn (fun () -> poller s) :: List.map spawn_worker workers;
  log s (fun () ->
      if s.threaded then Printf.sprintf "Started %d dispatch threads." count
      else
        Printf.sprintf "Started %d dispatch domains, %d blocking tasks each."
          count s.blocking_per_worker)

let stop s =
  if Atomic.get s.started then begin
    clear_tasks s;
    Atomic.set s.stopped true;
    wake_up s;
    List.iter signal_worker s.workers;
    List.iter
      (fun w -> Mutex.protect w.aux_m (fun () -> Condition.broadcast w.aux_c))
      s.workers;
    (* Let the tasks still parked on the workers finish, bounded because a
       blocking task is under no obligation to return. *)
    let deadline = time () +. drain_timeout in
    while
      ((not (Atomic.get s.poller_done))
      || List.exists (fun w -> 0 < Atomic.get w.blocking) s.workers)
      && time () < deadline
    do
      Thread.delay 0.01
    done;
    (* A domain terminates once every thread created inside it has finished, and
       a task is free to start one that outlives it: a binding logging from its
       own thread pins the domain that ran it for good. Reaping is handed to a
       thread of our own so that waiting for one cannot hold up stopping. *)
    let join = function
      | `Domain d -> Domain.join d
      | `Thread t -> Thread.join t
    in
    List.iter (fun m -> ignore (Thread.create (fun () -> join m) ())) s.members;
    s.members <- [];
    s.workers <- [];
    (* Freeing what the loop waits on while it is still in there is a use after
       free, and a descriptor is the cheaper thing to lose. *)
    if Atomic.get s.poller_done then Pollset.close s.pollset
  end

module Async = struct
  (* m is used to make sure that
   * calls to [wake_up] and [stop]
   * are thread-safe. *)
  type t = { stop : bool Atomic.t; mutable fd : fd option; m : Mutex.t }

  exception Stopped

  let add ~priority (scheduler : 'a scheduler) f =
    (* A socket pair to wake up the task. See [create] for why this is not a
       pipe. *)
    let out_pipe, in_pipe = Unix_utils.socketpair () in
    Unix.set_nonblock in_pipe;
    let stop = Atomic.make false in
    let tmp = Bytes.create 1024 in
    let rec task l =
      if List.exists (( = ) (`Read out_pipe)) l then
        (* Consume data from the pipe *)
        ignore (Unix_utils.read out_pipe tmp 0 1024);
      if Atomic.get stop then begin
        begin try
          (* This interface is purely asynchronous
           * so we close both sides of the pipe here. *)
          Unix.close in_pipe;
          Unix.close out_pipe
        with _ -> ()
        end;
        []
      end
      else begin
        let delay = f () in
        let event = if delay >= 0. then [`Delay delay] else [] in
        [{ priority; events = `Read out_pipe :: event; handler = task }]
      end
    in
    let task = { priority; events = [`Read out_pipe]; handler = task } in
    add scheduler task;
    { stop; fd = Some in_pipe; m = Mutex.create () }

  let wake_up t =
    Mutex.lock t.m;
    try
      begin match t.fd with
        | Some t -> (
            try ignore (Unix_utils.write t (Bytes.of_string " ") 0 1)
            with
            | Unix.Unix_error (Unix.EAGAIN, _, _)
            | Unix.Unix_error (Unix.EWOULDBLOCK, _, _)
            ->
              ())
        | None -> raise Stopped
      end;
      Mutex.unlock t.m
    with e ->
      Mutex.unlock t.m;
      raise e

  let stop t =
    Mutex.lock t.m;
    try
      begin match t.fd with
        | Some c ->
            Atomic.set t.stop true;
            ignore (Unix_utils.write c (Bytes.of_string " ") 0 1)
        | None -> raise Stopped
      end;
      t.fd <- None;
      Mutex.unlock t.m
    with e ->
      Mutex.unlock t.m;
      raise e
end

module type Transport_t = sig
  type t

  val sock : t -> Unix.file_descr
  val read : t -> Bytes.t -> int -> int -> int
  val write : t -> Bytes.t -> int -> int -> int
end

module Unix_transport : Transport_t with type t = Unix.file_descr = struct
  type t = Unix.file_descr

  let sock s = s
  let read = Unix_utils.read
  let write = Unix_utils.write
end

module type Io_t = sig
  type socket
  type marker = Length of int | Split of string

  type failure =
    | Io_error
    | Unix of (Unix.error * string * string * Printexc.raw_backtrace)
    | Unknown of exn * Printexc.raw_backtrace
    | Timeout

  (** Raised by [read] and [write]. On a read, whatever had been read before the
      failure is left in the handle's [data]. *)
  exception Error of failure

  (** [data] holds what a read consumed past its marker, which the next read on
      the same socket picks up. *)
  type 'a handle = {
    scheduler : 'a scheduler;
    socket : socket;
    mutable data : string;
  }

  val handle : 'a scheduler -> socket -> 'a handle

  (** [read ?timeout ~priority h marker] returns the data up to [marker],
      parking the computation until enough has arrived. [timeout] applies to
      each wait rather than to the call. *)
  val read : ?timeout:float -> priority:'a -> 'a handle -> marker -> string

  val write :
    ?timeout:float ->
    ?offset:int ->
    ?length:int ->
    priority:'a ->
    'a handle ->
    Bytes.t ->
    unit
end

module MakeIo (Transport : Transport_t) : Io_t with type socket = Transport.t =
struct
  type socket = Transport.t
  type marker = Length of int | Split of string

  type failure =
    | Io_error
    | Unix of (Unix.error * string * string * Printexc.raw_backtrace)
    | Unknown of exn * Printexc.raw_backtrace
    | Timeout

  exception Error of failure

  type 'a handle = {
    scheduler : 'a scheduler;
    socket : socket;
    mutable data : string;
  }

  let handle scheduler socket = { scheduler; socket; data = "" }

  (** Split a buffer at [marker], returning what precedes it and what follows,
      or [None] while the marker has not arrived. The marker is resolved once so
      that a [Split] pattern is compiled per read rather than per chunk. *)
  let matcher = function
    | Split r ->
        let rex = Pcre.regexp r in
        let rec find = function
          | Pcre.Text s :: Pcre.Delim _ :: rest ->
              let rem = Buffer.create 10 in
              List.iter
                (function
                  | Pcre.Text s | Pcre.Delim s -> Buffer.add_string rem s
                  | _ -> ())
                rest;
              Some (s, Buffer.contents rem)
          | _ :: rest -> find rest
          | [] -> None
        in
        fun buffer ->
          find (Pcre.full_split ~max:2 ~rex (Buffer.contents buffer))
    | Length n ->
        fun buffer ->
          if n <= Buffer.length buffer then
            Some
              ( Buffer.sub buffer 0 n,
                Buffer.sub buffer n (Buffer.length buffer - n) )
          else None

  let wait_events ~timeout socket =
    match timeout with
      | None -> ([`Read socket], fun _ -> false)
      | Some t -> ([`Read socket; `Delay t], List.mem (`Delay t))

  let read ?timeout ~priority h marker =
    let length = 1024 in
    let buffer = Buffer.create length in
    let buf = Bytes.make length ' ' in
    Buffer.add_string buffer h.data;
    h.data <- "";
    let socket = Transport.sock h.socket in
    let events, timed_out = wait_events ~timeout socket in
    let take = matcher marker in
    let fail failure =
      h.data <- Buffer.contents buffer;
      raise (Error failure)
    in
    let rec loop () =
      match take buffer with
        | Some (s, rem) ->
            h.data <- rem;
            s
        | None ->
            let fired = await ~priority h.scheduler events in
            if timed_out fired then fail Timeout;
            let n =
              try Transport.read h.socket buf 0 length with
                | Unix.Unix_error (x, y, z) ->
                    fail (Unix (x, y, z, Printexc.get_raw_backtrace ()))
                | e -> fail (Unknown (e, Printexc.get_raw_backtrace ()))
            in
            if n <= 0 then fail Io_error;
            Buffer.add_subbytes buffer buf 0 n;
            loop ()
    in
    loop ()

  let write ?timeout ?(offset = 0) ?length ~priority h data =
    let len = match length with Some len -> len | None -> Bytes.length data in
    let socket = Transport.sock h.socket in
    let events, timed_out =
      match timeout with
        | None -> ([`Write socket], fun _ -> false)
        | Some t -> ([`Write socket; `Delay t], List.mem (`Delay t))
    in
    (* Win32 blocks on a blocking socket rather than accepting a partial write,
       and does not report writability while the socket still has room: there
       the socket goes non-blocking and we write as much as it takes. *)
    let win32 = Sys.os_type = "Win32" in
    let restore () = if win32 then Unix.clear_nonblock socket in
    let fail failure =
      restore ();
      raise (Error failure)
    in
    let wait () =
      let fired = await ~priority h.scheduler events in
      if timed_out fired then fail Timeout
    in
    if win32 then Unix.set_nonblock socket;
    let rec loop pos =
      if pos < len then begin
        if not win32 then wait ();
        let n =
          try Transport.write h.socket data pos (len - pos) with
            | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) when win32 ->
                wait ();
                -1
            | Unix.Unix_error (x, y, z) ->
                fail (Unix (x, y, z, Printexc.get_raw_backtrace ()))
            | e -> fail (Unknown (e, Printexc.get_raw_backtrace ()))
        in
        if n = 0 then fail Io_error;
        loop (pos + max 0 n)
      end
    in
    loop offset;
    restore ()
end

module Io : Io_t with type socket = Unix.file_descr = MakeIo (Unix_transport)
