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
type event = [ `Delay of float | `Write of fd | `Read of fd ]

(** A submitted task: what it waits on and what to run with whichever of its
    events occurred. [fds] are the distinct descriptors of [events], in the
    order the core reports on them. *)
type 'a t = {
  prio : 'a;
  t0 : float;
  events : event list;
  fds : fd array;
  fire : event list -> 'a t list;
  (* The one worker allowed to run it, by domain. *)
  pinned : int option;
}

external time : unit -> float = "duppy_stub_now"

(** The scheduler's core, specified in SPEC.md. Tasks are known to it by a
    handle and plain data: no OCaml value crosses this interface. *)
module Core = struct
  type t

  external create : fd -> fd -> bool -> t = "duppy_stub_create"
  external backend : t -> string = "duppy_stub_backend"
  external start : t -> int -> int -> unit = "duppy_stub_start"
  external stop : t -> unit = "duppy_stub_stop"
  external reserve : t -> int -> int = "duppy_stub_reserve" [@@noalloc]
  external slots : t -> int = "duppy_stub_slots" [@@noalloc]
  external error : t -> string = "duppy_stub_error"

  external submit :
    t ->
    int ->
    int ->
    int ->
    int ->
    int ->
    float ->
    fd array ->
    int array ->
    unit = "duppy_stub_submit_bytecode" "duppy_stub_submit"

  external take : t -> int -> int array -> int = "duppy_stub_take" [@@noalloc]
  external wait : t -> int -> unit = "duppy_stub_wait"

  external blocking_done : t -> int -> unit = "duppy_stub_blocking_done"
  [@@noalloc]

  (** What [take] hands over, in the order of [duppy_work]. *)
  let work = function
    | 0 -> `None
    | 1 -> `Batch
    | 2 -> `One_direct
    | 3 -> `One_threaded
    | 4 -> `Stopped
    | 5 -> `Failed
    | _ -> assert false

  let ranks = 64
  let read = 1
  let write = 2
  let any_worker = -1
  let every_worker = -1
end

(** Handles to values, issued and resolved without a lock. A handle is passed
    from the domain that adds a value to the one that takes it through the core,
    whose own lock orders the two accesses. *)
module Slab = struct
  let chunk_bits = 12
  let chunk_size = 1 lsl chunk_bits
  let chunk_count = 1 lsl 12

  type 'a t = {
    chunks : 'a option array Atomic.t array;
    next : int Atomic.t;
    free : int list Atomic.t;
  }

  let create () =
    {
      chunks = Array.init chunk_count (fun _ -> Atomic.make [||]);
      next = Atomic.make 0;
      free = Atomic.make [];
    }

  let rec reuse t =
    match Atomic.get t.free with
      | [] -> Atomic.fetch_and_add t.next 1
      | handle :: rest as free ->
          if Atomic.compare_and_set t.free free rest then handle else reuse t

  let rec release t handle =
    let free = Atomic.get t.free in
    if not (Atomic.compare_and_set t.free free (handle :: free)) then
      release t handle

  let chunk t handle =
    if handle lsr chunk_bits >= chunk_count then
      failwith "Duppy: too many tasks";
    let cell = t.chunks.(handle lsr chunk_bits) in
    match Atomic.get cell with
      | [||] as empty ->
          let fresh = Array.make chunk_size None in
          if Atomic.compare_and_set cell empty fresh then fresh
          else Atomic.get cell
      | chunk -> chunk

  let add t v =
    let handle = reuse t in
    (chunk t handle).(handle land (chunk_size - 1)) <- Some v;
    handle

  let take t handle =
    let chunk = chunk t handle in
    let slot = handle land (chunk_size - 1) in
    let v = chunk.(slot) in
    chunk.(slot) <- None;
    release t handle;
    v

  let clear t = Array.iter (fun cell -> Atomic.set cell [||]) t.chunks
end

let fds_of_events events =
  List.sort_uniq compare
    (List.filter_map
       (function `Read fd | `Write fd -> Some fd | `Delay _ -> None)
       events)

let interest_for fd events =
  List.fold_left
    (fun acc ev ->
      match ev with
        | `Read f when f = fd -> acc lor Core.read
        | `Write f when f = fd -> acc lor Core.write
        | _ -> acc)
    0 events

let earliest_delay events =
  List.fold_left
    (fun earliest -> function
      | `Delay d -> Float.min earliest d | _ -> earliest)
    infinity events

(** Which of [t]'s events occurred, from what the core wrote at [position]: its
    handle, whether its delay elapsed, then a mask for each of [t.fds]. *)
let fired_events t taken position =
  let now = time () in
  let expired = taken.(position + 1) = 1 in
  let earliest = earliest_delay t.events in
  let occurred fd flag =
    let rec find i =
      i < Array.length t.fds
      &&
      if t.fds.(i) = fd then taken.(position + 2 + i) land flag <> 0
      else find (i + 1)
    in
    find 0
  in
  List.filter
    (function
      | `Delay d -> expired && (d <= earliest || now >= t.t0 +. d)
      | `Read fd -> occurred fd Core.read
      | `Write fd -> occurred fd Core.write)
    t.events

type execution_class = [ `Immediate | `Direct | `Threaded ]

(** Wraps every task body. Effect handlers do not cross the thread a task is
    dispatched to, so a caller whose tasks need one installs it here. *)
type wrapper = { wrap : 'a. (unit -> 'a) -> 'a }

(** One domain or thread of the pool. [taken] receives what the core hands this
    worker. [blocking] counts the tasks parked on its auxiliary threads, which
    live in [aux_pending] and the fields around it. *)
type 'a worker = {
  index : int;
  accepts : 'a -> bool;
  (* Set by [start] from the spawned domain; stays [-1] on a thread pool. *)
  mutable domain : int;
  taken : int array;
  blocking : int Atomic.t;
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
  rank : 'a -> int;
  classify : 'a -> execution_class;
  wrapper : wrapper;
  core : Core.t;
  wake_read : fd;
  wake_write : fd;
  tasks : 'a t Slab.t;
  (* Submitted before the pool exists, which is what decides who may take
     them. *)
  pending : 'a t list Atomic.t;
  started : bool Atomic.t;
  running : bool Atomic.t;
  stopped : bool Atomic.t;
  mutable threaded : bool;
  mutable selective : bool;
  mutable workers : 'a worker list;
  mutable members : member list;
}

let default_on_fatal exn bt =
  Printf.eprintf "Duppy: event loop crashed with %s\n%s\n%!"
    (Printexc.to_string exn)
    (Printexc.raw_backtrace_to_string bt);
  exit 1

(* Forces the portable backend, so that it runs where a native one exists. *)
let forced_fallback () = Sys.getenv_opt "DUPPY_BACKEND" = Some "poll"

let create ?(on_error = Printexc.raise_with_backtrace)
    ?(on_fatal = default_on_fatal) ?(rank = fun _ -> 0)
    ?(classify : 'a -> execution_class = fun _ -> `Threaded)
    ?(wrapper = { wrap = (fun fn -> fn ()) }) () =
  (* A socket pair rather than a pipe: on Windows only sockets can be made
     non-blocking, and a blocking wake-up write could hang its caller. *)
  let wake_read, wake_write = Unix_utils.socketpair () in
  Unix.set_nonblock wake_write;
  {
    on_error;
    on_fatal;
    log = None;
    rank;
    classify;
    wrapper;
    core = Core.create wake_read wake_write (forced_fallback ());
    wake_read;
    wake_write;
    tasks = Slab.create ();
    pending = Atomic.make [];
    started = Atomic.make false;
    running = Atomic.make false;
    stopped = Atomic.make false;
    threaded = false;
    selective = false;
    workers = [];
    members = [];
  }

let started s = Atomic.get s.started
let log s fn = match s.log with None -> () | Some log -> log (fn ())
let worker_for s domain = List.find_opt (fun w -> w.domain = domain) s.workers

exception Unknown_domain of int

let class_code = function `Immediate -> 0 | `Direct -> 1 | `Threaded -> 2

(** Workers that declare what they accept are told to the core as one bit each.
*)
let accepted_by s prio =
  if not s.selective then Core.every_worker
  else
    List.fold_left
      (fun mask w -> if w.accepts prio then mask lor (1 lsl w.index) else mask)
      0 s.workers

let submit s t =
  if not (Atomic.get s.stopped) then begin
    let rank = s.rank t.prio in
    if rank < 0 || rank >= Core.ranks then
      invalid_arg "Duppy: rank out of range";
    let pin =
      match t.pinned with
        | None -> Core.any_worker
        | Some domain -> (
            match worker_for s domain with
              | Some w -> w.index
              | None -> raise (Unknown_domain domain))
    in
    let delay =
      match earliest_delay t.events with
        | d when d = infinity -> -1.
        | d -> Float.max 0. (t.t0 +. d -. time ())
    in
    let handle = Slab.add s.tasks t in
    try
      Core.submit s.core handle rank
        (class_code (s.classify t.prio))
        pin (accepted_by s t.prio) delay t.fds
        (Array.map (fun fd -> interest_for fd t.events) t.fds)
    with exn ->
      ignore (Slab.take s.tasks handle);
      raise exn
  end

let flush_pending s =
  List.iter (submit s) (List.rev (Atomic.exchange s.pending []))

let rec hold s t =
  let pending = Atomic.get s.pending in
  if not (Atomic.compare_and_set s.pending pending (t :: pending)) then hold s t

(* A task held just as the pool starts is flushed by whoever looks last. *)
let add_t s tasks =
  List.iter
    (fun t ->
      if Atomic.get s.running then submit s t
      else begin
        hold s t;
        if Atomic.get s.running then flush_pending s
      end)
    tasks

module Task = struct
  (** Events and tasks from the user's point-of-view. *)

  type nonrec event = event

  type ('a, 'b) task = {
    priority : 'a;
    events : 'b list;
    handler : 'b list -> ('a, 'b) task list;
  }

  let rec t_of_task ?domain (task : ('a, [< event ]) task) =
    let events = (task.events :> event list) in
    {
      prio = task.priority;
      t0 = time ();
      events;
      fds = Array.of_list (fds_of_events events);
      fire =
        (fun fired ->
          let l =
            List.filter (fun ev -> List.mem (ev :> event) fired) task.events
          in
          List.map (t_of_task ?domain) (task.handler l));
      pinned = domain;
    }

  (* A pin names a worker that has to exist and accept the task, so a wrong one
     fails here rather than leaving a task nobody will ever take. *)
  let add ?domain s t =
    (match domain with
      | None -> ()
      | Some d -> (
          if (not (Atomic.get s.running)) || s.threaded then
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

let suspend ~priority s register =
  ignore
    (Effect.perform
       (Await
          {
            park =
              (fun resume ->
                register (fun () ->
                    Task.add s
                      {
                        priority;
                        events = [`Delay 0.];
                        handler =
                          (fun _ ->
                            resume [];
                            []);
                      }));
          }))

let reschedule ?(delay = 0.) ~priority s =
  ignore (await ~priority s [`Delay delay])

(* A deep handler is part of the continuation it captures, so resuming
   reinstates it and the computation can park again. Parking registers an
   ordinary task whose handler resumes, which is why the task returns no new
   work of its own. *)
let run fn =
  let open Effect.Deep in
  Effect_utils.try_with fn ()
    {
      effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
            | Await { park } ->
                Some
                  (fun (k : (a, unit) continuation) ->
                    park (fun events -> continue k events))
            | _ -> None);
    }

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
    while
      w.aux_pending = []
      && (not (Atomic.get s.stopped))
      && w.aux_total <= Core.slots s.core
    do
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
          (* [on_error] has already seen what escapes a job: like on a domain,
             it is fatal rather than left to kill this thread. *)
          (try job ()
           with exn ->
             let bt = Printexc.get_raw_backtrace () in
             s.on_fatal exn bt);
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
    Fun.protect
      ~finally:(fun () ->
        Atomic.decr w.blocking;
        Core.blocking_done s.core w.index)
      (fun () -> add_t s (run_task s fn))
  in
  if s.threaded then run ()
  else begin
    Mutex.lock w.aux_m;
    w.aux_pending <- w.aux_pending @ [run];
    if
      w.aux_total - w.aux_busy < List.length w.aux_pending
      && w.aux_total < Core.slots s.core
    then begin
      w.aux_total <- w.aux_total + 1;
      ignore (Thread.create (fun () -> aux_loop s w) ())
    end
    else Condition.signal w.aux_c;
    Mutex.unlock w.aux_m
  end

(** How long [stop] waits for a parked task before giving up on it. *)
let drain_timeout = 5.

(** What the core handed [w], as the handlers to run, in order. The handles are
    resolved before any of them runs, since running one takes from the core
    again. *)
let taken_tasks s w written =
  let rec decode position acc =
    if position >= written then List.rev acc
    else (
      match Slab.take s.tasks w.taken.(position) with
        | None -> List.rev acc
        | Some t ->
            let fired = fired_events t w.taken position in
            decode
              (position + 2 + Array.length t.fds)
              ((fun () -> t.fire fired) :: acc))
  in
  decode 0 []

let dispatch s w =
  let rec loop () =
    let result = Core.take s.core w.index w.taken in
    match (Core.work (result land 7), taken_tasks s w (result lsr 3)) with
      | `None, _ ->
          Core.wait s.core w.index;
          loop ()
      | `Batch, fns ->
          List.iter (fun fn -> add_t s (run_task s fn)) fns;
          loop ()
      | `One_direct, [fn] ->
          add_t s (run_task s fn);
          loop ()
      | `One_threaded, [fn] ->
          run_blocking s w fn;
          loop ()
      | `Stopped, _ -> ()
      | `Failed, _ ->
          s.on_fatal
            (Failure ("Duppy: event thread failed: " ^ Core.error s.core))
            (Printexc.get_callstack 0);
          loop ()
      | _ -> assert false
  in
  loop ()

let wake_auxiliaries s =
  List.iter
    (fun w -> Mutex.protect w.aux_m (fun () -> Condition.broadcast w.aux_c))
    s.workers

(* A lowered budget lets the auxiliary threads above it exit, and a raised one
   lets idle workers take the blocking tasks they had declined. *)
let reserve_blocking s =
  let change n =
    ignore (Core.reserve s.core n);
    wake_auxiliaries s
  in
  change 1;
  let released = Atomic.make false in
  fun () -> if Atomic.compare_and_set released false true then change (-1)

let start ?pool ?(max_blocking = 64) ?log:logger s =
  if not (Atomic.compare_and_set s.started false true) then
    failwith "Duppy.start: scheduler already started";
  s.log <- logger;
  let accepts =
    match pool with
      | Some (`Threads accepts) | Some (`Selective_domains accepts) -> accepts
      | Some (`Domains n) -> List.init (max 1 n) (fun _ _ -> true)
      | None -> List.init (Domain.recommended_domain_count ()) (fun _ _ -> true)
  in
  s.threaded <- (match pool with Some (`Threads _) -> true | _ -> false);
  s.selective <-
    (match pool with
      | Some (`Threads _) | Some (`Selective_domains _) -> true
      | _ -> false);
  let count = List.length accepts in
  if s.selective && count >= Sys.int_size then
    invalid_arg "Duppy.start: too many threads";
  let workers =
    List.mapi
      (fun index accepts ->
        {
          index;
          accepts;
          domain = -1;
          taken = Array.make 1024 0;
          blocking = Atomic.make 0;
          aux_m = Mutex.create ();
          aux_c = Condition.create ();
          aux_pending = [];
          aux_busy = 0;
          aux_total = 0;
        })
      accepts
  in
  s.workers <- workers;
  Core.start s.core count max_blocking;
  (* What was submitted before the pool is ready before any worker looks. *)
  flush_pending s;
  let guard fn () =
    try fn ()
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      s.on_fatal exn bt
  in
  (* The first worker of a domain pool is a thread on the calling domain, which
     exists anyway and collects only what it allocated itself.

     The parent reads a spawned domain's id directly, so a pin can be validated
     before the worker has run a single instruction. *)
  let spawn w =
    let run = guard (fun () -> dispatch s w) in
    if s.threaded then `Thread (Thread.create run ())
    else if w.index = 0 then begin
      w.domain <- (Domain.self () :> int);
      `Thread (Thread.create run ())
    end
    else (
      let d = Domain.spawn run in
      w.domain <- (Domain.get_id d :> int);
      `Domain d)
  in
  s.members <- List.map spawn workers;
  Atomic.set s.running true;
  flush_pending s;
  log s (fun () ->
      if s.threaded then Printf.sprintf "Started %d dispatch threads." count
      else
        Printf.sprintf
          "Started %d dispatch domains on %s, %d blocking tasks each." count
          (Core.backend s.core) (Core.slots s.core))

let stop s =
  if Atomic.get s.started && not (Atomic.exchange s.stopped true) then begin
    Core.stop s.core;
    (* The core dropped the tasks it held, so nothing will ask for their
       handlers. *)
    Slab.clear s.tasks;
    wake_auxiliaries s;
    (* Let the tasks still parked on the workers finish, bounded because a
       blocking task is under no obligation to return. *)
    let deadline = time () +. drain_timeout in
    while
      List.exists (fun w -> 0 < Atomic.get w.blocking) s.workers
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
    List.iter
      (fun fd -> try Unix.close fd with _ -> ())
      [s.wake_read; s.wake_write]
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
  val pending : t -> bool
  val read : t -> Bytes.t -> int -> int -> int
  val write : t -> Bytes.t -> int -> int -> int
end

module Unix_transport : Transport_t with type t = Unix.file_descr = struct
  type t = Unix.file_descr

  let sock s = s
  let pending _ = false
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
            if not (Transport.pending h.socket) then (
              let fired = await ~priority h.scheduler events in
              if timed_out fired then fail Timeout);
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
