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

let select, select_fname =
  match Sys.os_type with
    | "Unix" -> (Unix_utils.poll, "poll")
    | _ -> (Unix_utils.select, "select")

(** Events and tasks from the implementation point-of-view: * we have to hide
    the 'a parameter. *)

type e = { r : fd list; w : fd list; x : fd list; t : float }

type 'a t = {
  prio : 'a;
  enrich : e -> e;
  is_ready : e -> (unit -> 'a t list) option;
}

type execution_class = [ `Immediate | `Blocking ]

(** One domain of the pool. [wake] carries a signal across the window between
    registering as idle and blocking on [worker_c], so a wake-up sent in that
    window is not lost. [blocking] counts the tasks parked on this worker's
    auxiliary threads. *)
type worker = {
  worker_m : Mutex.t;
  worker_c : Condition.t;
  mutable wake : bool;
  blocking : int Atomic.t;
}

type 'a scheduler = {
  on_error : exn -> Printexc.raw_backtrace -> unit;
  on_fatal : exn -> Printexc.raw_backtrace -> unit;
  mutable log : (string -> unit) option;
  compare : 'a -> 'a -> int;
  classify : 'a -> execution_class;
  out_pipe : fd;
  in_pipe : fd;
  mutable tasks : 'a t list;
  tasks_m : Mutex.t;
  mutable ready : ('a * (unit -> 'a t list)) list;
  mutable idle : worker list;
  ready_m : Mutex.t;
  started : bool Atomic.t;
  stopped : bool Atomic.t;
  mutable blocking_per_worker : int;
  mutable workers : worker list;
  mutable domains : unit Domain.t list;
}

let clear_tasks s =
  Mutex.lock s.tasks_m;
  s.tasks <- [];
  Mutex.unlock s.tasks_m

let default_on_fatal exn bt =
  Printf.eprintf "Duppy: event loop crashed with %s\n%s\n%!"
    (Printexc.to_string exn)
    (Printexc.raw_backtrace_to_string bt);
  exit 1

let create ?(on_error = Printexc.raise_with_backtrace)
    ?(on_fatal = default_on_fatal) ?(compare = compare)
    ?(classify : 'a -> execution_class = fun _ -> `Blocking) () =
  (* A socket pair rather than a pipe: on Windows only sockets can be made
     non-blocking, and a blocking wake-up write could hang its caller. *)
  let out_pipe, in_pipe = Unix_utils.socketpair () in
  Unix.set_nonblock in_pipe;
  {
    on_error;
    on_fatal;
    log = None;
    compare;
    classify;
    out_pipe;
    in_pipe;
    tasks = [];
    tasks_m = Mutex.create ();
    ready = [];
    idle = [];
    ready_m = Mutex.create ();
    started = Atomic.make false;
    stopped = Atomic.make false;
    blocking_per_worker = 1;
    workers = [];
    domains = [];
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

(** Detach up to [n] idle workers. [s.ready_m] must be held. *)
let take_idle s n =
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

let wake_worker s w =
  Mutex.protect s.ready_m (fun () ->
      s.idle <- List.filter (fun x -> x != w) s.idle);
  signal_worker w

module Task = struct
  (** Events and tasks from the user's point-of-view. *)

  type event =
    [ `Delay of float | `Write of fd | `Read of fd | `Exception of fd ]

  type ('a, 'b) task = {
    priority : 'a;
    events : 'b list;
    handler : 'b list -> ('a, 'b) task list;
  }

  let time () = Unix.gettimeofday ()

  let rec t_of_task (task : ('a, [< event ]) task) =
    let t0 = time () in
    {
      prio = task.priority;
      enrich =
        (fun e ->
          List.fold_left
            (fun e -> function
              | `Delay s -> { e with t = min e.t (t0 +. s) }
              | `Read s -> { e with r = s :: e.r }
              | `Write s -> { e with w = s :: e.w }
              | `Exception s -> { e with x = s :: e.x })
            e task.events);
      is_ready =
        (fun e ->
          let l =
            List.filter
              (fun evt ->
                match (evt :> event) with
                  | `Delay s when time () > t0 +. s -> true
                  | `Read s when List.mem s e.r -> true
                  | `Write s when List.mem s e.w -> true
                  | `Exception s when List.mem s e.x -> true
                  | _ -> false)
              task.events
          in
          if l = [] then None
          else Some (fun () -> List.map t_of_task (task.handler l)));
    }

  let add_t s items =
    let ready = ref 0 in
    let f item =
      match item.is_ready { r = []; w = []; x = []; t = 0. } with
        | Some f ->
            Mutex.lock s.ready_m;
            s.ready <- (item.prio, f) :: s.ready;
            Mutex.unlock s.ready_m;
            incr ready
        | None ->
            Mutex.lock s.tasks_m;
            s.tasks <- item :: s.tasks;
            Mutex.unlock s.tasks_m
    in
    List.iter f items;
    if 0 < !ready then wake_idle s !ready;
    wake_up s

  let add s t = add_t s [t_of_task t]
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

type 'a work = Batch of (unit -> 'a t list) list | One of (unit -> 'a t list)

(** Pick this worker's next unit of work and the idle workers to signal for what
    is left behind. [s.ready_m] must be held.

    Immediate tasks go as one batch: they do not block, so running them in
    sequence on the calling domain costs less than a hand-off each. Blocking
    tasks go one at a time, so they spread over the pool. *)
let take_work s w =
  let immediate, blocking =
    List.partition (fun (p, _) -> s.classify p = `Immediate) s.ready
  in
  match immediate with
    | _ :: _ ->
        s.ready <- blocking;
        ( Some (Batch (List.rev_map snd immediate)),
          take_idle s (List.length blocking) )
    | [] -> (
        match blocking with
          | [] -> (None, [])
          | _ when s.blocking_per_worker <= Atomic.get w.blocking -> (None, [])
          | first :: _ ->
              let best =
                List.fold_left
                  (fun best x ->
                    if s.compare (fst x) (fst best) < 0 then x else best)
                  first blocking
              in
              s.ready <- List.filter (fun x -> x != best) blocking;
              (Some (One (snd best)), take_idle s (List.length s.ready)))

let run_task s fn =
  match fn () with
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        s.on_error exn bt;
        []
    | v -> v

(** Blocking tasks run on an auxiliary systhread inside the worker's domain:
    once the task parks in a syscall it releases the runtime lock and the domain
    goes back to dispatching. One thread per task rather than a pool, since a
    task in this class is long enough that the spawn does not show. *)
let run_blocking s w fn =
  Atomic.incr w.blocking;
  ignore
    (Thread.create
       (fun () ->
         let tasks = run_task s fn in
         Atomic.decr w.blocking;
         add_t s tasks;
         wake_worker s w)
       ())

let wait_for_work s w =
  Mutex.lock w.worker_m;
  while (not w.wake) && not (Atomic.get s.stopped) do
    Condition.wait w.worker_c w.worker_m
  done;
  w.wake <- false;
  Mutex.unlock w.worker_m

(** Let the tasks already parked on this worker finish before it returns. *)
let drain w =
  Mutex.lock w.worker_m;
  while 0 < Atomic.get w.blocking do
    Condition.wait w.worker_c w.worker_m
  done;
  Mutex.unlock w.worker_m

let dispatch s w =
  while not (Atomic.get s.stopped) do
    Mutex.lock s.ready_m;
    let work, wake = take_work s w in
    (match work with None -> s.idle <- w :: s.idle | Some _ -> ());
    Mutex.unlock s.ready_m;
    List.iter signal_worker wake;
    begin match work with
      | Some (Batch fns) -> List.iter (fun fn -> add_t s (run_task s fn)) fns
      | Some (One fn) -> run_blocking s w fn
      | None -> wait_for_work s w
    end
  done;
  drain w

(** Wait for events, then move the tasks they woke to the ready list. *)
let poll_once s =
  let e =
    Mutex.protect s.tasks_m (fun () ->
        List.fold_left
          (fun e t -> t.enrich e)
          { r = [s.out_pipe]; w = []; x = []; t = infinity }
          s.tasks)
  in
  let r, w, x =
    try
      let timeout = if e.t = infinity then -1. else max 0. (e.t -. time ()) in
      log s (fun () ->
          Printf.sprintf "Enter %s at %f, timeout %f (%d/%d/%d)." select_fname
            (time ()) timeout (List.length e.r) (List.length e.w)
            (List.length e.x));
      let r, w, x = select e.r e.w e.x timeout in
      log s (fun () ->
          Printf.sprintf "Left %s at %f (%d/%d/%d)." select_fname (time ())
            (List.length r) (List.length w) (List.length x));
      (r, w, x)
    with exn ->
      (* We do not know which socket caused the error, so every task currently
         in the loop is discarded. *)
      clear_tasks s;
      raise exn
  in
  (* Absorb more than one write: excessive wake-ups would otherwise fill the
     socket's buffer and make [wake_up] block. *)
  if List.mem s.out_pipe r then ignore (Unix_utils.read s.out_pipe tmp 0 1024);
  let e = { r; w; x; t = 0. } in
  let ready =
    Mutex.protect s.tasks_m (fun () ->
        let ready, waiting =
          List.fold_left
            (fun (ready, waiting) t ->
              match t.is_ready e with
                | Some fn -> ((t.prio, fn) :: ready, waiting)
                | None -> (ready, t :: waiting))
            ([], []) s.tasks
        in
        s.tasks <- waiting;
        ready)
  in
  match ready with
    | [] -> ()
    | _ ->
        let wake =
          Mutex.protect s.ready_m (fun () ->
              s.ready <- List.rev_append ready s.ready;
              take_idle s (List.length ready))
        in
        List.iter signal_worker wake

let poller s =
  while not (Atomic.get s.stopped) do
    poll_once s
  done

let start ?domains ?(max_blocking = 64) ?log:logger s =
  if not (Atomic.compare_and_set s.started false true) then
    failwith "Duppy.start: scheduler already started";
  s.log <- logger;
  let count =
    match domains with
      | Some n -> max 1 n
      | None -> max 1 (Domain.recommended_domain_count ())
  in
  s.blocking_per_worker <- max 1 (max_blocking / count);
  let workers =
    List.init count (fun _ ->
        {
          worker_m = Mutex.create ();
          worker_c = Condition.create ();
          wake = false;
          blocking = Atomic.make 0;
        })
  in
  s.workers <- workers;
  let spawn fn =
    Domain.spawn (fun () ->
        try fn ()
        with exn ->
          let bt = Printexc.get_raw_backtrace () in
          s.on_fatal exn bt)
  in
  s.domains <-
    spawn (fun () -> poller s)
    :: List.map (fun w -> spawn (fun () -> dispatch s w)) workers;
  log s (fun () ->
      Printf.sprintf "Started %d dispatch domains, %d blocking tasks each."
        count s.blocking_per_worker)

let stop s =
  if Atomic.get s.started then begin
    clear_tasks s;
    Atomic.set s.stopped true;
    wake_up s;
    List.iter signal_worker s.workers;
    List.iter Domain.join s.domains;
    s.domains <- [];
    s.workers <- []
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
