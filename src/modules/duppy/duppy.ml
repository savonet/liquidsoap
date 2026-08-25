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

  type bigarray =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  val sock : t -> Unix.file_descr
  val read : t -> Bytes.t -> int -> int -> int
  val write : t -> Bytes.t -> int -> int -> int
  val ba_write : t -> bigarray -> int -> int -> int
end

module Unix_transport : Transport_t with type t = Unix.file_descr = struct
  type t = Unix.file_descr

  type bigarray =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  let sock s = s
  let read = Unix_utils.read
  let write = Unix_utils.write

  external ba_write : t -> bigarray -> int -> int -> int
    = "ocaml_duppy_write_ba"
end

module type Io_t = sig
  type socket
  type marker = Length of int | Split of string

  type bigarray =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type failure =
    | Io_error
    | Unix of (Unix.error * string * string * Printexc.raw_backtrace)
    | Unknown of exn * Printexc.raw_backtrace
    | Timeout

  val read :
    ?recursive:bool ->
    ?init:string ->
    ?on_error:(string * failure -> unit) ->
    ?timeout:float ->
    priority:'a ->
    'a scheduler ->
    socket ->
    marker ->
    (string * string option -> unit) ->
    unit

  val write :
    ?exec:(unit -> unit) ->
    ?on_error:(failure -> unit) ->
    ?bigarray:bigarray ->
    ?offset:int ->
    ?length:int ->
    ?string:Bytes.t ->
    ?timeout:float ->
    priority:'a ->
    'a scheduler ->
    socket ->
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

  exception Io
  exception Timeout_exc

  type bigarray =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  let read ?(recursive = false) ?(init = "") ?(on_error = fun _ -> ()) ?timeout
      ~priority (scheduler : 'a scheduler) socket marker exec =
    let length = 1024 in
    let b = Buffer.create length in
    let buf = Bytes.make length ' ' in
    Buffer.add_string b init;
    let unix_socket = Transport.sock socket in
    let events, check_timeout =
      match timeout with
        | None -> ([`Read unix_socket], fun _ -> false)
        | Some f -> ([`Read unix_socket; `Delay f], List.mem (`Delay f))
    in
    let rec f l =
      if check_timeout l then raise Timeout_exc;
      if List.mem (`Read unix_socket) l then begin
        let input = Transport.read socket buf 0 length in
        if input <= 0 then raise Io;
        Buffer.add_subbytes b buf 0 input
      end;
      let ret =
        match marker with
          | Split r ->
              let rex = Pcre.regexp r in
              let acc = Buffer.contents b in
              let ret = Pcre.full_split ~max:2 ~rex acc in
              let rec p l =
                match l with
                  | Pcre.Text x :: Pcre.Delim _ :: l ->
                      let f b x =
                        match x with
                          | Pcre.Text s | Pcre.Delim s -> Buffer.add_string b s
                          | _ -> ()
                      in
                      if recursive then begin
                        Buffer.reset b;
                        List.iter (f b) l;
                        Some (x, None)
                      end
                      else begin
                        let b = Buffer.create 10 in
                        List.iter (f b) l;
                        Some (x, Some (Buffer.contents b))
                      end
                  | _ :: l' -> p l'
                  | [] -> None
              in
              p ret
          | Length n when n <= Buffer.length b ->
              let s = Buffer.sub b 0 n in
              let rem = Buffer.sub b n (Buffer.length b - n) in
              if recursive then begin
                Buffer.reset b;
                Buffer.add_string b rem;
                Some (s, None)
              end
              else Some (s, Some rem)
          | _ -> None
      in
      (* Catch all exceptions.. *)
      let f x =
        try f x with
          | Io ->
              on_error (Buffer.contents b, Io_error);
              []
          | Timeout_exc ->
              on_error (Buffer.contents b, Timeout);
              []
          | Unix.Unix_error (x, y, z) ->
              let bt = Printexc.get_raw_backtrace () in
              on_error (Buffer.contents b, Unix (x, y, z, bt));
              []
          | e ->
              let bt = Printexc.get_raw_backtrace () in
              on_error (Buffer.contents b, Unknown (e, bt));
              []
      in
      match ret with
        | Some x -> (
            match x with
              | s, Some _ when recursive ->
                  exec (s, None);
                  [{ priority; events; handler = f }]
              | _ ->
                  exec x;
                  [])
        | None -> [{ priority; events; handler = f }]
    in
    (* Catch all exceptions.. *)
    let f x =
      try f x with
        | Io ->
            on_error (Buffer.contents b, Io_error);
            []
        | Timeout_exc ->
            on_error (Buffer.contents b, Timeout);
            []
        | Unix.Unix_error (x, y, z) ->
            let bt = Printexc.get_raw_backtrace () in
            on_error (Buffer.contents b, Unix (x, y, z, bt));
            []
        | e ->
            let bt = Printexc.get_raw_backtrace () in
            on_error (Buffer.contents b, Unknown (e, bt));
            []
    in
    (* First one is without read,
     * in case init contains the wanted match.
     * Unless the user sets timeout to 0., this
     * should not interfere with user-defined timeout.. *)
    let task =
      { priority; events = [`Delay 0.; `Read unix_socket]; handler = f }
    in
    add scheduler task

  let write ?(exec = fun () -> ()) ?(on_error = fun _ -> ()) ?bigarray
      ?(offset = 0) ?length ?string ?timeout ~priority
      (scheduler : 'a scheduler) socket =
    let length, write =
      match (string, bigarray) with
        | Some s, _ ->
            let length =
              match length with Some length -> length | None -> Bytes.length s
            in
            (length, Transport.write socket s)
        | None, Some b ->
            let length =
              match length with
                | Some length -> length
                | None -> Bigarray.Array1.dim b
            in
            (length, Transport.ba_write socket b)
        | _ -> (0, fun _ _ -> 0)
    in
    let unix_socket = Transport.sock (socket : Transport.t) in
    let exec () =
      if Sys.os_type = "Win32" then Unix.clear_nonblock unix_socket;
      exec ()
    in
    let events, check_timeout =
      match timeout with
        | None -> ([`Write unix_socket], fun _ -> false)
        | Some f -> ([`Write unix_socket; `Delay f], List.mem (`Delay f))
    in
    let rec f pos l =
      try
        if check_timeout l then raise Timeout_exc;
        assert (List.exists (( = ) (`Write unix_socket)) l);
        let len = length - pos in
        let n = write pos len in
        if n <= 0 then (
          on_error Io_error;
          [])
        else if n < len then
          [{ priority; events = [`Write unix_socket]; handler = f (pos + n) }]
        else (
          exec ();
          [])
      with
        | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) when Sys.os_type = "Win32" ->
            [{ priority; events = [`Write unix_socket]; handler = f pos }]
        | Timeout_exc ->
            on_error Timeout;
            []
        | Unix.Unix_error (x, y, z) ->
            let bt = Printexc.get_raw_backtrace () in
            on_error (Unix (x, y, z, bt));
            []
        | e ->
            let bt = Printexc.get_raw_backtrace () in
            on_error (Unknown (e, bt));
            []
    in
    let task = { priority; events; handler = f offset } in
    if length > 0 then
      (* Win32 is particularly bad with writing on sockets. It is nearly impossible
       * to write proper non-blocking code. send will block on blocking sockets if
       * there isn't enough data available instead of returning a partial buffer
       * and WSAEventSelect will not return if the socket still has available space.
       * Thus, setting the socket to non-blocking and writing as much as we can. *)
      if Sys.os_type = "Win32" then begin
        Unix.set_nonblock unix_socket;
        List.iter (add scheduler) (f offset [`Write unix_socket])
      end
      else add scheduler task
    else exec ()
end

module Io : Io_t with type socket = Unix.file_descr = MakeIo (Unix_transport)

(** A monad for implicit continuations or responses *)
module Monad = struct
  type ('a, 'b) handler = { return : 'a -> unit; raise : 'b -> unit }
  type ('a, 'b) t = ('a, 'b) handler -> unit

  let return x h = h.return x
  let raise x h = h.raise x

  let bind f g h =
    let ret x =
      let process = g x in
      process h
    in
    f { return = ret; raise = h.raise }

  let ( >>= ) = bind
  let run ~return:ret ~raise f = f { return = ret; raise }

  let catch f g h =
    let raise x =
      let process = g x in
      process h
    in
    f { return = h.return; raise }

  let ( =<< ) x y = catch y x

  let rec fold_left f a = function
    | [] -> a
    | b :: l -> fold_left f (bind a (fun a -> f a b)) l

  let fold_left f a l = fold_left f (return a) l
  let iter f l = fold_left (fun () b -> f b) () l

  module type Monad_io_t = sig
    type socket

    module Io : Io_t with type socket = socket

    type ('a, 'b) handler = {
      scheduler : 'a scheduler;
      socket : Io.socket;
      mutable data : string;
      on_error : Io.failure -> 'b;
    }

    val exec :
      ?delay:float ->
      priority:'a ->
      ('a, 'b) handler ->
      ('c, 'b) t ->
      ('c, 'b) t

    val delay : priority:'a -> ('a, 'b) handler -> float -> (unit, 'b) t

    val read :
      ?timeout:float ->
      priority:'a ->
      marker:Io.marker ->
      ('a, 'b) handler ->
      (string, 'b) t

    val read_all :
      ?timeout:float ->
      priority:'a ->
      'a scheduler ->
      Io.socket ->
      (string, string * Io.failure) t

    val write :
      ?timeout:float ->
      priority:'a ->
      ('a, 'b) handler ->
      ?offset:int ->
      ?length:int ->
      Bytes.t ->
      (unit, 'b) t

    val write_bigarray :
      ?timeout:float ->
      priority:'a ->
      ('a, 'b) handler ->
      Io.bigarray ->
      (unit, 'b) t
  end

  module MakeIo (Io : Io_t) = struct
    type socket = Io.socket

    module Io = Io

    type ('a, 'b) handler = {
      scheduler : 'a scheduler;
      socket : Io.socket;
      mutable data : string;
      on_error : Io.failure -> 'b;
    }

    let exec ?(delay = 0.) ~priority h f h' =
      let handler _ =
        begin try f h'
        with e ->
          let bt = Printexc.get_raw_backtrace () in
          h'.raise (h.on_error (Io.Unknown (e, bt)))
        end;
        []
      in
      Task.add h.scheduler { Task.priority; events = [`Delay delay]; handler }

    let delay ~priority h delay = exec ~delay ~priority h (return ())

    let read ?timeout ~priority ~marker h h' =
      let process x =
        let s =
          match x with
            | s, None ->
                h.data <- "";
                s
            | s, Some s' ->
                h.data <- s';
                s
        in
        h'.return s
      in
      let init = h.data in
      h.data <- "";
      let on_error (s, x) =
        h.data <- s;
        h'.raise (h.on_error x)
      in
      Io.read ?timeout ~priority ~init ~recursive:false ~on_error h.scheduler
        h.socket marker process

    let read_all ?timeout ~priority s sock =
      let handler =
        { scheduler = s; socket = sock; data = ""; on_error = (fun e -> e) }
      in
      let buf = Buffer.create 1024 in
      let rec f () =
        let data = read ?timeout ~priority ~marker:(Io.Length 1024) handler in
        let process data =
          Buffer.add_string buf data;
          f ()
        in
        data >>= process
      in
      let catch_ret e =
        Buffer.add_string buf handler.data;
        match e with
          | Io.Io_error -> return (Buffer.contents buf)
          | e -> raise (Buffer.contents buf, e)
      in
      catch (f ()) catch_ret

    let write ?timeout ~priority h ?offset ?length s h' =
      let on_error x = h'.raise (h.on_error x) in
      let exec () = h'.return () in
      Io.write ?timeout ~priority ~on_error ~exec ?offset ?length ~string:s
        h.scheduler h.socket

    let write_bigarray ?timeout ~priority h ba h' =
      let on_error x = h'.raise (h.on_error x) in
      let exec () = h'.return () in
      Io.write ?timeout ~priority ~on_error ~exec ~bigarray:ba h.scheduler
        h.socket
  end

  module Io = MakeIo (Io)
end
