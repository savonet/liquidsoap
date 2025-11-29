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

external poll :
  Unix.file_descr array ->
  Unix.file_descr array ->
  Unix.file_descr array ->
  float ->
  Unix.file_descr array * Unix.file_descr array * Unix.file_descr array
  = "caml_poll"

let poll r w e timeout =
  let r = Array.of_list r in
  let w = Array.of_list w in
  let e = Array.of_list e in
  let r, w, e = poll r w e timeout in
  (Array.to_list r, Array.to_list w, Array.to_list e)

let select, select_fname =
  match Sys.os_type with
    | "Unix" -> (poll, "poll")
    | _ -> (Unix.select, "select")

(** [remove f l] is like [List.find f l] but also returns the result of removing
    * the found element from the original list. *)
let remove f l =
  let rec aux acc = function
    | [] -> raise Not_found
    | x :: l -> if f x then (x, List.rev_append acc l) else aux (x :: acc) l
  in
  aux [] l

(** Events and tasks from the implementation point-of-view: * we have to hide
    the 'a parameter. *)

type e = { r : fd list; w : fd list; x : fd list; t : float }

type 'a t = {
  prio : 'a;
  enrich : e -> e;
  is_ready : e -> (unit -> 'a t list) option;
}

type 'a scheduler = {
  on_error : exn -> Printexc.raw_backtrace -> unit;
  out_pipe : Unix.file_descr;
  in_pipe : Unix.file_descr;
  compare : 'a -> 'a -> int;
  select_m : Mutex.t;
  mutable tasks : 'a t list;
  tasks_m : Mutex.t;
  mutable ready : ('a * (unit -> 'a t list)) list;
  ready_m : Mutex.t;
  mutable queues : Condition.t list;
  queues_m : Mutex.t;
  mutable stop : bool;
  stop_m : Mutex.t;
  queue_stopped_c : Condition.t;
}

let clear_tasks s =
  Mutex.lock s.tasks_m;
  s.tasks <- [];
  Mutex.unlock s.tasks_m

let create ?(on_error = Printexc.raise_with_backtrace) ?(compare = compare) () =
  let out_pipe, in_pipe = Unix.pipe () in
  if not Sys.win32 then Unix.set_nonblock in_pipe;
  {
    on_error;
    out_pipe;
    in_pipe;
    compare;
    select_m = Mutex.create ();
    tasks = [];
    tasks_m = Mutex.create ();
    ready = [];
    ready_m = Mutex.create ();
    queues = [];
    queues_m = Mutex.create ();
    stop = false;
    stop_m = Mutex.create ();
    queue_stopped_c = Condition.create ();
  }

let wake_up s =
  try ignore (Unix.write s.in_pipe (Bytes.of_string "x") 0 1)
  with
  | Unix.Unix_error (Unix.EAGAIN, _, _)
  | Unix.Unix_error (Unix.EWOULDBLOCK, _, _)
  ->
    ()

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
    let f item =
      match item.is_ready { r = []; w = []; x = []; t = 0. } with
        | Some f ->
            Mutex.lock s.ready_m;
            s.ready <- (item.prio, f) :: s.ready;
            Mutex.unlock s.ready_m
        | None ->
            Mutex.lock s.tasks_m;
            s.tasks <- item :: s.tasks;
            Mutex.unlock s.tasks_m
    in
    List.iter f items;
    wake_up s

  let add s t = add_t s [t_of_task t]
end

open Task

let stop s =
  clear_tasks s;
  Mutex.lock s.stop_m;
  s.stop <- true;
  Mutex.unlock s.stop_m;
  Mutex.lock s.queues_m;
  while List.length s.queues > 0 do
    wake_up s;
    Mutex.lock s.ready_m;
    List.iter Condition.signal s.queues;
    Mutex.unlock s.ready_m;
    Condition.wait s.queue_stopped_c s.queues_m
  done;
  Mutex.unlock s.queues_m

let tmp = Bytes.create 1024

(** There should be only one call of #process at a time. * Process waits for
    tasks to become ready, and moves ready tasks * to the ready queue. *)
let process s log =
  (* Compute the union of all events. *)
  let e =
    List.fold_left
      (fun e t -> t.enrich e)
      { r = [s.out_pipe]; w = []; x = []; t = infinity }
      s.tasks
  in
  (* Poll for an event. *)
  let r, w, x =
    let rec f () =
      try
        let timeout = if e.t = infinity then -1. else max 0. (e.t -. time ()) in
        log
          (Printf.sprintf "Enter %s at %f, timeout %f (%d/%d/%d)." select_fname
             (time ()) timeout (List.length e.r) (List.length e.w)
             (List.length e.x));
        let r, w, x = select e.r e.w e.x timeout in
        log
          (Printf.sprintf "Left %s at %f (%d/%d/%d)." select_fname (time ())
             (List.length r) (List.length w) (List.length x));
        (r, w, x)
      with
        | Unix.Unix_error (Unix.EINTR, _, _) ->
            (* [EINTR] means that select was interrupted by
             * a signal before any of the selected events
             * occurred and before the timeout interval expired.
             * We catch it and restart.. *)
            log (Printf.sprintf "Select interrupted at %f." (time ()));
            f ()
        | e ->
            (* Uncaught exception:
             * 1) Discards all tasks currently in the loop (we do not know which
             *    socket caused an error).
             * 2) Re-Raise e *)
            clear_tasks s;
            raise e
    in
    f ()
  in
  (* Empty the wake_up pipe if needed. *)
  let () =
    if List.mem s.out_pipe r then
      (* For safety, we may absorb more than
       * one write. This avoids bad situation
       * when exceesive wake_up may fill up the
       * pipe's write buffer, causing a wake_up
       * to become blocking.. *)
      ignore (Unix.read s.out_pipe tmp 0 1024)
  in
  (* Move ready tasks to the ready list. *)
  let e = { r; w; x; t = 0. } in
  Mutex.lock s.tasks_m;
  (* Split [tasks] into [r]eady and still [w]aiting. *)
  let r, w =
    List.fold_left
      (fun (r, w) t ->
        match t.is_ready e with
          | Some f -> ((t.prio, f) :: r, w)
          | None -> (r, t :: w))
      ([], []) s.tasks
  in
  s.tasks <- w;
  Mutex.unlock s.tasks_m;
  Mutex.lock s.ready_m;
  s.ready <-
    List.stable_sort (fun (p, _) (p', _) -> s.compare p p') (s.ready @ r);
  Mutex.unlock s.ready_m

(** Code for a queue to process ready tasks. * Returns true a task was found
    (and hence processed). * * s.ready_m *must* be locked before calling * this
    function, and is freed *only* * if some task was processed. *)
let exec s (priorities : 'a -> bool) =
  (* This assertion does not work on
   * win32 because a thread can double-lock
   * the same mutex.. *)
  if Sys.os_type <> "Win32" then assert (not (Mutex.try_lock s.ready_m));
  match remove (fun (p, _) -> priorities p) s.ready with
    | (_, task), remaining ->
        s.ready <- remaining;
        Mutex.unlock s.ready_m;
        let tasks =
          match task () with
            | exception exn ->
                let bt = Printexc.get_raw_backtrace () in
                s.on_error exn bt;
                []
            | v -> v
        in
        add_t s tasks;
        true
    | exception Not_found -> false

exception Queue_stopped
exception Queue_processed

(** Main loop for queues. *)
let queue ?log ?(priorities = fun _ -> true) s name =
  let log =
    match log with Some e -> e | None -> Printf.printf "queue %s: %s\n" name
  in
  let c =
    let c = Condition.create () in
    Mutex.lock s.queues_m;
    s.queues <- c :: s.queues;
    Mutex.unlock s.queues_m;
    log (Printf.sprintf "Queue #%d starting..." (List.length s.queues));
    c
  in
  (* Try to process ready tasks, otherwise try to become the master,
   * or be a slave and wait for the master to get some more ready tasks. *)
  let run () =
    Mutex.lock s.stop_m;
    let stop = s.stop in
    Mutex.unlock s.stop_m;
    if stop then raise Queue_stopped;
    (* Lock the ready tasks until the queue has a task to proceed,
     * *or* is really ready to restart on its condition, see the
     * Condition.wait call below for the atomic unlock and wait. *)
    Mutex.lock s.ready_m;
    log (Printf.sprintf "There are %d ready tasks." (List.length s.ready));
    if exec s priorities then raise Queue_processed;
    let wake () =
      let is_ready =
        Mutex.lock s.ready_m;
        let is_ready = s.ready <> [] in
        Mutex.unlock s.ready_m;
        is_ready
      in
      (* Wake up other queues if there are remaining tasks *)
      if is_ready then begin
        Mutex.lock s.queues_m;
        List.iter (fun x -> if x <> c then Condition.signal x) s.queues;
        Mutex.unlock s.queues_m
      end
    in
    if Mutex.try_lock s.select_m then begin
      (* Processing finished for me
       * I can unlock ready_m now.. *)
      Mutex.unlock s.ready_m;
      process s log;
      Mutex.unlock s.select_m;
      wake ()
    end
    else begin
      (* We use s.ready_m mutex here.
       * Hence, we avoid race conditions
       * with any other queue being processing
       * a task that would create a new task:
       * without this mutex, the new task may not be
       * notified to this queue if it is going to sleep
       * in concurrency..
       * It also avoid race conditions when restarting
       * queues since s.ready_m is locked until all
       * queues have been signaled. *)
      Condition.wait c s.ready_m;
      Mutex.unlock s.ready_m
    end
  in
  let rec f () =
    begin try run () with Queue_processed -> ()
    end;
    (f [@tailcall]) ()
  in
  let on_done () =
    Mutex.lock s.queues_m;
    s.queues <- List.filter (fun q -> q <> c) s.queues;
    Condition.signal s.queue_stopped_c;
    Mutex.unlock s.queues_m
  in
  (try f () with
    | Queue_stopped -> ()
    | exn ->
        let bt = Printexc.get_raw_backtrace () in
        (try on_done () with _ -> ());
        Printexc.raise_with_backtrace exn bt);
  on_done ()

module Async = struct
  (* m is used to make sure that
   * calls to [wake_up] and [stop]
   * are thread-safe. *)
  type t = { stop : bool ref; mutable fd : fd option; m : Mutex.t }

  exception Stopped

  let add ~priority (scheduler : 'a scheduler) f =
    (* A pipe to wake up the task *)
    let out_pipe, in_pipe = Unix.pipe () in
    if not Sys.win32 then Unix.set_nonblock in_pipe;
    let stop = ref false in
    let tmp = Bytes.create 1024 in
    let rec task l =
      if List.exists (( = ) (`Read out_pipe)) l then
        (* Consume data from the pipe *)
        ignore (Unix.read out_pipe tmp 0 1024);
      if !stop then begin
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
            try ignore (Unix.write t (Bytes.of_string " ") 0 1)
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
            t.stop := true;
            ignore (Unix.write c (Bytes.of_string " ") 0 1)
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
  let read = Unix.read
  let write = Unix.write

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

  module Mutex_o = Mutex

  module Mutex = struct
    module type Mutex_control = sig
      type priority

      val scheduler : priority scheduler
      val priority : priority
    end

    module type Mutex_t = sig
      (** Type for a mutex. *)
      type mutex

      module Control : Mutex_control

      (** [create ()] creates a mutex. Implementation-wise, * a duppy task is
          created that will be used to select a * waiting computation, lock the
          mutex on it and resume it. * Thus, [priority] and [s] represents,
          resp., the priority * and scheduler used when running calling process'
          computation. *)
      val create : unit -> mutex

      (** A computation that locks a mutex * and returns [unit] afterwards.
          Computation * will be blocked until the mutex is successfully locked.
      *)
      val lock : mutex -> (unit, 'a) t

      (** A computation that tries to lock a mutex. * Returns immediately [true]
          if the mutex was successfully locked * or [false] otherwise. *)
      val try_lock : mutex -> (bool, 'a) t

      (** A computation that unlocks a mutex. * Should return immediately. *)
      val unlock : mutex -> (unit, 'a) t
    end

    module Factory (Control : Mutex_control) = struct
      (* A mutex is either locked or not
       * and has a list of tasks waiting to get
       * it. *)
      type mutex = {
        mutable locked : bool;
        mutable tasks : (unit -> unit) list;
      }

      module Control = Control

      let tmp = Bytes.create 1024
      let x, y = Unix.pipe ()
      let stop = ref false
      let wake_up () = ignore (Unix.write y (Bytes.of_string " ") 0 1)
      let ctl_m = Mutex_o.create ()

      let finalise _ =
        stop := true;
        wake_up ()

      let mutexes = Queue.create ()
      let () = Gc.finalise finalise mutexes

      let register () =
        let m = { locked = false; tasks = [] } in
        Queue.push m mutexes;
        m

      let cleanup m =
        Mutex_o.lock ctl_m;
        let q = Queue.create () in
        Queue.iter (fun m' -> if m <> m' then Queue.push m q) mutexes;
        Queue.clear mutexes;
        Queue.transfer q mutexes;
        Mutex_o.unlock ctl_m

      let task f =
        {
          Task.priority = Control.priority;
          events = [`Delay 0.];
          handler =
            (fun _ ->
              f ();
              []);
        }

      (* This should only be called when [ctl_m] is locked. *)
      let process_mutex tasks m =
        if not m.locked then (
          (* I don't think shuffling tasks
           * matters here.. *)
            match m.tasks with
            | x :: l ->
                m.tasks <- l;
                m.locked <- true;
                task x :: tasks
            | _ -> tasks)
        else tasks

      let rec handler _ =
        Mutex_o.lock ctl_m;
        if not !stop then begin
          let tasks = Queue.fold process_mutex [] mutexes in
          Mutex_o.unlock ctl_m;
          ignore (Unix.read x tmp 0 1024);
          { Task.priority = Control.priority; events = [`Read x]; handler }
          :: tasks
        end
        else begin
          Mutex_o.unlock ctl_m;
          try
            Unix.close x;
            Unix.close y;
            []
          with _ -> []
        end

      let () =
        Task.add Control.scheduler
          { Task.priority = Control.priority; events = [`Read x]; handler }

      let create () =
        Mutex_o.lock ctl_m;
        let ret = register () in
        Mutex_o.unlock ctl_m;
        Gc.finalise cleanup ret;
        ret

      let lock m h' =
        Mutex_o.lock ctl_m;
        if not m.locked then begin
          m.locked <- true;
          Mutex_o.unlock ctl_m;
          h'.return ()
        end
        else begin
          m.tasks <- h'.return :: m.tasks;
          Mutex_o.unlock ctl_m
        end

      let try_lock m h' =
        Mutex_o.lock ctl_m;
        if not m.locked then begin
          m.locked <- true;
          Mutex_o.unlock ctl_m;
          h'.return true
        end
        else begin
          Mutex_o.unlock ctl_m;
          h'.return false
        end

      let unlock m h' =
        Mutex_o.lock ctl_m;
        (* Here we allow inter-thread
         * and double unlock.. Double unlock
         * is not necessarily a problem and
         * inter-thread unlock well.. what is
         * a thread here ?? :-) *)
        m.locked <- false;
        let wake = m.tasks <> [] in
        Mutex_o.unlock ctl_m;
        if wake then wake_up ();
        h'.return ()
    end
  end

  module Condition = struct
    module Factory (Mutex : Mutex.Mutex_t) = struct
      type condition = {
        condition_m : Mutex_o.t;
        waiting : (unit -> unit) Queue.t;
      }

      module Control = Mutex.Control

      let create () =
        { condition_m = Mutex_o.create (); waiting = Queue.create () }

      (* Mutex.unlock m needs to happen _after_
       * the task has been registered. *)
      let wait c m h =
        let proc () = Mutex.lock m h in
        Mutex_o.lock c.condition_m;
        Queue.push proc c.waiting;
        Mutex_o.unlock c.condition_m;
        (* Mutex.unlock does not raise exceptions (for now..) *)
        let h' = { return = (fun () -> ()); raise = (fun _ -> assert false) } in
        Mutex.unlock m h'

      let wake_up h =
        let handler _ =
          h ();
          []
        in
        Task.add Control.scheduler
          { Task.priority = Control.priority; events = [`Delay 0.]; handler }

      let signal c h =
        Mutex_o.lock c.condition_m;
        let h' = Queue.pop c.waiting in
        Mutex_o.unlock c.condition_m;
        wake_up h';
        h.return ()

      let broadcast c h =
        let q = Queue.create () in
        Mutex_o.lock c.condition_m;
        Queue.transfer c.waiting q;
        Mutex_o.unlock c.condition_m;
        Queue.iter wake_up q;
        h.return ()
    end
  end

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
