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

(** Advanced scheduler and monad for server-oriented programming. *)

(**
    * {R {i {v
    *        The bars could not hold me;
    *        Force could not control me now.
    *        They try to keep me down, yeah!
    *        But Jah put I around.
    *        (...)
    *        Let me tell you this -
    *        I'm a duppy conqueror !
    *        v}  }  }
    * {R {b Lee "Scratch" Perry & Bob Marley - Duppy conqueror }}
    *
    * {2 Duppy task scheduler for OCaml.}
    *
    * {!Duppy} is a task scheduler for ocaml. It implements a wrapper
    * around the platform's readiness call: [epoll] or [kqueue], and
    * [select] elsewhere.
    *
    * Using {!Duppy.Task}, the programmer can easily submit tasks that need to wait
    * on a socket even, or for a given timeout (possibly zero).
    *
    * With {!Duppy.Async}, one can use a scheduler to submit asynchronous tasks.
    *
    * {!Duppy.Io} implements recursive easy reading and writing to a [Unix.file_descr]
    *
    * Finally, {!Duppy.run} lets a computation park on its events in direct
    * style instead of splitting into tasks.
    *
    * The scheduler runs a pool of domains, one per core: a task is dispatched
    * onto whichever domain is free when it becomes ready. It can also run a
    * pool of threads instead, for a program that must not run tasks in
    * parallel.*)

(** A scheduler is a device for processing tasks. * * ['a] is the type of
    objects used for priorities. *)
type 'a scheduler

(** How a task is run. A worker is one domain of the pool; on a thread pool,
    where a worker is a thread, [`Threaded] tasks run in place.

    [`Immediate]: a worker takes every ready one as a single batch and runs them
    in sequence on its own thread. No hand-off, no auxiliary thread, no slot of
    [max_blocking]. Nothing else is dispatched by that worker until the batch
    ends, so one task that blocks holds up the rest of the batch and the worker.
    A worker alternates a batch with one task of another class, so a ready list
    that refills as fast as it drains does not starve the others.

    [`Direct]: a worker takes one and runs it on its own thread. No hand-off, no
    auxiliary thread, no slot of [max_blocking], so it is taken even by a worker
    whose slots are all in use. The worker dispatches nothing else until it
    returns. Ready ones are taken one per worker and so spread over the pool.
    The worker's auxiliary threads share its domain lock, and may be given it
    while the task runs.

    [`Threaded]: a worker hands one to an auxiliary thread of its domain and
    goes back to dispatching. When the task parks in a syscall the runtime lock
    is released and the domain keeps running. Each holds a slot of
    [max_blocking] until it returns; a worker with none left declines them, and
    they wait for a slot.

    Among ready [`Direct] and [`Threaded] tasks a worker takes the least by
    [compare].

    {3 Choosing a class}

    Which work goes where belongs to the application. As a starting point:

    - [`Immediate] for reads and writes on descriptors known not to block, and
      for bookkeeping that only wakes something else.
    - [`Direct] for work that is time-sensitive and has to run ahead of the
      rest, such as a clock, and for work whose duration is predictable and
      large enough to be worth a scheduling step of its own.
    - [`Threaded] for everything that may block or run long for any reason: disk
      and network calls, loops sized by their data. It is the default because a
      task wrongly placed here costs a hand-off, while one wrongly placed in the
      other two stalls a worker. *)
type execution_class = [ `Immediate | `Direct | `Threaded ]

(** Wraps every task body. Effect handlers do not cross the thread a task is
    dispatched to, so a caller whose tasks need one installs it here rather than
    at each of its own entry points. *)
type wrapper = { wrap : 'a. (unit -> 'a) -> 'a }

exception Unknown_domain of int

(** Initiate a new scheduler. It has no domains until [start] is called.
  * @param on_error called when a task raises.
  * @param on_fatal called when the event loop itself crashes, which should be
  * considered a MAJOR FAILURE: all non-ready tasks are dropped. Default: print
  * the backtrace and exit.
  * @param compare the comparison function used to sort tasks according to priorities.
  * Works as in [List.sort]
  * @param classify how each priority is run. Default: [fun _ -> `Threaded]
  * @param wrapper wraps every task body. Default: run it as is *)
val create :
  ?on_error:(exn -> Printexc.raw_backtrace -> unit) ->
  ?on_fatal:(exn -> Printexc.raw_backtrace -> unit) ->
  ?compare:('a -> 'a -> int) ->
  ?classify:('a -> execution_class) ->
  ?wrapper:wrapper ->
  unit ->
  'a scheduler

(** [start s] spawns the scheduler's pool: one member running the event loop,
  * and the others running tasks. Raises [Failure] if [s] is already started.
  *
  * With [`Domains n], the pool is [n] domains and tasks run in parallel.
  * Spawning a domain makes [Unix.fork] fail from then on, so this must be
  * called after any daemonization.
  *
  * With [`Threads accepts], the pool is one systhread per predicate, each
  * taking only the tasks whose priority it accepts, and a [`Threaded] task runs
  * in place on the thread that took it. Nothing runs in parallel and
  * [Unix.fork] stays usable.
  * @param pool Default: [`Domains (Domain.recommended_domain_count ())]
  * @param current_domain also run one worker as a thread on the calling
  * domain, so that domain takes tasks too and a GC there reclaims what it
  * allocated. Ignored for a thread pool, which is on the calling domain
  * already. Default: [false]
  * @param max_blocking the most [`Threaded] tasks that may be in flight at
  * once, spread evenly over the domains. Each domain keeps at least one slot,
  * rounded up, so the whole budget is available even when it does not divide
  * evenly and a value below their number gives one per domain. Unused by a
  * thread pool. Default: [64]
  * @param log Logging function. Default: no logging *)
val start :
  ?pool:[ `Domains of int | `Threads of ('a -> bool) list ] ->
  ?current_domain:bool ->
  ?max_blocking:int ->
  ?log:(string -> unit) ->
  'a scheduler ->
  unit

(** Whether [start] has been called. *)
val started : 'a scheduler -> bool

(** Stop the scheduler, let the tasks already running finish, and wait for its
    domains to return. *)
val stop : 'a scheduler -> unit

(** [reserve_blocking s] adds one slot to the [`Threaded] budget set by
    [max_blocking] and returns the function giving it back. A task that may park
    for as long as a resource it owns lives takes one, so that it cannot starve
    the others. Auxiliary threads above a lowered budget exit once idle. *)
val reserve_blocking : 'a scheduler -> unit -> unit

(** Core task registration. * * A task will be a set of events to watch, and a
    corresponding function to * execute when one of the events is triggered. * *
    The executed function may then return a list of new tasks to schedule. *)
module Task : sig
  (** A task is a list of events awaited, * and a function to process events
      that have occurred. * * The ['a] parameter is the type of priorities, ['b]
      will be a subset of possible * events. *)
  type ('a, 'b) task = {
    priority : 'a;
    events : 'b list;
    handler : 'b list -> ('a, 'b) task list;
  }

  (** Type for possible events. * * Please not that currently, under win32, all
      socket used in ocaml-duppy * are expected to be in blocking mode only! *)
  type event =
    [ `Delay of float
    | `Write of Unix.file_descr
    | `Read of Unix.file_descr
    | `Exception of Unix.file_descr ]

  (** Schedule a task. With [domain], only the worker on that domain runs it,
      and so does every task its handler returns. Raises [Unknown_domain] when
      no started worker is on that domain or accepts the task's priority, and
      always on a thread pool, where workers share a domain. *)
  val add : ?domain:int -> 'a scheduler -> ('a, [< event ]) task -> unit
end

(** {2 Direct-style computations}

    [run f] executes [f] so that it can park on {!await} instead of splitting
    into tasks. It returns as soon as [f] finishes or parks; a parked
    computation resumes on whichever domain picks up its task, which may not be
    the one it started on. *)
val run : (unit -> unit) -> unit

(** [await ~priority s events] parks the calling computation until one of
    [events] occurs and returns those that did.

    Only a computation running under {!run} can park: calling this from a plain
    task handler raises [Effect.Unhandled]. *)
val await :
  priority:'a -> 'a scheduler -> [< Task.event ] list -> Task.event list

(** [suspend ~priority s register] parks the calling computation and calls
    [register] with a function that resumes it. The resumer may be called from
    any thread or domain, at most once; the computation then continues as a task
    of [priority]. *)
val suspend : priority:'a -> 'a scheduler -> ((unit -> unit) -> unit) -> unit

(** Monotonic seconds with an arbitrary origin, the clock of every delay. *)
val time : unit -> float

(** [reschedule ?delay ~priority s] parks and resumes the computation under
    [priority], to leave a priority a computation should no longer hold. *)
val reschedule : ?delay:float -> priority:'a -> 'a scheduler -> unit

(** Asynchronous task module * * This module implements an asynchronous API to
    {!Duppy.scheduler} * It allows to create a task that will run and then go to
    sleep. *)
module Async : sig
  type t

  (** Exception raised when trying to wake_up a task * that has been previously
      stopped *)
  exception Stopped

  (** [add ~priority s f] creates an asynchronous task in [s] with * priority
      [priority]. * * The task executes the function [f]. * If the task returns
      a positive float, the function will be executed * again after this delay.
      Otherwise it goes to sleep, and * you can use [wake_up] to resume the task
      and execute [f] again. * Only a single call to [f] is done at each time. *
      Multiple [wake_up] while previous task has not * finished will result in
      sequentialized calls to [f]. *)
  val add : priority:'a -> 'a scheduler -> (unit -> float) -> t

  (** Wake up an asynchronous task. * Raises [Stopped] if the task has been
      stopped. *)
  val wake_up : t -> unit

  (** Stop and remove the asynchronous task. Doesn't quit a running task. *
      Raises [Stopped] if the task has been stopped. *)
  val stop : t -> unit
end

(** Module type for Io functor. *)
module type Transport_t = sig
  type t

  val sock : t -> Unix.file_descr

  (** Whether [read] can return data without [sock] being readable, e.g. bytes a
      TLS layer already decrypted. *)
  val pending : t -> bool

  val read : t -> Bytes.t -> int -> int -> int
  val write : t -> Bytes.t -> int -> int -> int
end

(** Reading and writing a socket from a computation running under {!run}: both
    park until the socket is ready rather than returning to the scheduler. *)
module type Io_t = sig
  type socket

  (** Type for markers. [Split s] recognizes all regexp allowed by the [Re.Pcre]
      module. *)
  type marker = Length of int | Split of string

  (** Different types of failure. [Io_error] is raised when reading or writing
      returned 0, which usually means the socket was closed. *)
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
      parking the computation until enough of it has arrived.
      @param timeout
        applies to each wait rather than to the call. Default: wait forever. *)
  val read : ?timeout:float -> priority:'a -> 'a handle -> marker -> string

  (** [write ?timeout ~priority h data] writes all of [data], parking until the
      socket accepts it.
      @param timeout
        applies to each wait rather than to the call. Default: wait forever. *)
  val write :
    ?timeout:float ->
    ?offset:int ->
    ?length:int ->
    priority:'a ->
    'a handle ->
    Bytes.t ->
    unit
end

module MakeIo (Transport : Transport_t) : Io_t with type socket = Transport.t
module Io : Io_t with type socket = Unix.file_descr

(** {2 Some culture..}
    * {e Duppy is a Caribbean patois word of West African origin meaning ghost or spirit.
    * Much of Caribbean folklore revolves around duppies.
    * Duppies are generally regarded as malevolent spirits.
    * They are said to come out and haunt people at night mostly,
    * and people from the islands claim to have seen them.
    * The 'Rolling Calf', 'Three footed horse' or 'Old Higue' are examples of the more malicious spirits. }
    * {R {{:http://en.wikipedia.org/wiki/Duppy} http://en.wikipedia.org/wiki/Duppy}}*)
