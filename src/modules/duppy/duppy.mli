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
    * around [Unix.select].
    *
    * Using {!Duppy.Task}, the programmer can easily submit tasks that need to wait
    * on a socket even, or for a given timeout (possibly zero).
    *
    * With {!Duppy.Async}, one can use a scheduler to submit asynchronous tasks.
    *
    * {!Duppy.Io} implements recursive easy reading and writing to a [Unix.file_descr]
    *
    * Finally, {!Duppy.Monad} and {!Duppy.Monad.Io} provide a monadic interface to
    * program server code that with an implicit return/reply execution flow.
    *
    * The scheduler can use several queues running concurrently, each queue
    * processing ready tasks. Of course, a queue should run in its own thread.*)

(** A scheduler is a device for processing tasks. Several queues might run in *
    different threads, processing one scheduler's tasks. * * ['a] is the type of
    objects used for priorities. *)
type 'a scheduler

(** Initiate a new scheduler
  * @param compare the comparison function used to sort tasks according to priorities.
  * Works as in [List.sort] *)
val create :
  ?on_error:(exn -> Printexc.raw_backtrace -> unit) ->
  ?compare:('a -> 'a -> int) ->
  unit ->
  'a scheduler

(** Internal polling function. Uses `Unix.select` on windows and `poll`
    otherwise. *)
val poll :
  Unix.file_descr list ->
  Unix.file_descr list ->
  Unix.file_descr list ->
  float ->
  Unix.file_descr list * Unix.file_descr list * Unix.file_descr list

(** [queue ~log ~priorities s name]
 * starts a queue, on the scheduler [s] only processing priorities [p]
 * for which [priorities p] returns [true].
 *
 * Several queues can be run concurrently against [s].
 * @param log Logging function. Default: [Printf.printf "queue %s: %s\n" name]
 * @param priorities Predicate specifying which priority to process. Default: [fun _ -> _ -> true]
 *
 * An exception is raised from this call when duppy's event loops has
 * crashed. This exception should be considered a MAJOR FAILURE. All current
 * non-ready tasks registered for the calling scheduler are dropped. You may
 * restart Duppy's queues after it is raised but it should only be used to terminate
 * the process diligently!! *)
val queue :
  ?log:(string -> unit) ->
  ?priorities:('a -> bool) ->
  'a scheduler ->
  string ->
  unit

(** Stop all queues running on that scheduler and wait for them to return. *)
val stop : 'a scheduler -> unit

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

  (** Schedule a task. *)
  val add : 'a scheduler -> ('a, [< event ]) task -> unit
end

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

  type bigarray =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  val sock : t -> Unix.file_descr
  val read : t -> Bytes.t -> int -> int -> int
  val write : t -> Bytes.t -> int -> int -> int
  val ba_write : t -> bigarray -> int -> int -> int
end

(** Easy parsing of [Unix.file_descr]. * * With {!Duppy.Io.read}, you can pass a
    file descriptor to the scheduler, * along with a marker, and have it run the
    associated function when the * marker is found. * * With {!Duppy.Io.write},
    the schdeduler will try to write recursively to the file descriptor * the
    given string. *)
module type Io_t = sig
  type socket

  (** Type for markers. * * [Split s] recognizes all regexp allowed by the *
      [Pcre] module. *)
  type marker = Length of int | Split of string

  (** Type of [Bigarray] used here. *)
  type bigarray =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  (** Different types of failure. * * [Io_error] is raised when reading or
      writing * returned 0. This usually means that the socket * was closed. *)
  type failure =
    | Io_error
    | Unix of Unix.error * string * string
    | Unknown of exn
    | Timeout

  (** Wrapper to perform a read on a socket and trigger a function when
    * a marker has been detected, or enough data has been read.
    * It reads recursively on a socket, splitting into strings separated
    * by the marker (if any) and calls the given function on the list of strings.
    *
    * Can be used recursively or not, depending on the way you process strings.
    * Because of Unix's semantic, it is not possible to stop reading
    * at first marker, so there can be a remaining string. If not used
    * recursively, the second optional argument may contain a remaining
    * string. You should then initiate the next read with this value.
    *
    * The [on_error] function is used when reading failed on the socket.
    * Depending on your usage, it can be a hard failure, or simply a lost client.
    * The string passed to [on_error] contains data read before error
    * occurred.
    * @param recursive recursively read and process, default: [true]
    * @param init initial string for reading, default: [""]
    * @param on_error function used when read failed, default: [fun _ -> ()]
    * @param timeout Terminate with [Timeout] failure if nothing has been read
    *                after the given amount of time in seconds. More precisely,
    *                the exception is raised when no character have been read
    *                and the socket was not close while waiting. Default: wait
    *                forever. *)
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

  (** Similar to [read] but less complex.
    * [write ?exec ?on_error ?string ?bigarray ~priority scheduler socket]
    * write data from [string], or from [bigarray] if no string is given,
    * to [socket], and executes [exec] or [on_error] if errors occurred.
    *
    * Caveat: on Win32, all file descriptors are expected to be in blocking
    * mode before being passed to this call due to limitations in the emulation
    * of the unix/posix API. See code comments for more details.
    *
    * @param exec function to execute after writing, default: [fun () -> ()]
    * @param on_error function to execute when an error occurred, default: [fun _ -> ()]
    * @param string write data from this string
    * @param bigarray write data from this bigarray, if no [string] is given
    * @param timeout Terminate with [Timeout] failure if nothing has been written
    *                after the given amount of time in seconds. More precisely,
    *                the exception is raised when no character have been written
    *                and the socket was not close while waiting. Default: wait
    *                forever. *)
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

module MakeIo (Transport : Transport_t) : Io_t with type socket = Transport.t
module Io : Io_t with type socket = Unix.file_descr

(** Monadic interface to {!Duppy.Io}. * * This module can be used to write code
    * that runs in various Duppy's tasks and * raise values in a completely
    transparent way. * * You can see examples of its use * in the [examples/]
    directory of the * source code and in the files *
    [src/tools/{harbor.camlp4,server.camlp4}] * in liquidsoap's code. * * When a
    server communicates * with a client, it performs several * computations and,
    eventually, terminates. * A computation can either return a new * value or
    terminate. For instance: * * - Client connects. * - Server tries to
    authenticate the client. * - If authentication is ok, proceed with the next
    step. * - Otherwise terminate. * * The purpose of the monad is to embed *
    computations which can either return * a new value or raise a value that is
    used * to terminate. *)
module Monad : sig
  (** Type representing a computation * which returns a value of type ['a] * or
      raises a value of type ['b] *)
  type ('a, 'b) t

  (** [return x] create a computation that * returns value [x]. *)
  val return : 'a -> ('a, 'b) t

  (** [raise x] create a computation that raises * value [x]. *)
  val raise : 'b -> ('a, 'b) t

  (** Compose two computations. * [bind f g] is equivalent to: *
      [let x = f in g x] where [x] * has f's return type. *)
  val bind : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t

  (** [>>=] is an alternative notation * for [bind] *)
  val ( >>= ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t

  (** [run f ~return ~raise ()] executes [f] and process * returned values with
      [return] or raised values * with [raise]. *)
  val run : return:('a -> unit) -> raise:('b -> unit) -> ('a, 'b) t -> unit

  (** [catch f g] redirects values [x] raised during * [f]'s execution to [g].
      The name suggests the * usual [try .. with ..] exception catching. *)
  val catch : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t

  (** [=<<] is an alternative notation for catch. *)
  val ( =<< ) : ('b -> ('a, 'c) t) -> ('a, 'b) t -> ('a, 'c) t

  (** [fold_left f a [b1; b2; ..]] returns computation *
      [ (f a b1) >>= (fun a -> f a b2) >>= ...] *)
  val fold_left : ('a -> 'b -> ('a, 'c) t) -> 'a -> 'b list -> ('a, 'c) t

  (** [iter f [x1; x2; ..]] returns computation *
      [f x1 >>= (fun () -> f x2) >>= ...] *)
  val iter : ('a -> (unit, 'b) t) -> 'a list -> (unit, 'b) t

  (** This module implements monadic * mutex computations. They can be used * to
      write blocking code that is compatible * with duppy's tasks, i.e.
      [Mutex.lock m] blocks * the calling computation and not the calling
      thread. *)
  module Mutex : sig
    (** Information used to initialize a Mutex module. * [priority] and
        [scheduler] are used to initialize a task * which treat mutexes as well
        as conditions from the below * [Condition] module. *)
    module type Mutex_control = sig
      type priority

      val scheduler : priority scheduler
      val priority : priority
    end

    module type Mutex_t = sig
      (** Type for a mutex. *)
      type mutex

      module Control : Mutex_control

      (** [create ()] creates a mutex. *)
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

    module Factory (_ : Mutex_control) : Mutex_t
  end

  (** This module implements monadic * condition computations. They can be used
      * to write waiting code that is compatible * with duppy's tasks, i.e.
      [Condition.wait c m] blocks * the calling computation and not the calling
      thread * until [Condition.signal c] or [Condition.broadcast c] has * been
      called. *)
  module Condition : sig
    module Factory (Mutex : Mutex.Mutex_t) : sig
      (** Type of a condition, used in [wait] and [broadcast] *)
      type condition

      (** Create a condition. Implementation-wise, * a duppy task is created
          that will be used to select a * waiting computation, and resume it. *
          Thus, [priority] and [s] represents, resp., the priority * and
          scheduler used when running calling process' computation. *)
      val create : unit -> condition

      (** [wait h m] is a computation that:
        * {ul
        * {- Unlock mutex [m]}
        * {- Wait until [Condition.signal c] or [Condition.broadcast c]
             has been called}
        * {- Locks mutex [m]}
        * {- Returns [unit]}} *)
      val wait : condition -> Mutex.mutex -> (unit, 'a) t

      (** [broadcast c] is a computation that * resumes all computations waiting
          on [c]. It should * return immediately. *)
      val broadcast : condition -> (unit, 'a) t

      (** [signal c] is a computation that resumes one * computation waiting on
          [c]. It should return * immediately. *)
      val signal : condition -> (unit, 'a) t
    end
  end

  (** This module implements monadic computations * using [Duppy.Io]. It can be
      used to create * computations that read or write from a socket, * and also
      to redirect a computation in a different * queue with a new priority. *)
  module type Monad_io_t = sig
    type socket

    module Io : Io_t with type socket = socket

    (** {2 Type} *)

    (** A handler for this module * is a record that contains the * required
        elements. In particular, * [on_error] is a function that transforms * an
        error raised by [Duppy.Io] to a reply * used to terminate the
        computation. * [data] is an internal data buffer. It should * be
        initialized with [""]. It contains the * remaining data that was
        received when * using [read]. If an error occurred, * [data] contain
        data read before the * error. *)
    type ('a, 'b) handler = {
      scheduler : 'a scheduler;
      socket : Io.socket;
      mutable data : string;
      on_error : Io.failure -> 'b;
    }

    (** {2 Execution flow} *)

    (** [exec ?delay ~priority h f] redirects computation * [f] into a new queue
        with priority [priority] and * delay [delay] ([0.] by default). * It can
        be used to redirect a computation that * has to run under a different
        priority. For instance, * a computation that reads from a socket is
        generally * not blocking because the function is executed * only when
        some data is available for reading. * However, if the data that is read
        needs to be processed * by a computation that can be blocking, then one
        may * use [exec] to redirect this computation into an * appropriate
        queue. *)
    val exec :
      ?delay:float ->
      priority:'a ->
      ('a, 'b) handler ->
      ('c, 'b) t ->
      ('c, 'b) t

    (** [delay ~priority h d] creates a computation that returns * [unit] after
        delay [d] in seconds. *)
    val delay : priority:'a -> ('a, 'b) handler -> float -> (unit, 'b) t

    (** {2 Read/write} *)

    (** [read ?timeout ~priority ~marker h] creates a * computation that reads
        from [h.socket] * and returns the first string split * according to
        [marker]. This function * can be used to create a computation that *
        reads data from a socket. [timeout] parameter * forces the computation
        to return an error if * nothing has been read for more than [timeout] *
        seconds. Default: wait forever. *)
    val read :
      ?timeout:float ->
      priority:'a ->
      marker:Io.marker ->
      ('a, 'b) handler ->
      (string, 'b) t

    (** [read_all ?timeout ~priority s sock] creates a * computation that reads
        all data from [sock] * and returns it. Raised value contains data * read
        before an error occurred. *)
    val read_all :
      ?timeout:float ->
      priority:'a ->
      'a scheduler ->
      Io.socket ->
      (string, string * Io.failure) t

    (** [write ?timeout ~priority h s] creates a computation * that writes
        string [s] to [h.socket]. This * function can be used to create a
        computation * that sends data to a socket. [timeout] parameter * forces
        the computation to return an error if * nothing has been written for
        more than [timeout] * seconds. Default: wait forever. *)
    val write :
      ?timeout:float ->
      priority:'a ->
      ('a, 'b) handler ->
      ?offset:int ->
      ?length:int ->
      Bytes.t ->
      (unit, 'b) t

    (** [write_bigarray ?timeout ~priority h ba] creates a computation * that
        writes data from [ba] to [h.socket]. This function * can to create a
        computation that writes data to a socket. *)
    val write_bigarray :
      ?timeout:float ->
      priority:'a ->
      ('a, 'b) handler ->
      Io.bigarray ->
      (unit, 'b) t
  end

  module MakeIo (Io : Io_t) :
    Monad_io_t with type socket = Io.socket and module Io = Io

  module Io : Monad_io_t with type socket = Unix.file_descr and module Io = Io
end

(** {2 Some culture..}
    * {e Duppy is a Caribbean patois word of West African origin meaning ghost or spirit.
    * Much of Caribbean folklore revolves around duppies.
    * Duppies are generally regarded as malevolent spirits.
    * They are said to come out and haunt people at night mostly,
    * and people from the islands claim to have seen them.
    * The 'Rolling Calf', 'Three footed horse' or 'Old Higue' are examples of the more malicious spirits. }
    * {R {{:http://en.wikipedia.org/wiki/Duppy} http://en.wikipedia.org/wiki/Duppy}}*)
