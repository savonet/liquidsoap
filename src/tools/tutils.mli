(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

(** Multithreading utilities *)

(** {1 Thread wrapper}
  Give names to threads, and forbid them to raise an exception;
  if that happens, the thread dies anyway but it is logged and [main]
  will notice it.
  The main process is expected to run [main] after having launched
  the needed threads: that function will sleep until a thread
  raises an exception. *)
val create : ('a -> unit) -> 'a -> string -> Thread.t

val main : unit -> unit
val has_started : unit -> bool
val shutdown : unit -> unit

(** Special exception allowed for "clean" termination of Tutils threads.
  * All other exceptions are reported as bugs. *)
exception Exit

(** Wait for the threads to terminate,
  * never return if some thread keeps running. *)
val join_all : unit -> unit

(** {1 Multi-tasking scheduler} *)

(** Priorities for the different scheduler usages. *)
type priority =
  | Blocking  (** For example a last.fm submission. *)
  | Maybe_blocking  (** Request resolutions vary a lot. *)
  | Non_blocking  (** Non-blocking tasks like the server. *)

(** task scheduler *)
val scheduler : priority Duppy.scheduler

val scheduler_shutdown_atom : Dtools.Init.t

(** {1 Misc} *)

(** Waits for [f()] to become true on condition [c].
  * The mutex [m] protecting data accessed by [f] is in the same state before
  * and after the call. *)
val wait : Condition.t -> Mutex.t -> (unit -> bool) -> unit

(** Make a function work in critical section, protected by a given lock. *)
val mutexify : Mutex.t -> ('a -> 'b) -> 'a -> 'b

exception Timeout of float

type event =
  [ `Read of Unix.file_descr
  | `Write of Unix.file_descr
  | `Both of Unix.file_descr ]

(* Wait some events: [`Read socket], [`Write socket] or [`Both timeout]
 * Raises [Timeout elapsed_time] if timeout is reached. *)
val wait_for : ?log:(string -> unit) -> event -> float -> unit

(** [finalize ~k f] calls [f] and returns it result,
  * and always executes [k], even when [f] raises an exception. *)
val finalize : k:(unit -> unit) -> (unit -> 'a) -> 'a

(** Tests whether a mutex is locked, without blocking.
  * We cannot check on Win32, where [true] is always returned:
  * it always "seems" OK, we don't raise false alarms.
  * This is meant to be used for assertions. *)
val seems_locked : Mutex.t -> bool

(** Thread-safe equivalent to Lazy.from_fun. *)
val lazy_cell : (unit -> 'a) -> unit -> 'a

(** Preemptive stoppable thread.
  *
  * The thread function receives a [should_stop,has_stop] pair on startup.
  * It should regularly poll the [should_stop] and stop when asked to.
  * Before stopping it should call [has_stopped].
  *
  * The function returns a [kill,wait] pair. The first function should be
  * called to request that the thread stops, and the second to wait
  * that it has effectively stopped. *)
val stoppable_thread :
  ((unit -> bool) * (unit -> unit) -> unit) ->
  string ->
  (unit -> unit) * (unit -> unit)
