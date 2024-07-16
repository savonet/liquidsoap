(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

(** Multithreading utilities *)

val error_handler : bt:string -> exn -> bool
val error_handlers : (bt:string -> exn -> bool) Stack.t

(** {1 Thread wrapper}
  Give names to threads, and forbid them to raise an exception;
  if that happens, the thread dies anyway but it is logged and [main]
  will notice it.
  The main process is expected to run [main] after having launched
  the needed threads: that function will sleep until a thread
  raises an exception. *)
val create : ('a -> unit) -> 'a -> string -> Thread.t

val main : unit -> unit
val start : unit -> unit
val running : unit -> bool
val finished : unit -> bool
val shutdown : int -> unit
val cleanup : unit -> unit
val exit_code : unit -> int
val exit : unit -> unit

(** Special exception allowed for "clean" termination of Tutils threads.
  * All other exceptions are reported as bugs. *)
exception Exit

(** Wait for the threads to terminate,
  * never return if some thread keeps running. *)
val join_all : unit -> unit

(** {1 Multi-tasking scheduler} *)

(** Priorities for the different scheduler usages. *)
type priority =
  [ `Blocking  (** For example a last.fm submission. *)
  | `Maybe_blocking  (** Request resolutions vary a lot. *)
  | `Non_blocking  (** Non-blocking tasks like the server. *) ]

(** task scheduler *)
val scheduler : priority Duppy.scheduler

(** {1 Misc} *)

(** Waits for [f()] to become true on condition [c].
  * The mutex [m] protecting data accessed by [f] is in the same state before
  * and after the call. *)
val wait : Condition.t -> Mutex.t -> (unit -> bool) -> unit

exception Timeout of float

type event =
  [ `Read of Unix.file_descr
  | `Write of Unix.file_descr
  | `Both of Unix.file_descr ]

(* Wait some events: [`Read socket], [`Write socket] or [`Both timeout]
 * Raises [Timeout elapsed_time] if timeout is reached. *)
val wait_for : ?log:(string -> unit) -> event -> float -> unit

(** Tests whether a mutex is locked, without blocking.
  * We cannot check on Win32, where [true] is always returned:
  * it always "seems" OK, we don't raise false alarms.
  * This is meant to be used for assertions. *)
val seems_locked : Mutex.t -> bool

val write_all : ?timeout:float -> Unix.file_descr -> bytes -> unit
