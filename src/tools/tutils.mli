(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

(** Make a function work in critical section, protected by a given lock. *)
val mutexify : Mutex.t -> ('a -> 'b) -> ('a -> 'b)

(** Thread wrapper.
  Give names to threads, and forbid them to raise an exception.
  The main process is expected to run raise_everything after having launched
  the needed threads. So the main process will sleep, until a thread
  aborts. In that case it will log and output the error, and return. *)
val create : ('a -> unit) -> 'a -> string -> Thread.t
val main : unit -> unit

(** Wait for the threads to terminate,
  * never return if some thread keeps running. *)
val join_all : unit -> unit

(** Check if a thread is running. *)
val running : string -> Thread.t -> bool

module Task :
sig
  type task     = unit -> return_t
  and  return_t = Sleep of task | Yield of task | Finish

  type id

  val create  : ?k:(id -> unit) -> task -> unit
  val wake_up : id   -> unit
end
