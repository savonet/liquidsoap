(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

(** Liquidsoap's scheduler: {!Duppy}, bound to the application's one scheduler,
    with every body it runs prepared for script code. *)

(** Priorities for the different scheduler usages. *)
type priority =
  [ `Clock  (** A clock resuming to produce its next frames. *)
  | `Blocking
    (** Keeps its domain busy until done and never parks, such as a listener
        writer. *)
  | `Threaded
    (** May wait on a socket or a file, such as a request resolution or a
        last.fm submission. *)
  | `Non_blocking  (** Non-blocking tasks like the server. *) ]

(** The scheduler itself, for the {!Duppy} entry points that take one inside a
    computation started here: {!Duppy.Io.handle}, {!Duppy.await}. *)
val raw : priority Duppy.scheduler

(** Start the scheduler. Later calls do nothing. *)
val start : unit -> unit

module Task : sig
  type ('a, 'b) task = ('a, 'b) Duppy.Task.task = {
    priority : 'a;
    events : 'b list;
    handler : 'b list -> ('a, 'b) task list;
  }

  type event = Duppy.Task.event

  (** {!Duppy.Task.add} on {!raw}. The handler, and those of the tasks it
      returns, can run script code. *)
  val add : ?domain:int -> (priority, [< event ]) task -> unit
end

module Async : sig
  type t = Duppy.Async.t

  exception Stopped

  (** {!Duppy.Async.add} on {!raw}. The function can run script code. *)
  val add : priority:priority -> (unit -> float) -> t

  val wake_up : t -> unit
  val stop : t -> unit
end

(** {!Duppy.run}. The computation can run script code, before and after it
    parks. *)
val run : (unit -> unit) -> unit

(** {!Duppy.reschedule} on {!raw}. *)
val reschedule : ?delay:float -> priority:priority -> unit -> unit

(** {!Duppy.reserve_blocking} on {!raw}. *)
val reserve_blocking : unit -> unit -> unit
