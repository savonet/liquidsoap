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

(** A set of file descriptors watched for readiness, kept across waits.

    [epoll] on Linux and [kqueue] on BSD hold the registration in the kernel and
    report only what fired, so a wait costs what is ready rather than what is
    watched. Elsewhere this falls back to [select], which does not, and the only
    gain is that callers need not know which they got. *)
type t

(** What a descriptor is watched for, and what a wait reports about it. Errors
    and hangups are reported whether or not [except] was asked for. *)
type interest = { read : bool; write : bool; except : bool }

val create : unit -> t
val close : t -> unit

(** The mechanism in use, for logs. Setting [LIQ_POLLSET_BACKEND=select] forces
    the fallback, so it can be exercised where the native mechanisms exist. *)
val backend : t -> string

(** [set t fd i] watches [fd] for [i], replacing what it was watched for.
    Watching for neither reading nor writing leaves it watched for errors alone.
*)
val set : t -> Unix.file_descr -> interest -> unit

(** Descriptors that were never added, and ones already closed, are accepted and
    ignored: a closed descriptor leaves the kernel's set on its own, and the
    caller cannot always tell that it has. *)
val remove : t -> Unix.file_descr -> unit

val mem : t -> Unix.file_descr -> bool

(** [wait t ~timeout] blocks until a descriptor is ready or [timeout] seconds
    pass, and reports what fired for each. A negative [timeout] waits
    indefinitely, [0.] polls.

    Readiness is level-triggered on every backend: a descriptor that stays ready
    is reported by every wait until it is drained or removed. *)
val wait : t -> timeout:float -> (Unix.file_descr * interest) list
