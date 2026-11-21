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

(** The locks behind the [atomic] builtin. Sections on one lock exclude each
    other and nothing else, so both sides of a race have to take it. A section
    can be re-entered by the thread running it. *)

type lock

(** A lock of its own, independent of {!global}. *)
val create : unit -> lock

(** The process-wide lock, used when no lock is given. *)
val global : lock

val run : ?fast:bool -> ?lock:lock -> (unit -> 'a) -> 'a
