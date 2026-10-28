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

(** Additive signal handlers, safe to register from any thread. *)

(** [add signal handler] runs [handler] whenever [signal] is received, after the
    handlers added before it. Returns a function removing it. Handlers run in
    signal context: they must not take locks, and their exceptions are ignored.
    Once a handler has been added, the signal no longer has its default action,
    even after all are removed. *)
val add : int -> (int -> unit) -> unit -> unit
