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

(** An error at runtime. *)

type runtime_error = private { kind : string; msg : string; pos : Pos.t list }

exception Runtime_error of runtime_error

val on_error : (runtime_error -> unit) -> unit
val make : ?message:string -> pos:Pos.t list -> string -> runtime_error

val raise :
  ?bt:Printexc.raw_backtrace ->
  ?message:string ->
  pos:Pos.t list ->
  string ->
  'a

(** Register a function telling which other exceptions a script may catch, as
    the error kind and script positions it sees them with. They keep being
    reported as they are when nothing catches them. *)
val register_catchable : (exn -> (string * Pos.t list) option) -> unit

(** The error kind and positions under which a script may catch [exn], if any.
*)
val catchable : exn -> (string * Pos.t list) option

(** Run the [on_error] listeners on an error that is not raised. *)
val notify : runtime_error -> unit
