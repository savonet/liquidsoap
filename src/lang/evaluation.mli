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

module Queue = Queues.Queue

(** Evaluate a term in a given environment. *)
val eval : ?env:(string * Value.t) list -> Term.t -> Value.t

(** Evaluate a toplevel term. *)
val eval_toplevel : ?interactive:bool -> Term.t -> Value.t

(** Apply a function to arguments. *)
val apply : ?pos:Pos.t -> Value.t -> (string * Value.t) list -> Value.t

(** Register a function to be executed after the next runtime evaluation completes. *)
val on_after_eval : (unit -> unit) -> unit

(** Execute a function then run all the [after_eval] callbacks. *)
val after_eval :
  ?mode:[ `Guess | `Force | `Collect of (unit -> unit) Queue.t ] ->
  (unit -> 'a) ->
  'a
