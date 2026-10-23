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

(** The fault model's parameters and the counter of consecutive item faults
    (faults.md §3, §4).

    A source, or an output field, owns one counter. Every stage that drops an
    item reports it with {!item}; every item delivered calls {!delivered}. The
    first drop of each kind is logged as a warning, repeats at most once per
    {!fault_log_interval} with their count. *)

val max_consecutive_faults : int
val fault_log_interval : float
val fault_retry_delay : float

type t

val create : Log.t -> t

(** [item t ~kind message] logs the drop under [kind] and counts it. The result
    is [`Source_fault] when the count reaches {!max_consecutive_faults}, which
    resets it. *)
val item : t -> kind:string -> string -> [ `Item_fault | `Source_fault ]

(** A fault whose item carries no media: logged like an item fault, not counted.
*)
val uncounted : t -> kind:string -> string -> unit

val delivered : t -> unit

(** Consecutive faults since the last delivered item. *)
val count : t -> int
