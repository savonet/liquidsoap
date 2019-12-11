(*****************************************************************************

  Liquidsoap, a programmable stream generator.
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

(** A clock represents the rate at which a source runs. In itself it does not
  * do much, except forcing that one source belongs to exactly one clock,
  * which prevents inconsistent uses of the source. Clocks are assigned to
  * sources at the end of the typing phase. *)

(** In [`CPU] mode, synchronization is governed by the CPU clock.
  * In [`None] mode, there is no synchronization control. Latency in
  * is governed by the time it takes for the sources to produce and
  * output data.
  * In [`Auto] mode, synchronization is governed by the CPU unless at
  * least one active source is declared [self_sync] in which case latency
  * is delegated to this source. A typical example being a source linked
  * to a sound card, in which case the source latency is governed
  * by the sound card's clock. Another case is synchronous network
  * protocol such as [input.srt]. *)
type sync = [`Auto | `CPU | `None]

class clock : ?sync:sync -> string -> Source.clock

(** Indicates whether the application has started to run or not. *)
val running : unit -> bool

(** {2 Global clock management} *)

(** When created, sources have a clock variable, which gets unified
  * with other variables or concrete clocks. When the time comes to
  * initialize the source, if its clock isn't defined yet, it gets
  * assigned to a default clock and that clock will take care of
  * starting it.
  *
  * Taking all freshly created sources, assigning them to the default
  * clock if needed, and starting them, is performed by [collect].
  * This is typically called after each script execution.
  * Technically we could separate collection and clock assignment,
  * which might simplify some things if it becomes unmanageable in the
  * future.
  *
  * Sometimes we need to be sure that collect doesn't happen during
  * the execution of a function. Otherwise, sources might be assigned
  * the default clock too early. This is done using [collect_after].
  * This need is not cause by running collect in too many places, but
  * simply because there is no way to control collection on a per-thread
  * basis (collect only the sources created by a given thread of
  * script execution).
  *
  * Functions running using [collect_after] should be kept short.
  * However, in theory, with multiple threads, we could have plenty
  * of short functions always overlapping so that collection can
  * never be done. This shouldn't happen too much, but in any case
  * we can't get rid of this without a more fine-grained collect,
  * which would require (heavy) execution contexts to tell from
  * which thread/code a given source has been added. *)

val collect_after : (unit -> 'a) -> 'a

val force_init : (Source.active_source -> bool) -> Source.active_source list

val start : unit -> unit

val stop : unit -> unit

val fold : (Source.clock -> 'a -> 'a) -> 'a -> 'a

type clock_variable = Source.clock_variable

val to_string : clock_variable -> string

val create_unknown :
  sources:Source.active_source list ->
  sub_clocks:clock_variable list ->
  clock_variable

val create_known : clock -> clock_variable

val unify : clock_variable -> clock_variable -> unit

val forget : clock_variable -> clock_variable -> unit

val get : clock_variable -> Source.clock
