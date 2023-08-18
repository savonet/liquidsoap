(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2023 Savonet team

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

(* Some notes on clocks and streaming model after reworking it on June 21, 2021
 * By: Romain
 *
 * - Sources are animated via their output
 * - Each round of streaming is called a clock tick
 * - A unit of data during a clock tick is called a frame
 * - Frames contain breaks which indicate filling positions during the clock tick.
 * - Sources implement a [#get_frame] method which fills a frame with as much data
 *   as is available at each clock tick.
 * - The source gets a frame at a given position, fills it with as much data it can
 *   and add a break at the position last filled
 * - Each call to [#get_frame] must add exactly one break, even if no data could be
 *   added.
 * - If [#get_frame] does not fill the frame entirely, it is considered that the
 *   current track has ended. [#get_frame] can be called again if the source's
 *   [#is_ready] is [true] to keep filling the frame.
 * - Since sources can belong to multiple output, [#get_frame] result is cached.
 *   [#get] is the public API to fill a frame and uses the cache if the source is
 *   used by multiple outputs (called activations in the code).
 * - Each output implements a [#output] method that takes care of calling its
 *   underlying source's [#get] method as many times as possible to fill it during
 *   a clock tick.
 * - If a fallible output cannot fully fill the frame during a clock tick, it
 *   considers that its underlying source has failed and stops. This should never
 *   happen with infallible outputs.
 * - [#output] is called unconditionally so outputs should know when to query the
 *   underlying source and handle automatic restart there
 * - [#output] is also the function responsible for detecting end-of-track, source
 *   failures and etc. If an output fails to end and trigger its [on_stop]
 *   callback, it is most likely because [#output] should have been called
 * - Once done filling the frame, [#output] passes the frame to [#send_frame]
 *
 *
 * Now the clocks!
 * - Sources that are animated via clocks are called [active_source]. outputs are
 *   the most common case but some sources can also be [active_source] if they
 *   need to be actively animated, such as [input.http] for instance.
 * - Clocks implement a [#end_tick] method which is responsible for animating all
 *   its active source.
 * - Each call to [#end_tick] calls [#output] on each of the active sources and
 *   then [#after_output] on them.
 * - [#after_output] is responsible for cleaning up things after a clock tick, in
 *   particular clearing the frame cache for [#get]
 * - The best way to get the result of a clock tick for a given source is to plug
 *   it into an output and use the [#send_frame] method. There is no way currently
 *   to call [#get] on the underlying source between [#output] and [#after_output].
 *   Each call to [#end_tick] performs both operations at once.
 * - When running, a clock creates its own thread that executes [#end_tick] in a
 *   loop, implemented in the [#run] function.
 * - Latency during a clock tick run by [#run] can be controlled by the clock
 *   itself, if all the active sources report [#self_sync] as [false]. If any of
 *   the active outputs report [true] for [#self_sync] then the clock delegates
 *   latency control to its active sources, expecting the call to [#output] to last
 *   as long as needed to maintain correct latency. Otherwise, [#run] uses the CPU
 *   clock (via calls to [gettimeofday]) to control latency, delaying or accelerating
 *   accordingly depending on how long it takes to fill up a frame compared to the
 *   duration of each frame.
 *
 * About dependent clocks:
 * - The following is mostly implemented in [Child_support] for reference.
 * - In some operators such as [cross], [cue_cut], [soundtouch] and etc, the main
 *   operator wants full control over the children's clock.
 * - In this case, the children's source must all be statically non [self_sync]
 * - A main clock and a children clock are created. All children are placed in the
 *   children clock, which is marked as not running. It will not create its own
 *   [#run] thread. The main source is placed on the main clock. This clock is
 *   unknown and could possibly also not be running its own [#run] thread if the main
 *   source is, in turn, controlled by another source (yey abstractions!)
 * - During each of its clock tick, the main source may decide to call [#end_tick]
 *   on the children clock, thus animating its own outputs.
 * - If the operator needs more that one frame of data from its children during one
 *   of the main clock tick, it can call [#end_tick] multiple times. For instance,
 *   [soundtouch] when accelerating the rate or [cross] when buffering crossfade
 *   data.
 * - If the operator needs no more data, it can also not call [#end_tick] on the
 *   children clock, for instance [soundtouch] when slowing down the rate.
 * - However, one should remember that the children clock needs to be eventually
 *   animated in order to allow the children outputs to detect source failure,
 *   trigger [on_stop] events and etc.
 * - Thus, the convention proposed in [Child_support] is that, if the main source
 *   is not available, it should always call [#end_tick] on each of the main clock
 *   tick. This is implemented by using a [needs_tick] that is always set to [true]
 *   on each [#after_output]. If [true], a [#end_tick] is executed on the children
 *   clock during [#after_output]. [needs_tick] can optionally be set to [false]
 *   during the call to [#get_frame] but, if the source is not ready, this call will
 *   not be executed, leading to [#end_tick] being called on each main clock tick *)

val time_implementation : unit -> Liq_time.implementation

(** A clock represents the rate at which a source runs. In itself it does not
  * do much, except forcing that one source belongs to exactly one clock,
  * which prevents inconsistent uses of the source. Clocks are assigned to
  * sources at the end of the typing phase. *)

val clock :
  ?start:bool ->
  ?on_error:(exn -> Printexc.raw_backtrace -> unit) ->
  ?sync:Source.sync ->
  string ->
  Source.clock

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
  ?start:bool ->
  sources:Source.active_source list ->
  sub_clocks:clock_variable list ->
  unit ->
  clock_variable

val create_known : Source.clock -> clock_variable
val unify : clock_variable -> clock_variable -> unit
val forget : clock_variable -> clock_variable -> unit
val get : clock_variable -> Source.clock
