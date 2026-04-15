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

(** Clocks drive the streaming loop in Liquidsoap. Each clock owns a set of
    sources and ticks them forward in lockstep, one frame at a time.

    {1 Clock hierarchy}

    Clocks form a tree. The top-level clocks each run in their own thread and
    determine the overall timing (CPU-bound, wall-clock, or free-running). Each
    top-level clock may have passive {e sub-clocks} that it ticks as part of its
    own tick, after processing its own sources. Sub-clocks are used by operators
    that need to control a child source at a different rate or in a separate
    scheduling domain (see [Child_support]).

    Sub-clocks are registered and deregistered dynamically via
    [register_sub_clock] / [deregister_sub_clock]. Operators that use a
    sub-clock should register it when they wake up and deregister it when they
    sleep, so that idle sub-clocks do not accumulate in the parent's tick loop.

    {1 Source activation}

    Sources are not directly added to a clock. Instead they are {e activated}:
    [source#wake_up activating_source] pushes the source into the clock's
    [pending_activations] queue and returns an [activation] token. The token
    must be passed back to [source#sleep] to deactivate the source. A clock
    drains its [pending_activations] queue at the start of each tick via
    [_activate_pending_sources].

    {1 Clock unification}

    Every source carries a clock cell (a [Unifier.t]). When an operator is
    constructed it calls [Clock.unify] on its own clock and each child's clock,
    merging them into a single cell. This ensures that all sources in a
    connected graph share the same clock. Unification checks that the clocks'
    {e controllers} are compatible; incompatible controllers (e.g. two different
    self-syncing sources) raise [Clock_main].

    {1 Tick sequence}

    On each tick a clock:
    + drains [pending_activations], waking up newly added sources;
    + calls [output] on every active and output source;
    + fires [on_tick] callbacks;
    + ticks each registered sub-clock that has not yet advanced this cycle;
    + increments its tick counter;
    + fires [after_tick] callbacks and handles timing/sleep. *)

exception Invalid_state
exception Has_stopped

type t
type active_source = < id : string ; reset : unit ; output : unit >

type source_type =
  [ `Passive | `Active of active_source | `Output of active_source ]

type sync_source = Clock_base.sync_source
type self_sync = [ `Static | `Dynamic ] * sync_source option
type controller = [ `None | `Clock of t | `Other of string * < id : string > ]

val string_of_sync_source : sync_source -> string

type sync_source_entry = {
  name : string;
  sync_source : sync_source;
  stack : Pos.t list;
}

type clock_sync_error = {
  name : string;
  stack : Pos.t list;
  sync_sources : sync_source_entry list;
}

exception Sync_error of clock_sync_error

module type SyncSource = sig
  type t

  val to_string : t -> string
end

module MkSyncSource (S : SyncSource) : sig
  val make : S.t -> sync_source
end

type activation = < id : string >

type source =
  < id : string
  ; stack : Pos.t list
  ; self_sync : self_sync
  ; source_type : source_type
  ; active : bool
  ; activations : activation list
  ; wake_up : source -> activation
  ; sleep : activation -> unit
  ; is_ready : bool
  ; get_frame : Frame.t >

type active_sync_mode = [ `Automatic | `CPU | `Unsynced | `Passive ]
type sync_mode = [ active_sync_mode | `Stopping | `Stopped ]

val string_of_sync_mode : sync_mode -> string
val active_sync_mode_of_string : string -> active_sync_mode

val create :
  ?stack:Liquidsoap_lang.Pos.t list ->
  ?controller:controller ->
  ?on_error:(exn -> Printexc.raw_backtrace -> unit) ->
  ?id:string ->
  ?sync:active_sync_mode ->
  unit ->
  t

val active_sources : t -> source list
val passive_sources : t -> source list
val outputs : t -> source list
val pending_activations : t -> source list
val sources : t -> source list
val clocks : unit -> t list
val sub_clocks : t -> t list
val id : t -> string
val set_id : t -> string -> unit
val descr : t -> string
val sync : t -> sync_mode
val start : ?force:bool -> t -> unit
val started : t -> bool
val stop : t -> unit
val global_stop : unit -> unit
val set_stack : t -> Liquidsoap_lang.Pos.t list -> unit
val self_sync : t -> bool
val time : t -> float
val unify : pos:Liquidsoap_lang.Pos.Option.t -> t -> t -> unit
val register_sub_clock : t -> t -> unit
val deregister_sub_clock : t -> t -> unit
val attach : t -> source -> unit
val detach : t -> source -> unit
val activate_pending_sources : t -> unit
val ticks : t -> int
val on_tick : t -> (unit -> unit) -> unit
val tick : t -> unit
val after_tick : t -> (unit -> unit) -> unit
val time_implementation : unit -> Liq_time.implementation
val after_eval : unit -> unit
val dump : unit -> string
val dump_sources : t -> string
val dump_all_sources : unit -> string
