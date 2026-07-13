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

(** Clocks animate the sources attached to them by performing regular ticks:
    each tick asks every animated source to produce one frame of data.

    A clock is either stopped, started with its own streaming thread, or passive
    ([`Passive] sync mode), in which case it is ticked by whoever controls it
    (see [tick]). Clocks can be unified until they are started: unification
    merges the two clocks into a single one. *)

exception Invalid_state
exception Has_stopped

(** A clock handle. Two handles can dereference to the same underlying clock
    after unification. *)
type t

type active_source = < id : string ; reset : unit ; output : unit >

type source_type =
  [ `Passive | `Active of active_source | `Output of active_source ]

(** A sync source is an entity that controls the pace of the streaming loop by
    its own means, e.g. a soundcard blocking on its hardware timer or a network
    protocol regulated by packet timestamps. A clock with a current sync source
    is in self-sync mode and delegates latency control to it; at most one unique
    sync source is allowed per clock at any given time. *)
type sync_source = Clock_base.sync_source

type self_sync = [ `Static | `Dynamic ] * sync_source option

(** The entity animating a clock: another clock (for sub-clocks, e.g. the child
    clock of a [cross]), the clock's own streaming thread once started, or
    [`None] when the clock is not animated yet. Clocks with incompatible
    controllers cannot be unified. *)
type controller = [ `None | `Clock of t | `Other of string * < id : string > ]

val conf_latency : float Dtools.Conf.t
val conf_max_latency : float Dtools.Conf.t
val string_of_sync_source : sync_source -> string
val latency_of_sync_source : sync_source -> float option
val max_latency_of_sync_source : sync_source -> float option

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

(** Raised when more than one unique sync source is simultaneously active in the
    same clock. *)
exception Sync_error of clock_sync_error

module type SyncSource = sig
  type t

  val time_implementation : t -> Liq_time.implementation
  val to_string : t -> string
  val latency : t -> float
  val max_latency : t -> float
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
  ; get_frame : Frame.t
  ; source_state : sync_source option
  ; on_sync_source_change :
      (old:sync_source option -> sync_source option -> unit) -> unit -> unit >

(** Sync mode of a clock:
    - [`Automatic]: the clock delegates latency control to its current sync
      source if it has one and is CPU-led otherwise. This is the default.
    - [`CPU]: the clock always follows the CPU time.
    - [`Unsynced]: no latency control, the clock ticks as fast as possible.
    - [`Passive]: the clock does not have its own thread and is ticked
      explicitly by whoever controls it. *)
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

(** All the clocks of the application. *)
val clocks : unit -> t list

(** Clocks animated by this clock: they are ticked as part of this clock's tick
    (unless already ticked during it) and stopped when this clock stops. *)
val sub_clocks : t -> t list

val id : t -> string
val set_id : t -> string -> unit
val descr : t -> string
val sync : t -> sync_mode

(** Start a clock that is stopped. This is done automatically for non-passive
    clocks with at least one output; [~force:true] bypasses both the output
    requirement and the application-started check. *)
val start : ?force:bool -> t -> unit

val started : t -> bool
val stop : t -> unit

(** Request all clocks to stop, before shutdown. *)
val global_stop : unit -> unit

val set_stack : t -> Liquidsoap_lang.Pos.t list -> unit

(** Whether the clock currently has a sync source. *)
val self_sync : t -> bool

(** Stream time of the clock, in seconds: [ticks * frame_duration]. Returns
    [-1.] for a stopped clock. *)
val time : t -> float

(** Unify two clocks into a single one. At least one of them must be stopped:
    the stopped clock is merged into the other one, which inherits its pending
    sources, sub-clocks and error handlers. Raises
    [Liquidsoap_lang.Error.Clock_conflict] when the sync modes are incompatible,
    [Clock_loop] when unification would make a clock its own transitive
    sub-clock, and [Clock_main] when the controllers are incompatible. *)
val unify : pos:Liquidsoap_lang.Pos.Option.t -> t -> t -> unit

val register_sub_clock : t -> t -> unit
val deregister_sub_clock : t -> t -> unit

(** Attach a source to the clock. The source is placed in the pending
    activations and activated when the clock starts or at the beginning of its
    next tick. *)
val attach : t -> source -> unit

val detach : t -> source -> unit

(** Activate pending sources without waiting for the next tick. Used to activate
    the sources of passive clocks before their controller starts ticking them.
*)
val activate_pending_sources : t -> unit

val ticks : t -> int

(** Execute a callback at the end of the current tick, before latency control.
    Callbacks are one-shot: they are flushed when executed. *)
val on_tick : t -> (unit -> unit) -> unit

(** Perform one tick of a started clock. Only meant for passive clocks and
    tests: non-passive clocks tick from their own thread. *)
val tick : t -> unit

(** Execute a callback after the current tick, including latency control.
    Callbacks are one-shot: they are flushed when executed. *)
val after_tick : t -> (unit -> unit) -> unit

val time_implementation : unit -> Liq_time.implementation

(** A time implementation that always returns [0] for [time] and is a no-op for
    [sleep_until]. For use by self-sync sources that drive their own clock and
    do not need liquidsoap's timing. *)
val unconstrained_time : Liq_time.implementation

(** Start the clocks created during the last script evaluation. *)
val after_eval : unit -> unit

val dump : unit -> string
val dump_sources : t -> string
val dump_all_sources : unit -> string
