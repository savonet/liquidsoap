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

type clock_variable

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
type sync = [ `Auto | `CPU | `None ]

(** The liveness type of a source indicates whether or not it can
  * fail to broadcast.
  * A Infallible source never fails; it is always ready. *)
type source_t = Fallible | Infallible

(** Instrumentation. *)

type metadata = (int * (string, string) Hashtbl.t) list
type clock_sync_mode = [ sync | `Unknown ]

type watcher = {
  get_ready :
    stype:source_t ->
    is_output:bool ->
    id:string ->
    content_kind:Frame.content_kind ->
    clock_id:string ->
    clock_sync_mode:clock_sync_mode ->
    unit;
  leave : unit -> unit;
  get_frame :
    start_time:float ->
    end_time:float ->
    start_position:int ->
    end_position:int ->
    is_partial:bool ->
    metadata:metadata ->
    unit;
  after_output : unit -> unit;
}

(** The [source] use is to send data frames through the [get] method. *)
class virtual source :
  ?name:string
  -> Frame.content_kind
  -> object

       (** {1 Naming} *)

       (** Identifier of the source. *)
       method id : string

       method set_id : ?definitive:bool -> string -> unit

       (** {1 Liveness type}
    *
    * [stype] is the liveness type, telling whether a scheduler is
    * fallible or not, i.e. [get] will never fail.
    * It is defined by each operator based on its sources' types. *)

       method virtual stype : source_t

       (** {1 Init/shutdown} *)

       (** Registered server commands for the source *)
       val mutable ns_kind : string

       method register_command :
         descr:string -> ?usage:string -> string -> (string -> string) -> unit

       (** Register a callback, to be executed when source shuts down. *)
       method on_shutdown : (unit -> unit) -> unit

       (** The clock under which the source will run, initially unknown. *)
       method clock : clock_variable

       (** Does the source provide its own synchronization?
    * Examples: Alsa, AO, SRT I/O, etc.. This information
    * is used at the clock level to decide wether or not
    * we should synchronize with the CPU clock after producing
    * a frame (for [`Auto] clocks). Please note that in the case
    * of multiple sources filling the frame with different notion
    * notion of synchronization, there is no consistent notion
    * of time or synchronization. In this case (and with a [`Auto]
    * clock), we simply decide based on wether there is one [self_sync]
    * source or not. This logic should dictate how the method is
    * implemented by the various operators. *)
       method virtual self_sync : bool

       (** Choose your clock, by adjusting to your children source,
    * or anything custom. *)
       method private set_clock : unit

       (** The operator says to the source that he will ask it frames. *)
       method get_ready : ?dynamic:bool -> source list -> unit

       (** Called when the source must be ready and had no active operator,
    * means that the source has to initialize. This method is called by
    * [get_ready] and not called externally. *)
       method private wake_up : source list -> unit

       (** Opposite of [get_ready] : the operator no longer needs the source. *)
       method leave : ?dynamic:bool -> source -> unit

       method private sleep : unit

       (** Check if a source is up or not. *)
       method is_up : bool

       (** {1 Streaming} *)

       (** What kind of content does this source produce. *)
       method kind : Frame.content_kind

       (** Frame currently being filled. *)
       val memo : Frame.t

       method get_memo : Frame.t

       (** Number of frames left in the current track. Defaults to -1=infinity. *)
       method virtual remaining : int

       (* [self#seek_ticks x] skips [x] master ticks.
        * returns the number of ticks actually skipped.
        * By default it always returns 0, refusing to seek at all.
        * That method may be called from any thread, concurrently
        * with [#get], so they should not interfer. *)
       method seek : int -> int

       (** [is_ready] tells you if [get] can be called. *)
       method virtual is_ready : bool

       (** [get buf] asks the source to fill the buffer [buf] if possible.
    * The [get] call is partial when the buffer is not completely filled.
    * [get] should never be called with a full buffer,
    * and without checking that the source is ready. *)
       method get : Frame.t -> unit

       method virtual private get_frame : Frame.t -> unit

       (** Tells the source to finish the reading of current track. *)
       method virtual abort_track : unit

       method is_output : bool

       (** Wait for output round to finish.
    * Typically, output nodes compute an audio frame (a full buffer),
    * then launch a few output threads, which take care of encoding
    * and outputting (to a file, network, ...).
    * In that case, after_output allows the node to wait for its
    * output threads. *)
       method after_output : unit

       method advance : unit

       (** {1 Utilities} *)

       (** Create a request with a "source" metadata. *)
       method private create_request :
         ?metadata:(string * string) list ->
         ?persistent:bool ->
         ?indicators:Request.indicator list ->
         string ->
         Request.t

       method private log : Log.t

       method add_watcher : watcher -> unit
     end

(* Entry-points sources, which need to actively perform some task. *)
and virtual active_source :
  ?name:string
  -> Frame.content_kind
  -> object
       inherit source

       (** Special init phase for outputs. This method is called by Root after the
    * standard get_ready propagation, after the Root clock is started.
    * It allows enhancements of the initial latency. *)
       method virtual output_get_ready : unit

       (** Start a new output round, triggers computation of a new frame. *)
       method virtual output : unit

       (** Do whatever needed when the latency gets too big and is reset. *)
       method virtual output_reset : unit

       (** Is the source active ?
    * If the returned value is [false], then [output_reset]
    * should not be called on that source.
    * If [output_reset] does nothing, this function can return any value.
    * TODO that kind of detail could be left inside #output_reset *)
       method virtual is_active : bool
     end

(* This is for defining a source which has children *)
class virtual operator :
  ?name:string
  -> Frame.content_kind
  -> source list
  -> object
       inherit source
     end

(* Most usual active source: the active_operator, pulling one source's data
 * and outputting it. *)
class virtual active_operator :
  ?name:string
  -> Frame.content_kind
  -> source list
  -> object
       inherit active_source
     end

val has_outputs : unit -> bool
val iterate_new_outputs : (active_source -> unit) -> unit

(** {1 Clocks}
  * Tick identifiers are useful (cf. [#get_tick]) but we don't need much
  * more than the guarantee that the next tick is different from the
  * current one. Booleans should be OK, in any case an overflow on int
  * is not a problem. *)

class type clock =
  object
    (** Identifier of the clock. *)
    method id : string

    method sync_mode : sync

    (** Attach an active source to the clock. *)
    method attach : active_source -> unit

    (** Detach active sources that satisfy a given criterion. *)
    method detach : (active_source -> bool) -> unit

    (** Manage subordinate clocks *)

    method attach_clock : clock_variable -> unit

    method detach_clock : clock_variable -> unit

    method sub_clocks : clock_variable list

    (** Streaming *)

    method start_outputs : (active_source -> bool) -> unit -> active_source list

    method get_tick : int

    method end_tick : unit
  end

exception Clock_conflict of string * string
exception Clock_loop of string * string

module Clock_variables : sig
  val to_string : clock_variable -> string

  val create_unknown :
    sources:active_source list ->
    sub_clocks:clock_variable list ->
    clock_variable

  val create_known : clock -> clock_variable
  val unify : clock_variable -> clock_variable -> unit
  val forget : clock_variable -> clock_variable -> unit
  val get : clock_variable -> clock
  val is_known : clock_variable -> bool
end
