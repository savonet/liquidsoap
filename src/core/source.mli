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

(** Type for source's self_sync. The boolean indicates whether the operator
    takes care of synchronization by itself or not. The first component indicates
    whether this value can change during the source's lifetime (it cannot if this
    is [`Static]. *)
type self_sync = [ `Static | `Dynamic ] * bool

(** The liveness type of a source indicates whether or not it can
  * fail to broadcast.
  * A `Infallible source never fails; it is always ready. *)
type source_t = [ `Fallible | `Infallible ]

exception Unavailable

type streaming_state =
  [ `Unavailable | `Ready of unit -> unit | `Done of Frame.t ]

(** Instrumentation. *)

type metadata = (int * Frame.metadata) list
type clock_sync_mode = [ sync | `Unknown ]

type watcher = {
  wake_up :
    stype:source_t ->
    is_active:bool ->
    id:string ->
    ctype:Frame.content_type ->
    clock_id:string ->
    clock_sync_mode:clock_sync_mode ->
    unit;
  sleep : unit -> unit;
  generate_data :
    start_time:float ->
    end_time:float ->
    length:int ->
    is_partial:bool ->
    metadata:metadata ->
    unit;
  before_output : unit -> unit;
  after_output : unit -> unit;
}

(** The [source] use is to send data frames through the [get] method. *)
class virtual source :
  ?pos:Pos.t
  -> ?name:string
  -> unit
  -> object
       method private mutexify : 'a 'b. ('a -> 'b) -> 'a -> 'b

       (** {1 Naming} *)

       (** Identifier of the source. *)
       method id : string

       method set_name : string -> unit
       method set_id : ?definitive:bool -> string -> unit

       (** Position in script *)
       method pos : Pos.Option.t

       method set_pos : Pos.Option.t -> unit

       (* {1 Liveness type}
          [stype] is the liveness type, telling whether a scheduler is
          fallible or not, i.e. [get] will never fail.
          It is defined by each operator based on its sources' types. *)
       method virtual stype : source_t

       (** {1 Init/shutdown} *)

       (** Register a callback, to be executed when source shuts down. *)
       method on_sleep : (unit -> unit) -> unit

       (** The clock under which the source will run, initially unknown. *)
       method clock : clock_variable

       (** Does the source provide its own synchronization?
           Examples: Alsa, AO, SRT I/O, etc.. This information
           is used at the clock level to decide whether or not
           we should synchronize with the CPU clock after producing
           a frame (for [`Auto] clocks). Please note that in the case
           of multiple sources filling the frame with different notion
           notion of synchronization, there is no consistent notion
           of time or synchronization. In this case (and with a [`Auto]
           clock), we simply decide based on whether there is one [self_sync]
           source or not. This logic should dictate how the method is
           implemented by the various operators. *)
       method virtual self_sync : self_sync

       (** Choose your clock, by adjusting to your children source,
           or anything custom. *)
       method private set_clock : unit

       (** The operator says to the source that he will ask it frames. It may be called multiple times. *)
       method get_ready : source list -> unit

       (** Register a callback when wake_up is called. *)
       method on_wake_up : (unit -> unit) -> unit

       (** Called when the source must be ready and had no active operator,
           means that the source has to initialize. This method is called by
           [get_ready] and not called externally. It should be called only once
           over the course of the source use. *)
       method private wake_up : source list -> unit

       (** Opposite of [get_ready] : the operator no longer needs the source. it may be called multiple times. *)
       method leave : ?failed_to_start:bool -> source -> unit

       (** Register a callback when sleep is called. *)
       method on_sleep : (unit -> unit) -> unit

       method private sleep : unit

       (** Check if a source is up or not. *)
       method is_up : bool

       (** {1 Streaming} *)

       method frame_type : Type.t

       (** This is called when content-type can be computed,
           i.e. either after frame type has been passed from
           the typing system during `check_eval` or at `wake_up` *)
       method content_type_computation_allowed : unit

       method has_content_type : bool

       (** What type of content does this source produce. *)
       method content_type : Frame.content_type

       (** This method fails when content is not PCM. *)
       method private audio_channels : int

       method private video_dimensions : int * int

       (** A buffer that can be used by the source. *)
       method buffer : Generator.t

       (** An empty frame that can be used by the source. *)
       method empty_frame : Frame.t

       (** Number of frames left in the current track. Defaults to -1=infinity. *)
       method virtual remaining : int

       method elapsed : int
       method duration : int

       (** Return the source effectively used to seek, used
           by the muxer to determine if there is a unique seeking
           source. Should return [self] if there isn't a unique
           source. *)
       method virtual seek_source : source

       (** [self#seek_ticks x] skips [x] main ticks.
           returns the number of ticks actually skipped.
           By default it always returns 0, refusing to seek at all.
           That method may be called from any thread, concurrently
           with [#get], so they should not interfere. *)
       method seek : int -> int

       (** The source's last metadata. *)
       method last_metadata : Frame.metadata option

       (** Register a callback to be called on new metadata *)
       method on_metadata : (Frame.metadata -> unit) -> unit

       (** Register a callback to be called on new track. Callback
           is called with the most recent metadata before a given
           track mark. *)
       method on_track : (Frame.metadata -> unit) -> unit

       method streaming_state : streaming_state
       method get_data : Frame.t

       method map_mutable_chunks :
         Frame.field -> (Frame.t -> Frame.t) -> Frame.t

       method get_mutable_field : Frame.field -> Content.data

       method set_data :
         Frame.field ->
         (?offset:int -> ?length:int -> 'a -> Content.data) ->
         'a ->
         Frame.t

       method position : int
       method audio_position : int
       method video_position : int
       method is_partial : bool
       method virtual private can_generate_data : bool
       method virtual private generate_data : Frame.t
       method is_ready : bool

       (** Tells the source to end its current track. *)
       method virtual abort_track : unit

       method is_active : bool

       (* Register callback to be executed on #before_output. *)
       method on_before_output : (unit -> unit) -> unit

       (* Register callback to be executed on #output. *)
       method on_output : (unit -> unit) -> unit

       (* Register callback to be executed on #after_output. *)
       method on_after_output : (unit -> unit) -> unit
       method private has_ticked : unit

       (** {1 Utilities} *)

       method log : Log.t
       method add_watcher : watcher -> unit
     end

(* Entry-points sources, which need to actively perform some task. *)
and virtual active_source :
  ?pos:Pos.t
  -> ?name:string
  -> unit
  -> object
       inherit source

       (** Start a new output round, triggers computation of a new frame. *)
       method virtual output : unit

       (** Do whatever needed when the latency gets too big and is reset. *)
       method virtual reset : unit
     end

(* This is for defining a source which has children *)
class virtual operator :
  ?pos:Pos.t
  -> ?name:string
  -> source list
  -> object
       inherit source
     end

(* Most usual active source: the active_operator, pulling one source's data
 * and outputting it. *)
class virtual active_operator :
  ?pos:Pos.t
  -> ?name:string
  -> source list
  -> object
       inherit active_source
     end

(* Helper to generate data from a sequence of source.
   Data generation calls [get_source] on track marks.
   When frame is partial, a track mark is added unless [merge] is
   set to [true]. *)
class virtual generate_from_multiple_sources :
  merge:(unit -> bool)
  -> track_sensitive:(unit -> bool)
  -> unit
  -> object
       method virtual get_source : reselect:bool -> unit -> source option
       method virtual empty_frame : Frame.t
       method virtual on_after_output : (unit -> unit) -> unit
       method private can_generate_data : bool
       method private generate_data : Frame.t
     end

val has_outputs : unit -> bool
val iterate_new_outputs : (active_source -> unit) -> unit

(** {1 Clocks}
    Tick identifiers are useful (cf. [#get_tick]) but we don't need much
    more than the guarantee that the next tick is different from the
    current one. Booleans should be OK, in any case an overflow on int
    is not a problem. *)

class type clock =
  object
    (** Identifier of the clock. *)
    method id : string

    method sync_mode : sync
    method start : bool
    method stop : unit

    (** Attach an active source to the clock. *)
    method attach : active_source -> unit

    (** Detach active sources that satisfy a given criterion. *)
    method detach : (active_source -> bool) -> unit

    (** true if the source is currently attached to the clock. *)
    method is_attached : active_source -> bool

    (** Manage subordinate clocks *)

    method attach_clock : clock_variable -> unit
    method detach_clock : clock_variable -> unit
    method sub_clocks : clock_variable list

    (** Streaming *)

    method start_outputs : (active_source -> bool) -> unit -> active_source list
    method on_before_output : (unit -> unit) -> unit
    method on_output : (unit -> unit) -> unit
    method on_after_output : (unit -> unit) -> unit
    method get_tick : int
    method end_tick : unit
  end

module Clock_variables : sig
  val to_string : clock_variable -> string

  val create_unknown :
    ?start:bool ->
    sources:active_source list ->
    sub_clocks:clock_variable list ->
    unit ->
    clock_variable

  val create_known : clock -> clock_variable
  val unify : pos:Pos.Option.t -> clock_variable -> clock_variable -> unit
  val forget : clock_variable -> clock_variable -> unit
  val get : clock_variable -> clock
  val is_known : clock_variable -> bool
  val should_start : clock_variable -> bool

  (* This is exported for testing purposes only at the moment. *)
  val subclocks : clock_variable -> clock_variable list
end
