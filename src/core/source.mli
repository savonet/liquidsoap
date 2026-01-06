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

open Mm

(** In [`CPU] mode, synchronization is governed by the CPU clock.

    In [`None] mode, there is no synchronization control. Latency in is governed
    by the time it takes for the sources to produce and output data.

    In [`Auto] mode, synchronization is governed by the CPU unless at least one
    active source is declared [self_sync] in which case latency is delegated to
    this source. A typical example being a source linked to a sound card, in
    which case the source latency is governed by the sound card's clock.

    Another case is synchronous network protocol such as [input.srt]. *)
type sync = [ `Auto | `CPU | `None ]

(** A source can be: passive, active or output. Active sources and outputs are
    animated on each clock cycle. Output are kept around until they are manually
    shutdown. Active and passive sources can be garbage collected if they are
    not connected to any output. The [output] method is called on each clock
    cycle on active sources and outputs. The [reset] method is called when there
    is too much latency. *)
type active = < id : string ; reset : unit ; output : unit >

type source_type = [ `Passive | `Active of active | `Output of active ]

exception Unavailable

type streaming_state =
  [ `Pending | `Unavailable | `Ready of unit -> unit | `Done of Frame.t ]

(** Instrumentation. *)

type metadata = (int * Frame.metadata) list

type watcher = {
  wake_up :
    fallible:bool ->
    source_type:source_type ->
    id:string ->
    ctype:Frame.content_type ->
    clock_id:string ->
    unit;
  sleep : unit -> unit;
  generate_frame :
    start_time:float ->
    end_time:float ->
    length:int ->
    has_track_mark:bool ->
    metadata:metadata ->
    unit;
  before_streaming_cycle : unit -> unit;
  after_streaming_cycle : unit -> unit;
}

(** Callbacks executed when computing frames. *)

type position_callback = {
  mode : [ `Remaining | `Elapsed ];
  allow_partial : bool;
  position : unit -> int;
  on_position : pos:float -> Frame.metadata -> unit;
  mutable executed : bool;
}

type frame_callback = { before : bool; on_frame : unit -> unit }

type on_frame =
  [ `Metadata of Frame.metadata -> unit
  | `Track of Frame.metadata -> unit
  | `Position of position_callback
  | `Frame of frame_callback ]

(** The [source] use is to send data frames through the [get] method. *)
class virtual source :
  ?stack:Pos.t list ->
  ?clock:Clock.t ->
  name:string ->
  unit ->
object
  method private mutexify : 'a 'b. ('a -> 'b) -> 'a -> 'b

  (** {1 Naming} *)

  (** Identifier of the source. *)
  method id : string

  method set_id : ?force:bool -> string -> unit

  (** Position in script *)
  method pos : Pos.Option.t

  method stack : Pos.t list
  method set_stack : Pos.t list -> unit
  method stack_unifier : Pos.t list Unifier.t

  (** {1 Source characteristics} *)

  (** If [false], [is_ready] should always return [true]. *)
  method virtual fallible : bool

  (** The source type. Defaults to [`Passive] *)
  method source_type : source_type

  (** [true] if the source needs to be animated on each clock tick. *)
  method active : bool

  (** {1 Init/shutdown} *)

  (** Register a callback, to be executed when source shuts down. *)
  method on_sleep : (unit -> unit) -> unit

  (** The clock under which the source will run. *)
  method clock : Clock.t

  (** Does the source provide its own synchronization? Examples: Alsa, AO, SRT
      I/O, etc.. This information is used at the clock level to decide whether
      or not we should synchronize with the CPU clock after producing a frame
      (for [`Auto] clocks). Please note that in the case of multiple sources
      filling the frame with different notion notion of synchronization, there
      is no consistent notion of time or synchronization. In this case (and with
      a [`Auto] clock), we simply decide based on whether there is one
      [self_sync] source or not. This logic should dictate how the method is
      implemented by the various operators. *)
  method virtual self_sync : Clock.self_sync

  method source_sync : bool -> Clock.sync_source option

  (** Register a callback when wake_up is called. *)
  method on_wake_up : (unit -> unit) -> unit

  (** Called when the source must be ready. Each call to [wake_up] must be
      matched by a corresponding call to [sleep] with the returned activation to
      allow the source to sleep and be collected. Can be called multiple times
  *)
  method wake_up : Clock.source -> Clock.activation

  (** Register a callback when sleep is called. *)
  method on_sleep : (unit -> unit) -> unit

  (** Called when the source can release all its resources. Can be called
      concurrently and multiple times. *)
  method sleep : Clock.activation -> unit

  (** Return the list of the source's activations. *)
  method activations : Clock.activation list

  (** Check if a source is up or not. *)
  method is_up : bool

  (** {1 Streaming} *)

  method frame_type : Type.t

  (** This is called when content-type can be computed, i.e. either after frame
      type has been passed from the typing system during `check_eval` or at
      `wake_up` *)
  method content_type_computation_allowed : unit

  method has_content_type : bool

  (** What type of content does this source produce. *)
  method content_type : Frame.content_type

  (** This method fails when content is not PCM. *)
  method private audio_channels : int

  method private samplerate : float
  method private video_dimensions : int * int

  (** A buffer that can be used by the source. *)
  method buffer : Generator.t

  method private generate_video :
    ?create:(pos:int -> width:int -> height:int -> unit -> Video.Canvas.image) ->
    field:Frame.Fields.field ->
    int ->
    Content.Video.data

  method last_image : Frame.Fields.field -> Video.Canvas.image

  method private nearest_image :
    pos:int ->
    last_image:Video.Canvas.image ->
    Content.Video.data ->
    Video.Canvas.image

  (** An empty frame that can be used by the source. *)
  method empty_frame : Frame.t

  (** An empty frame with a track mark. *)
  method end_of_track : Frame.t

  (** Number of main ticks left in the current track. Defaults to -1=unknown. *)
  method virtual remaining : int

  (* Elapsed time since the last track mark. *)
  method elapsed : int

  (* Estimated total duration of the current track. -1=unknown. *)
  method duration : int

  (** Return the source effectively used to seek. Used by the muxer to determine
      if there is a unique seeking source. Should return [self] if there isn't
      such a unique source. *)
  method virtual effective_source : source

  (** [self#seek_ticks x] skips [x] main ticks. returns the number of ticks
      actually skipped. By default it always returns 0, refusing to seek at all.
      That method may be called from any thread, concurrently with [#get_frame],
      so they should not interfere. *)
  method seek : int -> int

  (** The source's last metadata. *)
  method last_metadata : (int * Frame.metadata) option

  method clear_last_metadata : unit
  method reset_last_metadata_on_track : bool
  method set_reset_last_metadata_on_track : bool -> unit

  (** Register a server command. The command is registered when the source wakes
      up under its own id as namespace and deregistered when it goes down. *)
  method register_command :
    ?usage:string -> descr:string -> string -> (string -> string) -> unit

  (** Register a callback to be called when computing frames. *)
  method on_frame : on_frame -> unit

  (** These two are used by [generate_from_multiple_sources] and should not be
      used otherwise. *)
  method private execute_on_track : Frame.t -> unit

  method private set_last_metadata : Frame.t -> unit

  (** Insert a metadata at the beginning of the new frame. Also add a track mark
      when [new_track] is [true] *)
  method insert_metadata : new_track:bool -> Frame.metadata -> unit

  (** Sources must implement this method. It should return [true] when the
      source can produce data during the current streaming cycle. *)
  method virtual private can_generate_frame : bool

  method on_before_streaming_cycle : (unit -> unit) -> unit
  method on_after_streaming_cycle : (unit -> unit) -> unit

  (** Sources must implement this method. It should return the data produced
      during the current streaming cycle. Sources are responsible for producing
      as much data as possible, up-to the frame size setting. *)
  method virtual private generate_frame : Frame.t

  (** This method is based on [can_generate_frame] and has the same value
      through the whole streaming cycle. *)
  method is_ready : bool

  (** If the source is ready, this method computes the frame generated by the
      source during the current streaming cycle. Returned value is cached and
      should be the same throughout the whole streaming cycle. *)
  method get_frame : Frame.t

  (** This method passes the frame returned by [#get_frame] to the given callback.
           The callback should return the portion of the frame (of the form: [start, end))
           that was effectively used. This method is used when a consumer of the source's data
           only uses an initial chunk of the frame. In this case, the remaining data is cached
           whenever possible and returned during the next streaming cycle. Final returned value
           is the same as the partial chunk returned from the callback for easy method call chaining.

           Calling this method is equivalent to doing: {[
             let frame = Frame.slice source#peek_frame len in
             source#consumed (Frame.position frame);
             frame
           ]} *)
  method get_partial_frame : (Frame.t -> Frame.t) -> Frame.t

  (** Check a frame without consuming any of its data. *)
  method peek_frame : Frame.t

  (** Manually mark amount of consumed data from the source. *)
  method consumed : int -> unit

  (** This method requests a specific field of the frame that can be mutated. It
      is used by a consumer of the source that will modify the source's data
      (e.g. [amplify]). The source will do its best to minimize data copy
      according to the streaming context. Typically, if there is only one
      consumer of the source's data, it should be safe to pass its data without
      copying it. *)
  method get_mutable_content : Frame.field -> Content.data

  (** This method is the same as [#get_mutable_content] but returns a full frame
      with the requested mutable field included. *)
  method get_mutable_frame : Frame.field -> Frame.t

  (** By convention, frames produced during the streaming cycle can only have at
      most one track mark. In case of multiple track marks (which most likely
      indicate a programming problem), all subsequent track marks past the first
      one are dropped.

      This function returns a pair: [(initial_frame, new_track option)] of an
      initial frame and, if a track mark is present in the frame, the optional
      portion of the frame contained after this track mark.

      This method can be used to implement operations that should be aware of
      new tracks. *)
  method private split_frame : Frame.t -> Frame.t * Frame.t option

  (** This method is a convenience function to set some data. It returns the
      frame produced by the source during the current streaming cycle with the
      given field data replaced by the data passed to the function with length
      set as the frame's length. *)
  method set_frame_data :
    Frame.field ->
    (?offset:int -> ?length:int -> 'a -> Content.data) ->
    'a ->
    Frame.t

  (** Various information related to the current frame. *)

  method frame_position : int
  method frame_audio_position : int
  method frame_has_track_mark : bool
  method frame_track_mark : int option
  method frame_metadata : (int * Frame.Metadata.t) list

  (** Tells the source to end its current track. *)
  method virtual abort_track : unit

  (** {1 Utilities} *)

  method log : Log.t
  method add_watcher : watcher -> unit
end

(* Entry-points sources, which need to actively perform some task. *)
and virtual active_source :
  ?stack:Pos.t list ->
  ?clock:Clock.t ->
  name:string ->
  unit ->
object
  inherit source

  (** Do whatever needed when the latency gets too big and is reset. *)
  method virtual reset : unit

  method virtual output : unit
end

(* This is for defining a source which has children *)
class virtual operator :
  ?stack:Pos.t list ->
  ?clock:Clock.t ->
  name:string ->
  source list ->
object
  inherit source
end

(* Most usual active source: the active_operator, pulling one source's data
   and outputting it. *)
class virtual active_operator :
  ?stack:Pos.t list ->
  ?clock:Clock.t ->
  name:string ->
  source list ->
object
  inherit active_source
end

(** Type governing whether or not the same source should be re-selected.
    - [`Force] means that the operator should at least try to see if a new
      source should be selected.
    - [`Ok] means that the operator should re-select the current source whenever
      possible.
    - [`After_position p] means that the operator should re-select the current
      source only if it can produce data past the given position The
      [generate_from_multiple_sources] implements a [can_reselect] that can be
      called with the currently selected source to validate this logic. *)
type reselect = [ `Ok | `Force | `After_position of int ]

(* Helper to generate data from a sequence of source.
   Data generation calls [get_source] on track marks.
   When frame is partial, a track mark is added unless [merge] is
   set to [true]. *)
class virtual generate_from_multiple_sources :
  merge:(unit -> bool) ->
  track_sensitive:(unit -> bool) ->
  unit ->
object
  method virtual get_source : reselect:reselect -> unit -> source option
  method virtual split_frame : Frame.t -> Frame.t * Frame.t option
  method virtual empty_frame : Frame.t
  method virtual private execute_on_track : Frame.t -> unit
  method virtual private set_last_metadata : Frame.t -> unit
  method virtual log : Log.t
  method virtual id : string
  method private can_reselect : reselect:reselect -> source -> bool
  method private can_generate_frame : bool
  method private generate_frame : Frame.t
  method virtual reset_last_metadata_on_track : bool
  method virtual clear_last_metadata : unit
end
