(** This module provides an API to AVfilter. *)

open Avutil

type config

type ground_arg =
  [ `String of string
  | `Int of int
  | `Int64 of int64
  | `Float of float
  | `Rational of rational ]

type valued_arg = [ ground_arg | `Array of ground_arg list ]
type args = [ `Flag of string | `Pair of string * valued_arg ]
type ('a, 'b) av = { audio : 'a; video : 'b }
type ('a, 'b) io = { inputs : 'a; outputs : 'b }

(** (attached/unattached, audio/video, input/output) pad *)
type ('a, 'b, 'c) pad

type ('a, 'b) pads =
  (('a, [ `Audio ], 'b) pad list, ('a, [ `Video ], 'b) pad list) av

type flag =
  [ `Dynamic_inputs
  | `Dynamic_outputs
  | `Slice_threads
  | `Support_timeline_generic
  | `Support_timeline_internal ]

type 'a filter = {
  name : string;
  description : string;
  options : Avutil.Options.t;
  flags : flag list;
  io : (('a, [ `Input ]) pads, ('a, [ `Output ]) pads) io;
}

type 'a input = [ `Frame of 'a frame | `Flush ] -> unit
type 'a context
type 'a output = { context : 'a context; handler : unit -> 'a frame }
type 'a entries = (string * 'a) list
type inputs = ([ `Audio ] input entries, [ `Video ] input entries) av
type outputs = ([ `Audio ] output entries, [ `Video ] output entries) av
type t = (inputs, outputs) io

(* Output context. *)
val time_base : _ context -> Avutil.rational
val frame_rate : [ `Video ] context -> Avutil.rational
val width : [ `Video ] context -> int
val height : [ `Video ] context -> int
val pixel_aspect : [ `Video ] context -> Avutil.rational option
val pixel_format : [ `Video ] context -> Avutil.Pixel_format.t
val channels : [ `Audio ] context -> int
val channel_layout : [ `Audio ] context -> Avutil.Channel_layout.t
val sample_rate : [ `Audio ] context -> int
val sample_format : [ `Audio ] context -> Avutil.Sample_format.t
val set_frame_size : [ `Audio ] context -> int -> unit

(** Get the separator character used for array-type options of a filter. Raises
    [Failure] if the option does not exist or does not support arrays (e.g. on
    FFmpeg < 7). *)
val get_array_separator : filter_name:string -> option_name:string -> char

exception Exists

(** Filter list. *)
val filters : [ `Unattached ] filter list

val find : string -> [ `Unattached ] filter
val find_opt : string -> [ `Unattached ] filter option

(** Buffers (input). *)
val abuffer : [ `Unattached ] filter

val buffer : [ `Unattached ] filter

(** Sinks (output). *)
val abuffersink : [ `Unattached ] filter

val buffersink : [ `Unattached ] filter

(** Pad name. *)
val pad_name : _ pad -> string

(** Name of the filter of which this pad is an instance *)
val filter_name : _ pad -> string

(** Initiate a filter graph configuration. *)
val init : unit -> config

(** Attach a filter to a filter graph configuration. Raises [Exists] if there is
    already a filter by that name in the graph. Number of inputs or outputs can
    change from the filter's specifications, in particular if the filter has the
    [`Dynamic_input] or [`Dynamic_output] flag set. *)
val attach :
  ?args:args list ->
  name:string ->
  [ `Unattached ] filter ->
  config ->
  [ `Attached ] filter

(** Link two filter pads. *)
val link :
  ([ `Attached ], 'a, [ `Output ]) pad ->
  ([ `Attached ], 'a, [ `Input ]) pad ->
  unit

type command_flag = [ `Fast ]

(** Send a command to a attached filter pad. *)
val process_command :
  ?flags:command_flag list ->
  cmd:string ->
  ?arg:string ->
  [ `Attached ] filter ->
  string

(** Parse a graph described by a string and attach outputs/inputs to it. *)
type ('a, 'b, 'c) parse_node = {
  node_name : string;
  node_args : args list option;
  node_pad : ('a, 'b, 'c) pad;
}

type ('a, 'b) parse_av =
  ( ('a, [ `Audio ], 'b) parse_node list,
    ('a, [ `Video ], 'b) parse_node list )
  av

type 'a parse_io = (('a, [ `Input ]) parse_av, ('a, [ `Output ]) parse_av) io

val parse : [ `Attached ] parse_io -> string -> config -> unit

(** Check validity and configure all the links and formats in the graph and
    return its outputs and outputs. *)
val launch : config -> t

module Utils : sig
  type audio_converter

  type audio_params = {
    sample_rate : int;
    channel_layout : Avutil.Channel_layout.t;
    sample_format : Avutil.Sample_format.t;
  }

  val init_audio_converter :
    ?out_params:audio_params ->
    ?out_frame_size:int ->
    in_time_base:Avutil.rational ->
    in_params:audio_params ->
    unit ->
    audio_converter

  val time_base : audio_converter -> Avutil.rational

  val convert_audio :
    audio_converter ->
    (Avutil.audio Avutil.frame -> unit) ->
    [ `Frame of Avutil.audio Avutil.frame | `Flush ] ->
    unit
end
