(** Bindings to libavfilter. The interface is the one of spec/avfilter.md §4. *)

open Avutil

(** A filter graph. It is used in two phases.

    {!init} creates it in the configuring phase: {!attach} creates filter
    instances in it, {!link} connects their pads and {!parse} adds the filters
    and links of a textual description. {!launch} then configures the graph and
    returns its endpoints: the functions that push frames into its buffer
    sources and pull frames from its buffer sinks. Once the graph is launched,
    [attach], [link], [parse] and [launch] raise [Error (`Failure _)].
    {!process_command} works in both phases.

    A failed {!parse} or {!launch} leaves the graph failed: every operation on
    it, and on a filter, pad, input or output that belongs to it, then raises
    [Error (`Failure "Object failed!")]. A program that needs the graph again
    builds a new one.

    The value owns the native graph and its filter instances. They are released
    when the graph and every attached filter, attached pad, input, output and
    sink context obtained from it have been collected.

    Under concurrent use, an operation that conflicts with one running on the
    same graph raises [Error (`Failure "Object in use!")] at once; it does not
    wait. The sink accessors run together with one another. Every other
    operation, pushing and pulling included, runs alone: pushing to a source
    while another thread pulls from a sink of the same graph makes one of the
    two raise. *)
type config

(** The value of a filter option. It is rendered to text for FFmpeg: [`String s]
    is [s] verbatim, [`Int] and [`Int64] are in decimal, [`Float] is a decimal
    text that FFmpeg reads back to the same value and [`Rational {num; den}] is
    [num/den].

    Nothing is quoted or escaped. A string that contains a colon, an equal sign,
    a single quote, a backslash or the array separator is written by the caller
    in FFmpeg's option-string syntax. *)
type ground_arg =
  [ `String of string
  | `Int of int
  | `Int64 of int64
  | `Float of float
  | `Rational of rational ]

(** The value of a filter option: a single value, or [`Array values] for an
    array-typed option. The elements of an array are rendered in list order and
    joined with the separator {!get_array_separator} returns for the filter and
    the option. *)
type valued_arg = [ ground_arg | `Array of ground_arg list ]

(** One argument of a filter. [`Pair (key, value)] is rendered as [key=value].
    [`Flag s] is rendered as [s] alone: a value with no key, which FFmpeg binds
    to the options of the filter in their declaration order and rejects when it
    follows a [`Pair]. *)
type args = [ `Flag of string | `Pair of string * valued_arg ]

(** An audio part and a video part. *)
type ('a, 'b) av = { audio : 'a; video : 'b }

(** An input part and an output part. *)
type ('a, 'b) io = { inputs : 'a; outputs : 'b }

(** A pad of a filter. ['a] is [[ `Unattached ]] for a pad of a filter of the
    registry and [[ `Attached ]] for a pad of a filter instance of a graph. ['b]
    is the media type, [[ `Audio ]] or [[ `Video ]]. ['c] is the direction,
    [[ `Input ]] or [[ `Output ]].

    An attached pad keeps its graph alive. *)
type ('a, 'b, 'c) pad

(** The pads of one direction of a filter, split by media type, each list in
    ascending pad order. A pad that is neither audio nor video is in neither
    list. *)
type ('a, 'b) pads =
  (('a, [ `Audio ], 'b) pad list, ('a, [ `Video ], 'b) pad list) av

(** A property FFmpeg declares for a filter.

    - [`Dynamic_inputs]: the filter may add input pads when it is initialised,
      depending on its arguments.
    - [`Dynamic_outputs]: the filter may add output pads when it is initialised,
      depending on its arguments.
    - [`Slice_threads]: the filter supports multithreading by splitting frames
      into parts processed concurrently.
    - [`Support_timeline_generic]: the filter supports FFmpeg's generic [enable]
      expression option. While the expression is false, frames pass through
      unchanged.
    - [`Support_timeline_internal]: as [`Support_timeline_generic], with the
      filter itself handling the frames while the expression is false. *)
type flag =
  [ `Dynamic_inputs
  | `Dynamic_outputs
  | `Slice_threads
  | `Support_timeline_generic
  | `Support_timeline_internal ]

(** A filter. A [[ `Unattached ] filter] comes from the registry ({!filters},
    {!find}, {!abuffer} and the like) and describes a filter FFmpeg provides. A
    [[ `Attached ] filter] comes from {!attach} and designates one instance of
    that filter in a graph, which it keeps alive.

    An operation that takes an attached filter finds the instance through the
    pads of [io]. It raises [Error (`Failure _)] when the record has no attached
    pad and when its pads belong to several instances.

    - [name]: the name of the filter, not of an instance.
    - [description]: FFmpeg's description, the empty string when it gives none.
    - [options]: the options of the filter, for inspection through
      [Avutil.Options]. Their values are set through the [args] of {!attach}.
    - [io]: the static pads of an unattached filter, the actual pads of an
      attached one. *)
type 'a filter = {
  name : string;
  description : string;
  options : Avutil.Options.t;
  flags : flag list;
  io : (('a, [ `Input ]) pads, ('a, [ `Output ]) pads) io;
}

(** The function that feeds one buffer source of a launched graph.

    [`Frame frame] gives the frame to the source. The source takes a reference
    of its own: the caller's frame is unchanged and stays usable.

    [`Flush] marks the end of the stream on that source. A flushed source
    accepts no further frame: [`Frame _] then raises [Error `Eof].

    Each call releases the OCaml runtime lock while FFmpeg works. It raises
    [Avutil.Error] when FFmpeg reports a failure. *)
type 'a input = [ `Frame of 'a frame | `Flush ] -> unit

(** A buffer sink of a launched graph, as read by the accessors below. It keeps
    the graph alive. The accessors raise [Error (`Failure "Object failed!")] on
    a failed graph and [Error (`Failure "Object in use!")] while another thread
    changes the graph, pushes to it or pulls from it. *)
type 'a context

(** One buffer sink of a launched graph.

    - [context]: the sink, for the accessors.
    - [handler]: [handler ()] returns the next frame the sink has ready. Each
      call returns a new frame value; the frame data is not copied.

    It raises [Error `Eagain] when no frame is ready yet. It raises [Error `Eof]
    once the sink is drained after the end of the stream was marked with
    [`Flush]. Both are part of normal operation. Any other FFmpeg failure raises
    [Avutil.Error].

    Each call releases the OCaml runtime lock while FFmpeg works. *)
type 'a output = { context : 'a context; handler : unit -> 'a frame }

(** Endpoints by filter instance name, in the order the instances were created
    in the graph. *)
type 'a entries = (string * 'a) list

(** The buffer sources of a launched graph. *)
type inputs = ([ `Audio ] input entries, [ `Video ] input entries) av

(** The buffer sinks of a launched graph. *)
type outputs = ([ `Audio ] output entries, [ `Video ] output entries) av

(** The endpoints of a launched graph, as returned by {!launch}. *)
type t = (inputs, outputs) io

(** The time base of the sink: the unit of the timestamps of the frames it
    delivers. *)
val time_base : _ context -> Avutil.rational

(** The frame rate of the sink. *)
val frame_rate : [ `Video ] context -> Avutil.rational

(** The width of the frames of the sink, in pixels. *)
val width : [ `Video ] context -> int

(** The height of the frames of the sink, in pixels. *)
val height : [ `Video ] context -> int

(** The sample aspect ratio of the frames of the sink. [None] when it is
    unknown, which FFmpeg reports as a ratio of numerator 0. *)
val pixel_aspect : [ `Video ] context -> Avutil.rational option

(** The pixel format of the frames of the sink.
    @raise Avutil.Error
      with [`Failure _] when the format is one this build of the bindings has no
      constructor for. *)
val pixel_format : [ `Video ] context -> Avutil.Pixel_format.t

(** The number of channels of the frames of the sink. *)
val channels : [ `Audio ] context -> int

(** The channel layout of the frames of the sink, as an independent copy.
    @raise Avutil.Error when FFmpeg fails to give the layout. *)
val channel_layout : [ `Audio ] context -> Avutil.Channel_layout.t

(** The sample rate of the frames of the sink, in Hz. *)
val sample_rate : [ `Audio ] context -> int

(** The sample format of the frames of the sink.
    @raise Avutil.Error
      with [`Failure _] when the format is one this build of the bindings has no
      constructor for. *)
val sample_format : [ `Audio ] context -> Avutil.Sample_format.t

(** [set_frame_size sink size] makes the sink deliver frames of exactly [size]
    samples per channel, except the last one. It excludes every other operation
    on the graph while it runs.
    @raise Avutil.Error with [`Failure _] when [size] is below 1. *)
val set_frame_size : [ `Audio ] context -> int -> unit

(** The character that separates the elements of an array-typed option of a
    filter: the separator the option declares, and [','] when it declares none.
    [filter_name] is the name of a filter FFmpeg registers, the four buffer
    filters included; [option_name] is the name of one of its options. {!attach}
    calls it to render an [`Array] argument.
    @raise Avutil.Error
      with [`Failure _] when FFmpeg has no filter of that name, when the filter
      has no option of that name and when the option is not array-typed. *)
val get_array_separator : filter_name:string -> option_name:string -> char

(** Raised by {!attach} when the graph already has a filter instance of the
    requested name. *)
exception Exists

(** Every filter FFmpeg registers except the four buffer filters below, sorted
    by name in ascending byte order. The list is computed once, when the module
    is loaded. *)
val filters : [ `Unattached ] filter list

(** [find name] is the element of {!filters} of that name. The four buffer
    filters are reached through {!abuffer}, {!buffer}, {!abuffersink} and
    {!buffersink}, not through [find].
    @raise Not_found when {!filters} has no filter of that name. *)
val find : string -> [ `Unattached ] filter

(** As {!find}, with [None] when {!filters} has no filter of that name. *)
val find_opt : string -> [ `Unattached ] filter option

(** The audio buffer source: the filter through which a program feeds audio
    frames to a graph. Each of its instances gives one entry of [inputs.audio]
    in the result of {!launch}. *)
val abuffer : [ `Unattached ] filter

(** The video buffer source: the filter through which a program feeds video
    frames to a graph. Each of its instances gives one entry of [inputs.video]
    in the result of {!launch}. *)
val buffer : [ `Unattached ] filter

(** The audio buffer sink: the filter from which a program takes the audio
    frames a graph produces. Each of its instances gives one entry of
    [outputs.audio] in the result of {!launch}. *)
val abuffersink : [ `Unattached ] filter

(** The video buffer sink: the filter from which a program takes the video
    frames a graph produces. Each of its instances gives one entry of
    [outputs.video] in the result of {!launch}. *)
val buffersink : [ `Unattached ] filter

(** The name of the pad, the empty string when FFmpeg gives none. *)
val pad_name : _ pad -> string

(** The name of the filter the pad belongs to: the filter, not the instance. *)
val filter_name : _ pad -> string

(** A new, empty graph in the configuring phase. Every option of the graph keeps
    FFmpeg's default. *)
val init : unit -> config

(** [attach ?args ~name filter graph] creates an instance of [filter] in [graph]
    and initialises it. The options of a filter are set here and nowhere else.

    The result has the name, description, options and flags of [filter], and the
    actual pads of the instance in [io]. Their number can differ from the pads
    of [filter]: some filters create pads when they are initialised.

    The call releases the OCaml runtime lock while FFmpeg creates the instance.
    When it raises, the graph is unchanged.
    @param args
      The arguments of the instance, none by default. They are rendered in list
      order and joined with colons into the option string FFmpeg parses; see
      {!args} and {!ground_arg}.
    @param name
      The name of the instance, unique in the graph. It is the key of the
      instance in the result of {!launch} when [filter] is a buffer source or
      sink.
    @raise Exists
      when the graph already has an instance of that name, whether [attach] or
      {!parse} created it.
    @raise Avutil.Error
      with [`Filter_not_found] when FFmpeg has no filter of the name of
      [filter]; with the FFmpeg error when FFmpeg rejects the arguments, an
      unknown option among them, or fails to create the instance; with
      [`Failure _] when the graph is launched and when an [`Array] argument
      names an option {!get_array_separator} rejects. *)
val attach :
  ?args:args list ->
  name:string ->
  [ `Unattached ] filter ->
  config ->
  [ `Attached ] filter

(** [link output input] connects an output pad of a filter instance to an input
    pad of another. Both pads have the same media type and belong to the same
    graph, which is in the configuring phase.
    @raise Avutil.Error
      with [`Failure _] when the pads belong to different graphs and when the
      graph is launched; with the FFmpeg error when FFmpeg refuses the link, for
      instance because a pad is already linked. *)
val link :
  ([ `Attached ], 'a, [ `Output ]) pad ->
  ([ `Attached ], 'a, [ `Input ]) pad ->
  unit

(** A flag of {!process_command}. With [`Fast], FFmpeg executes the command only
    when it is fast for the filter to do so. *)
type command_flag = [ `Fast ]

(** [process_command ?flags ~cmd ?arg filter] sends the command [cmd] with the
    argument [arg] to one filter instance and returns the answer the filter
    writes, cut at a fixed length. It works before and after {!launch}, and is
    the one way to control a filter of a running graph.

    The call releases the OCaml runtime lock and excludes every other operation
    on the graph while it runs.
    @param flags None by default.
    @param arg The empty string by default.
    @raise Avutil.Error
      with the FFmpeg error when the command fails; with [`Failure _] when
      [filter] has no attached pad. *)
val process_command :
  ?flags:command_flag list ->
  cmd:string ->
  ?arg:string ->
  [ `Attached ] filter ->
  string

(** An open end of a graph description given to {!parse}, and the pad it is
    connected to.

    - [node_name]: the label of the open end in the description.
    - [node_args]: ignored.
    - [node_pad]: the pad of an attached filter the open end is linked to. *)
type ('a, 'b, 'c) parse_node = {
  node_name : string;
  node_args : args list option;
  node_pad : ('a, 'b, 'c) pad;
}

(** Nodes split by media type. *)
type ('a, 'b) parse_av =
  ( ('a, [ `Audio ], 'b) parse_node list,
    ('a, [ `Video ], 'b) parse_node list )
  av

(** The open ends of a graph description, named as in FFmpeg's API, from the
    point of view of the description.

    [inputs] holds input pads of attached filters, typically the input of a
    sink: the open outputs of the description with that label are linked to
    them. [outputs] holds output pads of attached filters, typically the output
    of a source: they feed the open inputs of the description with that label.
*)
type 'a parse_io = (('a, [ `Input ]) parse_av, ('a, [ `Output ]) parse_av) io

(** [parse nodes description graph] adds the filters and links of [description],
    a graph description in FFmpeg's filtergraph syntax, to [graph], and connects
    the open ends of the description to the pads of [nodes].

    The filters the description creates belong to the graph and no
    [[ `Attached ] filter] value is returned for them. Their instance names
    count for {!Exists}. The buffer sources and sinks among them are endpoints
    in the result of {!launch}.

    The call releases the OCaml runtime lock while FFmpeg parses.
    @raise Avutil.Error
      with [`Failure _], before anything is parsed, when a pad of [nodes]
      belongs to another graph and when the graph is launched; with the FFmpeg
      error when parsing fails. A parsing failure leaves the graph failed:
      FFmpeg frees every filter of it, those created before the call included.
*)
val parse : [ `Attached ] parse_io -> string -> config -> unit

(** [launch graph] configures the graph, which enters the running phase, and
    returns its endpoints.

    The result lists every buffer source and every buffer sink of the graph by
    instance name, whichever of {!attach} and {!parse} created it, in the order
    the instances were created. [inputs] holds one {!input} function per buffer
    source and [outputs] one {!output} per buffer sink.

    Frames are then pushed with the input functions and pulled with the output
    handlers. A typical loop pushes a frame, then pulls from every sink until it
    raises [Error `Eagain]. At the end of the stream it pushes [`Flush] to every
    source and pulls until [Error `Eof].

    The call releases the OCaml runtime lock while FFmpeg configures the graph.
    @raise Avutil.Error
      with the FFmpeg error when the configuration fails, which leaves the graph
      failed; with [`Failure _] when the graph is already launched. *)
val launch : config -> t

(** An audio converter built on a private filter graph: it re-frames audio to a
    fixed frame size and, optionally, converts it to another sample rate,
    channel layout and sample format. *)
module Utils : sig
  (** A converter. It owns a launched graph made of an audio buffer source, a
      resampling filter when output parameters were given, and an audio buffer
      sink. *)
  type audio_converter

  (** The parameters of an audio stream. [sample_rate] is in Hz. *)
  type audio_params = {
    sample_rate : int;
    channel_layout : Avutil.Channel_layout.t;
    sample_format : Avutil.Sample_format.t;
  }

  (** Creates a converter and launches its graph.
      @param out_params
        The parameters of the frames delivered. By default they are the ones of
        the input and no resampling filter is used.
      @param out_frame_size
        When given, every frame delivered has exactly that many samples per
        channel, except the last one after a flush. By default the frames have
        the sizes the graph produces.
      @param in_time_base The time base of the timestamps of the input frames.
      @param in_params The parameters of the input frames.
      @raise Avutil.Error
        with [`Filter_not_found] when [out_params] is given and FFmpeg lacks its
        [aresample] filter; with [`Failure _] when [out_frame_size] is below 1;
        with the FFmpeg error of the step that failed otherwise. *)
  val init_audio_converter :
    ?out_params:audio_params ->
    ?out_frame_size:int ->
    in_time_base:Avutil.rational ->
    in_params:audio_params ->
    unit ->
    audio_converter

  (** The time base of the timestamps of the frames the converter delivers, read
      once when the converter is created. *)
  val time_base : audio_converter -> Avutil.rational

  (** [convert_audio converter callback input] pushes a frame, or the end of the
      stream with [`Flush], and calls [callback] on every frame that becomes
      available, in order.

      It first delivers the frames the converter already has ready, then pushes
      [input], then delivers until the converter has no frame ready. A call that
      delivers no frame is normal. After [`Flush] every remaining frame is
      delivered, and a later [`Frame _] raises [Error `Eof].

      [callback] runs on the caller's thread, between native calls, and may use
      the converter. An exception it raises propagates unchanged; the frames not
      yet delivered stay in the converter and the next [convert_audio] delivers
      them first.

      The pushed frame is unchanged and stays usable. Pushing and pulling
      release the OCaml runtime lock.
      @raise Avutil.Error when pushing or pulling fails. *)
  val convert_audio :
    audio_converter ->
    (Avutil.audio Avutil.frame -> unit) ->
    [ `Frame of Avutil.audio Avutil.frame | `Flush ] ->
    unit

  (** A filter to attach: its name and its arguments. *)
  type filter_spec = string * args list

  (** The filter that performs one step of {!Avutil.Display_matrix.transforms}:
      [transpose], [hflip], [vflip] or [rotate]. *)
  val filter_of_transform : Avutil.Display_matrix.transform -> filter_spec

  (** The [crop] filter that discards the borders, [None] when there is nothing
      to discard. *)
  val filter_of_cropping : Avutil.cropping -> filter_spec option

  (** The picture a display chain produces: its size, its pixel aspect ratio,
      and the filters that produce it. *)
  type display_layout = {
    width : int;
    height : int;
    pixel_aspect : Avutil.rational option;
    filters : filter_spec list;
  }

  (** What the bindings do not decide in the caller's place:
      - [`Invalid_cropping]: the cropping is negative or leaves no picture;
      - [`Odd_rotation a]: the display matrix turns the picture by [a] degrees
        clockwise, which is not a quarter turn, so the size of the result is a
        choice. *)
  type undecided =
    [ `Invalid_cropping of Avutil.cropping | `Odd_rotation of float ]

  (** [display_layout ?cropping ?display_matrix ?pixel_aspect ~width ~height ()]
      is the layout of a [width]x[height] picture once shown upright and
      cropped. [filters] is the chain that does it, in the order to link them:
      {!filter_of_cropping}, then {!filter_of_transform} on each step the
      display matrix asks for. [cropping] is the container's, found in the
      stream parameters:
      [Avcodec.Packet_side_data.cropping (Avcodec.params_side_data params)]. A
      quarter turn swaps the width and the height and inverts the pixel aspect
      ratio. It is [`Undecided] where there is no single right answer; the
      caller then decides, for instance from {!filter_of_transform}. *)
  val display_layout :
    ?cropping:Avutil.cropping ->
    ?display_matrix:Avutil.Display_matrix.t ->
    ?pixel_aspect:Avutil.rational ->
    width:int ->
    height:int ->
    unit ->
    [ `Layout of display_layout | `Undecided of undecided ]

  (** What {!convert_display} does not decide: {!undecided}, and a frame in a
      hardware pixel format, which no software filter can transform. *)
  type undecided_frame = [ undecided | `Hardware_frame ]

  (** The arguments of a [buffer] source that takes frames of the given format,
      with timestamps in [time_base]: the size, the pixel format, and the pixel
      aspect, colour space and colour range when the format has them. *)
  val video_buffer_args :
    time_base:Avutil.rational -> Avutil.Video.frame_format -> args list

  (** A launched graph that runs video frames through filters, one after the
      other: [chain_source] takes the frames and the end of the stream,
      [chain_sink] delivers them. *)
  type video_chain = {
    chain_source : [ `Video ] input;
    chain_sink : [ `Video ] output;
  }

  (** [video_chain ~time_base format filters] is the chain of [filters], in
      order, for frames of [format] with timestamps in [time_base].

      @raise Avutil.Error
        with [`Filter_not_found] when a filter is unknown, and with FFmpeg's
        error when the graph cannot be built. *)
  val video_chain :
    time_base:Avutil.rational ->
    Avutil.Video.frame_format ->
    filter_spec list ->
    video_chain

  (** A converter that delivers decoded video frames upright and cropped. It
      reads the display matrix of each frame and runs the filters of
      {!display_layout} on a private graph, rebuilt when the matrix or the
      {!Avutil.Video.frame_format} of the frames changes. Two threads must not
      use one at the same time. *)
  type display_converter

  (** [init_display_converter ?cropping ?on_undecided ~time_base ()] is a
      converter for frames whose timestamps are in [time_base]. [cropping] is as
      in {!display_layout}.

      [on_undecided] is asked what to do with the frames the converter cannot
      decide for, once per run of frames with the same question. [Some filters]
      is a decision: the frames go through those filters and lose their display
      matrix. [None] is no decision: the frames are delivered untouched, display
      matrix included, for something downstream to act on. By default it logs a
      warning through {!Avutil.Log.log} and answers [None]. Filters that cannot
      be built are no decision either: the converter logs a warning and delivers
      the frames untouched.

      [ignore] lists the properties of the frame format whose change does not
      rebuild the graph, as in {!Avutil.Video.same_frame_format}. *)
  val init_display_converter :
    ?cropping:Avutil.cropping ->
    ?on_undecided:(undecided_frame -> filter_spec list option) ->
    ?ignore:Avutil.Video.frame_property list ->
    time_base:Avutil.rational ->
    unit ->
    display_converter

  (** [convert_display converter callback input] pushes a frame, or the end of
      the stream, and calls [callback] on every frame that becomes available, in
      order.

      A delivered frame has no display matrix and keeps the timestamp it came
      with. A frame with nothing to apply goes through no filter: it is the
      pushed frame itself when that one has no display matrix, a frame sharing
      its data otherwise. A frame nothing is decided for (see
      {!init_display_converter}) is the pushed frame itself, display matrix
      included. The pushed frame is unchanged and stays usable.

      After [`Flush] every remaining frame is delivered and the converter can be
      used again. An exception raised by [callback] propagates unchanged.
      @raise Avutil.Error when building the graph, pushing or pulling fails. *)
  val convert_display :
    display_converter ->
    (Avutil.video Avutil.frame -> unit) ->
    [ `Frame of Avutil.video Avutil.frame | `Flush ] ->
    unit
end
