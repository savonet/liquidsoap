(** Bindings to libavutil. The interface is the one of spec/avutil.md §4. *)

type input

(** {!type:input} and [output] are phantom parameters that mark a value as
    reading or as writing: the "line" of a {!type:container} or a
    {!type:format}. This library only declares them; the libraries built on it
    give them their meaning. *)
type output

(** A container opened for reading or for writing, ['a] being {!type:input} or
    {!type:output}. This library only declares the type; the libraries built on
    it create and consume its values. *)
type 'a container

(** Phantom parameter of values that carry audio. *)
type audio = [ `Audio ]

(** Phantom parameter of values that carry video. *)
type video = [ `Video ]

(** Phantom parameter of values that carry subtitles. *)
type subtitle = [ `Subtitle ]

(** The media types FFmpeg knows, as a variant generated from FFmpeg's headers
    at build time. *)
type media_type = Media_types.t

(** A container format, ['line] being {!type:input} or {!type:output} and
    ['media] one of {!type:audio}, {!type:video} and {!type:subtitle}. This
    library only declares the type; the libraries built on it create and consume
    its values. *)
type ('line, 'media) format

(** A library version number. *)
type version = { major : int; minor : int; micro : int }

(** The version of the libavutil library loaded at run time, read once when this
    module is loaded. It may differ from the version of the headers the bindings
    were compiled against. *)
val version : version

(** [version_string v] is [major.minor.micro] in decimal, such as ["60.8.100"].
*)
val version_string : version -> string

(** Frames: the properties common to audio and video frames. *)
module Frame : sig
  (** One decoded audio or video frame. ['media] is {!type:audio} or
      {!type:video} and matches the content of the frame.

      The value owns one FFmpeg frame, which is released when the value is
      collected; there is no explicit close. The data buffers of the frame are
      reference counted by FFmpeg and may be shared with other frames. The size
      of those buffers is reported to the garbage collector.

      A function of the bindings that takes a frame leaves it unchanged and
      usable, apart from the setters below and {!Video.frame_visit} with
      [~make_writable:true]. A frame has no lock: any number of threads may read
      one at the same time, and a thread that changes one must be its only user
      at that moment. The bindings do not detect a violation. *)
  type 'media t

  (** The presentation timestamp of the frame, in units of the time base the
      frame is handled with. [None] when the frame has none (FFmpeg's
      [AV_NOPTS_VALUE]). *)
  val pts : _ t -> Int64.t option

  (** [set_pts frame pts] writes the presentation timestamp of [frame], [None]
      meaning no timestamp. It writes the same value as the best-effort
      timestamp, so {!best_effort_timestamp} agrees with {!pts} afterwards. *)
  val set_pts : _ t -> Int64.t option -> unit

  (** The duration of the frame, in the same units as {!pts}. [None] when the
      duration is 0, which FFmpeg reads as unknown. *)
  val duration : _ t -> Int64.t option

  (** [set_duration frame duration] writes the duration of [frame], [None] being
      stored as 0. *)
  val set_duration : _ t -> Int64.t option -> unit

  (** The decoding timestamp copied from the packet that produced the frame.
      [None] when the frame has none ([AV_NOPTS_VALUE]). *)
  val pkt_dts : _ t -> Int64.t option

  (** [set_pkt_dts frame dts] writes that decoding timestamp, [None] meaning no
      timestamp. *)
  val set_pkt_dts : _ t -> Int64.t option -> unit

  (** The metadata of the frame as [(key, value)] pairs, in the order FFmpeg
      holds them. The strings are copies. *)
  val metadata : _ t -> (string * string) list

  (** [set_metadata frame l] replaces the whole metadata of [frame] with [l].
      Entries that are absent from [l] are removed, a key that appears several
      times in [l] keeps its last value, and the empty list leaves the frame
      with no metadata. A key or a value ends at its first NUL byte.

      @raise Error
        when FFmpeg cannot build the new metadata. The metadata of the frame is
        then unchanged. *)
  val set_metadata : _ t -> (string * string) list -> unit

  (** The timestamp FFmpeg estimates for the frame when decoding, which its own
      components read. [None] when the frame has none ([AV_NOPTS_VALUE]). FFmpeg
      fills it only when decoding; {!set_pts} writes it too. *)
  val best_effort_timestamp : _ t -> Int64.t option
end

(** Shorthand for {!Frame.t}. *)
type 'media frame = 'media Frame.t

(** The failures the bindings report. The cases up to [`Experimental] each stand
    for one FFmpeg error code; the [`X_not_found] ones say that FFmpeg has no
    component, option or stream of the kind named.

    - [`Eof]: end of file or of stream.
    - [`Exit]: an immediate exit was requested; the operation should not be
      restarted.
    - [`Invalid_data]: FFmpeg found invalid data in its input.
    - [`Option_not_found]: an option name the object does not have, as raised by
      the getters of {!Options}.
    - [`Patch_welcome]: FFmpeg does not implement the feature.
    - [`Bug]: FFmpeg reports an internal bug.
    - [`Eagain]: the operation cannot proceed for the moment (the C [EAGAIN]).
    - [`Unknown]: an unknown error, typically from an external library.
    - [`Experimental]: FFmpeg flags the requested feature as experimental.
    - [`Other code]: any other FFmpeg error, with its raw, negative code.
      FFmpeg's [AVERROR_EXTERNAL] and the errors that wrap a system [errno]
      other than [EAGAIN] arrive this way.
    - [`Failure msg]: a condition the bindings detect themselves: an invalid
      argument, a value FFmpeg returns that the bindings have no constructor
      for, a handle in the wrong state. [msg] is for a human reader. Three
      messages are fixed: ["Container closed!"] for a closed handle,
      ["Object failed!"] for a failed one and ["Object in use!"] for a handle
      another operation is using at that moment. *)
type error =
  [ `Bsf_not_found
  | `Decoder_not_found
  | `Demuxer_not_found
  | `Encoder_not_found
  | `Eof
  | `Exit
  | `Filter_not_found
  | `Invalid_data
  | `Muxer_not_found
  | `Option_not_found
  | `Patch_welcome
  | `Protocol_not_found
  | `Stream_not_found
  | `Bug
  | `Eagain
  | `Unknown
  | `Experimental
  | `Other of int
  | `Failure of string ]

(** The exception that carries the failures of all the FFmpeg bindings, this
    library and the ones built on it.

    A negative code returned by an FFmpeg call raises it with the matching case
    of {!type:error}, and a condition the bindings detect themselves raises
    [Error (`Failure msg)]. Besides it, an allocation of the bindings that fails
    raises [Out_of_memory], and the lookups documented as such raise
    [Not_found].

    An uncaught [Error e] prints as [Avutil.Error(...)] with the text of
    {!string_of_error}[ e] between the parentheses. *)
exception Error of error

(** [string_of_error e] describes [e] for a human reader. For [`Failure msg] it
    is [msg]. For every other case it is the text FFmpeg gives for the
    corresponding error code. *)
val string_of_error : error -> string

(** [expr_parse_and_eval s] evaluates the arithmetic expression [s] with
    FFmpeg's expression evaluator and returns its value. The expression may use
    the evaluator's built-in constants and functions; it has no variable and no
    user function.

    @raise Error
      when [s] does not parse or does not evaluate. Nothing is written to the
      log. *)
val expr_parse_and_eval : string -> float

(** A buffer of bytes. *)
type data =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(** [create_data len] returns a fresh buffer of [len] bytes, managed by the
    OCaml runtime like any bigarray. Its content is unspecified.

    @raise Error with [`Failure] when [len] is negative. *)
val create_data : int -> data

(** A rational number [num/den], as FFmpeg uses for time bases, frame rates and
    aspect ratios. Each field must fit a C [int] when the value is given to
    FFmpeg; a function that receives one outside that range raises {!Error} with
    [`Failure]. *)
type rational = { num : int; den : int }

(** [string_of_rational r] is [num/den] in decimal, such as ["1/25"]. *)
val string_of_rational : rational -> string

(** FFmpeg's [FF_QP2LAMBDA]: the factor that converts a quantiser value (QP) to
    the lambda unit of encoder quality settings. Read once when this module is
    loaded. *)
val qp2lambda : int

(** Units of time. *)
module Time_format : sig
  (** The unit in which the libraries built on this one express a duration or a
      position: 1, 1000, 1000000 and 1000000000 units per second respectively.
  *)
  type t = [ `Second | `Millisecond | `Microsecond | `Nanosecond ]
end

(** FFmpeg's internal time base, [AV_TIME_BASE_Q]: [{ num = 1; den = 1000000 }],
    that is microseconds. *)
val time_base : unit -> rational

(** FFmpeg's log: its level, and the capture of its messages by an OCaml
    function. Both are state of the whole process. *)
module Log : sig
  (** FFmpeg's log levels, from the most silent to the most verbose. Each is the
      FFmpeg constant of the same name ([AV_LOG_QUIET] to [AV_LOG_TRACE]). *)
  type level =
    [ `Quiet
    | `Panic
    | `Fatal
    | `Error
    | `Warning
    | `Info
    | `Verbose
    | `Debug
    | `Trace ]

  (** [set_level l] sets FFmpeg's log level for the whole process: FFmpeg
      reports the messages of level [l] and of the less verbose levels. *)
  val set_level : level -> unit

  (** [set_callback f] captures FFmpeg's log messages, for the whole process,
      and delivers them to [f]. It replaces a callback installed earlier.
      FFmpeg's default logger prints nothing while a callback is installed.

      Capture. FFmpeg logs from any thread, its own worker threads included.
      From the return of [set_callback] on, each message at or below the current
      level ({!set_level}) is formatted as FFmpeg's default logger formats it,
      prefix included, cut at a fixed length, and put on a queue by the thread
      that logs. That thread runs no OCaml code and does not wait for the
      delivery. There is one message per log call of FFmpeg, so a message may be
      a part of a line. A message whose allocation fails is dropped.

      Delivery. One thread of this library calls the callback, never the thread
      that logged. The first [set_callback] of the program starts it, in the
      domain of the caller, and it runs until the program exits. It delivers the
      messages in the order they were queued, each at most once, and each to the
      callback that was installed when the message was queued: messages still
      queued for an earlier callback reach that callback, not [f]. It waits for
      messages with the OCaml runtime lock released.

      The callback runs with the runtime lock held. It may call any function of
      the bindings, [set_callback] and {!clear_callback} included, and functions
      that log. An exception it raises is caught and printed on standard error,
      and delivery goes on with the next message.

      [f] is kept alive until it is replaced or cleared and its last message is
      delivered. *)
  val set_callback : (string -> unit) -> unit

  (** [clear_callback ()] restores FFmpeg's default logging and drops the
      installed callback.

      It blocks until every message queued before the call has been delivered to
      that callback; other threads run meanwhile. Called from the callback
      itself it returns at once, and those messages are delivered after the
      callback returns. The outgoing callback receives nothing after that, and a
      callback installed later receives none of those messages.

      With no callback installed it only restores FFmpeg's default logging.
      {!set_callback} may be called again at any time afterwards. *)
  val clear_callback : unit -> unit
end

(** Channel layouts: how many audio channels, which ones and in which order. *)
module Channel_layout : sig
  (** The names of FFmpeg's layout masks, as a variant generated from FFmpeg's
      headers at build time. No function of this module takes or returns it. *)
  type layout = Channel_layout.t

  (** A channel layout. The value owns its own copy of an FFmpeg layout, shares
      nothing with the value or the object it was read from, is immutable, and
      is released when it is collected. Every function that returns a layout
      returns a fresh copy. *)
  type t

  (** Every standard layout FFmpeg lists, in FFmpeg's order. Built once when
      this module is loaded. *)
  val standard_layouts : t list

  (** The layout FFmpeg names ["stereo"]. *)
  val stereo : t

  (** The layout FFmpeg names ["mono"]. *)
  val mono : t

  (** The layout FFmpeg names ["5.1"]. *)
  val five_point_one : t

  (** [compare a b] is an equality test, despite its name: [true] when FFmpeg
      finds the two layouts equal, [false] when they differ.

      @raise Error when FFmpeg finds one of the layouts invalid. *)
  val compare : t -> t -> bool

  (** [find name] parses [name] as FFmpeg parses the description of a channel
      layout: a layout name such as ["stereo"] or ["5.1"], channel names joined
      by [+] such as ["FL+FR"], a channel count such as ["4c"], the value of a
      native mask such as ["0x4"].

      @raise Not_found when FFmpeg does not accept [name]. *)
  val find : string -> t

  (** FFmpeg's description of the layout, such as ["stereo"], in full whatever
      its length. {!find} accepts it.

      @raise Error when FFmpeg cannot describe the layout. *)
  val get_description : t -> string

  (** The number of channels of the layout. *)
  val get_nb_channels : t -> int

  (** [get_default n] returns FFmpeg's default layout for [n] channels, such as
      stereo for 2. When FFmpeg has no standard layout of [n] channels, the
      result is a layout of [n] channels in unspecified order, for which
      {!get_mask} returns [None] and {!get_nb_channels} returns [n].

      @raise Not_found when [n] is below 1, or above the largest C [int]. *)
  val get_default : int -> t

  (** The channel mask of the layout, one bit per channel, when the layout has
      FFmpeg's native channel order. [None] for any other order: unspecified,
      custom, ambisonic. *)
  val get_mask : t -> int64 option
end

(** Audio sample formats. *)
module Sample_format : sig
  (** The sample formats, as a variant generated from FFmpeg's headers at build
      time. [`None] is FFmpeg's "no format". *)
  type t = Sample_format.t

  (** FFmpeg's name of the format, such as ["s16"] or ["fltp"]. [None] for
      [`None] and for a format FFmpeg does not name. *)
  val get_name : t -> string option

  (** [find name] is the format FFmpeg names [name].

      @raise Not_found when FFmpeg knows no format of that name. *)
  val find : string -> t

  (** The integer value of the format in FFmpeg's C enumeration. *)
  val get_id : t -> int

  (** [find_id n] is the format whose value in FFmpeg's C enumeration is [n]:
      the inverse of {!get_id}.

      @raise Not_found when no constructor has that value. *)
  val find_id : int -> t
end

(** Colour spaces (the matrix between YUV and RGB). *)
module Color_space : sig
  (** The colour spaces, as a variant generated from FFmpeg's headers at build
      time. *)
  type t = Color_space.t

  (** FFmpeg's name of the value, and the empty string when FFmpeg has none. *)
  val name : t -> string

  (** [from_name s] is the value named [s]. A string that is exactly the name
      {!name} gives for a constructor converts to that constructor; any other
      string goes through FFmpeg's own lookup, which may match a prefix. [None]
      when FFmpeg does not know [s], and when it knows it as a value this type
      has no constructor for. *)
  val from_name : string -> t option
end

(** Colour ranges (limited or full). The functions behave as those of
    {!Color_space}. *)
module Color_range : sig
  type t = Color_range.t

  (** FFmpeg's name of the value, and the empty string when FFmpeg has none. *)
  val name : t -> string

  (** The value named by the string, as {!Color_space.from_name}. *)
  val from_name : string -> t option
end

(** Colour primaries. The functions behave as those of {!Color_space}. *)
module Color_primaries : sig
  type t = Color_primaries.t

  (** FFmpeg's name of the value, and the empty string when FFmpeg has none. *)
  val name : t -> string

  (** The value named by the string, as {!Color_space.from_name}. *)
  val from_name : string -> t option
end

(** Colour transfer characteristics. The functions behave as those of
    {!Color_space}. *)
module Color_trc : sig
  type t = Color_trc.t

  (** FFmpeg's name of the value, and the empty string when FFmpeg has none. *)
  val name : t -> string

  (** The value named by the string, as {!Color_space.from_name}. *)
  val from_name : string -> t option
end

(** Locations of the chroma samples. The functions behave as those of
    {!Color_space}. *)
module Chroma_location : sig
  type t = Chroma_location.t

  (** FFmpeg's name of the value, and the empty string when FFmpeg has none. *)
  val name : t -> string

  (** The value named by the string, as {!Color_space.from_name}. *)
  val from_name : string -> t option
end

(** Pixel formats. *)
module Pixel_format : sig
  (** The pixel formats, as a variant generated from FFmpeg's headers at build
      time. [`None] is FFmpeg's "no format". *)
  type t = Pixel_format.t

  (** The flags of a pixel format (planar, RGB, palette, hardware and so on),
      generated from FFmpeg's headers at build time. *)
  type flag = Pixel_format_flag.t

  (** Where one component (Y, U, R, alpha and so on) of a pixel is stored.

      - [plane]: index of the plane that holds the component.
      - [step]: number of elements between two horizontally consecutive pixels.
        Elements are bits for bitstream formats, bytes otherwise.
      - [offset]: number of elements before the component of the first pixel.
      - [shift]: number of least significant bits to shift away to get the
        value.
      - [depth]: number of bits of the component. *)
  type component_descriptor = {
    plane : int;
    step : int;
    offset : int;
    shift : int;
    depth : int;
  }

  (** FFmpeg's description of a pixel format, copied into plain OCaml data. Only
      {!descriptor} builds one.

      - [name]: FFmpeg's name of the format.
      - [nb_components]: number of components per pixel, 1 to 4.
      - [log2_chroma_w]: horizontal chroma subsampling. The chroma width is the
        luma width shifted right by this many bits, rounded up.
      - [log2_chroma_h]: vertical chroma subsampling, likewise for the height.
      - [flags]: the flags set on the format, in ascending order of bit value. A
        flag this build has no constructor for is left out.
      - [comp]: one entry per component, [nb_components] entries, in FFmpeg's
        order.
      - [alias]: alternative names of the format, if any. *)
  type descriptor = private {
    name : string;
    nb_components : int;
    log2_chroma_w : int;
    log2_chroma_h : int;
    flags : flag list;
    comp : component_descriptor list;
    alias : string option;
  }

  (** [descriptor f] returns FFmpeg's description of the format [f].

      @raise Not_found when the format has no descriptor, [`None] among them. *)
  val descriptor : t -> descriptor

  (** [bits d] is the number of bits per pixel of the format [d] describes,
      padding bits excluded. *)
  val bits : descriptor -> int

  (** [planes f] is the number of planes of the format [f].

      @raise Error when the format has no descriptor, [`None] among them. *)
  val planes : t -> int

  (** FFmpeg's name of the format, such as ["yuv420p"]. [None] for [`None] and
      for a format FFmpeg does not name. *)
  val to_string : t -> string option

  (** [of_string s] is the format FFmpeg names [s].

      @raise Error
        with [`Failure] when FFmpeg knows no format of that name, or knows one
        this build has no constructor for. *)
  val of_string : string -> t

  (** The integer value of the format in FFmpeg's C enumeration. *)
  val get_id : t -> int

  (** [find_id n] is the format whose value in FFmpeg's C enumeration is [n]:
      the inverse of {!get_id}.

      @raise Not_found when no constructor has that value. *)
  val find_id : int -> t
end

(** Audio frames. The timestamps and the metadata are in {!Frame}; the samples
    are read and written through the resampling library. *)
module Audio : sig
  (** [create_frame sample_format channel_layout sample_rate nb_samples] returns
      a new audio frame with that sample format, a copy of that channel layout,
      that sample rate in Hz, and buffers allocated by FFmpeg for [nb_samples]
      samples per channel. The sample data is unspecified.

      @raise Error
        with [`Failure] when [nb_samples] is below 1, or when [sample_rate] or
        [nb_samples] exceeds a C [int]; with FFmpeg's error when FFmpeg cannot
        allocate the buffers. Nothing remains allocated. *)
  val create_frame :
    Sample_format.t -> Channel_layout.t -> int -> int -> audio frame

  (** The sample format of the frame.

      @raise Error
        with [`Failure] when the frame has a format this build has no
        constructor for. *)
  val frame_get_sample_format : audio frame -> Sample_format.t

  (** The sample rate of the frame, in Hz. *)
  val frame_get_sample_rate : audio frame -> int

  (** The number of channels of the frame's channel layout. *)
  val frame_get_channels : audio frame -> int

  (** A copy of the frame's channel layout, independent of the frame. *)
  val frame_get_channel_layout : audio frame -> Channel_layout.t

  (** The number of samples per channel the frame holds. *)
  val frame_nb_samples : audio frame -> int
end

(** Video frames. The timestamps and the metadata are in {!Frame}. *)
module Video : sig
  (** The planes of a video frame, in the order of the pixel format: for each
      plane, [(data, linesize)]. [data] is the memory of the plane, of exactly
      [linesize] times the height of the plane bytes, and [linesize] is the
      number of bytes from one row to the next. A row is [linesize] bytes long:
      the pixels, then FFmpeg's padding. The height of a chroma plane is the
      height of the frame reduced by the vertical chroma subsampling of the
      format, rounded up. *)
  type planes = (data * int) array

  (** [create_frame width height pixel_format] returns a new video frame of that
      size in pixels and that format, with buffers allocated by FFmpeg and
      aligned, which shows in the line sizes. The pixel data is unspecified.

      @raise Error
        with [`Failure] when [width] or [height] is below 1 or exceeds a C
        [int]; with FFmpeg's error when FFmpeg cannot allocate the buffers.
        Nothing remains allocated. *)
  val create_frame : int -> int -> Pixel_format.t -> video frame

  (** [frame_get_linesize frame n] is the line size of plane [n] of [frame], in
      bytes: the second component of entry [n] of {!planes}.

      @raise Error
        with [`Failure] when [n] is not the index of a plane of the frame, and
        when the frame holds no software pixel data (a hardware frame). *)
  val frame_get_linesize : video frame -> int -> int

  (** [frame_visit ~make_writable f frame] gives [f] direct access to the pixel
      data of [frame]: it builds the {!planes} of the frame, calls [f planes] on
      the calling thread, and returns [frame] itself.

      The bigarrays of [planes] share their memory with the buffers of the
      frame. Nothing is copied: a read sees the frame's pixels and a write
      changes them. The palette of a paletted format is not a plane and is not
      given.

      Lifetime. The bigarrays stay valid for as long as the program holds them:
      after [f] returns, once [frame] is otherwise unreachable, and after a
      later call with [~make_writable:true] gave the frame other buffers, in
      which case they still designate the buffers they were built on. One limit:
      a view derived from one of them ([Bigarray.Array1.sub], a reshape) does
      not keep the memory alive by itself. Keep the bigarray it was derived from
      reachable for as long as the view is in use.

      The frame is not locked while [f] runs, and [f] may call any function on
      it. An exception [f] raises propagates unchanged; the frame keeps the
      buffers it had when [f] was called, including private ones a
      [~make_writable:true] just gave it.

      @param make_writable
        With [true], a frame that shares its buffers with other frames first
        receives buffers of its own holding a copy of the data, so that the
        writes of [f] reach this frame only; the caller must then be the only
        user of the frame during the call. With [false] the frame is left as it
        is, and a write through the planes changes every frame that shares the
        buffers, such as the frames a decoder still references. Pass [true]
        whenever [f] writes.
      @raise Error
        with [`Failure] when the frame holds no software pixel data (a hardware
        frame) or has a plane with a negative line size; with FFmpeg's error
        when the frame cannot be made writable. *)
  val frame_visit :
    make_writable:bool -> (planes -> unit) -> video frame -> video frame

  (** The width of the frame, in pixels. *)
  val frame_get_width : video frame -> int

  (** The height of the frame, in pixels. *)
  val frame_get_height : video frame -> int

  (** The pixel format of the frame.

      @raise Error
        with [`Failure] when the frame has a format this build has no
        constructor for. The same holds for the five colour getters below. *)
  val frame_get_pixel_format : video frame -> Pixel_format.t

  (** The aspect ratio of one pixel (width over height). [None] when it is
      unknown, which FFmpeg stores as a zero numerator. *)
  val frame_get_pixel_aspect : video frame -> rational option

  (** The colour space of the frame. *)
  val frame_get_color_space : video frame -> Color_space.t

  (** The colour range of the frame. *)
  val frame_get_color_range : video frame -> Color_range.t

  (** The colour primaries of the frame. *)
  val frame_get_color_primaries : video frame -> Color_primaries.t

  (** The colour transfer characteristic of the frame. *)
  val frame_get_color_trc : video frame -> Color_trc.t

  (** The location of the chroma samples of the frame. *)
  val frame_get_chroma_location : video frame -> Chroma_location.t
end

(** Subtitles. *)
module Subtitle : sig
  (** One subtitle: a native FFmpeg subtitle with its rectangles and their
      bitmaps and texts. The value owns all of it, is immutable, and is released
      when it is collected. *)
  type frame

  (** The kinds of subtitle rectangle (none, bitmap, text, ASS), generated from
      FFmpeg's headers at build time. *)
  type subtitle_type = Subtitle_type.t

  (** The flags of a subtitle rectangle, generated from FFmpeg's headers at
      build time. *)
  type subtitle_flag = Subtitle_flag.t

  (** A default ASS script header: a [[Script Info]] section, a [[V4+ Styles]]
      section with one style named [Default], and the [Format] line of the
      [[Events]] section. Every line ends with CR LF. *)
  val header_ass_default : unit -> string

  (** The bitmap of a rectangle.

      - [x], [y]: position of the top left corner.
      - [w], [h]: width and height.
      - [nb_colors]: number of colours of the palette.
      - [planes]: [(data, linesizes)], the 4 planes and their 4 line sizes. Both
        arrays have exactly 4 elements; an absent plane is an empty bigarray.
        The size of plane [i] is fixed by the other fields: [AVPALETTE_SIZE]
        bytes for plane 1 of a rectangle of type bitmap, which is its palette;
        otherwise [h * linesizes.(i)] when both are positive; otherwise 0. *)
  type pict = {
    x : int;
    y : int;
    w : int;
    h : int;
    nb_colors : int;
    planes : data array * int array;
  }

  (** One rectangle of a subtitle.

      - [pict]: the bitmap, when the rectangle has one.
      - [text]: plain text, or the empty string when there is none.
      - [ass]: an ASS event line, or the empty string when there is none. *)
  type rectangle = {
    pict : pict option;
    flags : subtitle_flag list;
    rect_type : subtitle_type;
    text : string;
    ass : string;
  }

  (** The whole content of a subtitle, as plain OCaml data.

      - [format]: FFmpeg's subtitle format field, 0 for graphics. It fits an
        unsigned 16-bit integer.
      - [start_display_time]: when the display starts, in milliseconds relative
        to [pts]. From 0 to [2^32 - 1].
      - [end_display_time]: when the display ends, in the same unit and range.
      - [pts]: presentation timestamp in units of {!time_base} (microseconds),
        or [None] for no timestamp. *)
  type content = {
    format : int;
    start_display_time : int;
    end_display_time : int;
    rectangles : rectangle list;
    pts : int64 option;
  }

  (** [create_frame content] builds a subtitle that holds a copy of [content]:
      the texts and every non-empty plane are copied, and an empty [text] or
      [ass] is stored as absent. [get_content (create_frame content)] equals
      [content].

      @raise Error
        with [`Failure], leaving nothing allocated, when: a display time or
        [format] is outside its range; an integer of a [pict] does not fit a C
        [int]; an array of [planes] does not have exactly 4 elements; a plane's
        length is neither 0 nor the size stated at {!type:pict}; the first plane
        of a [pict] is empty. *)
  val create_frame : content -> frame

  (** [get_content frame] returns a fresh copy of the content of [frame]. A
      rectangle has [pict = Some _] exactly when it has a first plane. Each
      plane is a new bigarray managed by the OCaml runtime; changing it does not
      change [frame].

      @raise Error
        with [`Failure] when a rectangle has a type this build has no
        constructor for. *)
  val get_content : frame -> content

  (** The presentation timestamp of the subtitle: the [pts] field of
      {!get_content}, without copying the rest. *)
  val get_pts : frame -> int64 option
end

(** The options of FFmpeg objects: listing the options a kind of object
    declares, and reading the value of an option on a live object. Options are
    set when an object is created, through an {!opts} table. *)
module Options : sig
  (** The description of the options of one kind of FFmpeg object (a codec, a
      format, a filter): an FFmpeg option class, or no class at all. The
      libraries built on this one return these values; they are valid for the
      life of the process. *)
  type t

  (** The flags FFmpeg sets on an option, each the [AV_OPT_FLAG_*] constant of
      the same name. The [`X_param] ones say what the option applies to:
      encoding, decoding, audio, video, subtitles, bitstream filters, filters.

      - [`Export]: meant for exporting values to the caller.
      - [`Readonly]: can be read and not set.
      - [`Runtime_param]: can be set by the user at run time.
      - [`Child_consts]: the named constants of the option can also reside in
        child objects. *)
  type flag =
    [ `Encoding_param
    | `Decoding_param
    | `Audio_param
    | `Video_param
    | `Subtitle_param
    | `Export
    | `Readonly
    | `Bsf_param
    | `Runtime_param
    | `Filtering_param
    | `Deprecated
    | `Child_consts ]

  (** What FFmpeg declares about the value of an option.

      - [default]: the default value. [None] when the option declares none, when
        the declared one names no value (FFmpeg's "none" format, a negative
        boolean default, a text that is not a valid channel layout), and for
        every array option.
      - [min], [max]: the smallest and the largest value accepted. Present for
        the numeric kinds only; for the 64-bit integer kinds a bound beyond the
        64-bit range is that range's limit.
      - [values]: the named values the option accepts, as [(name, value)] in
        FFmpeg's order. Filled for the integer, floating-point and boolean
        kinds; empty for the others and for array options. *)
  type 'a entry = {
    default : 'a option;
    min : 'a option;
    max : 'a option;
    values : (string * 'a) list;
  }

  (** The type of a non-array option, each case the FFmpeg option type of the
      same name, with what FFmpeg declares about its value.

      - [`Flags]: a bit mask; the named values are its bits.
      - [`Rational]: FFmpeg declares the default and the bounds as
        floating-point numbers; each is given as the nearest rational.
      - [`UInt64]: an unsigned 64-bit value carried in a signed [int64].
      - The cases that carry a [string entry] give their default as the text
        FFmpeg declares: [`Binary], [`Dict], [`Image_size], [`Video_rate] and
        [`Color] are not parsed. *)
  type ground =
    [ `Flags of int64 entry
    | `Int of int entry
    | `Int64 of int64 entry
    | `Float of float entry
    | `Double of float entry
    | `String of string entry
    | `Rational of rational entry
    | `Binary of string entry
    | `Dict of string entry
    | `UInt64 of int64 entry
    | `Image_size of string entry
    | `Pixel_fmt of Pixel_format.t entry
    | `Sample_fmt of Sample_format.t entry
    | `Video_rate of string entry
    | `Duration of int64 entry
    | `Color of string entry
    | `Channel_layout of Channel_layout.t entry
    | `Bool of bool entry ]

  (** The type of an option: a {!ground} type, or [`Array] of the type of its
      elements. The entry of an array option has neither default nor named
      values. *)
  type spec = [ ground | `Array of ground ]

  (** One option.

      - [help]: FFmpeg's help text, [None] when it is absent or empty.
      - [flags]: the flags set on the option, in ascending order of bit value.
  *)
  type opt = {
    name : string;
    help : string option;
    flags : flag list;
    spec : spec;
  }

  (** [opts c] lists the options of the class [c] and, recursively, of its child
      classes: the options of [c] in FFmpeg's order, then those of each child
      class in FFmpeg's order of child classes.

      An option whose FFmpeg type has no case in {!ground} is left out, so the
      call succeeds whatever the installed FFmpeg declares. FFmpeg's named
      constants are not options of their own: they appear in the [values] of the
      option they belong to. A value with no class gives [[]]. *)
  val opts : t -> opt list

  (** An object that carries options, inside a value of one of the libraries
      built on this one (a container, a codec context, a filter), which creates
      it. It keeps that owner alive, and reads through it are checked against
      the state of the owner. *)
  type obj

  (** The type of the readers below. [get ~name obj] returns the current value
      of the option [name] of [obj]. There is no setter on a live object.

      Every string or layout returned is a copy. When the owner of [obj] has a
      use lock, the read takes it shared, for the time of the read.

      @param search_children
        With [true], the option is also looked up in the child objects of [obj].
        Default: [false].
      @param name The option name, as the [name] field of {!type:opt} gives it.
      @raise Error
        with [`Option_not_found] when the object has no option [name]; with
        another FFmpeg error when FFmpeg cannot give the value in the type
        asked; with [`Failure "Container closed!"] when the owner of [obj] is
        closed, and [`Failure "Object in use!"] when another operation is
        changing the owner at that moment. *)
  type 'a getter = ?search_children:bool -> name:string -> obj -> 'a

  (** The value of the option as text, as FFmpeg renders it. *)
  val get_string : string getter

  (** The value of an integer option.

      @raise Error with [`Failure] when it is outside the range of [int]. *)
  val get_int : int getter

  (** The value of an integer option, as 64 bits. *)
  val get_int64 : int64 getter

  (** The value of the option as a float. *)
  val get_float : float getter

  (** The value of the option as a rational. *)
  val get_rational : rational getter

  (** The value of an image-size option, as [(width, height)] in pixels. *)
  val get_image_size : (int * int) getter

  (** The value of a pixel-format option.

      @raise Error
        with [`Failure] when it is a format this build has no constructor for.
  *)
  val get_pixel_fmt : Pixel_format.t getter

  (** The value of a sample-format option.

      @raise Error
        with [`Failure] when it is a format this build has no constructor for.
  *)
  val get_sample_fmt : Sample_format.t getter

  (** The value of a video-rate option, in frames per second as a rational. *)
  val get_video_rate : rational getter

  (** The value of a channel-layout option, as an independent copy. *)
  val get_channel_layout : Channel_layout.t getter

  (** The value of a dictionary option, as [(key, value)] pairs in the
      dictionary's order. *)
  val get_dictionary : (string * string) list getter
end

(** The value of an option given to FFmpeg. FFmpeg receives it as text: a string
    as it is, an integer in decimal, a float in a decimal form that FFmpeg
    parses back to the same value. *)
type value =
  [ `String of string | `Int of int | `Int64 of int64 | `Float of float ]

(** A table of options, from option name to value, as the [?opts] argument of
    the operations that create or open an FFmpeg object.

    Such an operation hands the entries to FFmpeg and, when it succeeds, removes
    from the table every entry FFmpeg consumed: the keys still in the table
    afterwards are the options FFmpeg did not recognise. A caller checks for
    mistyped names by looking at what is left. When the operation raises, the
    table is untouched. {!HwContext.create_device_context} is the one operation
    that never changes its table.

    A key bound several times in the table counts once, with its most recent
    binding. Names and string values end at their first NUL byte. *)
type opts = (string, value) Hashtbl.t

(** [string_of_opts t] renders the table as [key=value] items joined by commas,
    such as ["b=128000,preset=fast"]. The order of the items is unspecified and
    nothing is escaped, so the text is meant for display. *)
val string_of_opts : opts -> string

(** [filter_opts unused t] removes from [t], in place, every key that is not an
    element of [unused]: [t] keeps only the options listed as unused. *)
val filter_opts : string array -> opts -> unit

(** Hardware acceleration: devices, and pools of frames on a device. *)
module HwContext : sig
  (** The kinds of hardware device (VAAPI, CUDA, VideoToolbox and so on),
      generated from FFmpeg's headers at build time. *)
  type device_type = Hw_device_type.t

  (** An open hardware device. The value owns one reference to it, is immutable,
      and drops the reference when it is collected. A frame context and an
      encoder given the device take references of their own, so the device stays
      open as long as one of them needs it. *)
  type device_context

  (** A pool of hardware frames on a device. The value owns one reference to it,
      is immutable, and drops the reference when it is collected. The pool keeps
      its device open by itself, and an encoder given the pool takes a reference
      of its own. *)
  type frame_context

  (** [create_device_context ?device ?opts device_type] opens a hardware device
      of that type.

      The call may take time; it releases the OCaml runtime lock while FFmpeg
      opens the device, so other threads run meanwhile.

      @param device
        The device to open, in a syntax that depends on the type. Omitted or
        empty, FFmpeg picks its default device.
      @param opts
        Options for the device, specific to its type. FFmpeg does not report
        which entries it consumed, so the table is never changed.
      @raise Error
        with FFmpeg's error when the device cannot be opened. Nothing remains
        allocated. *)
  val create_device_context :
    ?device:string -> ?opts:opts -> device_type -> device_context

  (** [create_frame_context ~width ~height ~src_pixel_format ~dst_pixel_format
       device] creates and initialises a pool of hardware frames on [device].
      Every parameter of the pool other than the ones below keeps FFmpeg's
      default. The call releases the OCaml runtime lock while FFmpeg initialises
      the pool.

      @param width Width of the frames, in pixels.
      @param height Height of the frames, in pixels.
      @param src_pixel_format The software format of the data the frames carry.
      @param dst_pixel_format
        The hardware format of the frames. It must be a hardware format.
      @raise Error
        with FFmpeg's error when the pool cannot be initialised, and with
        [`Failure] when [width] or [height] exceeds a C [int]. Nothing remains
        allocated. *)
  val create_frame_context :
    width:int ->
    height:int ->
    src_pixel_format:Pixel_format.t ->
    dst_pixel_format:Pixel_format.t ->
    device_context ->
    frame_context
end
