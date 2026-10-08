(** Bindings to libavformat. The interface is the one of spec/avformat.md §4.

    A container is an opened media file, network stream or device. An
    [input container] demultiplexes packets and can decode them to frames. An
    [output container] multiplexes packets and can encode frames to them.
    Loading this module initialises FFmpeg's network layer for the process.

    {2 Life cycle}

    A container is released by {!close}, or by the garbage collector when it is
    unreachable. Every stream value, every {!uninitialized_stream_copy} and the
    value of {!input_obj} keeps its container alive. Only [close] finalises an
    output: a collected output is truncated.

    An operation on a closed container, or on a stream of one, raises
    [Error (`Failure "Container closed!")]. An output on which a stream creation
    failed half-way is failed: every operation on it except [close] raises
    [Error (`Failure "Object failed!")].

    {2 Errors}

    A failure reported by FFmpeg raises [Avutil.Error] with the mapped code. A
    condition the binding detects itself raises [Error (`Failure message)]. A
    failed allocation raises [Out_of_memory]. An exception raised by a function
    given to {!open_input}, {!read_input} or {!write_frame} propagates
    unchanged.

    {2 Threads}

    The functions that open, probe, read, seek, create an encoding stream,
    write, flush, tell and close release the OCaml runtime lock while FFmpeg
    works, so other OCaml threads run meanwhile.

    One container serves one modifying operation at a time. [read_input],
    [seek], the stream creations, the setters, the writes, [flush], [tell] and
    [close] are exclusive; the getters and the stream lists may run together. An
    operation that finds the container taken in a conflicting way, by another
    thread or domain, raises [Error (`Failure "Object in use!")] at once; it
    does not wait. Streams share the guard of their container.

    {2 Option tables}

    A function taking [?opts] hands the entries of the table to FFmpeg. After a
    successful call the table holds exactly the entries FFmpeg did not consume.
    After a failure the table is untouched. *)

open Avutil

(** The version of the libavformat loaded at run time, read once when this
    module is loaded. *)
val avformat_version : version

(** The option class of containers, for listing the container options with
    [Avutil.Options]. *)
val container_options : Options.t

(** FFmpeg's input and output formats: demuxers, muxers and devices. A format
    value is a handle on a static FFmpeg object and needs no release. *)
module Format : sig
  (** The name of an input format. It may be a comma-separated list of aliases.
  *)
  val get_input_name : (input, _) format -> string

  (** The descriptive name of an input format. *)
  val get_input_long_name : (input, _) format -> string

  (** The name of an output format. It may be a comma-separated list of aliases.
  *)
  val get_output_name : (output, _) format -> string

  (** The descriptive name of an output format. *)
  val get_output_long_name : (output, _) format -> string

  (** [find_input_format name] is FFmpeg's input format of that short name, or
      [None] when there is none. The media parameter of the result is chosen by
      the caller and is not checked. *)
  val find_input_format : string -> (input, 'a) format option

  (** FFmpeg's guess of an output format. The media parameter of the result is
      chosen by the caller and is not checked.

      @param short_name a format short name, such as ["mp4"].
      @param filename a file name.
      @param mime a MIME type.
      @return
        [None] when FFmpeg guesses nothing. An omitted or empty argument does
        not take part in the guess. *)
  val guess_output_format :
    ?short_name:string ->
    ?filename:string ->
    ?mime:string ->
    unit ->
    (output, 'a) format option

  (** The default audio codec of the format; [`None] when it has none. *)
  val get_audio_codec_id : (output, audio) format -> Avcodec.Audio.id

  (** The default video codec of the format; [`None] when it has none. *)
  val get_video_codec_id : (output, video) format -> Avcodec.Video.id

  (** The default subtitle codec of the format; [`None] when it has none. *)
  val get_subtitle_codec_id : (output, subtitle) format -> Avcodec.Subtitle.id
end

(** What a [configure_*_stream] function of {!open_input} answers for one
    stream.

    - [codec]: the preferred decoder of the stream. It decodes the stream when
      {!read_input} reads it as frames, and may serve to probe it. [None]
      selects FFmpeg's default decoder for the stream's codec.
    - [opts]: options for the decoder of the stream. They are given to probing
      and to the decoder when it is opened. The table is read once, is not
      modified, and unused entries are not reported. *)
type 'media stream_config = {
  codec : ('media, Avcodec.decode) Avcodec.codec option;
  opts : opts option;
}

(** [open_input url] opens [url] for reading and probes its streams. It blocks
    while FFmpeg opens and probes, with the runtime lock released.

    In order: the demuxer is opened on [url]; each stream then present is
    offered, in index order, to the configure function of its media type; the
    streams are probed. FFmpeg's probing takes one forced decoder per media
    type: the preferred decoder of the first stream of each type that has one.
    When a later stream of the same type prefers another decoder, a warning goes
    to FFmpeg's log. A stream that the demuxer adds later has no configuration.

    Any failure, an exception of a configure function included, releases
    everything opened so far before it reaches the caller.

    @param interrupt
      polled by FFmpeg during every blocking I/O of the container, for the whole
      life of the container, its release included. Answering [true], or raising,
      aborts the blocking operation, which then raises [Error `Exit]. It runs
      with the runtime lock held, possibly on a thread of FFmpeg's own, and is
      kept alive until the container is released. An operation on the container
      called from it raises the in-use error.
    @param format forces the demuxer. By default FFmpeg detects it.
    @param opts
      options of the demuxer open. On success the consumed entries are removed
      from the table.
    @param configure_audio_stream
      called once per audio stream with a copy of the stream's codec parameters
      as they are before probing. Same for [configure_video_stream] and
      [configure_subtitle_stream]. No container value exists yet when they run.
    @raise Error
      with [`Failure] when [url] is empty and [format] is absent; with the
      mapped FFmpeg code when the open or the probing fails. *)
val open_input :
  ?interrupt:(unit -> bool) ->
  ?format:(input, _) format ->
  ?opts:opts ->
  ?configure_audio_stream:(audio Avcodec.params -> audio stream_config) ->
  ?configure_video_stream:(video Avcodec.params -> video stream_config) ->
  ?configure_subtitle_stream:(subtitle Avcodec.params -> subtitle stream_config) ->
  string ->
  input container

(** A custom read function. [read buffer offset length] stores at most [length]
    bytes into [buffer] from [offset] and returns the number of bytes stored.
    [length] is at least 1 and at most the size of the container's I/O buffer.

    - 0 means end of input.
    - A negative result is given to FFmpeg as the error code of the read.
    - A result greater than [length], and an exception, fail the read: the
      operation in progress raises the error FFmpeg propagates, and the text of
      the exception goes to FFmpeg's log.

    [buffer] belongs to the binding and is meaningful during the call only: the
    function must not keep it. The function runs with the runtime lock held,
    inside the container operation that triggered it, normally on that
    operation's thread. An operation on the same container called from it raises
    the in-use error. *)
type read = bytes -> int -> int -> int

(** A custom write function. [write buffer offset length] consumes at most
    [length] bytes of [buffer] from [offset] and returns the number of bytes
    consumed. When it consumed fewer than [length], it is called again with the
    remainder.

    - A negative result is given to FFmpeg as the error code of the write.
    - A result of 0, a result greater than [length], and an exception, fail the
      write.

    The bytes received, concatenated, are exactly the bytes the same output
    would write to a file. The rules of {!read} on the buffer, the lock and the
    container apply. *)
type write = bytes -> int -> int -> int

(** A custom seek function. [seek offset whence] moves the position of the
    custom I/O by [offset] bytes relative to [whence] and returns the new
    position. A negative result is given to FFmpeg as an error code, and an
    exception fails the seek. FFmpeg's size queries are answered as unsupported
    without calling the function. *)
type seek = int -> Unix.seek_command -> int

(** [open_input_stream read] opens an input that reads through [read] instead of
    a URL, and probes its streams, as {!open_input} does. It takes no interrupt
    function and no stream configuration. It blocks while FFmpeg opens and
    probes, calling [read] and [seek], with the runtime lock released around
    FFmpeg's work.

    The container keeps [read], [seek] and its I/O buffer alive until it is
    released.

    @param format forces the demuxer. By default FFmpeg detects it.
    @param opts
      options of the demuxer open. On success the consumed entries are removed
      from the table.
    @param seek the function FFmpeg calls to reposition the input. *)
val open_input_stream :
  ?format:(input, _) format ->
  ?opts:opts ->
  ?seek:seek ->
  read ->
  input container

(** The duration of the input in units of [format], rounded to the nearest unit.
    [None] when FFmpeg does not know it, and when it rounds to 0.

    @param format defaults to [`Second]. *)
val get_input_duration :
  ?format:Time_format.t -> input container -> Int64.t option

(** The metadata of the container, as key and value pairs in the order FFmpeg
    stores them. *)
val get_input_metadata : input container -> (string * string) list

(** The format the demuxer detected or was given. *)
val get_input_format : input container -> (input, _) format option

(** The container as an option-bearing object, for reading the current value of
    its options with [Avutil.Options]. The result keeps the container alive;
    reading through it after the container is closed raises the closed error. *)
val input_obj : input container -> Options.obj

(** A stream of a container: the container and a stream index. It has no
    resource of its own and keeps its container alive.

    The three parameters are phantom. ['line] is [input] or [output]. ['media]
    is the media type of the stream. ['mode] is [`Packet] or [`Frame]: whether
    the stream is read or written as packets or as frames. For an output it is
    fixed by the function that creates the stream; for the stream lists and the
    [find_best_*_stream] functions the caller chooses it.

    Some demuxers add streams while packets are read. An operation on a stream
    whose index the container does not have raises [Error (`Failure _)]. *)
type ('line, 'media, 'mode) stream

(** The current audio streams of the container, in ascending index order. Each
    element is the stream index, a stream value, and an independent copy of the
    codec parameters of the stream. It works on inputs and outputs. *)
val get_audio_streams :
  'a container -> (int * ('a, audio, 'b) stream * audio Avcodec.params) list

(** As {!get_audio_streams}, for the video streams. *)
val get_video_streams :
  'a container -> (int * ('a, video, 'b) stream * video Avcodec.params) list

(** As {!get_audio_streams}, for the subtitle streams. *)
val get_subtitle_streams :
  'a container ->
  (int * ('a, subtitle, 'b) stream * subtitle Avcodec.params) list

(** As {!get_audio_streams}, for the data streams. *)
val get_data_streams :
  'a container ->
  (int * ('a, [ `Data ], 'b) stream * [ `Data ] Avcodec.params) list

(** The audio stream FFmpeg considers the best of the input, with no preference
    given: its index, a stream value, and a copy of its codec parameters. It
    releases the runtime lock.

    @raise Error with [`Stream_not_found] when the input has no such stream. *)
val find_best_audio_stream :
  input container -> int * (input, audio, 'a) stream * audio Avcodec.params

(** As {!find_best_audio_stream}, for video. *)
val find_best_video_stream :
  input container -> int * (input, video, 'a) stream * video Avcodec.params

(** As {!find_best_audio_stream}, for subtitles. *)
val find_best_subtitle_stream :
  input container ->
  int * (input, subtitle, 'a) stream * subtitle Avcodec.params

(** The container of an input stream. It reads the stream value only and
    succeeds on a closed container. *)
val get_input : (input, _, _) stream -> input container

(** The index of the stream in its container. It reads the stream value only and
    succeeds on a closed container. *)
val get_index : (_, _, _) stream -> int

(** The container of an output stream. It reads the stream value only and
    succeeds on a closed container. *)
val get_output : (output, _, _) stream -> output container

(** An independent copy of the codec parameters of the stream. *)
val get_codec_params : (_, 'media, _) stream -> 'media Avcodec.params

(** The average frame rate of the stream; [None] when it is unset. *)
val get_avg_frame_rate : (_, video, _) stream -> Avutil.rational option

(** Sets the average frame rate of the stream; [None] unsets it. On an output it
    is accepted until the header is written.

    @raise Error with [`Failure] once the header of the output is written. *)
val set_avg_frame_rate : (_, video, _) stream -> Avutil.rational option -> unit

(** The time base of the stream: the unit, in seconds, of the timestamps of its
    packets. Each stream has its own. On an output, the muxer may replace it
    when it writes the header, so read it after the first write. *)
val get_time_base : (_, _, _) stream -> Avutil.rational

(** Sets the time base of the stream. It does not change the time base of the
    stream's encoder. On an output it is accepted until the header is written,
    and the muxer may still replace the value when it writes the header.

    @raise Error with [`Failure] once the header of the output is written. *)
val set_time_base : (_, _, _) stream -> Avutil.rational -> unit

(** The number of samples per channel the encoder of the stream wants in each
    frame; 0 for an encoder that accepts any.

    @raise Error
      with [`Failure] when the stream has no encoder, as a copied stream. *)
val get_frame_size : (output, audio, _) stream -> int

(** The sample aspect ratio of the stream: the width of a pixel divided by its
    height. [None] when it is unknown. *)
val get_pixel_aspect : (_, video, _) stream -> Avutil.rational option

(** The duration of the stream in units of [format], rounded to the nearest
    unit. [None] when FFmpeg does not know it, and when it rounds to 0.

    @param format defaults to [`Second]. *)
val get_duration :
  ?format:Time_format.t -> (input, _, _) stream -> Int64.t option

(** The metadata of the stream, as key and value pairs in the order FFmpeg
    stores them. *)
val get_metadata : (input, _, _) stream -> (string * string) list

(** A packet read from an input, with the index of its stream. The tag follows
    the actual media type of the stream. *)
type packet_result =
  [ `Audio_packet of int * audio Avcodec.Packet.t
  | `Video_packet of int * video Avcodec.Packet.t
  | `Subtitle_packet of int * subtitle Avcodec.Packet.t
  | `Data_packet of int * [ `Data ] Avcodec.Packet.t ]

(** A frame decoded from an input, with the index of its stream. *)
type frame_result =
  [ `Audio_frame of int * audio frame
  | `Video_frame of int * video frame
  | `Subtitle_frame of int * Avutil.Subtitle.frame ]

(** What one call of {!read_input} returns. *)
type input_result = [ packet_result | frame_result ]

(** [read_input container] returns the next packet or frame of the selected
    streams. It blocks while FFmpeg reads and decodes, with the runtime lock
    released.

    The [*_packet] lists select streams to return as packets, the [*_frame]
    lists streams to decode and return as frames. All default to empty. A stream
    in both is returned as packets. Each call returns one result or raises, in
    this order:

    + A frame that a decoder holds ready from a packet read by an earlier call
      is returned, whatever the selections of this call.
    + A packet is read. A packet of a stream that is neither audio, video,
      subtitle nor data is dropped.
    + A packet of a stream selected as packets is returned.
    + A packet of a stream selected as frames is given to the decoder of the
      stream. The first frame it produces is returned; further frames of the
      same packet are returned by the following calls. A packet that produces no
      frame is skipped and the next one is read.
    + Any other packet is given to [on_unhandled_packet], and the next one is
      read.
    + At the end of the input, the decoders of the audio and video streams
      selected as frames by this call are drained: each frame they still hold,
      because of reordering or threading, is returned, one per call. When none
      remains the call raises [Error `Eof], and so does every later call until a
      {!seek}.

    With empty selections the call consumes the whole input and raises
    [Error `Eof].

    A decoder is opened the first time a packet of its stream is decoded: the
    preferred decoder given to {!open_input}, else FFmpeg's default for the
    codec, with the options configured for the stream. When the opening fails
    the call raises and a later call attempts it again.

    A returned packet or frame is independent of the container: later reads do
    not change it. A decoded subtitle with no timestamp takes the one of its
    packet. A subtitle with a zero end display time takes the duration of its
    packet, in milliseconds, when that is positive.

    @param on_unhandled_packet
      called, on the calling thread, with each packet of an audio, video,
      subtitle or data stream that is in no selection. The packet is the
      function's own. The container is not in use while it runs. When it raises,
      the read ends with that exception and the next read continues with the
      following packet.
    @raise Error
      with [`Eof] at the end of the input; with [`Decoder_not_found] when a
      stream selected as frames has no decoder; with [`Exit] when the interrupt
      function aborted the read; with [`Failure] when a selected stream belongs
      to another container. *)
val read_input :
  ?on_unhandled_packet:(packet_result -> unit) ->
  ?audio_packet:(input, audio, [ `Packet ]) stream list ->
  ?audio_frame:(input, audio, [ `Frame ]) stream list ->
  ?video_packet:(input, video, [ `Packet ]) stream list ->
  ?video_frame:(input, video, [ `Frame ]) stream list ->
  ?subtitle_packet:(input, subtitle, [ `Packet ]) stream list ->
  ?subtitle_frame:(input, subtitle, [ `Frame ]) stream list ->
  ?data_packet:(input, [ `Data ], [ `Packet ]) stream list ->
  input container ->
  input_result

(** The flags of {!seek}, with the meaning of FFmpeg's [AVSEEK_FLAG_*]
    constants: [Seek_flag_backward] seeks backward, [Seek_flag_byte] takes the
    positions as byte offsets, [Seek_flag_any] accepts any frame, non-key frames
    included, and [Seek_flag_frame] takes the positions as frame numbers. *)
type seek_flag =
  | Seek_flag_backward
  | Seek_flag_byte
  | Seek_flag_any
  | Seek_flag_frame

(** [seek ~fmt ~ts container] repositions the input at time [ts]. It blocks
    while FFmpeg seeks, with the runtime lock released.

    On success every opened decoder of the container is reset and every frame
    decoded before the seek is discarded: the first frame {!read_input} returns
    afterwards belongs to the new position. The end-of-input condition is
    cleared. On failure the decoders are unchanged.

    @param flags defaults to none.
    @param stream
      the stream the positions refer to. They are converted from [fmt] to the
      time base of that stream. Without it they are converted to FFmpeg's
      internal time base.
    @param min_ts the lowest acceptable position. Unbounded by default.
    @param max_ts the highest acceptable position. Unbounded by default.
    @param fmt
      the unit of [ts], [min_ts] and [max_ts]. With [Seek_flag_byte] or
      [Seek_flag_frame] the three values are passed as given and [fmt] is not
      read.
    @raise Error
      with the mapped FFmpeg code when the seek fails; with [`Failure] when
      [stream] belongs to another container. *)
val seek :
  ?flags:seek_flag list ->
  ?stream:(input, _, _) stream ->
  ?min_ts:Int64.t ->
  ?max_ts:Int64.t ->
  fmt:Time_format.t ->
  ts:Int64.t ->
  input container ->
  unit

(** [open_output url] opens [url] for writing. It blocks while FFmpeg opens the
    I/O, with the runtime lock released. Nothing is written yet: streams are
    added next, and the header is written by the first write.

    An output is open, then started once its header is written, then closed. The
    header is written by the first {!write_packet}, {!write_frame} or
    {!write_subtitle_frame}, or by {!close}. A failed header write leaves the
    output open and the next write attempts it again. Streams, metadata, time
    bases and frame rates are accepted until the header is written.

    Any failure releases everything opened so far.

    @param interrupt
      as for {!open_input}. It covers every blocking I/O of the container,
      including I/O the muxer opens by itself.
    @param format the muxer. By default FFmpeg guesses it from [url].
    @param interleaved
      when [true], the default, the muxer buffers and orders the written packets
      across streams. When [false] they are written in call order.
    @param opts
      consumed, in this order, by the generic container options, the private
      options of the muxer, and the I/O protocol. On success the consumed
      entries are removed from the table.

    For a format that needs no file, a device for one, no I/O is opened. *)
val open_output :
  ?interrupt:(unit -> bool) ->
  ?format:(output, _) format ->
  ?interleaved:bool ->
  ?opts:opts ->
  string ->
  output container

(** [open_output_format format] opens an output of a format that needs no file,
    a device for one. [interleaved] is as for {!open_output}. [opts] is consumed
    by the generic container options, then by the private options of the muxer.

    @raise Error with [`Failure] when the format needs a file. *)
val open_output_format :
  ?interleaved:bool -> ?opts:opts -> (output, _) format -> output container

(** [open_output_stream write format] opens an output of the given format that
    writes through [write] instead of a URL. [interleaved] and [opts] are as for
    {!open_output_format}.

    The container keeps [write], [seek] and its I/O buffer alive until it is
    released. The writes, {!flush} and {!close} call the functions.

    @param seek the function FFmpeg calls to reposition the output.
    @raise Error with [`Failure] when the format needs no file. *)
val open_output_stream :
  ?opts:opts ->
  ?interleaved:bool ->
  ?seek:seek ->
  write ->
  (output, _) format ->
  output container

(** Whether the header of the output is written. *)
val output_started : output container -> bool

(** Replaces the whole metadata of the output with the list. Entries absent from
    the list are removed; a key that appears twice keeps its last value. On
    failure the metadata is unchanged.

    @raise Error with [`Failure] once the header is written. *)
val set_output_metadata : output container -> (string * string) list -> unit

(** As {!set_output_metadata}, for the metadata of one stream. It accepts input
    streams too. *)
val set_metadata : (_, _, _) stream -> (string * string) list -> unit

(** [new_stream_copy ~params output] adds a stream that receives packets encoded
    elsewhere, through {!write_packet}. It is {!new_uninitialized_stream_copy}
    followed by {!initialize_stream_copy}.

    The stream gets a copy of [params], with the codec tag cleared so that the
    muxer chooses its own. Only the codec parameters are copied. The time base
    is the muxer's choice unless {!set_time_base} is called. The average frame
    rate is not part of the codec parameters: set it with {!set_avg_frame_rate},
    typically from {!get_avg_frame_rate} of the source stream.

    @raise Error with [`Failure] once the header is written. *)
val new_stream_copy :
  params:'mode Avcodec.params ->
  output container ->
  (output, 'mode, [ `Packet ]) stream

(** A stream reserved in an output and not yet given its codec parameters. It
    keeps the output alive. *)
type uninitialized_stream_copy

(** Reserves a stream in the output, so that its index is known before its codec
    parameters are. The stream is added at once and takes the next index.

    @raise Error with [`Failure] once the header is written. *)
val new_uninitialized_stream_copy :
  output container -> uninitialized_stream_copy

(** Gives a reserved stream a copy of [params], as {!new_stream_copy} does, and
    returns the stream.

    @raise Error
      with [`Failure] on a second initialisation of the same reservation, and
      once the header is written. *)
val initialize_stream_copy :
  params:'mode Avcodec.params ->
  uninitialized_stream_copy ->
  (output, 'mode, [ `Packet ]) stream

(** [new_audio_stream ~channel_layout ~sample_rate ~sample_format ~time_base
     ~codec output] adds a stream that receives audio frames through
    {!write_frame} and encodes them with [codec]. It releases the runtime lock
    while the encoder is opened.

    The encoder is created and opened first, then the muxer accepts the stream,
    then the stream takes the time base and the codec parameters of the encoder.
    The output owns the encoder and frees it when it is released. When the
    output format wants codec headers out of band, the encoder is asked for
    global headers. A failure of the encoder leaves the output unchanged. A
    failure after the muxer accepted the stream leaves the output failed.

    @param opts
      options of the encoder. On success the consumed entries are removed from
      the table. An entry that addresses the same setting as a typed argument
      prevails over it. The encoder is opened with an automatic thread count
      unless a [threads] entry is given.
    @param sample_rate in Hz.
    @param time_base
      the time base of the encoder, in which the timestamps of the frames are
      expressed.
    @raise Error with [`Failure] once the header is written. *)
val new_audio_stream :
  ?opts:opts ->
  channel_layout:Channel_layout.t ->
  sample_rate:int ->
  sample_format:Avutil.Sample_format.t ->
  time_base:Avutil.rational ->
  codec:[ `Encoder ] Avcodec.Audio.t ->
  output container ->
  (output, audio, [ `Frame ]) stream

(** As {!new_audio_stream}, for a stream that receives video frames.

    @param frame_rate
      the frame rate given to the encoder, and the average frame rate of the
      stream. By default both stay unset.
    @param hardware_context
      a hardware device context or hardware frame context for the encoder. With
      a frame context, {!write_frame} uploads each frame before encoding it.
    @param width in pixels.
    @param height in pixels. *)
val new_video_stream :
  ?opts:opts ->
  ?frame_rate:Avutil.rational ->
  ?hardware_context:Avcodec.Video.hardware_context ->
  pixel_format:Avutil.Pixel_format.t ->
  width:int ->
  height:int ->
  time_base:Avutil.rational ->
  codec:[ `Encoder ] Avcodec.Video.t ->
  output container ->
  (output, video, [ `Frame ]) stream

(** As {!new_audio_stream}, for a stream that receives subtitles through
    {!write_subtitle_frame}.

    @param header
      the subtitle header given to the encoder of a text subtitle codec. For
      such a codec it defaults to {!Avutil.Subtitle.header_ass_default} [()]. *)
val new_subtitle_stream :
  ?opts:opts ->
  ?header:string ->
  time_base:Avutil.rational ->
  codec:[ `Encoder ] Avcodec.Subtitle.t ->
  output container ->
  (output, subtitle, [ `Frame ]) stream

(** Adds a data stream of the given codec identifier and time base, with no
    encoder. It receives packets through {!write_packet}.

    @raise Error with [`Failure] once the header is written. *)
val new_data_stream :
  time_base:Avutil.rational ->
  codec:Avcodec.Unknown.id ->
  output container ->
  (output, [ `Data ], [ `Packet ]) stream

(** The codec string of the stream for an HLS playlist, in the style of RFC
    6381, computed from its codec parameters:

    - H.264: [avc1.] and the profile, constraint and level bytes of the SPS in
      hexadecimal, when the extradata starts with an Annex-B SPS.
    - HEVC: [hvc1.<profile>.4.L<level>.B01] when the codec tag is [hvc1] and the
      profile and level are known; else, for a codec tag that is set, the tag as
      its four characters.
    - AAC: [mp4a.40.] and the profile plus one, when the profile is known.
    - MP2: [mp4a.40.33]. MP3: [mp4a.40.34]. AC-3: [ac-3]. E-AC-3: [ec-3]. FLAC:
      [fLaC].

    [None] for every other codec and case. *)
val codec_attr : _ stream -> string option

(** The bit rate of the stream, in bits per second, when its codec parameters
    give one; else the maximum bit rate of its coded-picture-buffer properties
    when it has them and it is not 0; else [None]. *)
val bitrate : _ stream -> int option

(** [write_packet stream time_base packet] writes a packet to the stream.
    [time_base] is the time base of the timestamps and duration of [packet];
    they are converted to the time base of the stream. The header of the output
    is written first when it is not written yet. It blocks while FFmpeg writes,
    with the runtime lock released.

    The muxer takes a reference of its own: [packet] is unchanged and stays
    usable. With an interleaved output the muxer may hold the packet until
    packets of the other streams arrive; {!flush} and {!close} write it out.

    @raise Error with the mapped FFmpeg code when the header or the write fails.
*)
val write_packet :
  (output, 'media, [ `Packet ]) stream ->
  Avutil.rational ->
  'media Avcodec.Packet.t ->
  unit

(** [write_frame stream frame] encodes [frame] with the encoder of the stream
    and writes every packet that becomes available. The header of the output is
    written first when it is not written yet. It blocks while FFmpeg encodes and
    writes, with the runtime lock released.

    The timestamps of the packets are converted from the time base of the
    encoder to the time base of the stream. A frame that produces no packet is
    not an error: the encoder may hold frames, and {!close} writes what it still
    holds. With a hardware frame context the frame is uploaded first, its
    properties included. [frame] is unchanged.

    @param on_keyframe
      called, on the calling thread, before each key packet is given to the
      muxer. The position {!tell} reports at that moment is the start of that
      key packet. The container is not in use while it runs: it may call
      {!flush} or {!tell} on it. When it raises, the key packet is written, then
      the exception propagates; packets still in the encoder are written by the
      next write or by {!close}.
    @raise Error with [`Failure] when the stream has no encoder. *)
val write_frame :
  ?on_keyframe:(unit -> unit) ->
  (output, 'media, [ `Frame ]) stream ->
  'media frame ->
  unit

(** [write_subtitle_frame stream subtitle] encodes a subtitle and writes it as
    one packet. The header of the output is written first when it is not written
    yet. It releases the runtime lock.

    The timestamp of the packet is the timestamp of the subtitle plus its start
    display time; its duration is the end display time minus the start display
    time. A subtitle that encodes to nothing writes no packet. [subtitle] is
    read only.

    @raise Error
      with the code the encoder reports when the encoded subtitle exceeds the
      size the binding allows for one. *)
val write_subtitle_frame :
  (output, subtitle, [ `Frame ]) stream -> Avutil.Subtitle.frame -> unit

(** On an output whose header is written, makes the muxer write out the packets
    it buffers and flushes the I/O. Encoders are not flushed: the frames they
    hold are written by {!close}. On an output whose header is not written it
    does nothing. It blocks, with the runtime lock released. *)
val flush : output container -> unit

(** The byte position of the I/O of the container; [None] for a container with
    no I/O of its own. It takes the container exclusively, so it raises the
    in-use error from a custom I/O function of the same container. *)
val tell : _ container -> int option

(** Closes the container and releases what it owns: the I/O it opened, the
    decoders or encoders of its streams, and the functions installed on it. It
    blocks, with the runtime lock released. On a closed container it does
    nothing. A container in use by another operation raises the in-use error and
    stays open.

    On an output that is not failed, [close] first finalises the file. It
    flushes every audio and video encoder into the muxer, in stream order, which
    writes the header when it is not written yet. Then, when the header is
    written, it writes the trailer and flushes the I/O. An output with no
    stream, or with only copy and data streams to which nothing was written, is
    closed with no header and no trailer.

    Every step is attempted even when an earlier one failed, and the container
    is closed in every case.

    @raise Error with the first failure of the steps above. *)
val close : _ container -> unit
