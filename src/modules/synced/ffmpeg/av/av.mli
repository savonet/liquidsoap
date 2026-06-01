(** This module perform demuxing then decoding for reading and coding then
    muxing for writing multimedia container formats. *)

open Avutil

val avformat_version : version

(* A value suitable for listing available options on a given container. *)
val container_options : Options.t

(** {5 Format} *)

module Format : sig
  (** Return the name of the input format *)
  val get_input_name : (input, _) format -> string

  (** Return the long name of the input format *)
  val get_input_long_name : (input, _) format -> string

  (** Guess input format based on its short name. *)
  val find_input_format : string -> (input, 'a) format option

  (** Return the name of the output format *)
  val get_output_name : (output, _) format -> string

  (** Return the long name of the output format *)
  val get_output_long_name : (output, _) format -> string

  (** Guess output format based on the passed arguments. *)
  val guess_output_format :
    ?short_name:string ->
    ?filename:string ->
    ?mime:string ->
    unit ->
    (output, 'a) format option

  (** Return the audio codec id of the output audio format *)
  val get_audio_codec_id : (output, audio) format -> Avcodec.Audio.id

  (** Return the video codec id of the output video format *)
  val get_video_codec_id : (output, video) format -> Avcodec.Video.id

  (** Return the subtitle codec id of the output subtitle format *)
  val get_subtitle_codec_id : (output, subtitle) format -> Avcodec.Subtitle.id
end

(** {5 Input} *)

type 'media stream_config = {
  codec : ('media, Avcodec.decode) Avcodec.codec option;
  opts : opts option;
}

(** [Av.open_input url] open the input [url] (a file name or http URL). After
    returning, if [opts] was passed, unused options are left in the hash table.
    The optional [configure_audio_stream], [configure_video_stream] and
    [configure_subtitle_stream] callbacks are called once per stream of their
    respective type after the container is opened. The returned [codec], if any,
    is used as the decoder for that stream; [opts] are passed as per-stream
    options to [avformat_find_stream_info]. Raise Error if the opening failed.
*)
val open_input :
  ?interrupt:(unit -> bool) ->
  ?format:(input, _) format ->
  ?opts:opts ->
  ?configure_audio_stream:(audio Avcodec.params -> audio stream_config) ->
  ?configure_video_stream:(video Avcodec.params -> video stream_config) ->
  ?configure_subtitle_stream:(subtitle Avcodec.params -> subtitle stream_config) ->
  string ->
  input container

type read = bytes -> int -> int -> int
type write = bytes -> int -> int -> int
type seek = int -> Unix.seek_command -> int

(** [Av.open_input_stream read] creates an input stream from the given read
    callback. Exceptions from the callback are caught and result in a native
    [Avutil.Error `Unknown] error. *)
val open_input_stream :
  ?format:(input, _) format ->
  ?opts:opts ->
  ?seek:seek ->
  read ->
  input container

(** [Av.get_input_duration ~format:fmt input] return the duration of an [input]
    in the [fmt] time format (in second by default). *)
val get_input_duration :
  ?format:Time_format.t -> input container -> Int64.t option

(** Return the input tag (key, value) list. *)
val get_input_metadata : input container -> (string * string) list

(** Return the input format of the container. *)
val get_input_format : input container -> (input, _) format option

(** [Av.set_input_metadata container tags] set the metadata of the [container]
    with the [tags] tag list. This can be used to clear existing metadata on an
    opened container. *)
val set_input_metadata : input container -> (string * string) list -> unit

(** Return a value of type [obj], suited for use with [Avutils.Options] getters.
*)
val input_obj : input container -> Options.obj

(** Input/output, audio/video/subtitle, mode stream type *)
type ('line, 'media, 'mode) stream

(** Return the audio stream list of the input. The result is a list of tuple
    containing the index of the stream in the container, the stream and the
    codec of the stream. *)
val get_audio_streams :
  'a container -> (int * ('a, audio, 'b) stream * audio Avcodec.params) list

(** Same as {!Av.get_audio_streams} for the video streams. *)
val get_video_streams :
  'a container -> (int * ('a, video, 'b) stream * video Avcodec.params) list

(** Same as {!Av.get_audio_streams} for the subtitle streams. *)
val get_subtitle_streams :
  'a container ->
  (int * ('a, subtitle, 'b) stream * subtitle Avcodec.params) list

(** Same as {!Av.get_audio_streams} for the data streams. *)
val get_data_streams :
  'a container ->
  (int * ('a, [ `Data ], 'b) stream * [ `Data ] Avcodec.params) list

(** Return the best audio stream of the input. The result is a tuple containing
    the index of the stream in the container, the stream and the codec of the
    stream. Raise Error if no stream could be found. *)
val find_best_audio_stream :
  input container -> int * (input, audio, 'a) stream * audio Avcodec.params

(** Same as {!Av.find_best_audio_stream} for the video streams. *)
val find_best_video_stream :
  input container -> int * (input, video, 'a) stream * video Avcodec.params

(** Same as {!Av.find_best_audio_stream} for the subtitle streams. *)
val find_best_subtitle_stream :
  input container ->
  int * (input, subtitle, 'a) stream * subtitle Avcodec.params

(** Return the input container of the input stream. *)
val get_input : (input, _, _) stream -> input container

(** Return the index of the stream. *)
val get_index : (_, _, _) stream -> int

(** [Av.get_codec stream] return the codec of the [stream]. Raise Error if the
    codec allocation failed. *)
val get_codec_params : (_, 'media, _) stream -> 'media Avcodec.params

(** [get_avg_frame_rate stream] returns the average frame rate, if set. *)
val get_avg_frame_rate : (_, video, _) stream -> Avutil.rational option

(** [set_avg_frame_rate stream rate] sets the average frame rate, if set. *)
val set_avg_frame_rate : (_, video, _) stream -> Avutil.rational option -> unit

(** [Av.get_time_base stream] return the time base of the [stream]. *)
val get_time_base : (_, _, _) stream -> Avutil.rational

(** [Av.get_container_stream_time_base ~index input] return the time base of the
    stream at index [index]. Raises [Not_found] is the stream is not found. *)
val get_container_stream_time_base : index:int -> _ container -> Avutil.rational

(** [Av.set_time_base stream time_base] set the [stream] time base to
    [time_base]. *)
val set_time_base : (_, _, _) stream -> Avutil.rational -> unit

(** [Av.get_frame_size stream] return the frame size for the given audio stream.
*)
val get_frame_size : (output, audio, _) stream -> int

(** [Av.get_pixel_aspect stream] return the pixel aspect of the [stream]. *)
val get_pixel_aspect : (_, video, _) stream -> Avutil.rational option

(** Same as {!Av.get_input_duration} for the input streams. *)
val get_duration :
  ?format:Time_format.t -> (input, _, _) stream -> Int64.t option

(** Same as {!Av.get_input_metadata} for the input streams. *)
val get_metadata : (input, _, _) stream -> (string * string) list

type packet_result =
  [ `Audio_packet of int * audio Avcodec.Packet.t
  | `Video_packet of int * video Avcodec.Packet.t
  | `Subtitle_packet of int * subtitle Avcodec.Packet.t
  | `Data_packet of int * [ `Data ] Avcodec.Packet.t ]

type frame_result =
  [ `Audio_frame of int * audio frame
  | `Video_frame of int * video frame
  | `Subtitle_frame of int * Avutil.Subtitle.frame ]

type input_result = [ packet_result | frame_result ]

(** Reads the selected streams if any or all streams otherwise. Return the next
    [Audio] [Video] [Subtitle] of [Data] index and packet or frame of the input
    or [Error `Eof] if the end of the input is reached. Raise Error if the
    reading failed.

    [on_unhandled_packet] receives unhandled packets. Defaults to: [fun _ -> ()]
*)
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

(** Seek mode. *)
type seek_flag =
  | Seek_flag_backward
  | Seek_flag_byte
  | Seek_flag_any
  | Seek_flag_frame

(** [Av.seek ?flags ?stream ?min_ts ?max_ts ~fmt ~ts container] seek in the
    container [container] to position [ts]. You can pass an optional [stream] to
    use for seeking, [max_ts] and [min_ts] to force seeking to happen within a
    given timestamp window and [flags] to specify certain property of the
    seeking operation. Raise Error if the seeking failed. *)
val seek :
  ?flags:seek_flag list ->
  ?stream:(input, _, _) stream ->
  ?min_ts:Int64.t ->
  ?max_ts:Int64.t ->
  fmt:Time_format.t ->
  ts:Int64.t ->
  input container ->
  unit

(** {5 Output} *)

(** [Av.open_output ?interrupt ?format ?interleaved ?opts filename] open the
    output file named [filename]. [interrupt] is used to interrupt blocking
    functions, [format] may contain an optional format, [interleaved] indicates
    if FFmpeg's interleaved API should be used, [opts] may contain any option
    settable on the stream's internal AVFormat. After returning, if [opts] was
    passed, unused options are left in the hash table. Raise Error if the
    opening failed. *)
val open_output :
  ?interrupt:(unit -> bool) ->
  ?format:(output, _) format ->
  ?interleaved:bool ->
  ?opts:opts ->
  string ->
  output container

(** [Av.open_stream callbacks] open the output container with the given
    callbacks. [opts] may contain any option settable on Ffmpeg avformat. After
    returning, if [opts] was passed, unused options are left in the hash table.
    Raise Error if the opening failed. Exceptions from the callback are caught
    and result in a native [Avutil.Error `Unknown] error. *)
val open_output_stream :
  ?opts:opts ->
  ?interleaved:bool ->
  ?seek:seek ->
  write ->
  (output, _) format ->
  output container

val reopen_output_stream : output container -> unit

(** Returns [true] if the output has already started, in which case no new *
    stream or metadata can be added. *)
val output_started : output container -> bool

(** [Av.set_output_metadata dst tags] set the metadata of the [dst] output with
    the [tags] tag list. This must be set before starting writing streams. Raise
    Error if a writing already taken place or if the setting failed. *)
val set_output_metadata : output container -> (string * string) list -> unit

(** Same as {!Av.set_output_metadata} for the output streams. *)
val set_metadata : (_, _, _) stream -> (string * string) list -> unit

(** Return the output container of the output stream. *)
val get_output : (output, _, _) stream -> output container

(* Create a new stream that only supports packet input and does not do any
   encoding. Used for remuxing with encoded data. *)
val new_stream_copy :
  params:'mode Avcodec.params ->
  output container ->
  (output, 'mode, [ `Packet ]) stream

(* Asynchronous stream copy creation *)
type uninitialized_stream_copy

(* Create a new uninitialized stream copy. This can be used to reserve a stream index
   in the output container while waiting for its actual parameters. *)
val new_uninitialized_stream_copy :
  output container -> uninitialized_stream_copy

(* Initialize a stream copy. *)
val initialize_stream_copy :
  params:'mode Avcodec.params ->
  uninitialized_stream_copy ->
  (output, 'mode, [ `Packet ]) stream

(** Add a new audio stream to the given container. Stream only supports frames
    and encodes its input.

    [opts] may contain any option settable on the stream's internal AVCodec.
    After returning, if [opts] was passed, unused options are left in the hash
    table.

    Frames passed to this stream for encoding must have a PTS set according to
    the given [time_base]. [1/sample_rate] is usually a good value for the
    [time_base].

    Please note that some codec require a fixed frame size, denoted by the
    absence of the [`Variable_frame_size] codec capabilities. In this case, the
    user is expected to pass frames containing exactly
    [Av.get_frame_size stream].

    [Avfilter] can be used to slice frames into frames of fixed size. See
    [Avfilter.Utils.convert_audio] for an example.

    Raise Error if the opening failed. *)
val new_audio_stream :
  ?opts:opts ->
  channel_layout:Channel_layout.t ->
  sample_rate:int ->
  sample_format:Avutil.Sample_format.t ->
  time_base:Avutil.rational ->
  codec:[ `Encoder ] Avcodec.Audio.t ->
  output container ->
  (output, audio, [ `Frame ]) stream

(** Add a new video stream to the given container. Stream only supports frames
    and encodes its input.

    [opts] may contain any option settable on the stream's internal AVCodec.
    After returning, if [opts] was passed, unused options are left in the hash
    table.

    Frames passed to this stream for encoding must have a PTS set according to
    the given [time_base]. [1/frame_rate] is usually a good value for the
    [time_base].

    [hardware_context] can be used to pass optional hardware device and frame
    context to enable hardware encoding on this stream.

    Raise Error if the opening failed. *)
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

(** Add a new subtitle stream to the given container. Stream only supports
    frames and encodes its input.

    [header] sets the subtitle header for the stream. For codecs with the
    [Text_sub] property (e.g. ASS, SubRip, WebVTT), a default ASS header is used
    when [header] is not provided. Use {!Avutil.Subtitle.header_ass_default} to
    retrieve the default header.

    [opts] may contain any option settable on the stream's internal AVCodec.
    After returning, if [opts] was passed, unused options are left in the hash
    table.

    Raise Error if the opening failed. *)
val new_subtitle_stream :
  ?opts:opts ->
  ?header:string ->
  time_base:Avutil.rational ->
  codec:[ `Encoder ] Avcodec.Subtitle.t ->
  output container ->
  (output, subtitle, [ `Frame ]) stream

(** Add a new data stream to the given container.

    [opts] may contain any option settable on the stream's internal AVCodec.
    After returning, if [opts] was passed, unused options are left in the hash
    table.

    Raise Error if the opening failed. *)
val new_data_stream :
  time_base:Avutil.rational ->
  codec:Avcodec.Unknown.id ->
  output container ->
  (output, [ `Data ], [ `Packet ]) stream

(** Return a codec attribute suitable for HLS playlists when available. *)
val codec_attr : _ stream -> string option

(** Return the stream's bitrate when available, suitable for HLS playlists. *)
val bitrate : _ stream -> int option

(** [Av.write_packet os time_base pkt] write the [pkt] packet to the [os] output
    stream. [time_base] is the packet's PTS/DTS/duration time base.

    Raise Error if the writing failed. *)
val write_packet :
  (output, 'media, [ `Packet ]) stream ->
  Avutil.rational ->
  'media Avcodec.Packet.t ->
  unit

(** [Av.write_frame ?on_keyframe os frm] write the [frm] frame to the [os]
    output stream.

    Frame PTS should be set and counted in units of [time_base], as passed when
    creating the stream

    If [on_keyframe] is provided, it is called on keyframe, _before_ the
    keyframe is submitted to the muxer.

    Raise Error if the writing failed. *)
val write_frame :
  ?on_keyframe:(unit -> unit) ->
  (output, 'media, [ `Frame ]) stream ->
  'media frame ->
  unit

(** [Av.write_subtitle_frame ?on_keyframe os frm] write the subtitle [frm] frame
    to the [os] output stream.

    Frame PTS should be set and counted in units of [time_base], as passed when
    creating the stream

    If [on_keyframe] is provided, it is called on keyframe, _before_ the
    keyframe is submitted to the muxer.

    Raise Error if the writing failed. *)
val write_subtitle_frame :
  (output, subtitle, [ `Frame ]) stream -> Avutil.Subtitle.frame -> unit

(** Flush the underlying muxer. *)
val flush : output container -> unit

(* If available, call [avio_tell] on the container's underlying AVIOContext. *)
val tell : _ container -> int option

(** Close an input or output container. *)
val close : _ container -> unit
