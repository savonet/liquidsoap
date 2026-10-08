(** Bindings to libavcodec. The interface is the one of spec/avcodec.md §4. *)

open Avutil

(** The version of the libavcodec library loaded at run time. It is read once,
    when this module is loaded. *)
val version : version

(** A codec FFmpeg registers: the static description of one encoder or decoder
    implementation, such as ["libmp3lame"] or ["h264"].

    ['media] is the media kind ([audio], [video], [subtitle] or [[`Data]]) and
    ['mode] is {!type:encode} or {!type:decode}. Both are phantom parameters,
    and every function that returns a codec gives the parameters that match it.

    A codec holds no stream state. It is valid for the life of the process and
    is immutable, so any number of threads may use one. A decoder or an encoder
    is the working instance built from a codec, see {!Audio.create_decoder},
    {!Audio.create_encoder}, {!Video.create_decoder} and
    {!Video.create_encoder}.

    Equality between two codec values is not defined: compare their names. *)
type ('media, 'mode) codec

(** The parameters of one encoded stream: its codec identifier and the
    properties of its content, such as the sample rate and the channel layout of
    an audio stream or the picture size of a video stream. Parameters say how a
    stream is encoded. They hold no codec state and decode nothing.

    A value owns a private deep copy of the native parameters and shares nothing
    with the object it was read from. It is immutable and the garbage collector
    releases it. {!val:params} and {!BitstreamFilter.init} return parameters,
    and so do the libraries built on this one. *)
type 'media params

(** An opened decoder: the working state that turns the packets of one stream
    into frames, driven by {!val:decode} and {!flush_decoder}.

    A decoder value owns its native codec context. The garbage collector
    releases it. There is no explicit close, and collection flushes nothing.

    A decoder is in one of three states:
    - open, from its creation: {!val:decode} accepts packets;
    - draining, once {!flush_decoder} signalled the end of the stream and the
      decoder accepted it: {!val:decode} raises [Error `Eof];
    - drained, once the decoder delivered its last frame: {!val:decode} raises
      [Error `Eof] and {!flush_decoder} returns at once.

    A drained decoder stays drained. Decoding again takes a new decoder.

    {b Concurrent use.} A decoder is memory-safe under any interleaving of calls
    from several threads or domains. Each native send or receive step takes the
    decoder exclusively. An operation that finds the decoder taken by another
    thread raises [Error (`Failure "Object in use!")] at once; it does not wait.
    After any error the decoder is valid and in one of the three states.

    The interface builds audio and video decoders only: the types
    [subtitle decoder] and [[`Data] decoder] have no value. *)
type 'media decoder

(** An opened encoder: the working state that turns frames into the packets of
    one stream, driven by {!val:encode} and {!flush_encoder}.

    Ownership, release, the three states and the behaviour under concurrent use
    are those of a {!decoder}, with {!val:encode} and {!flush_encoder} in the
    place of {!val:decode} and {!flush_decoder}. A video encoder created with a
    hardware context holds a reference of its own to that context and keeps it
    alive.

    The interface builds audio and video encoders only: the types
    [subtitle encoder] and [[`Data] encoder] have no value. *)
type 'media encoder

(** The ['mode] parameter of a {!codec} that encodes. *)
type encode = [ `Encoder ]

(** The ['mode] parameter of a {!codec} that decodes. *)
type decode = [ `Decoder ]

(** A profile of a codec, as FFmpeg lists it in the codec's descriptor. [id] is
    FFmpeg's number for the profile (an [AV_PROFILE_*] constant) and
    [profile_name] is its name, the empty string when FFmpeg gives none. *)
type profile = { id : int; profile_name : string }

(** FFmpeg's description of a codec identifier. It describes the format, not one
    implementation of it: an encoder and a decoder of the same identifier share
    one descriptor.

    - [media_type]: the media kind of the format.
    - [name]: the short name of the format, the empty string when FFmpeg gives
      none.
    - [long_name]: a descriptive name, [None] when FFmpeg gives none.
    - [properties]: the properties of the format (intra-only, lossy, lossless
      and so on), in ascending order of FFmpeg's bit values. A property the
      bindings have no constructor for is left out.
    - [mime_types]: the MIME types associated with the format, in FFmpeg's
      order. Empty when FFmpeg lists none.
    - [profiles]: the profiles FFmpeg lists for the format, in FFmpeg's order.
      Empty when it lists none. *)
type descriptor = {
  media_type : Avutil.media_type;
  name : string;
  long_name : string option;
  properties : Codec_properties.t list;
  mime_types : string list;
  profiles : profile list;
}

(** The integer value of FFmpeg's [AV_CODEC_FLAG_QSCALE] codec flag ("use fixed
    qscale"). It is read once, when this module is loaded. *)
val flag_qscale : int

(** [params encoder] is a fresh, independent snapshot of the encoder's
    parameters, as FFmpeg derives them from the opened codec context. It is what
    a container needs to declare the stream the encoder produces.

    The call only reads the encoder.

    @raise Avutil.Error
      with the mapped FFmpeg code when FFmpeg fails to fill the parameters, and
      with [`Failure "Object in use!"] when another thread is inside a send or
      receive step on the encoder. *)
val params : 'media encoder -> 'media params

(** [descriptor params] is the descriptor of the codec identifier the parameters
    carry, [None] when FFmpeg has none for it. Every text of the result is a
    copy. *)
val descriptor : 'media params -> descriptor option

(** [time_base encoder] is the encoder's time base: the unit, in seconds, of the
    timestamps of the packets it produces.

    @raise Avutil.Error
      with [`Failure "Object in use!"] when another thread is inside a send or
      receive step on the encoder. *)
val time_base : 'media encoder -> Avutil.rational

(** [name codec] is the codec's name, the one {!Audio.find_encoder_by_name} and
    the other lookups by name accept. It is the empty string when FFmpeg gives
    none. *)
val name : _ codec -> string

(** A capability of a codec implementation, FFmpeg's [AV_CODEC_CAP_*] flags. *)
type capability = Codec_capabilities.t

(** [capabilities codec] is the list of the capabilities the codec declares, in
    ascending order of FFmpeg's bit values. A capability the bindings have no
    constructor for is left out. It accepts encoders and decoders. *)
val capabilities : ([< `Audio | `Video ], _) codec -> capability list

(** A way to set up hardware acceleration for a codec, FFmpeg's
    [AV_CODEC_HW_CONFIG_METHOD_*] flags. *)
type hw_config_method = Hw_config_method.t

(** One hardware configuration a codec supports.

    - [pixel_format]: for a decoder, a hardware pixel format it may decode to
      when suitable hardware is available. For an encoder, a pixel format it may
      accept; [`None] then stands for every pixel format the codec supports.
    - [methods]: the setup methods usable with this configuration, in ascending
      order of FFmpeg's bit values.
    - [device_type]: the type of hardware device of the configuration. FFmpeg
      sets it for the [`Hw_device_ctx] and [`Hw_frames_ctx] methods; it is
      meaningless for the others. *)
type hw_config = {
  pixel_format : Pixel_format.t;
  methods : hw_config_method list;
  device_type : HwContext.device_type;
}

(** [hw_configs codec] is the list of the hardware configurations the codec
    declares, in FFmpeg's order. It is empty for a codec with none. A
    configuration whose pixel format or device type the bindings have no
    constructor for is left out.

    The list tells which {!Video.hardware_context} a video encoder can be given:
    a device context of type [device_type] when [methods] has [`Hw_device_ctx],
    a frame context when it has [`Hw_frames_ctx]. *)
val hw_configs : ([< `Audio | `Video ], _) codec -> hw_config list

(** Packets: one unit of encoded data of a stream, with its timestamps, its
    flags and its side data. An encoder produces packets and a decoder consumes
    them; containers read and write them.

    {b Concurrent use.} A packet has no guard. Any number of threads may read
    one at the same time, including giving it to decoders, bitstream filters and
    containers. The setters and {!Packet.add_side_data} change the packet: the
    thread that calls one must be the only user of the packet at that moment,
    and the bindings do not detect a violation. *)
module Packet : sig
  (** A packet. The value owns the packet structure: its timestamps, flags,
      stream index and side data. The payload is a reference-counted buffer that
      other packets and FFmpeg may also reference; OCaml never changes it.

      The garbage collector releases a packet, and is told the size of its
      payload. No operation of the bindings empties or consumes a packet passed
      to it: the packet stays usable afterwards.

      ['media] is a phantom parameter. No operation relies on it for memory
      safety: a packet is opaque bytes to every consumer. *)
  type 'media t

  (** Packet flags:
      - [`Keyframe]: the packet contains a keyframe;
      - [`Corrupt]: the packet content is corrupted;
      - [`Discard]: the packet is needed to keep the decoder state valid, and
        its decoded output is to be dropped;
      - [`Trusted]: the packet comes from a trusted source;
      - [`Disposable]: the packet holds frames the decoder may discard, that is
        non-reference frames. *)
  type flag = [ `Keyframe | `Corrupt | `Discard | `Trusted | `Disposable ]

  (** ReplayGain information, FFmpeg's [AVReplayGain]. [track_gain] and
      [album_gain] are in microbels: divide by 100000 to get decibels. FFmpeg's
      "unknown" gain is [INT32_MIN], -2147483648. [track_peak] and [album_peak]
      are peak amplitudes where 100000 is full scale, 0 when unknown. *)
  type replaygain = {
    track_gain : int;
    track_peak : int;
    album_gain : int;
    album_peak : int;
  }

  (** The side data the bindings read and write:
      - [`Replaygain]: ReplayGain information of an audio stream;
      - [`Strings_metadata]: a list of key and value pairs;
      - [`Metadata_update]: a list of key and value pairs, the updated metadata
        that appeared in the stream. *)
  type side_data =
    [ `Replaygain of replaygain
    | `Strings_metadata of (string * string) list
    | `Metadata_update of (string * string) list ]

  (** [add_side_data packet data] attaches a copy of [data] to the packet.

      A packet holds one entry per kind of side data: adding a kind the packet
      already carries replaces that entry. On failure the packet is unchanged.

      For [`Replaygain], each gain must fit a signed 32-bit integer and each
      peak an unsigned 32-bit integer. For the two metadata kinds, each key and
      value ends at its first NUL byte, and a later pair replaces an earlier
      pair of the same key.

      @raise Avutil.Error
        with [`Failure _] when a gain does not fit a signed 32-bit integer or a
        peak an unsigned one.
      @raise Out_of_memory when the entry cannot be allocated. *)
  val add_side_data : 'media t -> side_data -> unit

  (** [side_data packet] is the packet's side data of the three kinds of
      {!type-side_data}, in the packet's order. The pairs of a metadata entry
      are in the order of the entry. An entry of any other kind, and a
      ReplayGain entry too short to hold the four fields, are left out.

      On a packet with no side data, [side_data p] after [add_side_data p d] is
      [[d]]. *)
  val side_data : 'media t -> side_data list

  (** [dup packet] is a new packet with a copy of the properties and of the side
      data of [packet]. Both packets reference the same payload: no payload byte
      is copied. Changing one packet through its setters leaves the other
      unchanged.

      @raise Avutil.Error with the mapped FFmpeg code when FFmpeg fails. *)
  val dup : 'media t -> 'media t

  (** [get_flags packet] is the list of the flags set on the packet, in the
      order of {!flag}. *)
  val get_flags : 'media t -> flag list

  (** [set_flags packet flags] replaces the packet's flags with [flags]. The
      empty list clears them all, and every bit FFmpeg defines outside {!flag}
      is cleared too. *)
  val set_flags : 'media t -> flag list -> unit

  (** [get_size packet] is the size of the payload, in bytes. *)
  val get_size : 'media t -> int

  (** [get_stream_index packet] is the index of the stream the packet belongs to
      in its container. *)
  val get_stream_index : 'media t -> int

  (** [set_stream_index packet index] sets the packet's stream index.

      @raise Avutil.Error with [`Failure _] when [index] does not fit a C [int].
  *)
  val set_stream_index : 'media t -> int -> unit

  (** [get_pts packet] is the presentation timestamp, in units of the time base
      of the packet's producer (the encoder's time base for an encoded packet).
      [None] when the packet has none. No time-base conversion happens here. *)
  val get_pts : 'media t -> Int64.t option

  (** [set_pts packet pts] sets the presentation timestamp. [None] removes it.
  *)
  val set_pts : 'media t -> Int64.t option -> unit

  (** [get_dts packet] is the decoding timestamp, in the same unit as the
      presentation timestamp. [None] when the packet has none. *)
  val get_dts : 'media t -> Int64.t option

  (** [set_dts packet dts] sets the decoding timestamp. [None] removes it. *)
  val set_dts : 'media t -> Int64.t option -> unit

  (** [get_duration packet] is the duration of the packet, in the same unit as
      its timestamps. [None] when it is unknown, which FFmpeg stores as 0. *)
  val get_duration : 'media t -> Int64.t option

  (** [set_duration packet duration] sets the duration. [None] marks it unknown.
      [Some 0L] is FFmpeg's "unknown" too and reads back as [None]. *)
  val set_duration : 'media t -> Int64.t option -> unit

  (** [get_position packet] is the byte position of the packet in its stream.
      [None] when it is unknown, which FFmpeg stores as -1. *)
  val get_position : 'media t -> Int64.t option

  (** [set_position packet position] sets the byte position. [None] marks it
      unknown. [Some (-1L)] is FFmpeg's "unknown" too and reads back as [None].
  *)
  val set_position : 'media t -> Int64.t option -> unit

  (** [to_bytes packet] is a fresh copy of the payload. *)
  val to_bytes : 'media t -> bytes

  (** [content packet] is a fresh copy of the payload. *)
  val content : 'media t -> string

  (** [create content] is a new packet whose payload is a copy of [content]. It
      has no timestamp, no duration, no position, stream index 0, no flag and no
      side data. The caller chooses ['media].

      @raise Avutil.Error
        with [`Failure _] when [content] is longer than a C [int] can count, and
        with the mapped FFmpeg code when FFmpeg fails to allocate the payload.
  *)
  val create : string -> 'media t
end

(** Audio codecs: lookup, supported configurations, decoders, encoders and the
    audio fields of codec parameters. *)
module Audio : sig
  (** An audio codec. *)
  type 'mode t = (audio, 'mode) codec

  (** The identifier of an audio format, FFmpeg's [AV_CODEC_ID_*] values of the
      audio family. An identifier names a format; several codecs may implement
      the same one. *)
  type id = Codec_id.audio

  (** [descriptor id] is FFmpeg's descriptor of the format, [None] when FFmpeg
      has none for it. Every text of the result is a copy. *)
  val descriptor : id -> descriptor option

  (** Every audio identifier the bindings know. *)
  val codec_ids : Codec_id.audio list

  (** The audio encoders FFmpeg registers, in FFmpeg's order. The list is built
      once, when this module is loaded. It holds the encoders whose identifier
      is in {!id}, so {!get_id} succeeds on each of them. *)
  val encoders : encode t list

  (** The audio decoders FFmpeg registers, under the rules of {!encoders}. *)
  val decoders : decode t list

  (** [find_encoder_by_name name] is the audio encoder FFmpeg registers under
      [name], for example ["libmp3lame"].

      @raise Avutil.Error
        with [`Encoder_not_found] when FFmpeg has no encoder of that name, or
        has one that is not an audio codec. *)
  val find_encoder_by_name : string -> encode t

  (** [find_encoder id] is an encoder FFmpeg registers for the format [id].

      @raise Avutil.Error
        with [`Encoder_not_found] when FFmpeg has no encoder for it. *)
  val find_encoder : id -> encode t

  (** [find_decoder_by_name name] is the audio decoder FFmpeg registers under
      [name].

      @raise Avutil.Error
        with [`Decoder_not_found] when FFmpeg has no decoder of that name, or
        has one that is not an audio codec. *)
  val find_decoder_by_name : string -> decode t

  (** [find_decoder id] is a decoder FFmpeg registers for the format [id].

      @raise Avutil.Error
        with [`Decoder_not_found] when FFmpeg has no decoder for it. *)
  val find_decoder : id -> decode t

  (** [get_supported_channel_layouts codec] is the list of the channel layouts
      the codec declares it supports, in the codec's order. Each layout is an
      independent copy. The empty list means the codec declares none, which
      FFmpeg uses for a codec with no restriction.

      @raise Avutil.Error with the mapped FFmpeg code when the query fails. *)
  val get_supported_channel_layouts : _ t -> Avutil.Channel_layout.t list

  (** [get_supported_sample_formats codec] is the list of the sample formats the
      codec declares it supports, in the codec's order. The empty list means the
      codec declares none. A format the bindings have no constructor for is left
      out.

      @raise Avutil.Error with the mapped FFmpeg code when the query fails. *)
  val get_supported_sample_formats : _ t -> Avutil.Sample_format.t list

  (** [get_supported_sample_rates codec] is the list of the sample rates, in Hz,
      the codec declares it supports, in the codec's order. The empty list means
      the codec declares none.

      @raise Avutil.Error with the mapped FFmpeg code when the query fails. *)
  val get_supported_sample_rates : _ t -> int list

  (** [find_best_channel_layout codec default] is a channel layout the codec
      supports, [default] when possible. It is [default] when the codec declares
      no layout or declares one equal to it under
      {!Avutil.Channel_layout.compare}. Otherwise it is the first layout the
      codec declares.

      @raise Avutil.Error as {!get_supported_channel_layouts}. *)
  val find_best_channel_layout :
    _ t -> Avutil.Channel_layout.t -> Avutil.Channel_layout.t

  (** [find_best_sample_format codec default] is [default] when the codec
      declares no sample format or declares [default]. Otherwise it is the first
      sample format the codec declares.

      @raise Avutil.Error as {!get_supported_sample_formats}. *)
  val find_best_sample_format :
    _ t -> Avutil.Sample_format.t -> Avutil.Sample_format.t

  (** [find_best_sample_rate codec default] is [default] when the codec declares
      no sample rate or declares [default]. Otherwise it is the first sample
      rate the codec declares.

      @raise Avutil.Error as {!get_supported_sample_rates}. *)
  val find_best_sample_rate : _ t -> int -> int

  (** [create_decoder ?params codec] allocates a decoder for [codec] and opens
      it. The decoder decodes in software and takes no option. FFmpeg chooses
      its number of threads.

      Opening releases the OCaml runtime lock: other OCaml threads run
      meanwhile. On failure everything allocated is released.

      @param params
        The parameters of the stream to decode, as read from a container. They
        are copied onto the decoder before it is opened; the value is read and
        not retained. Without [params] the decoder opens with the codec's
        defaults.
      @raise Avutil.Error
        with the mapped FFmpeg code when the parameters cannot be applied or the
        codec cannot be opened. *)
  val create_decoder : ?params:audio params -> decode t -> audio decoder

  (** [sample_format decoder] is the sample format of the frames the decoder
      currently produces.

      @raise Avutil.Error
        with [`Failure "Object in use!"] when another thread is inside a send or
        receive step on the decoder, and with [`Failure _] for a format the
        bindings have no constructor for. *)
  val sample_format : audio decoder -> Sample_format.t

  (** [create_encoder ?opts ~channel_layout ~sample_rate ~sample_format
       ~time_base codec] allocates an encoder for [codec], sets the four typed
      arguments on it, and opens it with the entries of [opts].

      Opening releases the OCaml runtime lock: other OCaml threads run
      meanwhile. On failure everything allocated is released and [opts] is
      untouched.

      @param opts
        Options for the codec. The opening consumes the entries it recognises:
        the codec's private options first, then the generic codec context
        options. After a successful call the table holds exactly the entries
        that were not consumed, so a key still in it was not used. An entry that
        addresses the same setting as a typed argument is applied after it and
        prevails. FFmpeg chooses the number of threads unless a [threads] entry
        is given. No table means no option and nothing reported.
      @param channel_layout
        The layout of the frames the encoder will receive. It is copied.
      @param sample_rate The sample rate of those frames, in Hz.
      @param sample_format The sample format of those frames.
      @param time_base
        The unit, in seconds, of the timestamps of the frames given to the
        encoder and of the packets it produces. [{ num = 1; den = sample_rate }]
        counts in samples.
      @raise Avutil.Error
        with the mapped FFmpeg code when the codec cannot be opened, for example
        when it rejects one of the arguments or the value of an option, and with
        [`Failure _] when [sample_rate] or a member of [time_base] does not fit
        a C [int]. *)
  val create_encoder :
    ?opts:opts ->
    channel_layout:Channel_layout.t ->
    sample_rate:int ->
    sample_format:Avutil.Sample_format.t ->
    time_base:Avutil.rational ->
    encode t ->
    audio encoder

  (** [frame_size encoder] is the number of samples per channel the encoder
      wants in each frame given to {!val:Avcodec.encode}. It is 0 for an encoder
      that accepts frames of any size.

      @raise Avutil.Error
        with [`Failure "Object in use!"] when another thread is inside a send or
        receive step on the encoder. *)
  val frame_size : audio encoder -> int

  (** [get_name codec] is the codec's name, as {!val:Avcodec.name}. *)
  val get_name : _ codec -> string

  (** [get_description codec] is the codec's descriptive name, the empty string
      when FFmpeg gives none. *)
  val get_description : _ codec -> string

  (** [string_of_id id] is FFmpeg's name of the format, for example ["mp3"]. *)
  val string_of_id : id -> string

  (** [get_id codec] is the identifier of the format the codec implements.

      @raise Avutil.Error
        with [`Failure _] for a codec whose identifier is outside {!id}. No
        codec of {!encoders} or {!decoders} is one, but the lookups by name can
        return one: FFmpeg types a few codecs as audio while their identifier is
        in {!Unknown.id}. *)
  val get_id : _ t -> id

  (** [get_params_id params] is the codec identifier of the parameters.

      @raise Avutil.Error
        with [`Failure _] when the identifier is outside {!id}. *)
  val get_params_id : audio params -> id

  (** [get_channel_layout params] is an independent copy of the channel layout
      of the stream. *)
  val get_channel_layout : audio params -> Avutil.Channel_layout.t

  (** [get_nb_channels params] is the number of channels of the stream's channel
      layout. *)
  val get_nb_channels : audio params -> int

  (** [get_sample_format params] is the sample format of the stream.

      @raise Avutil.Error
        with [`Failure _] for a format the bindings have no constructor for. *)
  val get_sample_format : audio params -> Avutil.Sample_format.t

  (** [get_bit_rate params] is the average bit rate of the encoded stream, in
      bits per second. *)
  val get_bit_rate : audio params -> int

  (** [get_sample_rate params] is the sample rate of the stream, in Hz. *)
  val get_sample_rate : audio params -> int
end

(** Video codecs: lookup, supported configurations, decoders, encoders and the
    video fields of codec parameters. *)
module Video : sig
  (** A video codec. *)
  type 'mode t = (video, 'mode) codec

  (** The identifier of a video format, FFmpeg's [AV_CODEC_ID_*] values of the
      video family. An identifier names a format; several codecs may implement
      the same one. *)
  type id = Codec_id.video

  (** [descriptor id] is FFmpeg's descriptor of the format, [None] when FFmpeg
      has none for it. Every text of the result is a copy. *)
  val descriptor : id -> descriptor option

  (** Every video identifier the bindings know. *)
  val codec_ids : Codec_id.video list

  (** The video encoders FFmpeg registers, in FFmpeg's order. The list is built
      once, when this module is loaded. It holds the encoders whose identifier
      is in {!id}, so {!get_id} succeeds on each of them. *)
  val encoders : encode t list

  (** The video decoders FFmpeg registers, under the rules of {!encoders}. *)
  val decoders : decode t list

  (** [find_encoder_by_name name] is the video encoder FFmpeg registers under
      [name], for example ["libx264"].

      @raise Avutil.Error
        with [`Encoder_not_found] when FFmpeg has no encoder of that name, or
        has one that is not a video codec. *)
  val find_encoder_by_name : string -> encode t

  (** [find_encoder id] is an encoder FFmpeg registers for the format [id].

      @raise Avutil.Error
        with [`Encoder_not_found] when FFmpeg has no encoder for it. *)
  val find_encoder : id -> encode t

  (** [find_decoder_by_name name] is the video decoder FFmpeg registers under
      [name].

      @raise Avutil.Error
        with [`Decoder_not_found] when FFmpeg has no decoder of that name, or
        has one that is not a video codec. *)
  val find_decoder_by_name : string -> decode t

  (** [find_decoder id] is a decoder FFmpeg registers for the format [id].

      @raise Avutil.Error
        with [`Decoder_not_found] when FFmpeg has no decoder for it. *)
  val find_decoder : id -> decode t

  (** [get_supported_frame_rates codec] is the list of the frame rates, in
      frames per second, the codec declares it supports, in the codec's order.
      The empty list means the codec declares none, which FFmpeg uses for a
      codec with no restriction.

      @raise Avutil.Error with the mapped FFmpeg code when the query fails. *)
  val get_supported_frame_rates : _ t -> Avutil.rational list

  (** [get_supported_color_spaces codec] is the list of the colour spaces the
      codec declares it supports, in the codec's order. The empty list means the
      codec declares none. A value the bindings have no constructor for is left
      out.

      @raise Avutil.Error with the mapped FFmpeg code when the query fails. *)
  val get_supported_color_spaces : _ t -> Avutil.Color_space.t list

  (** [get_supported_color_ranges codec] is the list of the colour ranges the
      codec declares it supports, under the rules of
      {!get_supported_color_spaces}.

      @raise Avutil.Error with the mapped FFmpeg code when the query fails. *)
  val get_supported_color_ranges : _ t -> Avutil.Color_range.t list

  (** [get_supported_pixel_formats codec] is the list of the pixel formats the
      codec declares it supports, under the rules of
      {!get_supported_color_spaces}. The list may hold hardware pixel formats.

      @raise Avutil.Error with the mapped FFmpeg code when the query fails. *)
  val get_supported_pixel_formats : _ t -> Avutil.Pixel_format.t list

  (** [find_best_frame_rate codec default] is [default] when the codec declares
      no frame rate or declares one equal to it as a fraction. Otherwise it is
      the first frame rate the codec declares.

      @raise Avutil.Error as {!get_supported_frame_rates}. *)
  val find_best_frame_rate : _ t -> Avutil.rational -> Avutil.rational

  (** [find_best_pixel_format ?hwaccel codec default] is a pixel format to give
      the codec, [default] when possible. It is [default] when the codec
      declares no pixel format or declares [default]. Otherwise it is the first
      format the codec declares that [hwaccel] allows, and [default] when none
      is left.

      @param hwaccel
        Whether a hardware pixel format, one whose descriptor carries the
        [`Hwaccel] flag, may be chosen in place of [default]. Defaults to
        [false]: hardware formats are skipped.
      @raise Avutil.Error as {!get_supported_pixel_formats}. *)
  val find_best_pixel_format :
    ?hwaccel:bool -> _ t -> Avutil.Pixel_format.t -> Avutil.Pixel_format.t

  (** [create_decoder ?params codec] allocates a decoder for [codec] and opens
      it. The decoder decodes in software and takes no option. FFmpeg chooses
      its number of threads.

      Opening releases the OCaml runtime lock: other OCaml threads run
      meanwhile. On failure everything allocated is released.

      @param params
        The parameters of the stream to decode, as read from a container. They
        are copied onto the decoder before it is opened; the value is read and
        not retained. Without [params] the decoder opens with the codec's
        defaults.
      @raise Avutil.Error
        with the mapped FFmpeg code when the parameters cannot be applied or the
        codec cannot be opened. *)
  val create_decoder : ?params:video params -> decode t -> video decoder

  (** A hardware context for a video encoder:
      - [`Device_context]: a hardware device. The encoder receives frames as
        they are given to it.
      - [`Frame_context]: a pool of hardware frames. The encoder uploads each
        frame given to it into a frame of the pool before encoding it, see
        {!val:Avcodec.encode}. *)
  type hardware_context =
    [ `Device_context of HwContext.device_context
    | `Frame_context of HwContext.frame_context ]

  (** [create_encoder ?opts ?frame_rate ?hardware_context ~pixel_format ~width
       ~height ~time_base codec] allocates an encoder for [codec], sets the
      typed arguments on it, and opens it with the entries of [opts].

      Opening releases the OCaml runtime lock: other OCaml threads run
      meanwhile. On failure everything allocated is released and [opts] is
      untouched.

      @param opts
        Options for the codec. The opening consumes the entries it recognises:
        the codec's private options first, then the generic codec context
        options. After a successful call the table holds exactly the entries
        that were not consumed, so a key still in it was not used. An entry that
        addresses the same setting as a typed argument is applied after it and
        prevails. FFmpeg chooses the number of threads unless a [threads] entry
        is given. No table means no option and nothing reported.
      @param frame_rate
        The frame rate of the stream, in frames per second. It is left unset
        without it, and a rate with a zero numerator counts as not given.
      @param hardware_context
        The hardware device or hardware frame pool the encoder works with, see
        {!hardware_context} and {!Avcodec.hw_configs}. The encoder takes a
        reference of its own to the context and keeps it alive.
      @param pixel_format The pixel format the encoder is opened with.
      @param width The picture width, in pixels.
      @param height The picture height, in pixels.
      @param time_base
        The unit, in seconds, of the timestamps of the frames given to the
        encoder and of the packets it produces.
      @raise Avutil.Error
        with the mapped FFmpeg code when the codec cannot be opened, for example
        when it rejects one of the arguments or the value of an option, and with
        [`Failure _] when [width], [height] or a member of [frame_rate] or
        [time_base] does not fit a C [int]. *)
  val create_encoder :
    ?opts:opts ->
    ?frame_rate:Avutil.rational ->
    ?hardware_context:hardware_context ->
    pixel_format:Avutil.Pixel_format.t ->
    width:int ->
    height:int ->
    time_base:Avutil.rational ->
    encode t ->
    video encoder

  (** [get_name codec] is the codec's name, as {!val:Avcodec.name}. *)
  val get_name : _ codec -> string

  (** [get_description codec] is the codec's descriptive name, the empty string
      when FFmpeg gives none. *)
  val get_description : _ codec -> string

  (** [string_of_id id] is FFmpeg's name of the format, for example ["h264"]. *)
  val string_of_id : id -> string

  (** [get_id codec] is the identifier of the format the codec implements.

      @raise Avutil.Error
        with [`Failure _] for a codec whose identifier is outside {!id}. No
        codec of {!encoders} or {!decoders} is one, but the lookups by name can
        return one: FFmpeg types a few codecs as video while their identifier is
        in {!Unknown.id}. *)
  val get_id : _ t -> id

  (** [get_params_id params] is the codec identifier of the parameters.

      @raise Avutil.Error
        with [`Failure _] when the identifier is outside {!id}. *)
  val get_params_id : video params -> id

  (** [get_width params] is the picture width, in pixels. *)
  val get_width : video params -> int

  (** [get_height params] is the picture height, in pixels. *)
  val get_height : video params -> int

  (** [get_sample_aspect_ratio params] is the aspect ratio of one pixel, width
      over height, as the parameters store it. It is [0/1] when unknown. *)
  val get_sample_aspect_ratio : video params -> Avutil.rational

  (** [get_pixel_format params] is the pixel format of the stream, [None] when
      the parameters have none.

      @raise Avutil.Error
        with [`Failure _] for a format the bindings have no constructor for. *)
  val get_pixel_format : video params -> Avutil.Pixel_format.t option

  (** [get_pixel_aspect params] is {!get_sample_aspect_ratio} as an option:
      [None] when the ratio is unknown, that is when its numerator is 0. *)
  val get_pixel_aspect : video params -> Avutil.rational option

  (** [get_bit_rate params] is the average bit rate of the encoded stream, in
      bits per second. *)
  val get_bit_rate : video params -> int
end

(** Subtitle codecs: lookup only. This module builds no subtitle decoder or
    encoder. Subtitles are decoded and encoded through containers, in the [Av]
    library. *)
module Subtitle : sig
  (** A subtitle codec. *)
  type 'mode t = (subtitle, 'mode) codec

  (** The identifier of a subtitle format, FFmpeg's [AV_CODEC_ID_*] values of
      the subtitle family. *)
  type id = Codec_id.subtitle

  (** [descriptor id] is FFmpeg's descriptor of the format, [None] when FFmpeg
      has none for it. Every text of the result is a copy. *)
  val descriptor : id -> descriptor option

  (** Every subtitle identifier the bindings know. *)
  val codec_ids : Codec_id.subtitle list

  (** The subtitle encoders FFmpeg registers, in FFmpeg's order. The list is
      built once, when this module is loaded. It holds the encoders whose
      identifier is in {!id}, so {!get_id} succeeds on each of them. *)
  val encoders : encode t list

  (** The subtitle decoders FFmpeg registers, under the rules of {!encoders}. *)
  val decoders : decode t list

  (** [find_encoder_by_name name] is the subtitle encoder FFmpeg registers under
      [name].

      @raise Avutil.Error
        with [`Encoder_not_found] when FFmpeg has no encoder of that name, or
        has one that is not a subtitle codec. *)
  val find_encoder_by_name : string -> encode t

  (** [find_encoder id] is an encoder FFmpeg registers for the format [id].

      @raise Avutil.Error
        with [`Encoder_not_found] when FFmpeg has no encoder for it. *)
  val find_encoder : id -> encode t

  (** [find_decoder_by_name name] is the subtitle decoder FFmpeg registers under
      [name].

      @raise Avutil.Error
        with [`Decoder_not_found] when FFmpeg has no decoder of that name, or
        has one that is not a subtitle codec. *)
  val find_decoder_by_name : string -> decode t

  (** [find_decoder id] is a decoder FFmpeg registers for the format [id].

      @raise Avutil.Error
        with [`Decoder_not_found] when FFmpeg has no decoder for it. *)
  val find_decoder : id -> decode t

  (** [get_name codec] is the codec's name, as {!val:Avcodec.name}. *)
  val get_name : _ codec -> string

  (** [get_description codec] is the codec's descriptive name, the empty string
      when FFmpeg gives none. *)
  val get_description : _ codec -> string

  (** [string_of_id id] is FFmpeg's name of the format. *)
  val string_of_id : id -> string

  (** [get_id codec] is the identifier of the format the codec implements.

      @raise Avutil.Error
        with [`Failure _] for a codec whose identifier is outside {!id}. No
        codec of {!encoders} or {!decoders} is one. *)
  val get_id : _ t -> id

  (** [get_params_id params] is the codec identifier of the parameters.

      @raise Avutil.Error
        with [`Failure _] when the identifier is outside {!id}. *)
  val get_params_id : subtitle params -> id
end

(** The formats FFmpeg places in none of the audio, video and subtitle families.
    This module offers their identifiers only: no codec list, no lookup, no
    decoder and no encoder. *)
module Unknown : sig
  (** A codec of this family. No function of the interface returns one. *)
  type 'mode t = ([ `Data ], 'mode) codec

  (** The identifier of a format of this family. *)
  type id = Codec_id.unknown

  (** Every identifier of this family the bindings know. *)
  val codec_ids : Codec_id.unknown list

  (** [string_of_id id] is FFmpeg's name of the format. *)
  val string_of_id : id -> string

  (** [get_params_id params] is the codec identifier of the parameters.

      @raise Avutil.Error
        with [`Failure _] when the identifier is outside {!id}. *)
  val get_params_id : [ `Data ] params -> id
end

(** A codec identifier of any family: every member of FFmpeg's [AVCodecID]
    enumeration the bindings know. *)
type id = Codec_id.codec_id

(** [string_of_id id] is FFmpeg's name of the format. *)
val string_of_id : id -> string

(** Bitstream filters: transformations applied to the packets of a stream, from
    packets to packets.

    A filter instance is fed with {!BitstreamFilter.send_packet} and read with
    {!BitstreamFilter.receive_packet}. The library provides no drain loop: the
    caller alternates the two, reading until [Error `Eagain] before sending
    again, and until [Error `Eof] after {!BitstreamFilter.send_eof}. *)
module BitstreamFilter : sig
  (** A bitstream filter FFmpeg registers. Values come from {!filters} only: the
      record is private.

      - [name]: the filter's name, the empty string when FFmpeg gives none.
      - [codecs]: the codec identifiers the filter declares it works on, in
        FFmpeg's order. Empty when the filter accepts any. An identifier the
        bindings have no constructor for is left out.
      - [options]: the filter's private options, the ones {!init} accepts in
        [opts]. It lists no option for a filter that has none. *)
  type filter = private {
    name : string;
    codecs : id list;
    options : Avutil.Options.t;
  }

  (** An initialised instance of a bitstream filter, for one stream. The value
      owns the native filter context and the garbage collector releases it.

      {b Concurrent use.} {!send_packet}, {!send_eof} and {!receive_packet} each
      take the instance exclusively. One that finds the instance taken by
      another thread raises [Error (`Failure "Object in use!")] at once; it does
      not wait. After any error the instance is valid. *)
  type 'a t

  (** Every bitstream filter FFmpeg registers, in FFmpeg's order. The list is
      built once, when this module is loaded. *)
  val filters : filter list

  (** [init ?opts filter params] creates and initialises an instance of [filter]
      for a stream of parameters [params]. It returns the instance and the
      parameters of the stream the instance outputs.

      [params] is copied into the instance and is not modified. The returned
      parameters are an independent copy. The input time base keeps FFmpeg's
      default. Initialisation releases the OCaml runtime lock. On failure
      everything allocated is released and [opts] is untouched.

      @param opts
        Options for the filter, among those [filter.options] lists. After a
        successful call the table holds exactly the entries the filter did not
        consume, so a key still in it was not used. No table means no option and
        nothing reported.
      @raise Avutil.Error
        with [`Bsf_not_found] when FFmpeg has no filter of that name, and with
        the mapped FFmpeg code when an option value is rejected or the filter
        fails to initialise. *)
  val init : ?opts:opts -> filter -> 'a params -> 'a t * 'a params

  (** [send_packet filter packet] gives [packet] to the filter. The filter takes
      a reference of its own to the payload: [packet] is unchanged and stays
      usable. FFmpeg treats a packet with no payload and no side data as the end
      of the stream.

      The call releases the OCaml runtime lock.

      @raise Avutil.Error
        with [`Eagain] when the filter's output must be read with
        {!receive_packet} first, which is part of normal operation; with the
        mapped FFmpeg code on another refusal; and with
        [`Failure "Object in use!"] when another thread is using the instance.
  *)
  val send_packet : 'a t -> 'a Packet.t -> unit

  (** [send_eof filter] signals the end of the stream to the filter. The packets
      the filter still holds are then read with {!receive_packet}, until it
      raises [Error `Eof].

      The call releases the OCaml runtime lock.

      @raise Avutil.Error
        with the mapped FFmpeg code when the filter refuses the signal, and with
        [`Failure "Object in use!"] when another thread is using the instance.
  *)
  val send_eof : 'a t -> unit

  (** [receive_packet filter] is the next packet the filter has ready. The
      packet is a new value that owns what the filter produced.

      The call releases the OCaml runtime lock.

      @raise Avutil.Error
        with [`Eagain] when the filter needs more input and with [`Eof] when it
        is drained, both part of normal operation; with the mapped FFmpeg code
        when filtering fails; and with [`Failure "Object in use!"] when another
        thread is using the instance. *)
  val receive_packet : 'a t -> 'a Packet.t
end

(** [decode decoder f packet] gives [packet] to the decoder and calls [f] on
    every frame that becomes available, in order, each as soon as it is
    received. A packet may produce no frame, or several; producing none is not
    an error.

    The steps are:
    + deliver the frames the decoder has ready, which are those an earlier call
      interrupted by an exception left behind;
    + send the packet;
    + deliver the frames the decoder has ready.

    The decoder references the payload of [packet], which is unchanged and stays
    usable. Each frame passed to [f] is a new value that owns what the decoder
    produced. [f] may keep it.

    Each send and each receive releases the OCaml runtime lock: other OCaml
    threads run during the decoding work. [f] runs on the calling thread with
    the lock held, between two native steps. The decoder is free while [f] runs:
    [f] may call any operation on it. The call as a whole is not atomic with
    respect to other threads.

    An exception raised by [f] propagates unchanged. The frames not yet
    delivered stay in the decoder and the next {!val:decode} or {!flush_decoder}
    delivers them first. When [f] raises in the first step the packet was not
    sent; in the third step the decoder accepted it.

    @raise Avutil.Error
      with [`Eof] when the decoder is draining or drained, that is once
      {!flush_decoder} signalled the end of the stream; with the mapped FFmpeg
      code when decoding fails; and with [`Failure "Object in use!"] when
      another thread is inside a step on the decoder. It never raises
      [Error `Eagain]. *)
val decode : 'media decoder -> ('media frame -> unit) -> 'media Packet.t -> unit

(** [flush_decoder decoder f] signals the end of the stream to the decoder and
    calls [f] on every frame it still holds, in order. It is the call that ends
    a stream: collecting a decoder delivers nothing.

    What it does depends on the state of the decoder (see {!decoder}):
    - open: it delivers the frames the decoder has ready, signals the end of the
      stream, then delivers frames until the decoder reports the end of the
      stream. The decoder is then drained.
    - draining, after a flush that [f] interrupted by an exception: it delivers
      the remaining frames.
    - drained: it returns at once and calls [f] on nothing.

    The lock, the thread [f] runs on, the exceptions of [f] and the ownership of
    the frames are as in {!val:decode}.

    @raise Avutil.Error
      with the mapped FFmpeg code when decoding fails, and with
      [`Failure "Object in use!"] when another thread is inside a step on the
      decoder. It never raises [Error `Eagain]. *)
val flush_decoder : 'media decoder -> ('media frame -> unit) -> unit

(** [encode encoder f frame] gives [frame] to the encoder and calls [f] on every
    packet that becomes available, in order, each as soon as it is received. A
    frame may produce no packet, or several; producing none is not an error.

    The steps are those of {!val:decode}: deliver the packets the encoder has
    ready, send the frame, deliver again.

    The encoder references the buffers of [frame], which is unchanged and stays
    usable. An encoder created with a [`Frame_context] hardware context instead
    uploads the frame: it takes a frame from the pool of the context, transfers
    the pixel data to it, copies the properties of [frame] onto it, timestamps
    included, and encodes that frame. The pool frame is released after the send.

    Each packet passed to [f] is a new value that owns what the encoder
    produced. Its timestamps are in the encoder's time base, see
    {!val:time_base}.

    Each send, upload included, and each receive releases the OCaml runtime
    lock. [f] runs on the calling thread with the lock held, between two native
    steps, with the encoder free: [f] may call any operation on it. An exception
    raised by [f] propagates unchanged, and the packets not yet delivered stay
    in the encoder: the next {!val:encode} or {!flush_encoder} delivers them
    first.

    @raise Avutil.Error
      with [`Eof] when the encoder is draining or drained, that is once
      {!flush_encoder} signalled the end of the stream; with the mapped FFmpeg
      code when the upload or the encoding fails; and with
      [`Failure "Object in use!"] when another thread is inside a step on the
      encoder. It never raises [Error `Eagain]. *)
val encode : 'media encoder -> ('media Packet.t -> unit) -> 'media frame -> unit

(** [flush_encoder encoder f] signals the end of the stream to the encoder and
    calls [f] on every packet it still holds, in order. It is the call that ends
    a stream: collecting an encoder flushes nothing.

    The three states are handled as in {!flush_decoder}: an open encoder is
    drained, a draining one delivers what remains, a drained one returns at
    once. A drained encoder accepts no more frame: encoding again takes a new
    encoder.

    The lock, the thread [f] runs on, the exceptions of [f] and the ownership of
    the packets are as in {!val:encode}.

    @raise Avutil.Error
      with the mapped FFmpeg code when encoding fails, and with
      [`Failure "Object in use!"] when another thread is inside a step on the
      encoder. It never raises [Error `Eagain]. *)
val flush_encoder : 'media encoder -> ('media Packet.t -> unit) -> unit
