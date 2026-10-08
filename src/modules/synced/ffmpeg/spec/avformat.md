# av

Containers: demuxing and decoding on the input side, encoding and muxing on
the output side, custom I/O. The OCaml library is named `av`; it binds
libavformat. It follows [binding-contract.md](binding-contract.md); section
numbers match.

## 1. Scope

`Av` binds libavformat. It also drives libavcodec, for the decoders and
encoders a container owns, and uses libavutil for dictionaries, options and
rescaling.

It depends on `avutil` and `avcodec`, and on the OCaml `unix` library for
`Unix.seek_command`.

**Module initialisation** reads the version of the libavformat loaded at run
time, initialises FFmpeg's network layer, and wraps the option class of
containers as `container_options`. It cannot fail (I1).

**Provided to dependent libraries**, through its installed C header:

| Service          | Contract                                                                                                                   |
| ---------------- | -------------------------------------------------------------------------------------------------------------------------- |
| native container | The format context of a container value. Raises the state errors of the contract's §5.3. Needs the runtime lock.           |
| container guard  | Taking and releasing the guard of a container value.                                                                       |
| formats          | The native format of a format value; constructors that wrap an input or an output format. A null pointer raises a failure. |

## 2. Objects

### 2.1 Container — `input container`, `output container`

- **Native object**: one format context, the decoders or encoders of its
  streams, and the I/O context it opened or was given.
- **Creation**: `open_input`, `open_input_stream`, `open_output`,
  `open_output_format`, `open_output_stream`.
- **Ownership**: the container owns its format context, every per-stream
  codec context, and its I/O context.
- **Keeps alive**: the closures installed on it — interrupt, custom read,
  write and seek — from installation until release (C3, L8).
- **Kept alive by**: every stream value, every `uninitialized_stream_copy`,
  and the value `input_obj` returns (L3).
- **Release**: `close`, or collection.
- **Guard**: §6.2.

#### Release

`close` (L5, L6):

1. On a closed container, returns.
2. On an output that is not failed: flushes every audio and video encoder
   into the muxer, in stream order, which writes the header when it is not
   written yet; then, when the header is written, writes the trailer.
3. Releases the native object: closes the I/O the container opened, frees the
   codec contexts and the format context, drops the closures.
4. The container is closed. When a step of 2 failed, raises the first
   failure.

Every step of 2 is attempted even when an earlier one failed. A failure of the
trailer write or of the final I/O flush is reported like any other.

An output with no stream, or with only copy and data streams to which nothing
was written, is closed without a header or a trailer.

Release by collection performs step 3 only (L7). An output that is collected
without `close` is truncated: its encoders are not flushed and it has no
trailer.

A container whose closures reference it is not collected until it is closed
or the closures become unreachable some other way. Implementations SHOULD NOT
let a closure that FFmpeg invokes only during an operation pin its container.

#### States of an input container

| Operation                  | Open                      | Closed       |
| -------------------------- | ------------------------- | ------------ |
| getters, stream operations | performed                 | closed error |
| `read_input`               | performed                 | closed error |
| `seek`                     | performed; decoders reset | closed error |
| `close`                    | releases → Closed         | nothing      |
| collection                 | releases                  | nothing      |

Inside Open, each stream read as frames has a decoder that is unopened, then
opened, then draining once the input ended. A successful `seek` returns every
decoder to the opened state.

#### States of an output container

**Open** (header not written) → **Started** (header written) → **Closed**. An
open output can also become **Failed**.

The header is written by the first `write_packet`, `write_frame` or
`write_subtitle_frame`, or by `close` (above). A failed header write leaves
the output Open; the next write attempts it again. No operation writes the
header by itself: what the muxer fixes when it writes the header, the time
bases of the streams among them, is known after the first write.

| Operation                                                          | Open                           | Started                       | Failed                   | Closed       |
| ------------------------------------------------------------------ | ------------------------------ | ----------------------------- | ------------------------ | ------------ |
| `new_*_stream`, `new_stream_copy`, `new_uninitialized_stream_copy` | adds a stream                  | failure: header written       | failed error             | closed error |
| `initialize_stream_copy`                                           | initialises the stream         | failure: header written       | failed error             | closed error |
| `set_output_metadata`, `set_metadata`                              | replaces the metadata          | failure: header written       | failed error             | closed error |
| `set_time_base`, `set_avg_frame_rate`                              | writes the field               | failure: header written       | failed error             | closed error |
| `write_packet`, `write_frame`, `write_subtitle_frame`              | writes the header, then writes | writes                        | failed error             | closed error |
| `flush`                                                            | nothing                        | flushes the muxer and the I/O | failed error             | closed error |
| `output_started`                                                   | `false`                        | `true`                        | failed error             | closed error |
| getters                                                            | performed                      | performed                     | failed error             | closed error |
| `close`                                                            | per "Release"                  | per "Release"                 | releases, writes nothing | nothing      |
| collection                                                         | releases                       | releases                      | releases                 | nothing      |

**Failed.** FFmpeg has no call that removes a stream from a muxer. A stream
creation is therefore ordered so that everything that can fail for a reason
other than memory happens before the muxer accepts the stream (§4.4). When a
step after that point fails, the output is failed: the stream cannot be
completed and cannot be removed.

### 2.2 Stream — `('line, 'media, 'mode) stream`

A dependent handle: a container and a stream index. It has no native object
and no release of its own. All three type parameters are phantom.

- `'line` and `'media` are given by the operation that produces the stream
  and match the container and the stream's codec type.
- `'mode` (`` `Packet `` or `` `Frame ``) is fixed by the creation functions
  of an output. For the stream lists of §4.3 the caller chooses it.

Run-time checks (B3):

| Situation                                                       | Outcome |
| --------------------------------------------------------------- | ------- |
| a stream given to `read_input` or `seek` with another container | failure |
| `get_frame_size`, `write_frame` on a stream that has no encoder | failure |
| a stream index the container no longer has                      | failure |

Some demuxers add streams while packets are read. Every operation uses the
container's current stream count.

### 2.3 Formats — `(input, _) format`, `(output, _) format`

Borrowed handles on FFmpeg's static input and output formats. The media
parameter is phantom: the caller chooses it for the results of
`find_input_format` and `guess_output_format`. Nothing depends on it for
safety.

### 2.4 `uninitialized_stream_copy`

A dependent handle on a stream reserved in an output and not yet given its
parameters. `initialize_stream_copy` consumes it once; a second
initialisation of the same reservation raises a failure.

## 3. Enumerations and constants

**Result tags of `read_input`**, by the codec type of the packet's stream:

| Codec type                  | Packet tag             | Frame tag             |
| --------------------------- | ---------------------- | --------------------- |
| audio                       | `` `Audio_packet ``    | `` `Audio_frame ``    |
| video                       | `` `Video_packet ``    | `` `Video_frame ``    |
| subtitle                    | `` `Subtitle_packet `` | `` `Subtitle_frame `` |
| data                        | `` `Data_packet ``     | none                  |
| other (attachment, unknown) | the packet is dropped  | none                  |

**`seek_flag`** (OCaml to C, OR-ed together):

| Constructor          | C constant             |
| -------------------- | ---------------------- |
| `Seek_flag_backward` | `AVSEEK_FLAG_BACKWARD` |
| `Seek_flag_byte`     | `AVSEEK_FLAG_BYTE`     |
| `Seek_flag_any`      | `AVSEEK_FLAG_ANY`      |
| `Seek_flag_frame`    | `AVSEEK_FLAG_FRAME`    |

**Whence of the custom seek function** (C to OCaml):

| C `whence`                   | `Unix.seek_command`                |
| ---------------------------- | ---------------------------------- |
| `SEEK_SET`                   | `SEEK_SET`                         |
| `SEEK_CUR`                   | `SEEK_CUR`                         |
| `SEEK_END`                   | `SEEK_END`                         |
| anything else (a size query) | the closure is not called (§7.1.3) |

**Parameters and defaults:**

| Name                  | Recommended   | Meaning                                                                                                                  |
| --------------------- | ------------- | ------------------------------------------------------------------------------------------------------------------------ |
| `IO_BUFFER_SIZE`      | 32768 bytes   | Size of the buffer between FFmpeg and the custom I/O closures. It bounds the memory one container with custom I/O holds. |
| `SUBTITLE_PACKET_MAX` | 1 MiB         | Largest encoded subtitle. FFmpeg's subtitle encoder writes into a buffer the caller sizes; it bounds that allocation.    |
| default time format   | `` `Second `` | `get_input_duration`, `get_duration`                                                                                     |
| default `interleaved` | `true`        | every output open                                                                                                        |

## 4. Operations

"State check" means the state errors of the tables of §2.1.

### 4.1 Top level

```ocaml
val avformat_version : version
```

The libavformat loaded at run time, read once at module load.

```ocaml
val container_options : Options.t
```

The option class of containers (`avformat_get_class`), for listing container
options with `Avutil.Options`.

### 4.2 `Format`

```ocaml
val get_input_name : (input, _) format -> string
val get_input_long_name : (input, _) format -> string
val get_output_name : (output, _) format -> string
val get_output_long_name : (output, _) format -> string
```

The format's name and long name (A3). A name may be a comma-separated list of
aliases.

```ocaml
val find_input_format : string -> (input, 'a) format option
```

FFmpeg's input format of that name (`av_find_input_format`); `None` when
there is none.

```ocaml
val guess_output_format :
  ?short_name:string -> ?filename:string -> ?mime:string -> unit ->
  (output, 'a) format option
```

FFmpeg's guess from any of a short name, a file name and a MIME type
(`av_guess_format`). An omitted or empty argument does not take part. `None`
when FFmpeg guesses nothing.

```ocaml
val get_audio_codec_id : (output, audio) format -> Avcodec.Audio.id
val get_video_codec_id : (output, video) format -> Avcodec.Video.id
val get_subtitle_codec_id : (output, subtitle) format -> Avcodec.Subtitle.id
```

The format's default codec of that kind (E3); `` `None `` when it has none.

### 4.3 Input

```ocaml
type 'media stream_config = {
  codec : ('media, Avcodec.decode) Avcodec.codec option;
  opts : opts option;
}
```

```ocaml
val open_input :
  ?interrupt:(unit -> bool) ->
  ?format:(input, _) format ->
  ?opts:opts ->
  ?configure_audio_stream:(audio Avcodec.params -> audio stream_config) ->
  ?configure_video_stream:(video Avcodec.params -> video stream_config) ->
  ?configure_subtitle_stream:(subtitle Avcodec.params -> subtitle stream_config) ->
  string -> input container
```

Opens the URL for reading and probes its streams.

1. An empty URL with no `format` raises a failure.
2. `interrupt` is installed before anything can block (§7.1.4).
3. The demuxer is opened on the URL, with `format` forced when given, and
   consumes `opts` ([avutil.md](avutil.md) §9.1).
4. For each stream then present, in index order, the configure function of
   its codec type, when given, is called with a copy of the stream's codec
   parameters as they are before probing (§7.2). Its result configures the
   stream:
   - `codec = Some c`: `c` is the stream's **preferred decoder**. It is used
     to decode the stream in frame mode (§4.3, `read_input`), and for probing
     as described next.
   - `opts`: options for that stream's decoder. They are given to probing and
     to the decoder when it is opened for frame mode. The table is read when
     the function returns; it is not modified and no unused entry is
     reported.
5. Streams are probed (`avformat_find_stream_info`). FFmpeg's probing accepts
   one forced decoder per media type: the preferred decoder of the first
   stream of each type that has one. When a later stream of the same type
   prefers another decoder, a warning is written to FFmpeg's log and the
   first one is used for probing.

A stream that appears after step 4 has no configuration.

Any failure, including an exception raised by a configure function, releases
everything and removes the interrupt closure before it reaches the caller
(L2).

```ocaml
type read = bytes -> int -> int -> int
type write = bytes -> int -> int -> int
type seek = int -> Unix.seek_command -> int
```

The custom I/O closures (§7.1).

```ocaml
val open_input_stream :
  ?format:(input, _) format -> ?opts:opts -> ?seek:seek -> read ->
  input container
```

As `open_input`, reading through the closures instead of a URL. It takes no
interrupt function and no stream configuration.

```ocaml
val get_input_duration :
  ?format:Time_format.t -> input container -> Int64.t option
```

The duration of the input in units of `format`. `None` when FFmpeg does not
know it, and when it converts to 0. See "Time conversion" below.

```ocaml
val get_input_metadata : input container -> (string * string) list
```

The container's metadata, in the dictionary's order.

```ocaml
val get_input_format : input container -> (input, _) format option
```

The format the demuxer detected or was given.

```ocaml
val input_obj : input container -> Options.obj
```

The container as an option-bearing object ([avutil.md](avutil.md) §9.4).

```ocaml
type ('line, 'media, 'mode) stream
```

§2.2.

```ocaml
val get_audio_streams :
  'a container -> (int * ('a, audio, 'b) stream * audio Avcodec.params) list
val get_video_streams :
  'a container -> (int * ('a, video, 'b) stream * video Avcodec.params) list
val get_subtitle_streams :
  'a container ->
  (int * ('a, subtitle, 'b) stream * subtitle Avcodec.params) list
val get_data_streams :
  'a container ->
  (int * ('a, [ `Data ], 'b) stream * [ `Data ] Avcodec.params) list
```

The container's current streams of that codec type, in ascending index order,
each with its index, a stream value and a copy of its codec parameters. They
work on inputs and outputs.

```ocaml
val find_best_audio_stream :
  input container -> int * (input, audio, 'a) stream * audio Avcodec.params
val find_best_video_stream :
  input container -> int * (input, video, 'a) stream * video Avcodec.params
val find_best_subtitle_stream :
  input container ->
  int * (input, subtitle, 'a) stream * subtitle Avcodec.params
```

The stream FFmpeg considers the best of that type (`av_find_best_stream`,
with no preference). When there is none it raises
``Error `Stream_not_found``.

```ocaml
val get_input : (input, _, _) stream -> input container
val get_index : (_, _, _) stream -> int
val get_output : (output, _, _) stream -> output container
```

The stream's container and index. No state check: they read the stream value
only.

```ocaml
val get_codec_params : (_, 'media, _) stream -> 'media Avcodec.params
```

An independent copy of the stream's codec parameters.

```ocaml
val get_avg_frame_rate : (_, video, _) stream -> Avutil.rational option
val set_avg_frame_rate : (_, video, _) stream -> Avutil.rational option -> unit
```

The stream's average frame rate; `None` is "unset" (a zero numerator).

```ocaml
val get_time_base : (_, _, _) stream -> Avutil.rational
val set_time_base : (_, _, _) stream -> Avutil.rational -> unit
```

The stream's time base. Setting it does not touch the stream's encoder. A
muxer may replace the time base of a stream when it writes the header.

```ocaml
val get_frame_size : (output, audio, _) stream -> int
```

The frame size of the stream's encoder ([avcodec.md](avcodec.md) §4.3). A
stream with no encoder raises a failure.

```ocaml
val get_pixel_aspect : (_, video, _) stream -> Avutil.rational option
```

The stream's sample aspect ratio; `None` when unknown.

```ocaml
val get_duration :
  ?format:Time_format.t -> (input, _, _) stream -> Int64.t option
```

The stream's duration in units of `format`. `None` when FFmpeg does not know
it, and when it converts to 0.

```ocaml
val get_metadata : (input, _, _) stream -> (string * string) list
```

The stream's metadata, in the dictionary's order.

**Time conversion.** A duration or a position `v` expressed in a time base
`n/d` is converted to units of a time format with `F` units per second as
`v * F * n / d`, rounded to the nearest integer as FFmpeg's rescaling does
(`av_rescale_q`). The conversion MUST be exact whenever the result fits in 64
bits: the intermediate product does not overflow.

```ocaml
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
```

The integer is the stream index.

```ocaml
val read_input :
  ?on_unhandled_packet:(packet_result -> unit) ->
  ?audio_packet:(input, audio, [ `Packet ]) stream list ->
  ?audio_frame:(input, audio, [ `Frame ]) stream list ->
  ?video_packet:(input, video, [ `Packet ]) stream list ->
  ?video_frame:(input, video, [ `Frame ]) stream list ->
  ?subtitle_packet:(input, subtitle, [ `Packet ]) stream list ->
  ?subtitle_frame:(input, subtitle, [ `Frame ]) stream list ->
  ?data_packet:(input, [ `Data ], [ `Packet ]) stream list ->
  input container -> input_result
```

Returns the next packet or frame of the selected streams. The packet lists
form the **packet selection**, the frame lists the **frame selection**; all
default to empty. A stream in both is in the packet selection.

Each call returns one result or raises:

1. **Pending frames first.** When a decoder holds a frame ready from a packet
   read by an earlier call, that frame is returned, whatever the selections
   of this call.
2. **Read a packet** from the demuxer. A packet of a stream that is neither
   audio, video, subtitle nor data is dropped.
3. **Packet selection.** The call returns `` `X_packet (index, packet) ``
   with a fresh packet. The tag follows the stream's actual codec type (§3).
4. **Frame selection.** The packet is given to the stream's decoder, which is
   opened on first use (below). The first frame it produces is returned as
   `` `X_frame (index, frame) ``. Further frames from the same packet are
   returned by the following calls, per step 1. A packet that produces no
   frame is not an error: the call goes back to step 2.
5. **Neither.** When `on_unhandled_packet` is given, it is called with
   `` `X_packet (index, packet) `` and a fresh packet (§7.2). The call goes
   back to step 2.
6. **End of input.** The audio and video decoders of the streams of the frame
   selection are drained: each remaining frame is returned, one per call. When none remains
   the call raises ``Error `Eof``, and so does every later call until a
   `seek`.

Consequences:

- Every frame of a stream read in frame mode is returned before
  ``Error `Eof``, including frames a decoder holds because of reordering or
  threading when the demuxer runs out of packets.
- A packet that produces several frames delivers all of them.
- With empty selections every packet is unhandled: the call consumes the
  whole input and raises ``Error `Eof``.
- A returned packet or frame is independent of the container: later reads do
  not change it (A5).

**Opening a decoder**, the first time a packet of a stream is decoded:

- the decoder is the stream's preferred decoder (`open_input`), else FFmpeg's
  default decoder for the stream's codec; when there is none the call raises
  ``Error `Decoder_not_found``;
- it is opened as [avcodec.md](avcodec.md) §4.9 describes, with the stream's
  codec parameters, the stream's time base as its packet time base, and the
  stream's configured options;
- a failure raises, and leaves the container as if the stream had never been
  read in frame mode: a later call attempts the opening again.

**Subtitle frames.** A decoded subtitle with no timestamp takes its packet's,
converted to FFmpeg's internal time base. A subtitle with a zero end display
time and a packet with a positive duration takes that duration, in
milliseconds, as its end display time.

```ocaml
type seek_flag =
  | Seek_flag_backward | Seek_flag_byte | Seek_flag_any | Seek_flag_frame

val seek :
  ?flags:seek_flag list ->
  ?stream:(input, _, _) stream ->
  ?min_ts:Int64.t -> ?max_ts:Int64.t ->
  fmt:Time_format.t -> ts:Int64.t -> input container -> unit
```

Repositions the input (`avformat_seek_file`).

- `ts`, and `min_ts` and `max_ts` when given, are times in units of `fmt`.
  They are converted to the time base of `stream` when one is given, else to
  FFmpeg's internal time base (see "Time conversion").
- With `Seek_flag_byte` or `Seek_flag_frame` the three values are a byte
  offset or a frame number: they are passed as given and `fmt` is not used.
- An absent `min_ts` or `max_ts` is unbounded.
- `flags` defaults to none.
- On success every opened decoder of the container is reset and every frame
  decoded before the seek is discarded: the first frame returned afterwards
  belongs to the new position. The end-of-input condition is cleared.
- On failure the mapped error is raised and the decoders are unchanged.

### 4.4 Output

```ocaml
val open_output :
  ?interrupt:(unit -> bool) -> ?format:(output, _) format ->
  ?interleaved:bool -> ?opts:opts -> string -> output container
```

Opens the URL for writing.

- The muxer is `format` when given, else FFmpeg's guess from the URL.
- `interleaved` (default `true`) selects whether written packets are
  interleaved by the muxer (`av_interleaved_write_frame`) or written in call
  order (`av_write_frame`).
- `opts` is consumed, in this order, by the generic container options, the
  muxer's private options, and the I/O protocol
  ([avutil.md](avutil.md) §9.1). An entry meant for a later consumer is not
  reported unused because an earlier one did not know it.
- `interrupt` covers every blocking I/O of the container, including I/O the
  muxer opens by itself (§7.1.4).
- For a format that needs no file, no I/O is opened.
- Any failure releases everything and removes the interrupt closure (L2).

```ocaml
val open_output_format :
  ?interleaved:bool -> ?opts:opts -> (output, _) format -> output container
```

Opens an output of a format that needs no file, a device for one. A format
that needs a file raises a failure.

```ocaml
val open_output_stream :
  ?opts:opts -> ?interleaved:bool -> ?seek:seek -> write ->
  (output, _) format -> output container
```

Opens an output of the given format that writes through the closures. A
format that needs no file raises a failure.

```ocaml
val output_started : output container -> bool
```

Whether the header is written.

```ocaml
val set_output_metadata : output container -> (string * string) list -> unit
val set_metadata : (_, _, _) stream -> (string * string) list -> unit
```

Replace the whole metadata of the container, or of the stream, with the list.
Entries present before and absent from the list are gone; a key that appears
twice keeps its last value. On failure the metadata is unchanged.
`set_metadata` accepts input streams too.

**Adding a stream.** Every creation function below:

- performs the state check;
- on success adds exactly one stream, whose identifier is its index;
- on failure leaves the output with the streams it had, or failed (§2.1).

```ocaml
val new_stream_copy :
  params:'mode Avcodec.params -> output container ->
  (output, 'mode, [ `Packet ]) stream
type uninitialized_stream_copy
val new_uninitialized_stream_copy : output container -> uninitialized_stream_copy
val initialize_stream_copy :
  params:'mode Avcodec.params -> uninitialized_stream_copy ->
  (output, 'mode, [ `Packet ]) stream
```

A stream that receives packets encoded elsewhere.

- `new_uninitialized_stream_copy` reserves the stream, so that its index is
  known before its parameters are.
- `initialize_stream_copy` gives the reserved stream a copy of `params`. The
  codec tag is cleared so that the muxer chooses its own.
- `new_stream_copy` is the two in sequence.

Only the codec parameters are copied. The time base is the muxer's choice
unless `set_time_base` is called. **The average frame rate is not part of the
codec parameters**: a copied video stream reports none unless the caller sets
it with `set_avg_frame_rate`, typically from `get_avg_frame_rate` of the
source stream.

```ocaml
val new_audio_stream :
  ?opts:opts -> channel_layout:Channel_layout.t -> sample_rate:int ->
  sample_format:Avutil.Sample_format.t -> time_base:Avutil.rational ->
  codec:[ `Encoder ] Avcodec.Audio.t -> output container ->
  (output, audio, [ `Frame ]) stream
val new_video_stream :
  ?opts:opts -> ?frame_rate:Avutil.rational ->
  ?hardware_context:Avcodec.Video.hardware_context ->
  pixel_format:Avutil.Pixel_format.t -> width:int -> height:int ->
  time_base:Avutil.rational -> codec:[ `Encoder ] Avcodec.Video.t ->
  output container -> (output, video, [ `Frame ]) stream
val new_subtitle_stream :
  ?opts:opts -> ?header:string -> time_base:Avutil.rational ->
  codec:[ `Encoder ] Avcodec.Subtitle.t -> output container ->
  (output, subtitle, [ `Frame ]) stream
```

A stream that receives frames and encodes them. In this order:

1. An encoder is created for `codec` with the typed arguments, as
   [avcodec.md](avcodec.md) §4.9 describes for audio and video, and with
   `opts`. When the output format wants codec headers out of band, the encoder
   is asked for global headers.
2. The muxer accepts a new stream.
3. The stream takes the encoder's time base and the encoder's codec
   parameters.

A failure in step 1 leaves the output unchanged. A failure in step 3 leaves it
failed.

- `new_video_stream`: `frame_rate`, when given, is also the stream's average
  frame rate; otherwise that stays unset.
- `new_subtitle_stream`: `header` is the subtitle header given to the encoder
  of a text subtitle codec; when omitted it is
  `Avutil.Subtitle.header_ass_default ()`. An empty header is none, and the
  encoder of a bitmap subtitle codec gets none.

```ocaml
val new_data_stream :
  time_base:Avutil.rational -> codec:Avcodec.Unknown.id -> output container ->
  (output, [ `Data ], [ `Packet ]) stream
```

A data stream of the given codec identifier and time base, with no encoder.

```ocaml
val codec_attr : _ stream -> string option
```

The codec string of the stream for an HLS playlist (RFC 6381 style), from its
codec parameters. `None` for every case not listed.

| Codec  | Result                                                                                                                                                                                     |
| ------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| H.264  | `avc1.` followed by the three bytes after the SPS NAL header, as two lowercase hex digits each, when the extradata starts with an Annex-B SPS and is long enough to hold them; else `None` |
| HEVC   | below                                                                                                                                                                                      |
| AAC    | `mp4a.40.` followed by the profile plus one, when the profile is known; else `None`                                                                                                        |
| MP2    | `mp4a.40.33`                                                                                                                                                                               |
| MP3    | `mp4a.40.34`                                                                                                                                                                               |
| AC-3   | `ac-3`                                                                                                                                                                                     |
| E-AC-3 | `ec-3`                                                                                                                                                                                     |
| FLAC   | `fLaC`                                                                                                                                                                                     |

HEVC: the profile and level are those of the codec parameters, replaced by the
ones read from the SPS when the extradata holds an Annex-B SPS. When the codec
tag is `hvc1` and both are known, the result is
`hvc1.<profile>.4.L<level>.B01`. Otherwise it is the codec tag as a
four-character code, and `None` when the tag is unset.

No read goes past the end of the extradata.

```ocaml
val bitrate : _ stream -> int option
```

The stream's bit rate when its codec parameters give one; else the maximum
bit rate of the stream's coded-picture-buffer properties when it has them and
it is not 0; else `None`.

```ocaml
val write_packet :
  (output, 'media, [ `Packet ]) stream -> Avutil.rational ->
  'media Avcodec.Packet.t -> unit
```

Writes a packet to the stream. The rational is the time base of the packet's
timestamps and duration; they are converted to the stream's time base. The
header is written first when needed. The caller's packet is unchanged (A4).

```ocaml
val write_frame :
  ?on_keyframe:(unit -> unit) -> (output, 'media, [ `Frame ]) stream ->
  'media frame -> unit
```

Encodes a frame with the stream's encoder and writes every packet that
becomes available.

- The header is written first when needed.
- Packet timestamps are converted from the encoder's time base to the
  stream's.
- `on_keyframe` is called before each key packet is given to the muxer
  (§7.2). The muxer position at that moment is the start of that key packet.
- With a hardware frame context the frame is uploaded first, properties
  included ([avcodec.md](avcodec.md) §4.10).
- A frame that produces no packet is not an error.
- The caller's frame is unchanged (A4).

```ocaml
val write_subtitle_frame :
  (output, subtitle, [ `Frame ]) stream -> Avutil.Subtitle.frame -> unit
```

Encodes a subtitle and writes it as one packet.

- The header is written first when needed.
- A subtitle that encodes to nothing writes no packet.
- The packet's timestamp is the subtitle's timestamp plus its start display
  time; its duration is the end display time minus the start display time.
- A subtitle that encodes to more than `SUBTITLE_PACKET_MAX` bytes raises the
  error the encoder reports.

```ocaml
val flush : output container -> unit
```

On a started output, makes the muxer write out what it buffers and flushes
the I/O. Encoders are not flushed. On an output whose header is not written
it does nothing.

```ocaml
val tell : _ container -> int option
```

The byte position of the container's I/O; `None` for a container with no I/O
of its own, or when FFmpeg reports no position. Positions beyond 32 bits are returned exactly.

```ocaml
val close : _ container -> unit
```

§2.1, "Release".

## 5. Errors

| Raised                                    | By                                                                                                                                                       |
| ----------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Error e`, `e` mapped from an FFmpeg code | every operation that calls a fallible FFmpeg function                                                                                                    |
| ``Error `Eof``                            | `read_input` at the end of the input                                                                                                                     |
| ``Error `Exit``                           | a blocking operation the interrupt function aborted                                                                                                      |
| ``Error `Stream_not_found``               | `find_best_*_stream`                                                                                                                                     |
| ``Error `Decoder_not_found``              | `read_input`, for a frame-mode stream with no decoder                                                                                                    |
| the state errors of the contract's §5.3   | every operation, per §2.1                                                                                                                                |
| ``Error (`Failure msg)``                  | the checks of §2.2 and §2.4; "header written"; an open with neither URL nor format; a format that does or does not need a file; a stream with no encoder |
| `Out_of_memory`                           | a failed allocation (F3)                                                                                                                                 |
| any exception                             | raised by a function of §7.2; propagates unchanged                                                                                                       |

No operation of this library raises `Not_found`.

A failing custom I/O closure is reported to FFmpeg as `AVERROR_EXTERNAL`
(§7.1). The operation in progress then raises whatever code FFmpeg
propagates: `` `Other `` of that code when FFmpeg passes it through unchanged.

## 6. Blocking and concurrency

### 6.1 The runtime lock

The runtime lock is released (contract M1, M4) around every open, probing,
`find_best_*_stream`, `read_input`, `seek`, the creation of an encoding
stream, the three writes, `flush`, `tell`, `close`, and release by
collection. `Format.find_input_format` and
`Format.guess_output_format` MAY release it.

### 6.2 Guards

A container has one guard, shared by its streams (M9).

| Exclusive                                                                                                                                      | Shared                                       |
| ---------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------- |
| `find_best_*_stream`, `read_input`, `seek`, every stream creation and initialisation, every setter, the three writes, `flush`, `tell`, `close` | every getter, the stream lists, option reads |

The container is not in use while a function of §7.2 runs: `on_keyframe` may
call `flush` or `tell` on its own container.

### 6.3 Global state

FFmpeg's network layer, initialised at module load. No other.

## 7. Callbacks

### 7.1 Functions FFmpeg calls

They follow the contract's §7.1. FFmpeg normally runs them on the thread that
is inside the container operation, with the lock released; it may run them on
another thread.

The `bytes` value given to a read or write closure belongs to the binding. Its
content is meaningful only during the call, and the closure MUST NOT keep it.

#### 7.1.1 Custom read — `read buffer offset length`

- The closure fills at most `length` bytes of `buffer` from `offset` and
  returns the number of bytes it stored. `length` is at least 1.
- 0 means end of input.
- A negative result is given to FFmpeg as the error code of the read.
- A result greater than `length`, and an exception, are failures of the
  closure: FFmpeg gets `AVERROR_EXTERNAL` (C1, C2).

#### 7.1.2 Custom write — `write buffer offset length`

- The closure consumes at most `length` bytes of `buffer` from `offset` and
  returns the number it consumed.
- When it consumed fewer than `length`, it is called again with the
  remainder, until everything FFmpeg handed over is written. FFmpeg itself
  treats a write as all or nothing; the binding makes a short count safe.
- A negative result is given to FFmpeg as the error code of the write.
- A result of 0, a result greater than `length`, and an exception, are
  failures of the closure: FFmpeg gets `AVERROR_EXTERNAL`.

The bytes the closures receive, concatenated, are exactly the bytes the same
muxing would write to a file, whatever the bit rate and whatever size FFmpeg
hands over at a time.

#### 7.1.3 Custom seek — `seek offset whence`

- Called for `SEEK_SET`, `SEEK_CUR` and `SEEK_END` (§3); it returns the new
  position.
- A size query is answered as "not supported" without calling the closure.
- A negative result is given to FFmpeg as an error code. An exception gives
  `AVERROR_EXTERNAL`.

#### 7.1.4 Interrupt — `interrupt ()`

- FFmpeg polls it during any blocking I/O of the container, for the whole
  life of the container, release included.
- `true` aborts the blocking operation, which then raises ``Error `Exit``.
- An exception aborts it too.
- It is never called after the container's release completed (L8).

### 7.2 Functions the binding calls

They follow the contract's §7.2.

| Function              | Called from   | State when called, and after it raises                                                                                                                                                              |
| --------------------- | ------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `configure_*_stream`  | `open_input`  | The input is opened and not yet probed; no container value exists. An exception releases everything.                                                                                                |
| `on_unhandled_packet` | `read_input`  | The packet is the function's own. An exception ends the read; the container stays usable and the next read continues with the following packet.                                                     |
| `on_keyframe`         | `write_frame` | A key packet is about to be written. An exception does not lose it: the packet is written, then the exception propagates. Packets still in the encoder are written by the next write or by `close`. |

## 8. Data transfer

| Path                                                                            | Copy or share                                                                            |
| ------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------- |
| URL, format names, MIME type, option keys and values, metadata, subtitle header | copied                                                                                   |
| codec parameters, in every direction                                            | deep copy                                                                                |
| packet returned by `read_input` or given to `on_unhandled_packet`               | a new packet referencing the demuxer's data buffer, or a copy of it; owned by the value  |
| frame returned by `read_input`                                                  | a new frame referencing the decoder's buffers; owned by the value                        |
| subtitle returned by `read_input`                                               | owned by the value                                                                       |
| packet given to `write_packet`                                                  | FFmpeg gets a reference of its own; the caller's packet is unchanged                     |
| frame given to `write_frame`                                                    | the encoder takes its own references; with a hardware frame context the data is uploaded |
| subtitle given to `write_subtitle_frame`                                        | read only                                                                                |
| custom read and write                                                           | bytes are copied between FFmpeg's buffer and the `bytes` value given to the closure      |

No bigarray, plane or stride handling in this library.

## 9. Options

| Operation                                                     | Consumers of the entries, in order                                 |
| ------------------------------------------------------------- | ------------------------------------------------------------------ |
| `open_input`, `open_input_stream`                             | the demuxer open                                                   |
| `stream_config.opts`                                          | probing, and the stream's decoder; nothing is reported             |
| `open_output`                                                 | generic container options, muxer private options, the I/O protocol |
| `open_output_format`, `open_output_stream`                    | generic container options, muxer private options                   |
| `new_audio_stream`, `new_video_stream`, `new_subtitle_stream` | the encoder open                                                   |

All but the second follow [avutil.md](avutil.md) §9.1. The header write takes
no option.

Option introspection: `container_options` lists container options and
`input_obj` reads them on an input. There is no such object for outputs or
streams.

## 10. Version-dependent behaviour

None.

## 11. Composite operations

- `new_stream_copy` is `new_uninitialized_stream_copy` then
  `initialize_stream_copy`.
- `write_frame` on a stream with an encoder follows the send and receive
  algorithm of [avcodec.md](avcodec.md) §11, with each received packet written
  to the muxer.
- The frame mode of `read_input` follows the decode algorithm of
  [avcodec.md](avcodec.md) §11, returning one frame per call.
