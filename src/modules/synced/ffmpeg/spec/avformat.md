# `av` — libavformat binding (as built)

The OCaml library is named `av` (public name `ffmpeg-av`). It demuxes and
decodes on the input side, encodes and muxes on the output side.
Mechanics of the C stubs are in
[language-notes/avformat.md](language-notes/avformat.md).

## 1. Scope

- Binds **libavformat** (containers, demuxing, muxing, custom I/O). It also
  calls **libavcodec** directly (decoder and encoder contexts owned by the
  container, packets, subtitles, codec parameters) and **libavutil**
  (dictionaries, options, rescaling, hardware frames, logging).
- Built when detection finds libavutil and libavformat at their minimum
  versions ([build.md](build.md) §2.3).
- Depends on the sibling binding libraries `avutil` and `avcodec`, and on the
  OCaml `unix` library (for `Unix.seek_command`).
- From `avutil` it uses: the `'a container`, `('line, 'media) format`,
  `input`, `output`, `audio`, `video`, `subtitle` phantom types, `opts`,
  `rational`, `Time_format.t`, `'media frame`, `Subtitle.frame`,
  `Channel_layout.t`, `Sample_format.t`, `Pixel_format.t`, `HwContext`,
  `Options`; the error-raising helper, the failure helper, the option
  dictionary helpers, the thread-registration helper, the rational, frame and
  subtitle wrappers.
- From `avcodec` it uses: `Avcodec.params`, `Avcodec.Packet.t`,
  `Avcodec.codec`, the codec-id conversions, the codec-opening helper, the
  codec-parameters and packet wrappers.
- Module initialisation: at load time the library calls
  `avformat_network_init()` once (and `av_register_all()` on libraries older
  than the supported range, §10).
- The installed C header exports to sibling stubs (used by `avdevice`):
  - a function returning the `AVFormatContext *` of a container value (it
    applies the closed check of §2.1);
  - accessors and constructors for input-format and output-format values
    (one-word abstract blocks holding the `AVInputFormat *` /
    `AVOutputFormat *`); the constructors raise `Error (`Failure "Empty input
    format")`/`"Empty output format"` on a null pointer;
  - a const-qualifier shim for those pointer types (§10);
  - the control-message callback pair (§7.5).

## 2. Objects

### 2.1 Container (`input container`, `output container`)

**C object.** One heap record per container, holding:

| Field                    | Meaning                                                                                                       |
| ------------------------ | ------------------------------------------------------------------------------------------------------------- |
| format context           | the `AVFormatContext *`; null after release                                                                   |
| stream table             | array of per-stream records (index, `AVCodecContext *` or null) and its allocated length                      |
| preferred decoders       | input only: array of `const AVCodec *`, one slot per stream present when the input was opened, and its length |
| control-message callback | OCaml closure, set by `avdevice` (§7.5)                                                                       |
| interrupt callback       | OCaml closure or unset (§7.4)                                                                                 |
| is-input flag            | 1 for inputs, 0 for outputs                                                                                   |
| closed flag              | 0 until released                                                                                              |
| pending stream index     | input only: index of the stream whose decoder may still hold frames, or -1                                    |
| work packet, work frame  | input only: one `AVPacket` and one `AVFrame` reused by every read                                             |
| work subtitle            | input only: one embedded `AVSubtitle` used as decode target                                                   |
| header-written flag      | output only                                                                                                   |
| write function           | output only: `av_interleaved_write_frame` or `av_write_frame`, chosen at open                                 |
| custom-I/O flag          | output only: 1 when the I/O context came from OCaml callbacks                                                 |
| I/O object               | the custom I/O value (§2.3), kept alive while the container is open                                           |

Three "best stream" slots exist in the record and are never written with a
stream; they are cleared on release.

**Creation.** `open_input`, `open_input_stream`, `open_output`,
`open_output_format`, `open_output_stream` (§4).

**Ownership.** The container owns its format context, every per-stream codec
context, the work packet/frame, and — for outputs opened on a URL with a
format that uses a file — the `AVIOContext` opened by `avio_open2`. It does
not own a custom `AVIOContext`; that belongs to the I/O object.

**What it keeps alive.** While not closed, the container holds strong
references (GC roots) to: the interrupt closure, the control-message closure,
the I/O object (and through it the read/write/seek closures and the transfer
buffer).

**What keeps it alive.** Every stream value holds its container. The value
returned by `input_obj` holds its container.

**Release.** Two paths lead to the same release routine:

1. `Av.close` (explicit). For outputs it first flushes encoders and writes
   the trailer (§4, `close`).
2. A `Gc.finalise` function attached by every opening function. It runs the
   release routine only; it does not flush encoders and does not write a
   trailer.

The release routine is idempotent. With the runtime lock released it:

1. frees the work packet and work frame;
2. if the format context is non-null: frees every per-stream record and its
   codec context (`avcodec_free_context`), frees the stream table, frees the
   preferred-decoder array;
3. if the context has an input format: `avformat_close_input`;
4. otherwise if it has an output format: `avio_closep(&pb)` unless the
   custom-I/O flag is set or the format has `AVFMT_NOFILE`; then
   `avformat_free_context`; the pointer is set to null.

Then, with the lock held, it drops the three GC roots (control-message
closure, interrupt closure, I/O object) and sets the closed flag.

The memory of the record itself is freed when the OCaml value is collected
(custom-block finaliser), after the `Gc.finalise` function has run.

**Use after close.** Every operation that takes a container or a stream
first checks the closed flag and raises
``Avutil.Error (`Failure "Container closed!")``. This includes a second call to
`close`. Two entry points skip the check: the GC cleanup (a no-op when
closed) and `get_container_stream_time_base`, which reads the format context
directly (null after release).

#### State machine — input container

States: **Open**, **Closed**. Inside Open the container tracks:

- per stream: decoder _unopened_ → _opened_ (first time a packet of that
  stream is read while the stream is selected for frames) → _draining_ (after
  end of file, once the drain step has sent the null packet). A successful
  seek flushes every opened decoder, which returns draining decoders to
  opened.
- the pending stream index: set to a stream when a packet was accepted by its
  audio/video decoder, reset to -1 when that decoder reports any error
  (including `EAGAIN`), after a seek, and initially.

| Operation                                             | Open                                              | Closed                        |
| ----------------------------------------------------- | ------------------------------------------------- | ----------------------------- |
| getters, `set_input_metadata`, stream getters/setters | performed                                         | `Failure "Container closed!"` |
| `read_input`                                          | performed                                         | same failure                  |
| `seek`                                                | performed; flushes decoders, clears pending index | same failure                  |
| `close`                                               | release → Closed                                  | same failure                  |
| GC cleanup                                            | release → Closed                                  | nothing                       |

#### State machine — output container

States: **Open, header not written** → **Header written** → **Closed**.

The header is written (`avformat_write_header(ctx, NULL)`, no options) by the
first of: `write_packet`, `write_frame` on any stream, `write_subtitle_frame`,
or the encoder flush performed by `close` when at least one stream has an
audio or video encoder. A failed header write leaves the flag clear.

| Operation                                                          | Header not written                                                                                                                 | Header written                                                   | Closed                        |
| ------------------------------------------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------- | ----------------------------- |
| `new_*_stream`, `new_uninitialized_stream_copy`, `new_stream_copy` | adds a stream                                                                                                                      | `Failure "Failed to create new stream : header already written"` | `Failure "Container closed!"` |
| `initialize_stream_copy`                                           | copies parameters                                                                                                                  | copies parameters (no check)                                     | same failure                  |
| `set_output_metadata`, `set_metadata`                              | replaces the dictionary                                                                                                            | `Failure "Failed to set metadata : header already written"`      | same failure                  |
| `set_time_base`, `set_avg_frame_rate`                              | writes the field                                                                                                                   | writes the field (no check)                                      | same failure                  |
| `write_packet`, `write_frame`, `write_subtitle_frame`              | writes header, then writes                                                                                                         | writes                                                           | same failure                  |
| `flush`                                                            | returns without doing anything                                                                                                     | flushes muxer and I/O                                            | same failure                  |
| `output_started`                                                   | `false`                                                                                                                            | `true`                                                           | same failure                  |
| `reopen_output_stream`                                             | replaces the I/O context                                                                                                           | replaces the I/O context                                         | same failure                  |
| `close`                                                            | flushes encoders (which writes the header if an audio/video encoder exists), writes the trailer if the header is written, releases | flushes encoders, writes trailer, releases                       | same failure                  |
| GC cleanup                                                         | releases, no trailer                                                                                                               | releases, no trailer                                             | nothing                       |

There is no separate "trailer written" state: the trailer is written only
inside `close`, immediately before release.

### 2.2 Stream (`('line, 'media, 'mode) stream`)

Not a C object. A stream is an OCaml record of the container and the stream
index. All three type parameters are phantom. It has no release of its own
and stays valid (as a value) after the container is closed; every use then
fails with the closed-container failure.

The stream index is not validated against the container's stream count by
the stream operations; indexes come from the binding itself.

Per-stream C state lives in the container's stream table:

| Stream kind                                     | Table entry             | Codec context   |
| ----------------------------------------------- | ----------------------- | --------------- |
| input stream never read as frames               | none                    | none            |
| input stream read as frames                     | created at first packet | decoder, opened |
| output encoding stream (audio, video, subtitle) | created with the stream | encoder, opened |
| output copy stream, data stream                 | created with the stream | none            |

For inputs the table is grown to the format context's current stream count
each time it is consulted during a read, because some demuxers add streams
while packets are read.

### 2.3 Custom I/O object (internal, not in the `.mli`)

Created by `open_input_stream` and `open_output_stream` before the container.

**C object.** A record holding one `AVIOContext *`, one OCaml `bytes` transfer
buffer of 32768 bytes, and the read, write and seek closures (each optional).

**Creation.**

1. Allocate the record. Allocate the 32768-byte OCaml transfer buffer and
   root it.
2. Allocate a 32768-byte C buffer with `av_malloc`.
3. Root each closure that was supplied.
4. `avio_alloc_context(buffer, 32768, write_flag, record, read_fn, write_fn,
seek_fn)`; `write_flag` is 1 iff a write closure was supplied; each
   function pointer is null when its closure is absent.
5. Any allocation failure raises `Out_of_memory` after undoing the roots.

**Ownership and lifetime.** The I/O object owns the `AVIOContext` and its
current C buffer. The container that uses it holds it alive until the
container is released. Its release has two stages, both driven by garbage
collection, never explicit:

1. a `Gc.finalise` function drops the roots of the transfer buffer and the
   three closures;
2. the custom-block finaliser frees `avio_context->buffer` (the buffer
   currently installed, which FFmpeg may have replaced), then
   `avio_context_free`, then the record.

Releasing a container never frees or closes a custom `AVIOContext`.

### 2.4 Formats (`(input, _) format`, `(output, _) format`)

A one-word block holding a pointer to a static `AVInputFormat` or
`AVOutputFormat`. Never released; the pointed object belongs to libavformat.
The media type parameter is phantom and unchecked.

### 2.5 `uninitialized_stream_copy`

An OCaml pair of the output container and the reserved stream index. No C
object.

## 3. Enumerations and constants

Generated tables consumed:

- polymorphic-variant hash constants for `` `Audio_packet``,
  `` `Video_packet``, `` `Subtitle_packet``, `` `Data_packet``,
  `` `Audio_frame``, `` `Video_frame``, `` `Subtitle_frame`` (result tags of
  `read_input`), C → OCaml only;
- through `avutil`: the time-format conversion (`` `Second`` 1,
  `` `Millisecond`` 1000, `` `Microsecond`` 1000000, `` `Nanosecond``
  1000000000; any other value 1) and the error mapping (§5);
- through `avcodec`: codec-id conversions (audio, video, subtitle C → OCaml in
  `Format.get_*_codec_id`; unknown-kind OCaml → C in `new_data_stream`).
  Their behaviour on a value with no mapping is specified by `avcodec`.

Hand-written tables:

Internal media-type selector (OCaml constant constructor ordinal → C), used by
the stream listing and best-stream stubs:

| Ordinal | OCaml (internal) | C                       |
| ------- | ---------------- | ----------------------- |
| 0       | `MT_audio`       | `AVMEDIA_TYPE_AUDIO`    |
| 1       | `MT_video`       | `AVMEDIA_TYPE_VIDEO`    |
| 2       | `MT_data`        | `AVMEDIA_TYPE_DATA`     |
| 3       | `MT_subtitle`    | `AVMEDIA_TYPE_SUBTITLE` |

`seek_flag` (OCaml → C, OR-ed together):

| Ordinal | OCaml                | C                      |
| ------- | -------------------- | ---------------------- |
| 0       | `Seek_flag_backward` | `AVSEEK_FLAG_BACKWARD` |
| 1       | `Seek_flag_byte`     | `AVSEEK_FLAG_BYTE`     |
| 2       | `Seek_flag_any`      | `AVSEEK_FLAG_ANY`      |
| 3       | `Seek_flag_frame`    | `AVSEEK_FLAG_FRAME`    |

Seek whence of the custom I/O seek callback (C → integer → OCaml):

| C `whence`                                                    | Integer passed | `Unix.seek_command`                            |
| ------------------------------------------------------------- | -------------- | ---------------------------------------------- |
| `SEEK_SET`                                                    | 0              | `SEEK_SET`                                     |
| `SEEK_CUR`                                                    | 1              | `SEEK_CUR`                                     |
| `SEEK_END`                                                    | 2              | `SEEK_END`                                     |
| anything else (`AVSEEK_SIZE`, values carrying `AVSEEK_FORCE`) | —              | callback not called; the C function returns -1 |

Media type of a demuxed packet → result tag:

| `codecpar->codec_type`      | Packet tag            | Frame tag            |
| --------------------------- | --------------------- | -------------------- |
| `AVMEDIA_TYPE_AUDIO`        | `` `Audio_packet``    | `` `Audio_frame``    |
| `AVMEDIA_TYPE_VIDEO`        | `` `Video_packet``    | `` `Video_frame``    |
| `AVMEDIA_TYPE_SUBTITLE`     | `` `Subtitle_packet`` | `` `Subtitle_frame`` |
| `AVMEDIA_TYPE_DATA`         | `` `Data_packet``     | —                    |
| other (attachment, unknown) | packet discarded      | —                    |

The frame tag of a non-subtitle decoder is `` `Audio_frame`` when the codec
context type is audio and `` `Video_frame`` otherwise.

Constants:

| Constant               | Value                                             | Use                                                                                                                                  |
| ---------------------- | ------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------ |
| custom I/O buffer size | 32768 bytes                                       | both the `AVIOContext` buffer and the OCaml transfer buffer; also the maximum length passed to a read or write closure per call      |
| subtitle encode buffer | 4096 bytes                                        | packet allocated for `avcodec_encode_subtitle`; the code comment says it should suffice for most text subtitles including styled ASS |
| codec attribute buffer | 32 bytes                                          | `codec_attr` formatting                                                                                                              |
| default time format    | `` `Second``                                      | `get_input_duration`, `get_duration`                                                                                                 |
| default `interleaved`  | `true`                                            | all output opens                                                                                                                     |
| best-stream arguments  | wanted -1, related -1, no decoder return, flags 0 | `av_find_best_stream`                                                                                                                |
| seek window defaults   | `INT64_MIN`, `INT64_MAX`                          | `min_ts`, `max_ts`                                                                                                                   |

## 4. Operations

Option handling common to all `?opts` arguments is in §9. "Closed check"
means the failure of §2.1.

### 4.1 Top level

```ocaml
val avformat_version : version
```

`avformat_version()` read once at load, split as major = `v lsr 16`,
minor = `(v lsr 8) land 0xff`, micro = `v land 0xff`.

```ocaml
val container_options : Options.t
```

The `AVClass` returned by `avformat_get_class()`, wrapped once at load, for
listing container options with `Avutil.Options`.

### 4.2 `Format`

```ocaml
val get_input_name : (input, _) format -> string
val get_input_long_name : (input, _) format -> string
```

Copy of `name` / `long_name`; the empty string when the C field is null.

```ocaml
val find_input_format : string -> (input, 'a) format option
```

Copies the name to a C string, calls `av_find_input_format` with the runtime
lock released. `None` when it returns null.

```ocaml
val get_output_name : (output, _) format -> string
val get_output_long_name : (output, _) format -> string
```

Same as the input variants.

```ocaml
val guess_output_format :
  ?short_name:string -> ?filename:string -> ?mime:string -> unit ->
  (output, 'a) format option
```

Each argument defaults to `""`. An empty string is passed to FFmpeg as a null
pointer, a non-empty one as a C copy. Calls `av_guess_format(short_name,
filename, mime)` with the lock released. `None` when it returns null.

```ocaml
val get_audio_codec_id : (output, audio) format -> Avcodec.Audio.id
val get_video_codec_id : (output, video) format -> Avcodec.Video.id
val get_subtitle_codec_id : (output, subtitle) format -> Avcodec.Subtitle.id
```

Convert the format's `audio_codec` / `video_codec` / `subtitle_codec` field
with the matching `avcodec` conversion. The format's media type parameter is
not checked against anything.

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

1. Build the option dictionary from `opts` (§9).
2. If the URL is non-empty, copy it to a C string. If the URL is empty and no
   format is given: free the dictionary and raise
   ``Error (`Failure "At least one format or url must be provided!")``.
3. Allocate the container record, `avformat_alloc_context()`, the work packet
   (`av_packet_alloc`) and the work frame (`av_frame_alloc`). Pending stream
   index is -1.
4. If `interrupt` is given: root the closure and set the format context's
   `interrupt_callback` to the binding's interrupt function with the
   container record as opaque (§7.4).
5. `avformat_open_input(&ctx, url_or_NULL, format_or_NULL, &dict)` with the
   lock released. On failure (and on any allocation failure in step 3): free
   the URL copy and the dictionary, unroot the interrupt closure, free the
   work packet and frame, call `avformat_close_input`, raise the mapped
   error (allocation failures raise ``Error (`Other AVERROR(ENOMEM))``).
6. Free the URL copy.
7. Allocate the preferred-decoder array and an array of per-stream option
   dictionaries, both sized to the stream count at this point.
8. For each stream, in index order, pick the callback for its codec type
   (audio, video, subtitle; no callback for other types). When one is given:
   call it with a **copy** of the stream's codec parameters as they are
   before stream probing. From the result: when `codec` is `Some c`, record
   `c` as the preferred decoder for that stream index; fill the stream's
   option dictionary from `opts` (an absent `opts` gives an empty one).
9. Per media type, the **first** codec returned by a callback becomes the
   format context's `audio_codec` / `video_codec` / `subtitle_codec` for the
   probing step. When a later stream of the same type returned a different
   codec, a warning is logged on the format context (`AV_LOG_WARNING`,
   "Multiple audio streams request different preferred decoders; using NAME
   for stream probing.", same for video and subtitle) and the first one is
   still used.
10. `avformat_find_stream_info(ctx, per_stream_dicts)` with the lock
    released.
11. Reset the three forced codec fields to null. Free every per-stream
    dictionary (their unused entries are not reported).
12. On probing failure: run the release routine, free the record, raise the
    mapped error.
13. Collect the unused keys of the container dictionary, wrap the container,
    return. The OCaml side attaches the GC cleanup and filters `opts` (§9).

The configure callbacks run on the calling thread with the runtime lock held,
between steps 5 and 10. An exception raised by one propagates out of
`open_input`.

Streams that appear after step 7 have no preferred decoder.

```ocaml
type read = bytes -> int -> int -> int
type write = bytes -> int -> int -> int
type seek = int -> Unix.seek_command -> int
```

Signatures of the custom I/O closures (§7.1–7.3).

```ocaml
val open_input_stream :
  ?format:(input, _) format -> ?opts:opts -> ?seek:seek -> read ->
  input container
```

1. Create an I/O object (§2.3) with the read closure, no write closure and
   the optional seek closure (wrapped so it receives a `Unix.seek_command`).
2. Build the option dictionary. `avformat_alloc_context()`; set its `pb` to
   the I/O object's `AVIOContext`.
3. Same as `open_input` steps 3 and 5 with a null URL, no interrupt.
4. `avformat_find_stream_info(ctx, NULL)` with the lock released; on failure
   run the release routine, free the record, raise.
5. Store and root the I/O object in the container. Report unused options.

There is no interrupt callback, no per-stream configuration and no preferred
decoder array for this kind of input.

```ocaml
val get_input_duration :
  ?format:Time_format.t -> input container -> Int64.t option
```

Reads the format context `duration` (in `AV_TIME_BASE` units). `None` when it
is `AV_NOPTS_VALUE`. Otherwise computes, in 64-bit integer arithmetic,
`(duration * F * 1) / AV_TIME_BASE` where `F` is the number of `format` units
per second. A result of exactly 0 is turned into `None`. Default format
`` `Second``.

```ocaml
val get_input_metadata : input container -> (string * string) list
```

Iterates the format context metadata dictionary with
`av_dict_get(dict, "", prev, AV_DICT_IGNORE_SUFFIX)` and returns copies of
each key and value, in dictionary iteration order.

```ocaml
val get_input_format : input container -> (input, _) format option
```

`Some` of the context's `iformat`, `None` when it is null.

```ocaml
val set_input_metadata : input container -> (string * string) list -> unit
```

Same routine as `set_output_metadata` (§4.4) on the container dictionary. The
header-written flag of an input is always clear, so it always proceeds.

```ocaml
val input_obj : input container -> Options.obj
```

Returns an option object designating the `AVFormatContext`, paired with the
container so that the container stays alive as long as the object does.

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

Scan the format context's streams and keep those whose
`codecpar->codec_type` equals the requested type. The result is in ascending
index order. Each element carries the index, a stream value and a fresh copy
of the codec parameters (as `get_codec_params`). The `'mode` parameter is
free: the caller chooses packet or frame mode by annotation. Works on input
and output containers.

```ocaml
val find_best_audio_stream :
  input container -> int * (input, audio, 'a) stream * audio Avcodec.params
val find_best_video_stream :
  input container -> int * (input, video, 'a) stream * video Avcodec.params
val find_best_subtitle_stream :
  input container ->
  int * (input, subtitle, 'a) stream * subtitle Avcodec.params
```

`av_find_best_stream(ctx, type, -1, -1, NULL, 0)` with the lock released. Any
negative result raises ``Error `Stream_not_found``, whatever the code was.
Otherwise returns the index, a stream value and a copy of its parameters.

```ocaml
val get_input : (input, _, _) stream -> input container
val get_index : (_, _, _) stream -> int
```

Record field reads. No C call, no closed check.

```ocaml
val get_codec_params : (_, 'media, _) stream -> 'media Avcodec.params
```

Closed check, then a deep copy (`avcodec_parameters_copy` into a freshly
allocated `AVCodecParameters`) of the stream's `codecpar`, owned by the
returned value.

```ocaml
val get_avg_frame_rate : (_, video, _) stream -> Avutil.rational option
val set_avg_frame_rate : (_, video, _) stream -> Avutil.rational option -> unit
```

Getter: `None` when `avg_frame_rate.num` is 0, else the rational. Setter:
writes the rational, or `0/1` for `None`. No state check besides closed.

```ocaml
val get_time_base : (_, _, _) stream -> Avutil.rational
```

The `AVStream` time base.

```ocaml
val get_container_stream_time_base : index:int -> _ container -> Avutil.rational
```

Scans the context's streams for one whose `index` field equals `index` and
returns its time base. Raises `Not_found` when none matches. Performs no
closed check.

```ocaml
val set_time_base : (_, _, _) stream -> Avutil.rational -> unit
```

Writes the `AVStream` time base. Does not touch any codec context. No check
on header state.

```ocaml
val get_frame_size : (output, audio, _) stream -> int
```

Reads `frame_size` from the stream's codec context in the stream table. It
assumes the stream has one (i.e. was created by `new_audio_stream`).

```ocaml
val get_pixel_aspect : (_, video, _) stream -> Avutil.rational option
```

The `AVStream` `sample_aspect_ratio`; `None` when its numerator is 0.

```ocaml
val get_duration :
  ?format:Time_format.t -> (input, _, _) stream -> Int64.t option
```

Reads the `AVStream` `duration`. `None` when `AV_NOPTS_VALUE`. Otherwise
`(duration * F * tb.num) / tb.den` in 64-bit integer arithmetic. A zero
result is returned as `Some 0L` (unlike `get_input_duration`).

```ocaml
val get_metadata : (input, _, _) stream -> (string * string) list
```

As `get_input_metadata` on the stream's dictionary.

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

OCaml side: every stream in every list must be physically the same container
as the argument, else `Stdlib.Failure "Inconsistent stream and input!"`. The
packet lists are concatenated (audio, video, subtitle, data) into the _packet
selection_; the frame lists (audio, video, subtitle) into the _frame
selection_. All lists default to empty.

Stub, after the closed check — repeat until something is returned or raised:

1. **If the pending stream index is -1**:
   1. `av_read_frame(ctx, work_packet)` with the lock released.
   2. `AVERROR(EAGAIN)`: restart the loop immediately.
   3. `AVERROR_EOF`: run the **drain step** (below). If it yields a frame,
      return it as `` `Audio_frame`` / `` `Video_frame``.
   4. Any negative code (including end of file after an empty drain): raise
      the mapped error. End of input is ``Error `Eof``.
   5. Classify the packet by the codec type of its stream (table in §3). A
      packet of another type is unreferenced and the loop restarts.
   6. If the packet's stream index is in the packet selection: clone the
      packet (`av_packet_clone`), unreference the work packet, return
      `` `X_packet (index, packet)``. The tag follows the stream's actual
      codec type.
   7. Else if the index is in the frame selection: take the stream's table
      entry (growing the table to the current stream count); if there is
      none, **open the decoder** (below). Continue at step 3 with the work
      packet as input.
   8. Else (unhandled): when `on_unhandled_packet` is given, clone the packet,
      unreference the work packet and call the closure with
      `` `X_packet (index, packet)``; otherwise unreference the work packet.
      Restart the loop.
2. **Else** take the table entry of the pending stream, with no input packet.
   This happens regardless of the current call's selections.
3. **Decode.**
   - _Subtitle decoder_: `avcodec_decode_subtitle2(dec, work_subtitle,
&got, packet)` with the lock released. On a negative result:
     unreference the packet, raise. When nothing was produced: unreference
     the packet, restart the loop. Otherwise adjust timing: if the subtitle
     `pts` is `AV_NOPTS_VALUE` and the packet has a pts, set it to the packet
     pts rescaled from the stream time base to `AV_TIME_BASE_Q`; if the
     packet duration is positive and `end_display_time` is 0, set
     `end_display_time` to the duration rescaled to milliseconds.
     Unreference the packet. Move the subtitle into a freshly allocated
     `AVSubtitle` (bitwise copy; the rectangles now belong to the new
     object), wrap it, return `` `Subtitle_frame (index, subtitle)``.
   - _Audio or video decoder_, with the lock released: when there is an
     input packet, `avcodec_send_packet(dec, packet)` then unreference the
     packet; on a negative result set pending to -1 and stop with that code;
     otherwise set pending to this stream. Then
     `avcodec_receive_frame(dec, work_frame)`; on a negative result set
     pending to -1. Outcome: `AVERROR(EAGAIN)` restarts the loop; another
     negative code is raised; success clones the work frame
     (`av_frame_clone`), unreferences the work frame and returns
     `` `Audio_frame`` / `` `Video_frame (index, frame)``. Pending stays set,
     so the next call asks the same decoder for another frame before reading
     a new packet.

**Open the decoder** (first frame-mode packet of a stream):

1. Index out of range raises
   `Failure "Failed to open stream N : index out of bounds"`.
2. Decoder = the preferred decoder recorded for this index at `open_input`,
   else `avcodec_find_decoder(codecpar->codec_id)`. None found raises
   ``Error `Decoder_not_found``.
3. A decoder whose type is not audio, video or subtitle raises
   `Failure "Failed to allocate stream N of media type T"`.
4. Allocate the table entry and store it in the table. Allocate a codec
   context for the decoder (`avcodec_alloc_context3`).
5. `avcodec_parameters_to_context(dec_ctx, codecpar)`.
6. Open through the `avcodec` codec-opening helper with no options (the
   helper sets `thread_count = 0` and calls `avcodec_open2` with the lock
   released).
7. A failure in step 5 or 6 frees the entry record and raises the mapped
   error. The table slot is not cleared and the codec context is not freed.

The decoder's `pkt_timebase` is not set. Options returned by the configure
callbacks are not given to the decoder.

**Drain step** (at end of file): for each index of the frame selection, in
order, skipping indexes beyond the table, streams with no opened decoder and
subtitle decoders: with the lock released call
`avcodec_send_packet(dec, NULL)` (result ignored) then
`avcodec_receive_frame(dec, work_frame)`. Success returns that stream.
`AVERROR_EOF` and `AVERROR(EAGAIN)` move to the next stream. Any other code
is raised. The drain step runs again on every later `read_input` call, so
successive calls return the remaining buffered frames one by one, then raise
``Error `Eof``.

Consequences visible to the caller:

- With empty selections every packet is unhandled; the call loops to the end
  of the input and raises ``Error `Eof``.
- A stream listed in both a packet list and a frame list is delivered as
  packets.
- The media type given by which packet list a stream sits in is not used.

```ocaml
type seek_flag =
  | Seek_flag_backward | Seek_flag_byte | Seek_flag_any | Seek_flag_frame

val seek :
  ?flags:seek_flag list ->
  ?stream:(input, _, _) stream ->
  ?min_ts:Int64.t -> ?max_ts:Int64.t ->
  fmt:Time_format.t -> ts:Int64.t -> input container -> unit
```

1. Closed check. `flags` defaults to `[]`.
2. Stream index = the given stream's index, or -1. The stream's container is
   not compared with the container argument.
3. Conversion, 64-bit integer arithmetic, `F` = `fmt` units per second:
   without a stream, `ts * AV_TIME_BASE / F`; with a stream,
   `ts * tb.den / (tb.num * F)`. The same conversion applies to `min_ts` and
   `max_ts` when given; absent bounds are `INT64_MIN` and `INT64_MAX`. The
   conversion is applied whatever the flags are.
4. `avformat_seek_file(ctx, index, min_ts, ts, max_ts, flags)` with the lock
   released. A negative result is raised.
5. On success: `avcodec_flush_buffers` on every opened decoder of the
   container, and the pending stream index is reset to -1, so no frame
   decoded before the seek is returned after it.

### 4.4 Output

```ocaml
val open_output :
  ?interrupt:(unit -> bool) -> ?format:(output, _) format ->
  ?interleaved:bool -> ?opts:opts -> string -> output container
```

Common output opening routine, with (format or null, file name copy, no
custom I/O, interrupt, interleaved, option dictionary):

1. Allocate a zeroed container record (is-input 0, header not written).
2. Select the write function: `av_interleaved_write_frame` when
   `interleaved` (default `true`), else `av_write_frame`.
3. If `interrupt` is given, root the closure and prepare an
   `AVIOInterruptCB` (binding's interrupt function, container record as
   opaque) for step 7. The format context's own `interrupt_callback` is not
   set.
4. `avformat_alloc_output_context2(&ctx, format, NULL, file_name)`. On
   failure: free the name, the dictionary and the record; raise.
5. `av_opt_set_dict(ctx, &dict)`: applies generic format options, leaving
   unrecognised entries in the dictionary. On failure: raise.
6. If the muxer has private data, `av_opt_set_dict(ctx->priv_data, &dict)`:
   applies muxer-private options. On failure: raise.
7. I/O:
   - with a custom `AVIOContext`: if the format has `AVFMT_NOFILE`, raise
     `Failure "Cannot set custom I/O on this format!"`; else install it as
     `pb` and set the custom-I/O flag;
   - without: unless the format has `AVFMT_NOFILE`, call
     `avio_open2(&ctx->pb, file_name, AVIO_FLAG_WRITE, interrupt_cb_or_NULL,
&dict)` with the lock released (protocol options are consumed from the
     same dictionary); on failure raise. `AVFMT_NOFILE` formats get no I/O
     context.
8. Free the file name copy.

The failure paths of steps 5–7 free the record, the name and the dictionary;
they do not free the format context. Step 7's `avio_open2` failure and step
5's failure also unroot the interrupt closure.

Then: collect unused keys, wrap, attach GC cleanup, filter `opts`.

```ocaml
val open_output_format :
  ?interleaved:bool -> ?opts:opts -> (output, _) format -> output container
```

The common routine with the given format, a null file name, no custom I/O
and no interrupt.

```ocaml
val open_output_stream :
  ?opts:opts -> ?interleaved:bool -> ?seek:seek -> write ->
  (output, _) format -> output container
```

Creates an I/O object (§2.3) with no read closure, the write closure and the
optional seek closure, then runs the common routine with the format, a null
file name, the I/O object's `AVIOContext` and no interrupt. On success the
I/O object is stored and rooted in the container.

```ocaml
val reopen_output_stream : output container -> unit
```

Closed check. If the context has no `pb`, raises
`Failure "Not a streamed output!"`. Otherwise calls
`avio_open_dyn_buf(&ctx->pb)` with the lock released: the `pb` field now
points to a new dynamic memory buffer context. The previous context is
neither flushed nor closed, the custom-I/O flag is unchanged. A negative
result is raised.

```ocaml
val output_started : output container -> bool
```

The header-written flag.

```ocaml
val set_output_metadata : output container -> (string * string) list -> unit
val set_metadata : (_, _, _) stream -> (string * string) list -> unit
```

Closed check; raises `Failure "Failed to set metadata : header already
written"` when the header-written flag is set. Frees the target dictionary
(container's, or the stream's), then `av_dict_set(dict, key, value, 0)` for
each pair in order (strings are copied; a later duplicate key replaces an
earlier one). A failing `av_dict_set` raises, leaving the pairs already set.
`set_metadata` accepts input streams as well.

```ocaml
val get_output : (output, _, _) stream -> output container
```

Record field read.

Adding a stream — common routine used by every creation function:

1. Closed check; raises `Failure "Failed to create new stream : header
already written"` when the header is written.
2. Grow the stream table to the format context's stream count plus one.
3. Allocate the table entry at the new index; when a codec is given, check
   its type (audio, video, subtitle, else `Failure "Failed to allocate
stream N of media type T"`) and allocate its context
   (`avcodec_alloc_context3`).
4. `avformat_new_stream(ctx, codec_or_NULL)`; set the stream's `id` to its
   index.

Opening an encoder — common routine:

1. If the output format has `AVFMT_GLOBALHEADER`, set
   `AV_CODEC_FLAG_GLOBAL_HEADER` on the encoder.
2. If a hardware device context is given, `hw_device_ctx =
av_buffer_ref(device)`; if a hardware frame context is given,
   `hw_frames_ctx = av_buffer_ref(frames)`.
3. Open through the `avcodec` codec-opening helper with the option
   dictionary (thread count 0, `avcodec_open2`, lock released).
4. Copy the encoder's `time_base` to the `AVStream` time base.
5. `avcodec_parameters_from_context(stream->codecpar, enc)`.
6. Any failure frees the dictionary and raises. The `AVStream` and the table
   entry added by the previous routine remain in the container.

```ocaml
val new_stream_copy :
  params:'mode Avcodec.params -> output container ->
  (output, 'mode, [ `Packet ]) stream
```

`new_uninitialized_stream_copy` followed by `initialize_stream_copy`.

```ocaml
type uninitialized_stream_copy
val new_uninitialized_stream_copy : output container -> uninitialized_stream_copy
```

Adds a stream with no codec (common routine) and returns the container with
the new index. The stream's codec parameters are left as FFmpeg initialises
them.

```ocaml
val initialize_stream_copy :
  params:'mode Avcodec.params -> uninitialized_stream_copy ->
  (output, 'mode, [ `Packet ]) stream
```

Closed check. `avcodec_parameters_copy(stream->codecpar, params)`, then sets
`codec_tag` to 0. Returns a stream value for the reserved index. Checks
neither the header-written flag nor whether the stream was already
initialised. The stream's time base is left to the muxer (or to
`set_time_base`).

```ocaml
val new_audio_stream :
  ?opts:opts -> channel_layout:Channel_layout.t -> sample_rate:int ->
  sample_format:Avutil.Sample_format.t -> time_base:Avutil.rational ->
  codec:[ `Encoder ] Avcodec.Audio.t -> output container ->
  (output, audio, [ `Frame ]) stream
```

1. OCaml side builds a private copy of `opts` extended with the derived
   audio options produced by the `avutil` audio-options helper from
   `channel_layout`, `sample_rate`, `sample_format`, `time_base` (sample rate
   `ar`, `channel_layout`, `sample_fmt`, `time_base`).
2. Add a stream with the codec (common routine).
3. Set the encoder's `sample_fmt` to the sample format's C id and copy the
   channel layout with `av_channel_layout_copy`; a failure frees the
   dictionary and raises.
4. Open the encoder (common routine) with the dictionary, no hardware
   context.
5. Report unused keys; only the caller's own `opts` table is filtered.

```ocaml
val new_video_stream :
  ?opts:opts -> ?frame_rate:Avutil.rational ->
  ?hardware_context:Avcodec.Video.hardware_context ->
  pixel_format:Avutil.Pixel_format.t -> width:int -> height:int ->
  time_base:Avutil.rational -> codec:[ `Encoder ] Avcodec.Video.t ->
  output container -> (output, video, [ `Frame ]) stream
```

1. OCaml side builds a private copy of `opts` extended by the `avutil`
   video-options helper (`pixel_format`, `video_size` as `WxH`, `time_base`,
   and the frame rate when given).
2. `hardware_context` is split: `` `Device_context d`` gives a device
   context, `` `Frame_context f`` gives a frame context, never both.
3. Add a stream with the codec, open the encoder with the hardware context
   and the dictionary. Pixel format, size and time base reach the encoder
   only through the dictionary.
4. Filter the caller's `opts`.
5. Call `set_avg_frame_rate stream frame_rate` (so an absent frame rate
   writes `0/1`).

```ocaml
val new_subtitle_stream :
  ?opts:opts -> ?header:string -> time_base:Avutil.rational ->
  codec:[ `Encoder ] Avcodec.Subtitle.t -> output container ->
  (output, subtitle, [ `Frame ]) stream
```

1. OCaml side: when `header` is absent and the codec's descriptor (looked up
   by codec id) lists the `` `Text_sub`` property, the header becomes
   `Avutil.Subtitle.header_ass_default ()`; otherwise no header.
2. Add a stream with the codec. Set the encoder `time_base` directly.
3. With a header: allocate `length + 1` zeroed bytes, copy the string, set
   `subtitle_header` and `subtitle_header_size = length`.
4. Open the encoder with the caller's dictionary (no derived options), no
   hardware context. Filter `opts`.

```ocaml
val new_data_stream :
  time_base:Avutil.rational -> codec:Avcodec.Unknown.id -> output container ->
  (output, [ `Data ], [ `Packet ]) stream
```

Adds a stream with no codec, then sets on the `AVStream`: the time base,
`codecpar->codec_type = AVMEDIA_TYPE_DATA`, `codecpar->codec_id` from the
`avcodec` unknown-id conversion. Takes no options (the `.mli` comment
mentions `opts`; the signature has none).

```ocaml
val codec_attr : _ stream -> string option
```

An RFC 6381 style codec string for HLS playlists, derived from the stream's
codec parameters. `None` when the context or stream is missing, and in every
case not listed:

| Codec id | Result                                                                                                                                                                                                            |
| -------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| H.264    | extradata must begin with `00 00 00 01` followed by a byte whose low 5 bits are 7 (SPS); result `avc1.` + bytes 5, 6, 7 as two lowercase hex digits each; otherwise `None`. The extradata length is not consulted |
| FLAC     | `fLaC`                                                                                                                                                                                                            |
| HEVC     | see below                                                                                                                                                                                                         |
| MP2      | `mp4a.40.33`                                                                                                                                                                                                      |
| MP3      | `mp4a.40.34`                                                                                                                                                                                                      |
| AAC      | `mp4a.40.` + (profile + 1) when the profile is known, else `None`                                                                                                                                                 |
| AC-3     | `ac-3`                                                                                                                                                                                                            |
| E-AC-3   | `ec-3`                                                                                                                                                                                                            |

HEVC: profile and level start from the codec parameters when known. The
extradata is scanned byte by byte, while at least 20 bytes remain, for
`00 00 00 01` followed by a byte `b` with `b & 0x7E == 0x42` (SPS). When one
is found, the bytes after the 6-byte start code and NAL header are stripped
of emulation-prevention bytes; a failure to allocate or fewer than 13
resulting bytes gives `None`; otherwise profile and level are read (byte 1
low 5 bits, byte 12) and the function **returns `None`**. When no SPS is
found: if the codec tag is `hvc1` and profile and level are known the result
is `hvc1.PROFILE.4.LLEVEL.B01`, otherwise the codec tag printed as a
four-character code.

```ocaml
val bitrate : _ stream -> int option
```

`Some codecpar->bit_rate` when non-zero. Otherwise `Some max_bitrate` from
the stream's `AV_PKT_DATA_CPB_PROPERTIES` side data when present (§10 for
where it is looked up), else `None`.

```ocaml
val write_packet :
  (output, 'media, [ `Packet ]) stream -> Avutil.rational ->
  'media Avcodec.Packet.t -> unit
```

1. Closed check. `Failure "Failed to write in closed output"` when the
   container has no stream table; `Stdlib.Failure "Internal error"` when the
   table has no entry for the index.
2. With the lock released: write the header if not yet written (failure is
   raised, flag stays clear).
3. On the caller's packet itself: set `stream_index`, set `pos = -1`,
   `av_packet_rescale_ts(packet, given_time_base, stream->time_base)`.
4. Call the container's write function with that packet. A negative result
   is raised.

The packet is not copied: its fields are modified in place and the muxer
receives the caller's packet object.

```ocaml
val write_frame :
  ?on_keyframe:(unit -> unit) -> (output, 'media, [ `Frame ]) stream ->
  'media frame -> unit
```

Closed check. `Failure "Invalid input: no streams provided"` without a stream
table. Dispatch on the encoder's codec type. For audio and video (also used
with a null frame by `close` to flush):

1. Checks: `Failure "Stream index not found!"` when the index is not below
   the stream count; `Failure "Failed to write frame with no encoder"`.
2. Release the lock. Write the header if needed (failure raised).
3. Allocate a packet.
4. If the encoder has a hardware frames context and a frame is given:
   allocate a frame, `av_hwframe_get_buffer(enc->hw_frames_ctx, hw, 0)`,
   `av_hwframe_transfer_data(hw, frame, 0)`, and use `hw` as the frame to
   send. No other frame field is copied. Failures free both and raise.
5. `avcodec_send_frame(enc, frame)`.
   - null frame and `AVERROR_EOF`: return (already flushed);
   - any other negative code (including `AVERROR_EOF` with a frame and
     `AVERROR(EAGAIN)`): free, raise.
6. Loop `avcodec_receive_packet(enc, packet)` until it fails. For each
   packet: if it has `AV_PKT_FLAG_KEY` and `on_keyframe` is given, re-acquire
   the lock, call the closure, release the lock; then set `stream_index`,
   `pos = -1`, rescale timestamps from the **encoder** time base to the
   stream time base, and call the container's write function. A negative
   write result ends the loop.
7. Free the packet and the hardware frame. Re-acquire the lock.
8. Final code `AVERROR(EAGAIN)` is success. `AVERROR_EOF` is success only
   for a null frame. Any other negative code is raised.

For a subtitle encoder, see `write_subtitle_frame`; `on_keyframe` is ignored.

```ocaml
val write_subtitle_frame :
  (output, subtitle, [ `Frame ]) stream -> Avutil.Subtitle.frame -> unit
```

Same entry point as `write_frame` without `on_keyframe`.

1. Checks as above, with `Failure "Failed to write subtitle frame with no
encoder"`.
2. Allocate a packet with a 4096-byte payload (`av_new_packet`).
3. With the lock released: write the header if needed, then
   `avcodec_encode_subtitle(enc, packet->data, 4096, subtitle)`. The header
   is written first, even when the subtitle then encodes to nothing.
4. A negative result frees the packet and raises. A zero result frees the
   packet and returns without writing.
5. Set `packet->size` to the encoded size, `pts = dts = subtitle->pts`
   (`AV_TIME_BASE` units), `duration = (end_display_time -
start_display_time) * AV_TIME_BASE / 1000`.
6. With the lock released: set `stream_index`, `pos = -1`, rescale from
   `AV_TIME_BASE_Q` to the stream time base, call the write function. Free
   the packet. A negative result is raised.

```ocaml
val flush : output container -> unit
```

Closed check. Returns immediately when the header is not written. Otherwise,
with the lock released, calls the container's write function with a null
packet and, when that succeeds and the context has a `pb`,
`avio_flush(pb)`. A negative write result is raised. Encoders are not
flushed.

```ocaml
val tell : _ container -> int option
```

Closed check. `None` when the context has no `pb`. Otherwise `avio_tell(pb)`
with the lock released; the result is held in a C `int`; negative raises the
mapped error, else `Some`.

```ocaml
val close : _ container -> unit
```

1. Closed check (a second `close` raises).
2. Output with a stream table: for each stream index in order, when the
   table entry has an audio or video encoder, run the `write_frame` routine
   with a null frame and no keyframe closure (this writes the header if it
   is not yet written, drains the encoder, writes the packets). Subtitle
   encoders are skipped. An error here is raised and the container stays
   open: later streams are not flushed, no trailer is written, nothing is
   released.
3. Output with a stream table and header written: `av_write_trailer(ctx)`
   with the lock released; its result is ignored.
4. Release routine (§2.1).

An output with no stream is released without writing a header or trailer. An
output with only copy/data streams to which no packet was written is
released without header or trailer.

## 5. Errors

| Raised                                                                                                                                                            | From                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| ----------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Avutil.Error e` with `e` mapped from a negative FFmpeg code by the `avutil` helper (named variant when the code is one of its known ones, else `` `Other code``) | every FFmpeg call whose result is checked: open, probe, read, decode, seek, header, encode, write, flush, `tell`, dictionary and parameter copies                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| ``Avutil.Error `Eof``                                                                                                                                             | `read_input` at end of input after the drain; `write_frame` on an encoder already flushed                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| ``Avutil.Error `Stream_not_found``                                                                                                                                | `find_best_*_stream` on any failure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| ``Avutil.Error `Decoder_not_found``                                                                                                                               | `read_input` when a frame-mode stream has no decoder                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| ``Avutil.Error (`Failure msg)``                                                                                                                                   | closed container; state and argument checks. Messages: "Container closed!", "At least one format or url must be provided!", "Failed to open stream N : index out of bounds", "Failed to allocate stream N of media type T", "Internal error: no packet for subtitle decoder!", "Cannot set custom I/O on this format!", "Not a streamed output!", "Failed to set metadata : header already written", "Failed to create new stream : header already written", "Failed to write in closed output", "Stream index not found!", "Failed to write frame with no encoder", "Failed to write subtitle frame with no encoder", "Invalid input: no streams provided", "Empty input format", "Empty output format", plus those of the `avutil`/`avcodec` wrappers ("Empty packet", "Empty frame", "Empty subtitle", "Failed to get codec parameters") |
| `Stdlib.Failure "Inconsistent stream and input!"`                                                                                                                 | `read_input`, OCaml side                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| `Stdlib.Failure "Internal error"`                                                                                                                                 | `write_packet` when the table entry is missing                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| `Not_found`                                                                                                                                                       | `get_container_stream_time_base`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| `Out_of_memory`                                                                                                                                                   | allocation failures of binding-side records, packets, frames, I/O object                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| any exception                                                                                                                                                     | raised by a configure callback, `on_unhandled_packet` or `on_keyframe`; propagates unchanged                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |

Six further `Failure` messages exist for a null format context ("Failed to
get closed input duration", "Failed to read closed input", "Failed to open
stream N of closed input", "Failed to seek closed input", "Failed to set
metadata to closed output", "Failed to add stream to closed output"); the
closed check precedes them, so they are not observable.

A raising I/O callback is reported to FFmpeg as `AVERROR_EXTERNAL`; the
operation in progress then raises whatever code FFmpeg propagates. The
`avutil` mapping has no named variant for `AVERROR_EXTERNAL`, so an unchanged
code surfaces as `` `Other``.

## 6. Blocking and concurrency

The runtime lock is released around:

| Operation                                                | FFmpeg calls made without the lock                                                                                                   |
| -------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------ |
| `Format.find_input_format`, `Format.guess_output_format` | `av_find_input_format`, `av_guess_format`                                                                                            |
| `open_input`, `open_input_stream`                        | `avformat_open_input`, `avformat_find_stream_info`                                                                                   |
| `find_best_*_stream`                                     | `av_find_best_stream`                                                                                                                |
| `read_input`                                             | `av_read_frame`, `avcodec_find_decoder`, `avcodec_open2`, `avcodec_send_packet`, `avcodec_receive_frame`, `avcodec_decode_subtitle2` |
| `seek`                                                   | `avformat_seek_file`                                                                                                                 |
| `open_output`                                            | `avio_open2`                                                                                                                         |
| `new_*_stream` (encoding)                                | `avcodec_open2`                                                                                                                      |
| `reopen_output_stream`                                   | `avio_open_dyn_buf`                                                                                                                  |
| `write_packet`                                           | `avformat_write_header`, timestamp rescale, write function                                                                           |
| `write_frame`                                            | header, hardware upload, `avcodec_send_frame`, `avcodec_receive_packet`, write function                                              |
| `write_subtitle_frame`                                   | header, `avcodec_encode_subtitle`, write function                                                                                    |
| `flush`                                                  | write function, `avio_flush`                                                                                                         |
| `tell`                                                   | `avio_tell`                                                                                                                          |
| `close`, GC cleanup                                      | encoder flush, `av_write_trailer`, the whole C part of the release routine                                                           |

Held throughout: `avformat_alloc_output_context2`, `av_opt_set_dict`,
`avformat_new_stream`, parameter copies, metadata, `avcodec_flush_buffers`
after a seek, all getters.

The container has no lock of its own. While a stub runs with the runtime
lock released, another OCaml thread can enter any operation on the same
container; nothing serialises them.

Thread registration: each C-to-OCaml callback of §7.1–7.4 first registers
the current thread with the OCaml runtime through the `avutil` helper
(idempotent), then acquires the runtime lock.

Global state: `avformat_network_init()` at module load. No other global.

## 7. Callbacks

### 7.1 Custom read (`read`)

- Trigger: FFmpeg's buffered I/O, on whatever thread runs the demuxing call
  (normally the OCaml thread inside `open_input_stream`, `read_input` or
  `seek`, with the lock released).
- Steps: `len = min(32768, requested)`; register thread; acquire lock; call
  `read buffer 0 len` on the shared transfer buffer.
- Result `n`: negative → returned to FFmpeg unchanged as an error code;
  otherwise `n` bytes are copied from the transfer buffer to FFmpeg's
  buffer; `0` → `AVERROR_EOF`; positive → `n`. `n` is not compared with
  `len`.
- Exception: caught; logged with `av_log(avio_context, AV_LOG_ERROR,
"Error while executing OCaml read callback: EXN\n")`; returns
  `AVERROR_EXTERNAL`.

### 7.2 Custom write (`write`)

- Trigger: FFmpeg's buffered I/O flushing during header, packet, trailer,
  `flush` writes.
- Steps: `len = min(32768, size)`; register thread; acquire lock; copy `len`
  bytes from FFmpeg's buffer into the transfer buffer; call
  `write buffer 0 len`.
- Result: returned to FFmpeg unchanged. Bytes beyond 32768 in one call are
  not passed and the function is not called again for them by the binding.
- Exception: caught, logged as "write", returns `AVERROR_EXTERNAL`.

### 7.3 Custom seek (`seek`)

- Trigger: FFmpeg seeking in the I/O context.
- Steps: map `whence` (§3); an unmapped value returns -1 without calling
  OCaml. Register thread; acquire lock; call the closure with the offset as
  an OCaml `int` and the whence. The user closure receives
  `Unix.seek_command`.
- Result: the returned integer, as the new position.
- Exception: caught, logged as "seek", returns `AVERROR_EXTERNAL`.

Closures of 7.1–7.3 are kept alive by the I/O object and released by its GC
finalisation (§2.3).

### 7.4 Interrupt (`?interrupt`)

- Inputs: installed as the format context's `interrupt_callback` before
  `avformat_open_input`; consulted by FFmpeg during any blocking I/O of that
  context for its whole life.
- Outputs: passed only to `avio_open2`; consulted during blocking I/O on the
  context that call opened.
- Steps: if no closure, return 0. Register thread, acquire lock, call
  `interrupt ()`. `true` returns 1 (abort the blocking operation), `false`
  returns 0.
- Exception: caught, logged as "interrupt" on the format context, returns 1
  (abort).
- Lifetime: rooted from open until the release routine.

### 7.5 Control message (exported to `avdevice`)

The header exports a setter and a getter:

- setter (container value, C callback, OCaml closure): closed check; stores
  the closure in the container (rooting it the first time, replacing it
  afterwards), sets the format context's `opaque` to the container record
  and its `control_message_cb` to the given C callback;
- getter (format context): returns the address of the stored closure through
  `opaque`.

The C callback itself, its thread and exception handling belong to
`avdevice`. The closure is released by the container's release routine.

### 7.6 Synchronous OCaml callbacks

Called on the calling thread, with the lock held, exceptions propagate:

| Callback              | Called from   | Note                                                                                                                        |
| --------------------- | ------------- | --------------------------------------------------------------------------------------------------------------------------- |
| `configure_*_stream`  | `open_input`  | an exception leaves the half-opened container unreferenced                                                                  |
| `on_unhandled_packet` | `read_input`  | receives a cloned packet; an exception aborts the read after the work packet was released                                   |
| `on_keyframe`         | `write_frame` | called before the key packet is given to the muxer; the lock is re-acquired for the call; an exception abandons that packet |

## 8. Data transfer

| Path                                                        | Copy or share                                                                                                                                                         |
| ----------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| URL, format names, MIME type                                | copied to C strings for the call, freed after                                                                                                                         |
| option keys and values                                      | copied into an `AVDictionary`                                                                                                                                         |
| metadata get                                                | keys and values copied to OCaml strings                                                                                                                               |
| metadata set                                                | copied by `av_dict_set`                                                                                                                                               |
| codec parameters (get, configure callback)                  | deep copy owned by the OCaml value                                                                                                                                    |
| codec parameters (`initialize_stream_copy`)                 | deep copy into the stream                                                                                                                                             |
| packet returned by `read_input` or to `on_unhandled_packet` | new `AVPacket` from `av_packet_clone`: new reference to the same data buffer (or a copy when the demuxer's packet is not reference counted); owned by the OCaml value |
| frame returned by `read_input`                              | new `AVFrame` from `av_frame_clone`: new references to the decoder's buffers; owned by the OCaml value                                                                |
| subtitle returned by `read_input`                           | new `AVSubtitle` taking over the decoded rectangles; owned by the OCaml value                                                                                         |
| packet given to `write_packet`                              | not copied; `stream_index`, `pos` and timestamps are overwritten in the caller's packet, which is handed to the muxer                                                 |
| frame given to `write_frame`                                | not copied; the encoder takes its own references; with a hardware frames context the pixel data is uploaded into a new hardware frame                                 |
| subtitle given to `write_subtitle_frame`                    | read only                                                                                                                                                             |
| subtitle header                                             | copied into `av_mallocz(len + 1)`                                                                                                                                     |
| custom read                                                 | callback fills the OCaml transfer buffer; bytes copied to FFmpeg's buffer                                                                                             |
| custom write                                                | bytes copied from FFmpeg's buffer to the OCaml transfer buffer                                                                                                        |
| rationals, durations, indexes                               | by value                                                                                                                                                              |

The transfer buffer is one `bytes` value reused for every call of one I/O
object; its content is only meaningful during a callback.

No bigarray, plane or stride handling in this library.

## 9. Options

- `opts` is an `Avutil.opts` hash table. `None` is treated as an empty table.
- The OCaml side turns it into an array of `(key, string value)` pairs; the
  stub builds an `AVDictionary` (`av_dict_set`, flags 0).
- After the FFmpeg calls that consume it, the keys still in the dictionary
  are returned as an array and the dictionary is freed. The OCaml side then
  removes from the caller's table every key **not** in that array: the table
  ends up holding exactly the unused options.

| Function                                                      | Consumers of the dictionary, in order                                                                                                      |
| ------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------ |
| `open_input`, `open_input_stream`                             | `avformat_open_input`                                                                                                                      |
| `open_input` per-stream `opts`                                | `avformat_find_stream_info` (one dictionary per stream; unused entries discarded)                                                          |
| `open_output`                                                 | `av_opt_set_dict` on the format context, then on the muxer private data, then `avio_open2`                                                 |
| `open_output_format`, `open_output_stream`                    | `av_opt_set_dict` on the format context, then on the muxer private data (and `avio_open2` for `open_output_format` on a file-based format) |
| `new_audio_stream`, `new_video_stream`, `new_subtitle_stream` | `avcodec_open2`                                                                                                                            |

`avformat_write_header` receives no options.

For `new_audio_stream` and `new_video_stream` the dictionary contains the
derived options in addition to the caller's; only the caller's table is
filtered, so a derived option the encoder did not use is not reported.

AVOption access: `container_options` (class for listing) and `input_obj`
(object for getters) feed `Avutil.Options`. There is no equivalent object for
output containers or streams.

## 10. Version-dependent behaviour

| Condition               | Effect                                                                                                                                               |
| ----------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------- |
| libavcodec < 58.9.100   | `av_register_all()` is called at load. Below the supported range                                                                                     |
| libavformat <= 59.0.100 | `AVInputFormat *` / `AVOutputFormat *` handled as non-const; const otherwise. Below the supported range                                              |
| libavformat major < 61  | the custom write function takes `uint8_t *buf`; from 61 it takes `const uint8_t *buf`. No behavioural difference                                     |
| libavcodec >= 60.15.100 | `bitrate` looks up CPB properties with `av_packet_side_data_get` in `codecpar->coded_side_data`; before, with `av_stream_get_side_data`              |
| libavcodec < 60.26.100  | `AV_PROFILE_UNKNOWN` / `AV_LEVEL_UNKNOWN` are aliases of `FF_PROFILE_UNKNOWN` / `FF_LEVEL_UNKNOWN` (from the `avcodec` header), used by `codec_attr` |

The `AVChannelLayout` API is used unconditionally.

## 11. Logic on the OCaml side

- **Version split** of `avformat_version` (§4.1).
- **Option round trip**: default table, array conversion, filtering (§9).
- **GC cleanup**: every open attaches `Gc.finalise` running the release
  routine; the I/O object gets a `Gc.finalise` dropping its roots.
- **Configure wrapper**: turns a `stream_config` record into the pair
  (codec option, option array) the stub expects.
- **Seek whence**: wraps the user's `seek` closure, converting integers 0, 1,
  2 to `Unix.SEEK_SET`, `SEEK_CUR`, `SEEK_END`.
- **Stream values**: built from container and index; `get_input`,
  `get_output`, `get_index` are field reads.
- **Stream lists**: the stub returns indexes in descending order; the OCaml
  side reverses while attaching the stream value and a parameters copy.
- **Metadata lists**: the stub returns pairs in reverse iteration order; the
  OCaml side reverses.
- **Durations**: index -1 means the container; `get_input_duration` maps
  `Some 0L` to `None`.
- **`read_input`**: container identity check and flattening of the seven
  lists into the packet selection (index with a media-type tag) and the
  frame selection (indexes).
- **`seek`**: flag list to array.
- **Metadata setters**: index -1 for the container, list to array.
- **Stream copy**: `new_stream_copy` is reserve then initialise.
- **Audio/video streams**: derived options go in a private copy of the
  table; video sets the average frame rate after creation.
- **Subtitle streams**: default ASS header selection for text subtitle
  codecs.
- **`write_subtitle_frame`**: `write_frame` without `on_keyframe`.
- **`input_obj`**: pairs the C object with its container.
