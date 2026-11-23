# Encoders: the generic seam (as-built)

What every encoder offers the outputs that drive it, as the code does it
today. It owns: the registry, factories, the instance record (the "stream
face"), the serialisation wrapper, the section numbering every implementation
file answers, and the table of which encoder implements what.

It relies on: the format values and the `%name(...)` language
([formats.md](formats.md)); the segmenting face, owned by the HLS output
([encoder-contract.md](../../outputs/hls/spec/encoder-contract.md)); the
generic output that slices frames before encoding
([output.md](../../outputs/spec/output.md)); draining
([draining.md](../../source/spec/draining.md)).

Each encoder's own behaviour lives in its implementation file, one per
encoder: [WAV](wav.md), [AVI](avi.md), [NDI](ndi.md),
[external](external.md); [LAME MP3](mp3.md), [Shine](shine.md),
[FDK-AAC](fdkaac.md), [native FLAC](flac.md); [Ogg](ogg.md) and its
[stream contract](ogg-streams.md) with the [Vorbis](ogg-vorbis.md),
[Opus](ogg-opus.md), [Speex](ogg-speex.md), [FLAC](ogg-flac.md) and
[Theora](ogg-theora.md) streams; [FFmpeg](ffmpeg.md) (its codecs in
[the FFmpeg tree](../../optionals/ffmpeg/spec/encoder.md)).
Callers are described in the caller files: [the encoded-output base](output-encoded.md),
[output.file](output-file.md), [output.external and pipe](output-pipe.md),
[icecast](output-icecast.md), [harbor](output-harbor.md), [srt](output-srt.md),
[output.url](output-url.md), [HLS](output-hls.md).

## 1. Vocabulary

- **Format**: the immutable value a script writes as `%name(...)`. One of ten
  kinds: WAV, AVI, NDI, Ogg, MP3, Shine, FLAC, FFmpeg, FDK-AAC, External
  ([formats.md §2](formats.md#2-the-format-kinds)). `%vorbis`, `%opus`,
  `%speex` and `%theora` are not kinds: they build an Ogg format.
- **Plug-in**: a function from a format to "a factory, or nothing".
- **Factory**: a function `(pos, name, initial metadata) → instance`.
- **Instance**: one encoder, a record of seven operations (§3), created per
  use by a caller.
- **Piece**: what one `encode` call receives: a frame, read from offset 0 to
  its position (its filled length).
- **Bytes**: an immutable rope of strings (`Strings.t` in Part B); handed to
  the caller, owned by it once returned.

## 2. Registry and factories

### 2.1 Registration

- Two registries exist: the **stream registry** (plug-ins answering a
  factory) and the **HLS registry** (plug-ins answering an HLS factory, §5).
- A plug-in registers under a unique name. Registering a second plug-in under
  an existing name fails (`Failure "Plugin already registered in <registry>:
<name>"`) at program start.
- Registration happens while the program's modules load. A registry cannot be
  enumerated before the loading phase is declared complete; an enumeration
  before that raises `Incomplete <registry name>`. Script evaluation (and
  `%ifencoder`) happen after it.
- Registered stream plug-ins today, with the kinds they answer (one plug-in
  per kind, so the order never matters):

| Plug-in name | Answers                              | Available when     |
| ------------ | ------------------------------------ | ------------------ |
| `wav`        | WAV                                  | always             |
| `avi`        | AVI                                  | always             |
| `ndi`        | NDI                                  | always             |
| `external`   | External                             | always             |
| `lame`       | MP3                                  | built with LAME    |
| `shine`      | Shine                                | built with Shine   |
| `fdkaac`     | FDK-AAC                              | built with fdk-aac |
| `flac`       | FLAC (native, not Ogg)               | built with libFLAC |
| `ogg`        | Ogg (any combination of its streams) | built with libogg  |
| `ffmpeg`     | FFmpeg                               | built with FFmpeg  |

- Registered HLS plug-ins: `packed-audio` (MP3, Shine, FDK-AAC with ADTS
  transport) and `ffmpeg` (FFmpeg formats whose muxer segments). The HLS tree
  owns them.

### 2.2 Lookup (`get_factory`)

- `get_factory(format)` asks the stream plug-ins in registration order and
  returns the first factory answered, wrapped so that every instance it
  creates is serialised (§4). When no plug-in answers it raises `Not_found`.
- Each caller turns `Not_found` into its own error (all `Invalid_value` on the
  format argument, at operator construction): `output.file` / pipe outputs
  "Unsupported encoding format"; `output.icecast` "No encoder found for that
  format"; `output.srt` "Cannot get a stream encoder for that format"; HLS
  variant "Unsupported format". The language checks the same thing earlier,
  at evaluation of the `%name(...)` expression ([formats.md
  §4](formats.md#4-evaluation-building-the-format-value)), so a script that
  evaluates a format always passes the callers' checks for it.
- A plug-in answering a factory does not mean an instance can be built: the
  Ogg plug-in answers every Ogg format and only discovers at instance
  creation that its Vorbis, Opus, Speex, FLAC or Theora stream encoder is not
  compiled in (§2.3).
- `get_hls_factory(format)` does the same over the HLS registry, wrapping its
  instances with the HLS serialisation; `Not_found` when no plug-in answers.

### 2.3 Instance creation

- A factory takes:
  - `pos`: the script position of the format value, or none. Encoders use it
    only to attach a position to errors they raise later.
  - `name`: the caller's identifier (the output's id). Encoders use it to name
    their log (`[<name>]`, `[ffmpeg.encoder.<name>]`) and the Ogg muxer uses
    it in its log.
  - initial metadata: the metadata the caller holds when it creates the
    instance (in export form: filtered by the caller's export settings, cover
    optional). Of all encoders, only these read it: LAME MP3 (an ID3v2 tag
    at the start of the first output when `id3v2` is set), native FLAC
    (Vorbis comments in its stream header), Ogg (comments of the first
    logical stream), FFmpeg (container metadata). The HLS packed-audio
    factory passes empty metadata.
- Every parameter left lazy by the format (sample rate, video size, ...; see
  [formats.md §5](formats.md#5-deferred-values)) is forced at creation, so
  format validation that depends on them fails here, not at evaluation.
- Creation is not serialised by the seam. It runs on whatever thread the
  caller creates the instance from (often the clock thread, at output
  start).
- What can fail at creation, as raised to the caller:
  - a deferred value invalid (`Runtime_error` kind `encoder`, e.g. MP3 "invalid
    samplerate value" with the format's position); observed: the clock running
    the output fails with "Clock failed in source output.file:
    Lang.Runtime_error { kind: "encoder", msg: "invalid samplerate value", ...
    }" (level 1, `critical`);
  - an Ogg stream encoder missing: `Runtime_error` "Could not find any <vorbis|
    opus|speex|flac> encoder."; for Theora, an untyped `Invalid_argument`;
  - library failures (encoder configuration rejected, process spawn failure,
    FFmpeg container open failure), as each implementation file states.
- The NDI factory always succeeds and returns an instance every stream
  operation of which raises "The NDI encoder can only be used with
  `output.ndi`!" (`Runtime_error` kind `invalid`). `output.ndi` never calls
  the factory; it reads the format.

## 3. The instance (stream face)

An instance has seven operations. All take and return values only; none of
them is told where the bytes go.

| Operation                | In                     | Out                      |
| ------------------------ | ---------------------- | ------------------------ |
| `encode`                 | a piece                | bytes                    |
| `encode_into` (optional) | a piece, a consumer    | nothing                  |
| `encode_metadata`        | metadata (export form) | nothing                  |
| `header`                 | nothing                | bytes                    |
| `restart_offset`         | nothing                | an offset, or none       |
| `bitrate`                | nothing                | bits per second, or none |
| `stop`                   | nothing                | bytes                    |

### 3.1 `encode`

- Encodes the piece's content from offset 0 to its position and returns
  every byte the encoder has ready, which may be none (codecs that work on
  fixed blocks buffer the remainder), or more than this piece (bytes held from
  earlier calls, a header on the first call).
- Tracks read: each encoder reads the fields its format types (audio, video);
  metadata and track marks inside the frame are never read by any encoder.
  Callers deliver metadata through `encode_metadata`, cutting pieces at
  metadata positions ([output.md](../../outputs/spec/output.md)).
- Returned bytes belong to the caller; the encoder keeps no reference to
  them.

### 3.2 `encode_into`

- Optional. Present only on FFmpeg instances. When present, it encodes as
  `encode` does and calls the consumer exactly once with an **output view**:
  a length and a copy function `blit(src_offset, dst, dst_offset, length)`.
  The view is valid only during the consumer call: the bytes live in the
  encoder's own buffer, which is reused afterwards.
- Reason (from the change that added it, "harbor: faster dedicated
  encoders"): a caller that keeps its own buffer copies the bytes there with
  no intermediate allocation.
- The consumer runs while the instance's lock is held (§4): it can not call
  back into the same instance.
- Only the harbor dedicated mode uses it; every other caller uses `encode`.

### 3.3 `encode_metadata`

- Hands in-stream metadata to the instance. Whether, how and when it shows in
  the output is the encoder's own; as-built, the effects are:
  - Ogg: ends the current logical streams (their final pages are emitted with
    the next output) and starts new ones carrying the metadata as comments,
    a new link in a chained Ogg stream.
  - FFmpeg: before the container has written its header, sets the container
    metadata; after, restarts the container when the format chains (Ogg
    muxers), otherwise is dropped with a `debug` log.
  - External: when the format says `restart_on_metadata`, stops the process
    (it is restarted); otherwise nothing.
  - All others (WAV, AVI, MP3, Shine, FDK-AAC, native FLAC): nothing, silently.
  - NDI: raises.
- Returns nothing; the bytes it causes come out of later `encode`/`stop`
  calls.

### 3.4 `header`

- Returns the bytes a consumer that joins the output now needs before the
  next bytes `encode` returns. It never changes the instance's output.
- As-built it is non-empty only for: WAV with `header=true` and AVI, both
  **only after the first `encode`** (which itself started with that header:
  before it, `header()` is empty so a caller that sends `header()` then the
  first output does not send the header twice); Ogg (the header pages of the
  current link); FFmpeg (the container header once captured).
- Empty for MP3, Shine, FDK-AAC, native FLAC and External, whatever their
  stream starts with (native FLAC starts with `fLaC` and STREAMINFO; External
  with whatever its process writes, including the WAV or AVI header the
  encoder itself feeds the process).
- Callers: harbor (both modes) and SRT send it to each peer that joins; file
  and Icecast outputs never call it.

### 3.5 `restart_offset`

- Returns the offset, within the bytes returned by the latest `encode` or
  `stop`, at which a new logical stream with its own header starts; none when
  those bytes start none.
- Non-none only for Ogg (start of the first page of the latest link, when that
  link began inside the bytes last returned) and FFmpeg (same, for chaining
  containers).
- Callers: harbor shared mode and SRT, to start a joining peer at a link
  start rather than mid-link.

### 3.6 `bitrate`

- The nominal bit rate in bits per second once the encoder knows it, none
  otherwise.
- Only FFmpeg answers (from its codec once opened); every other encoder
  answers none, including MP3 CBR whose rate is in the format. The
  format-level estimate `bitrate(format)` ([formats.md
  §3](formats.md#3-format-helpers)) is what other callers use.
- Caller: `output.icecast` for FFmpeg formats only.

### 3.7 `stop`

- Ends the instance: flushes what the codec holds and returns the final
  bytes. After `stop`, the instance is not used again by any caller.
- What it does per encoder: MP3, Shine, FDK-AAC, FLAC flush their codec; Ogg
  ends every logical stream; FFmpeg writes the container trailer (idempotent:
  a second `stop` returns nothing); External stops the process, waits for it
  to exit and returns what it wrote meanwhile; WAV and AVI return nothing
  (the WAV header is never rewritten with the real length; the AVI header
  carries no length).
- The seam does not prevent a second `stop` or an `encode` after `stop`;
  their effect is the implementation's.

### 3.8 Failure behaviour common to all operations

- An operation's exception propagates to the caller unchanged; the seam adds
  no logging, no retry, no translation. The lock (§4) is released.
- Callers decide: file outputs call `stop` under a catch-all when closing on
  error; `output.icecast` ignores `stop` failures; see the caller files.

## 4. Serialisation (`protect`)

- Every instance returned by `get_factory` is wrapped by `protect`: one mutex
  per instance; each of the seven operations takes it for its whole duration.
  `encode_into` holds it across the consumer call.
- Instances built without `get_factory` are wrapped by their builder when the
  builder is not the only user: `output.url` builds FFmpeg instances
  directly and wraps them with `protect` itself.
- HLS instances from `get_hls_factory` get the same, with one mutex over the
  ten HLS operations. The packed-audio HLS instance wraps a stream instance
  that itself came from `get_factory`: two distinct locks, always taken in the
  same order (HLS, then stream), so no deadlock.
- The lock serialises calls on one instance; it does not order them. Which
  thread calls which operation is the caller's (see the caller files).
- Encoders that run threads of their own (External: the process's stdout,
  stderr and exit callbacks) take their own internal lock as well.

## 5. The segmenting face

The HLS output drives a different record (`init`, `init_encode`,
`split_encode`, `insert_id3`, `hls_stop`, `codec_attrs`,
`independent_segments`, `free_split`, `hls_bitrate`, `hls_video_size`),
obtained from `get_hls_factory(format)` with `(pos, segment_duration, name)`
and no initial metadata. Its contract is
[encoder-contract.md](../../outputs/hls/spec/encoder-contract.md); the two
implementations are [encoder-ffmpeg.md](../../outputs/hls/spec/encoder-ffmpeg.md)
and [encoder-packed-audio.md](../../outputs/hls/spec/encoder-packed-audio.md).
The HLS operator also calls `get_factory(format)` on every variant, only to
reject a format no stream plug-in answers, before asking the HLS registry.

## 6. Which encoder implements what

Read from each implementation's instance record.

| Encoder       | `header` non-empty              | `encode_into` | `restart_offset` non-none | `bitrate` known | `encode_metadata` effective           | HLS face             | Writes I/O itself                                                                                             |
| ------------- | ------------------------------- | ------------- | ------------------------- | --------------- | ------------------------------------- | -------------------- | ------------------------------------------------------------------------------------------------------------- |
| WAV           | after 1st `encode`, if `header` | no            | no                        | no              | no                                    | no                   | no                                                                                                            |
| AVI           | after 1st `encode`              | no            | no                        | no              | no                                    | no                   | no                                                                                                            |
| NDI           | raises                          | no            | no                        | no              | raises                                | no                   | no (instance unusable)                                                                                        |
| External      | no                              | no            | no                        | no              | only `restart_on_metadata`            | no                   | yes: a child process's stdin; output read from its stdout by another thread                                   |
| MP3 (LAME)    | no                              | no            | no                        | no              | no (ID3v2 from initial metadata only) | packed-audio         | no                                                                                                            |
| Shine         | no                              | no            | no                        | no              | no                                    | packed-audio         | no                                                                                                            |
| FDK-AAC       | no                              | no            | no                        | no              | no                                    | packed-audio if ADTS | no                                                                                                            |
| FLAC (native) | no                              | no            | no                        | no              | no                                    | no                   | no                                                                                                            |
| Ogg           | yes (current link's headers)    | no            | yes                       | no              | yes (new link)                        | no                   | no                                                                                                            |
| FFmpeg        | once captured                   | yes           | yes (chaining muxers)     | yes             | before header; after, if chaining     | if muxer segments    | yes when its format names a URL: `output.url`, and `output.file` (which rewrites the format to `file:<path>`) |

## 7. Where the bytes go: what the seam assumes

- The stream face returns bytes; it never receives a destination. Every
  encoder except two produces a single byte stream with no seek and no
  rewrite (WAV writes its header once, with an unknown or user-given length,
  and never comes back to it).
- The two exceptions reach the destination around the seam:
  - FFmpeg: the destination is a field of the format value (`output = Url
u`). Callers that own a file or URL rewrite the format before calling the
    factory; the instance then writes, seeks and closes that destination
    itself and its `encode` returns nothing. Append and truncate are passed
    the same way (container option `truncate`).
  - External: bytes go to a child process; what comes back is collected
    asynchronously and returned by later calls.
- The initial metadata is the only information about "what the output is
  writing" given to a factory; the output's name is the only identity.
- Nothing in the seam tells an encoder whether its consumer can join late
  (harbor, SRT) or starts at byte 0 (file): `header` and `restart_offset` are
  answered by every instance and ignored by callers that do not need them.

## 8. Section numbering of implementation files

Every encoder implementation file answers these sections, under these
numbers; "Nothing." when a section does not apply. The common part of each
is above; the implementation file states only its own.

| §   | Section                       | Common rule                                                                                          |
| --- | ----------------------------- | ---------------------------------------------------------------------------------------------------- |
| 1   | Format and options            | [formats.md](formats.md): syntax, typing, deferred values, helpers.                                  |
| 2   | Factory and instance creation | §2.3: inputs `(pos, name, initial metadata)`, deferred values forced, failures raised to the caller. |
| 3   | `encode`                      | §3.1: piece from 0 to position; returns what is ready; in-frame metadata ignored.                    |
| 4   | `encode_into`                 | §3.2: optional; consumer called once under the lock; view valid during the call.                     |
| 5   | `encode_metadata`             | §3.3: no return; effect encoder-specific; table §6.                                                  |
| 6   | `header`                      | §3.4: what a joiner needs before the next output.                                                    |
| 7   | `restart_offset`              | §3.5: offset in the latest returned bytes, or none.                                                  |
| 8   | `bitrate`                     | §3.6: bits per second or none.                                                                       |
| 9   | `stop`                        | §3.7: final bytes; instance not used afterwards.                                                     |
| 10  | Bytes produced                | Nothing common: each encoder's layout.                                                               |
| 11  | Concurrency and failures      | §3.8 and §4: exceptions propagate; one lock per instance.                                            |
| 12  | Segmenting (HLS) face         | §5: the HLS tree owns it.                                                                            |
| 13  | Where the bytes go            | §7.                                                                                                  |
| 14  | Tests                         | [tests.md](tests.md) for the cross-cutting ones.                                                     |

## 9. Callers of the seam

Which entry points each caller uses (`git grep "Encoder\."` in `src/core`).
How they use them is in the caller files.

| Caller                                               | Calls                                                                                                                                |
| ---------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------ |
| language: `%name(...)` evaluation                    | `get_factory` (existence check only), `string_of_format`                                                                             |
| language: `%ifencoder` / `%ifnencoder`               | `get_factory` (existence check, all exceptions read as "absent")                                                                     |
| language: evaluation type check                      | `type_of_format`                                                                                                                     |
| `encoder.content_type`, `encoder.extension`          | `mime`, `extension` (any exception read as `""`)                                                                                     |
| pipe outputs (`output.file`, `output.external`, ...) | `get_factory`; instance `encode`, `encode_metadata`, `stop`; for `%ffmpeg` in `output.file`, rewrites the format's destination first |
| `output.icecast` (via icecast utils)                 | `get_factory`; instance `encode`, `encode_metadata`, `stop`, `bitrate` (FFmpeg only)                                                 |
| `output.harbor` shared mode                          | instance `encode`, `encode_metadata`, `header`, `restart_offset`, `stop`                                                             |
| `output.harbor` dedicated mode                       | instance `encode_into` (else `encode`), `encode_metadata`, `header`, `stop`                                                          |
| `output.srt`                                         | `get_factory`; instance `encode`, `encode_metadata`, `header`, `restart_offset`, `stop`                                              |
| `output.url` (FFmpeg)                                | builds FFmpeg instances directly, `protect`; `encode`, `encode_metadata`, `stop`                                                     |
| HLS output                                           | `get_factory` (check), `extension`, `mime`, `bitrate`, `video_size`, `iso_base_file_media_file_format`, `get_hls_factory`            |
| HLS packed-audio plug-in                             | `get_factory`; instance `encode`, `stop`; `bitrate`, `iso_base_file_media_file_format`                                               |
| FFmpeg test producer (tests only)                    | builds instances directly, unprotected; `encode`, `stop`                                                                             |

## 10. The previous normative text, against the code

The text this file replaces stated MUST-level rules for every encoder. Where
the code does not meet one, it is recorded in
[findings/seam.md](findings/seam.md):

| Previous rule                                                            | As-built                                                                                                           |
| ------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------ |
| A plug-in answers a factory or nothing; first answer used                | Holds (one plug-in per kind).                                                                                      |
| A format no plug-in answers is a configuration error at evaluation       | Holds for the kind; not for Ogg's inner streams (fails at creation).                                               |
| A factory takes the output's name and the initial metadata               | Holds; plus a position.                                                                                            |
| An output never reuses an instance across runs                           | Callers' (see caller files).                                                                                       |
| Every instance serialises its own calls                                  | Holds for `get_factory` and `output.url`; not for direct builders in tests.                                        |
| An instance is stopped exactly once; its stop drains it                  | Not enforced by the seam; WAV/AVI hold nothing; double stop is implementation-defined.                             |
| Callers hand zero-length pieces as well as full ones                     | The generic encoded output never hands a zero-length piece.                                                        |
| Metadata of a piece is handed before the piece                           | Holds for the generic output; `output.icecast` with ICY sends data first but then does not call `encode_metadata`. |
| `encode_into` copies out during the call                                 | Holds; only FFmpeg offers it.                                                                                      |
| `encode_metadata` is effective in later output                           | Silently ignored by WAV, AVI, MP3, Shine, FDK-AAC, native FLAC, External without restart.                          |
| `header` is complete for every format, container identification included | Native FLAC and External answer empty while their stream starts with a header.                                     |
| `restart_offset` relative to the latest `encode` or `stop`               | Holds.                                                                                                             |
| No ID3v2 tag or Xing frame in the middle of a live stream                | Holds for MP3 (ID3v2 only at the start, Xing disabled).                                                            |
