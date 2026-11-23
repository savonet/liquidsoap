# Encoders: the contract

Part A. What every encoder offers the outputs that drive it, and what it
asks of them. This file owns the vocabulary of the encoder seam, the
registry, the lifetime of a container writer, the stream face, how the
segmenting face relates to it, the capability declarations and the checks
they allow at evaluation, the delegated destination, who calls a writer, the
faults a writer reports, in-band metadata, and the section numbering every
implementation file and every caller file answers.

It relies on: format values, their typing and when they are validated
([formats.md](formats.md)); the destinations writers write into
([sink.md](sink.md)); the component that serves readers starting mid-stream
([shared-stream.md](../../outputs/spec/shared-stream.md)); the output base
([output.md](../../outputs/spec/output.md)); draining
([draining.md](../../source/spec/draining.md)); the fault classes
([faults.md](../../../spec/faults.md)); concurrency
([concurrency.md](../../../spec/concurrency.md)); the segmenting rules of the
HLS output ([encoder-contract.md](../../outputs/hls/spec/encoder-contract.md)).

## Vocabulary

| Term                         | Meaning                                                                                                                                                                                                                                                                      |
| ---------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Format                       | The immutable value a script writes as `%name(...)` ([formats.md](formats.md)).                                                                                                                                                                                              |
| Configuration                | A format value with every deferred value resolved: what a writer is built from.                                                                                                                                                                                              |
| Container writer (writer)    | The encoder seam outputs use. Built from a configuration and a sink's capabilities, it takes pieces in and writes bytes and markers out into the sink. The only encoder interface an output sees.                                                                            |
| Sink                         | The destination the output provides to a writer, with declared capabilities ([sink.md](sink.md)). The output owns it.                                                                                                                                                        |
| Capabilities                 | What a sink declares about itself: seekable, fresh or continuing, live with a latency bound, segmented, delegated ([sink.md §1](sink.md#1-capabilities)).                                                                                                                    |
| Piece                        | The part of a frame between two metadata positions ([output.md §3](../../outputs/spec/output.md#3-a-streaming-cycle)).                                                                                                                                                       |
| Marker                       | A typed item a writer writes into its sink between bytes: a stream start, an access point, the end (§3.4).                                                                                                                                                                   |
| Stream                       | A byte sequence a decoder reads on its own from its first byte: one file, one connection's body, one Ogg link, one container between two restarts. A writer writes one or more streams, one after the other.                                                                 |
| Header (of a stream)         | The bytes a stream begins with that a decoder needs before any media of that stream: Ogg header pages, `fLaC` and its metadata blocks, an init segment, an EBML header and track list, a WAV header. Possibly empty.                                                         |
| Stream start                 | The position where a stream begins, with its header (§3.4).                                                                                                                                                                                                                  |
| Access point                 | A byte position where a reader can start decoding, given the entry header: a unit boundary after which every elementary stream starts with a key packet.                                                                                                                     |
| Entry header                 | What a reader starting at an access point is sent first: the header of the stream the access point belongs to. A reader starting at a stream start needs none.                                                                                                               |
| Streamable                   | A configuration a reader can start decoding at any access point, given the entry header, and whose writer marks its access points.                                                                                                                                           |
| Needs a seekable destination | A configuration that cannot be produced without seeking back: no valid result exists on a sink that cannot seek (a non-fragmented MP4 or MOV).                                                                                                                               |
| Can continue                 | A configuration whose writer, built for a continuing sink, extends an existing stream of the same configuration into a valid result read as one: by writing media only (WAV PCM, raw MPEG audio, ADTS, native FLAC frames) or by starting a new chained stream (Ogg) (§3.5). |
| File form, streaming form    | The two forms of a header (§3.5). The file form holds final values written once the stream is complete; the streaming form holds the established values meaning "unknown", and is valid on every sink.                                                                       |
| Preamble                     | Bytes describing the whole file that precede the first media of a stream and that no reader starting mid-stream needs: an ID3v2 tag, an encoder-information frame.                                                                                                           |
| Tags                         | The writer's initial metadata as a stream's header or a preamble carries it: an ID3v2 tag, the entries of a comment header (Vorbis comments of an Ogg stream or of native FLAC), a `LIST`/`INFO` chunk, a container's tags, cover art included (§3.5).                       |
| File sink                    | A sink confirmed `seekable` and `fresh`: a regular file a writer writes from its first byte.                                                                                                                                                                                 |
| In-band metadata             | Metadata a writer carries in its own bytes after its first stream has started (§9).                                                                                                                                                                                          |
| Delegated destination        | A destination only the writer can open: a protocol URL, or a muxer that writes its own files or devices (§6).                                                                                                                                                                |
| Run                          | From a start of the output to the next stop ([outputs README](../../outputs/spec/README.md#vocabulary)).                                                                                                                                                                     |

## 1. Registry

1. A plug-in registers under a unique name. Registering a second plug-in
   under an existing name fails at program start. Every plug-in is
   registered before any script is type checked.
2. A lookup takes a format and answers a factory, or none. At most one
   plug-in answers a format, so the order of registration never changes an
   answer.
3. A plug-in answers a format only when every component that format needs is
   in the build: the library of its codec, each stream codec of an Ogg
   format, the muxer it names. Availability is a property of the
   configuration's components, not of the plug-in's kind. `%ndi` is the
   one exception: it is available when the NDI sender is in the build,
   although no plug-in answers it (item 7). A format that no
   plug-in answers for lack of a component is a configuration error naming
   that component; when and how it is reported is
   [formats.md §8](formats.md#8-availability-and-ifencoder).
4. A factory answers, without building a writer, its declarations (§5.2)
   and the format helpers
   ([formats.md §9](formats.md#9-helpers)). No writer is built at
   evaluation. Reason: building a writer starts libraries and processes and
   reads settings the script may still set; everything an output needs at
   evaluation is a function of the format value.
5. A factory builds a writer from: the configuration, the capabilities its
   sink confirmed ([sink.md §1](sink.md#1-capabilities)), the sink, the
   output's name (for logs), the initial metadata (§2.2), whether the
   output will offer it metadata in band (§9), and the format's script
   position (for errors). It returns the writer or a fault (§8).
6. Every registered writer offers the stream face (§3). The segmenting
   face (§4) MAY be registered apart, in a registry of its own whose
   factories are built with the segment duration; a configuration is
   segmentable when that registry answers it.
7. `%ndi` is not a writer. No plug-in answers it: its frames go to a sender
   that takes media frames, not bytes, and only `output.ndi` accepts the
   format ([formats.md §2](formats.md#2-names-and-kinds)). It is available
   (it types, and `%ifencoder(%ndi)` takes its first branch) whenever the
   NDI sender is in the build
   ([formats.md §8](formats.md#8-availability-and-ifencoder)).

## 2. Instances

### 2.1 One writer per destination stream

A writer serves one destination stream, from its first byte to its end: one
file, one connection, one dedicated reader's stream, one run of a shared
stream, one run of a segmented output. It is never reused. A reopen, a
reconnect, a new reader of a dedicated stream or a new run builds a new
writer. Reason: a writer's first bytes are its stream's header, and a
writer that outlives its destination puts that header in the wrong place, or
leaves the next destination without one.

When an output splits its stream over several destinations (a file output's
reopen), the writer of the first is ended and the next destination gets a
new writer, whose first stream starts with its own header. Each file of a
split decodes alone and signals its own delay and padding (§3.7). A writer
MAY carry its codec state from one destination to the next where the codec
allows a stream to start from a continued state without an undecodable
first unit (gapless splits); the implementation file states it, and the
default is a new writer.

### 2.2 Creation

- The capabilities are confirmed by the sink before the writer is built
  ([sink.md §1](sink.md#1-capabilities)). The writer chooses its header form
  and its preambles from them (§3.5) once, at creation.
- Every deferred value of the format is resolved before creation
  ([formats.md §6](formats.md#6-deferred-values)).
- **Initial metadata** is the exported form of the output's current
  metadata at the writer's first piece, as
  [output-encoded.md §3](output-encoded.md#3-writer-lifetime) defines it: a
  metadata at that position applies first, and a track mark without
  metadata empties it. A metadata handed with the writer's first piece is
  its initial metadata: the output does not also offer it in band. Reason:
  every output then starts every stream with the same metadata. Which tags
  carry it is §3.5 item 7.
- Creation writes nothing into the sink. The first bytes come with the first
  piece.
- Every check the factory can make from the configuration and the build runs
  before anything is started (a library opened, a process spawned)
  ([principles](../../../spec/principles.md), 14). A creation that fails part
  way releases what it built.

### 2.3 States

| State     | Entered by                               | Calls the caller may make                                      |
| --------- | ---------------------------------------- | -------------------------------------------------------------- |
| `writing` | creation                                 | `write`, `metadata` when offered (§9), `end`, property reads   |
| `failed`  | a call that reports an output fault (§8) | `end(abandoned)`; an `end(clean)` is taken as `end(abandoned)` |
| `ended`   | `end`                                    | `end`, which does nothing; property reads                      |

- A call outside this table is a broken promise of the caller. The writer
  MUST NOT act on it: it fails loudly with an internal error naming the
  writer and the call, which is a defect to fix, never a fault to recover
  from.
- A writer ended without having been given a piece writes nothing: no
  header, no trailer, no marker.
- Every resource a writer holds (a codec, a library handle, a process, a
  delegated connection) is released exactly once, at `end` or at a failed
  creation ([concurrency.md §4](../../../spec/concurrency.md#4-resources)).
- Calls on one writer never overlap: they all come from the output's
  streaming work (§7).

## 3. The stream face

The interface of the container writer.

| Operation          | Given                  | Effect                                                                                                   |
| ------------------ | ---------------------- | -------------------------------------------------------------------------------------------------------- |
| `write(piece)`     | a piece                | encodes the piece's content; writes into the sink the bytes and markers that result (§3.3, §3.4)         |
| `metadata(m)`      | exported metadata      | only on a writer that offers in-band metadata (§9): starts a new stream carrying `m` from the next piece |
| `end(how)`         | `clean` or `abandoned` | ends the writer (§3.6)                                                                                   |
| `bitrate`          | —                      | nominal bits per second, or none (§3.8)                                                                  |
| `delay`, `padding` | —                      | the current stream's signalled delay and padding, in samples at the codec's rate, or none (§3.7)         |

### 3.1 Capabilities in

The writer is built with its sink's capabilities. It never asks the sink
what kind it is afterwards; it never opens, names, closes, reconnects or
retries anything; it never learns how many readers the sink serves. It uses
the sink only through the operations of [sink.md §2](sink.md#2-operations),
and seeks only a sink that declared `seekable`.

### 3.2 Order of a call's output

Everything a call produces is written into the sink before the call
returns, in stream order. What the writer still holds when a call returns is
held only because a stage needs more input to produce it (a codec frame, a
page being filled, an interleaving queue); §3.6 and §3.9 bound it. Reason:
an output that sends something out of band after a piece places it after
exactly the bytes written so far
([sink.md §5](sink.md#5-out-of-band-metadata)).

### 3.3 Frames in

- `write` takes a piece of any length of at least one sample or one image
  period. The writer reads only the content fields its format types
  ([formats.md §4](formats.md#4-typing)); metadata and track marks inside the
  frame are not its input.
- **No result depends on the piece boundaries**
  ([principles](../../../spec/principles.md), 11). A piece shorter than a
  codec frame is held and completed by the next; positions and lengths are
  counted in samples and images. The decoded result of content cut into
  pieces of any lengths and offsets equals that of the same content in one
  piece: bit-exact for PCM, sample-count exact otherwise.
- A stage between the piece and the codec (a sample-rate converter, a
  re-framer, an image scaler) is a stage of
  [draining.md §1](../../source/spec/draining.md#1-stages): it is drained at
  `end` and whenever it is rebuilt.
- A sample-rate conversion stage is aligned: its first output sample has
  the media time of its first input sample, and its drain releases its
  tail. It adds no delay, and no writer counts one for it (§3.7).
- Images whose size, stride or plane layout differ from the configuration's
  are encoded at the configured size: the writer adapts them (copies by
  rows, scales), and the header always describes the payload. An image size
  the configuration cannot encode at all is a configuration error at
  evaluation ([formats.md §7](formats.md#7-when-a-format-is-validated)). A
  deprecated writer MAY adapt less, refusing every size it cannot encode
  instead of adapting it; its implementation file states what it accepts
  (`%theora`).
- **Integer conversion.** A writer that hands its codec or its bytes
  integer samples uses the media library's existing float-to-integer
  conversion for that sample size, as it is; this contract adds no rounding
  rule of its own. Every writer converting to one sample size uses the same
  conversion. Reason: one conversion for every writer, so that two writers
  of the same PCM configuration write the same samples.
- Media time: the writer counts the run's media time from the pieces it
  receives, in main ticks; it equals the output's at every piece boundary.

### 3.4 Bytes and markers out

A writer writes into its sink an ordered sequence of bytes and markers. A
marker sits between two bytes: it refers to the byte that follows it.

| Marker                           | Meaning                                                                                                                                                                                                                               |
| -------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `stream_start(header_length, t)` | A stream starts at the next byte. Its header is the `header_length` bytes that follow (0 for a format without header), or `unknown` for a writer that cannot know it (item 8). `t` is the media time of the first media that follows. |
| `access_point(t)`                | The next byte is an access point of the current stream. `t` is the media time of the first media that follows.                                                                                                                        |
| `end`                            | The writer has written its last byte.                                                                                                                                                                                                 |

1. The first item a writer writes is a `stream_start`.
2. A stream start is also an access point, at which a reader needs no entry
   header: the bytes there begin with the header. For every later access
   point of the same stream, the entry header is the header that stream
   start delimits.
3. A stream start is written with the header's bytes, never before them. A
   header the writer learns only from its codec's first packet (a codec with
   a delay) therefore delimits its stream like any other, and no access
   point is ever marked before the header it needs.
4. A writer marks every access point it can identify at the boundaries of
   the units it writes: an Ogg page that begins a packet, an MPEG audio
   frame, an ADTS or LOAS frame, a WAV sample block, an MPEG-TS table
   repetition before key access units, a Matroska cluster that starts at a
   key frame, a movie fragment that starts at a key frame. No access point
   lies inside a header or inside a unit. A writer whose codec library
   hands it whole frames on each call (the MPEG audio writers) marks one
   access point at the start of each call's output that holds bytes, and
   does not parse frames to find more.
5. A streamable configuration marks access points in every stream it
   writes, at least one per unit that can begin one, within a bounded
   interval of media time: one per key-frame interval of its video, and for
   audio one per unit, the unit being bounded by the writer's own parameter
   (`max_page_duration` for Ogg, `fragment_duration` for fragmented and
   Matroska containers, `ts_table_interval` for MPEG-TS; each
   implementation file names its bound). A configuration that is not
   streamable MAY mark none.
6. Media times of successive stream starts and access points do not
   decrease within a stream.
7. `end` is the last item, after the final bytes of §3.6.
8. `header_length` is `unknown` only for a writer whose bytes come from a
   process it does not parse (`%external`). A sink that needs the header (a
   shared stream) takes the entry header of such a stream to be empty; only
   a configuration the script declared streamable (§5.3) reaches such a
   sink.

A sink serving one reader from byte 0 MAY ignore every marker but `end`; a
sink serving readers that start mid-stream uses them through the shared
stream ([shared-stream.md](../../outputs/spec/shared-stream.md)); a
segmented sink cuts at them (§4).

### 3.5 Header forms and preambles

1. On a sink declared `seekable` and `fresh`, the writer writes the **file
   form**: a header that describes the whole stream (WAV and AVI sizes, FLAC
   total sample count and MD5 signature, an MP3 encoder-information frame
   with its frame count, table of contents, delay and padding, an MP4 movie
   box, Matroska sizes and cues), with its final values written once the
   stream is complete. A configuration that has a streaming form writes it
   first, and at a clean `end` seeks back and rewrites it with the final
   values, so that a file whose writer was abandoned, or whose process died,
   keeps a valid header.
2. On a fresh sink that is not seekable (a FIFO, a device, a process, a
   live or a segmented sink), the writer writes the **streaming form**: the
   established values a decoder reads as "unknown" or "to the end" (the
   sizes the format defines for an unknown length, a FLAC total of 0 and an
   MD5 of zeroes, Matroska unknown sizes, fragmented MP4). The streaming
   form is also the stream's header, hence the entry header of its access
   points. A configuration that has no streaming form needs a seekable
   destination (§5).
3. On a **continuing** sink, the writer extends the stream already there
   instead of starting a second one, wherever its format allows:

   | Format                                             | What the continuing writer writes       | Existing header updated at a clean end, where the sink is seekable                                                                                                 |
   | -------------------------------------------------- | --------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
   | WAV (PCM)                                          | samples only, no header                 | RIFF and data sizes                                                                                                                                                |
   | MP3 (LAME, Shine)                                  | MPEG frames only, no preamble           | nothing, declined: the existing encoder-information frame is left as it is, and describes the first stream only ([mp3.md §6](mp3.md#6-header-forms-and-preambles)) |
   | native FLAC                                        | cannot continue: refused (§5.3)         | —                                                                                                                                                                  |
   | Ogg                                                | a new chained link, with its own header | nothing: each link is complete on its own                                                                                                                          |
   | formats with an empty header (ADTS, LOAS, MPEG-TS) | media only                              | nothing                                                                                                                                                            |
   | the FFmpeg writer (its muxers that can continue)   | media only, or a new chained Ogg stream | nothing, declined: the muxer writes the header of the stream it starts and no other ([ffmpeg.md §6](ffmpeg.md#6-header-forms-and-preambles))                       |
   | `%external`                                        | the process's output, unchanged         | nothing: the writer does not know the format                                                                                                                       |

   The writer takes the existing stream to be of its own configuration.
   Where the sink is seekable, the writer SHOULD, at its clean end, read the
   existing header and rewrite its counts to cover the whole file. A writer
   MAY decline this SHOULD only with a reason its implementation file
   states, and then leaves the existing header as it was. It leaves a
   header that is not of its configuration unchanged, logged at `severe`
   with the file's name. Where the sink cannot be read back, the existing
   header keeps what it had: placeholders stay valid, and final values
   describe the first stream only. A configuration that cannot extend an
   existing stream validly (a non-fragmented MP4, a container that does not
   chain, native FLAC, whose frames are numbered from the stream's start)
   cannot continue, and is refused (§5.3); native FLAC's refusal names
   `%ogg(%flac)`, which can. Each implementation
   file's §6 states its row.

4. A FIFO, a device or a pipe is a stream: its sink is not seekable, and an
   append to one is fresh ([sink.md §1](sink.md#1-capabilities)).
5. Each writer writes its **preambles by default only on a file sink**. On
   a sink that is `live`, `continuing` or `segmented` they are off, and a
   preamble the script set explicitly there is an error (§5.3). Reason: a
   live destination is read from the middle of the stream, by a shared
   stream or by a server relaying to its listeners, and a server that sees
   a source reconnect forwards the new stream's first bytes to listeners in
   the middle of the previous one; on a continuing sink the preamble would
   land in the middle of the file. On another fresh sink (a FIFO, a device,
   a process) they are off by default, and an explicit one is written. A
   preamble is never part of an entry header.
6. The output never edits the bytes a writer writes and never rewrites a
   format to steer a writer: the writer adapts the defaults of its
   configuration to its capabilities by itself, so that the script does not
   have to ([formats.md §3](formats.md#3-the-parameter-specification)). A
   parameter the script set explicitly is never adapted.
7. **Tags.** Every writer whose stream can carry tags follows one rule,
   from the confirmed capabilities:

   | Sink                                                                         | Tags from the initial metadata                         |
   | ---------------------------------------------------------------------------- | ------------------------------------------------------ |
   | a file sink                                                                  | written by default                                     |
   | another fresh sink, neither live nor segmented (a FIFO, a device, a process) | not written by default; an explicit request is written |
   | `live`, `continuing` or `segmented`                                          | not written; an explicit request is refused (§5.3)     |

   Cover art follows the tags: it reaches a writer only when the output
   exports it ([output-encoded.md §4.2](output-encoded.md#42-the-exported-form)),
   which `output.file` does by default. Reason: a file is kept and read from
   its first byte, and its tags are what players and libraries show; a
   stream is read from its middle, and a continued file already has its
   tags. Ogg's comment headers are the exception: they are also its in-band
   channel (§9), so when the output offers metadata in band, every link's
   comment headers carry that link's metadata, the first link's initial
   metadata included, on every sink.

8. **Encoder identification.** Every comment header a writer builds (each
   Ogg stream's, native FLAC's) carries one `ENCODER=` entry naming the
   software and its version, on every sink, whether or not it carries tags;
   it replaces an `encoder` key of the metadata. A `LIST`/`INFO` chunk
   carries the same in `ISFT`. The vendor string stays the codec
   library's.

### 3.6 Draining and final bytes

`end(clean)`:

1. drains every stage the writer holds, upstream first, under
   [draining.md §2–§4](../../source/spec/draining.md#2-the-rule): the queue of
   samples short of a codec frame, the rate converter, the codec's
   look-ahead, the interleaving queue; a final partial unit is delivered
   short where the codec accepts it, else padded and the padding signalled
   (§3.7);
2. ends every stream in progress (the end-of-stream page of each Ogg logical
   stream, the container's trailer);
3. on a sink declared `seekable`, writes the file form's final values, or
   on a continuing sink updates the existing header's counts (§3.5);
4. writes `end`.

The final bytes are ordinary bytes: they reach the sink in order, and
delivering them is the sink's ([sink.md §4](sink.md#4-markers-and-the-end)).

`end(abandoned)` releases every stage without draining and writes nothing
more, `end` marker included. It is used when the sink failed or the writer
is `failed` (§8): a stage in error is released as it is
([draining.md §5](../../source/spec/draining.md#5-discarded-on-purpose)).

A drain, trailer or finalisation step that fails at `end(clean)` is logged
at `severe` with the writer's name and the cause, and the end goes on with
the next step; it is not escalated
([draining.md §5](../../source/spec/draining.md#5-discarded-on-purpose)).

A clean end on a seekable sink MAY seek, and read back what it wrote (a
muxer that moves its index to the start of the file). That work can take
long: the output runs a clean end as work allowed to block, and the
streaming thread never waits for it (§7).

### 3.7 Delay and padding

Every lossy codec delays its output and pads its last unit. A writer
signals both, for each stream, in every container that can carry them, with
the values its codec reports. A codec library that offers no query for its
delay is given a declared constant, stated with its reason in the
implementation file and checked by its tests against the decoded output
(Shine). No other value is a configured constant.

| Container                        | Where                                                                          |
| -------------------------------- | ------------------------------------------------------------------------------ |
| Ogg Opus                         | pre-skip from the codec's look-ahead; the last page's granule position         |
| Ogg Speex                        | granule positions net of the look-ahead; the last page's granule position      |
| Ogg Vorbis, FLAC                 | the last page's granule position                                               |
| MP3, file form                   | the encoder-information frame's delay and padding fields                       |
| MP4 and its family               | the edit list, or the equivalent the muxer writes                              |
| Matroska, WebM                   | the track's codec delay and the last block's discard padding                   |
| FLAC                             | the exact total sample count (file form)                                       |
| raw MPEG audio, ADTS, LOAS, ADIF | none: they have no field; the `delay` and `padding` properties give the values |

- The `delay` and `padding` properties give the values signalled for the
  current stream; `padding` is known from the stream's end.
- At every stream start (a reopened file, a new link, a restart) the values
  are those of the new stream.
- A decoder that honours the signalled values decodes exactly the input
  sample count of each stream, with a click at input sample `n` at decoded
  sample `n`, to within one sample.

### 3.8 Bitrate

`bitrate` is the nominal bit rate in bits per second. When the configuration
fixes it (a constant or average bit rate, a codec option), the writer
answers it from creation, before any piece, and the answer equals the format
helper's ([formats.md §9](formats.md#9-helpers)). Otherwise it is none until
the writer knows it, and stays known once known. Reason: an output announces
stream properties when it connects, before the first piece.

- A writer MAY know its bit rate at creation where the helper answers none,
  because the rate depends on a deferred value (a PCM rate read from the
  frame's settings): it answers it from creation.
- A library that bounds the configured bit rate to what the configuration
  can code sets the stream's real rate: the writer logs both values once at
  `important`, and `bitrate` answers the library's value. The helper,
  which reads the format alone, keeps the configured one.

### 3.9 Live latency bound

On a sink declared `live(L)` ([sink.md §1](sink.md#1-capabilities)), the
writer holds encoded data for at most `L` of stream time: a page being
filled, an interleaving queue waiting for a sparse stream, a muxer's buffer
is written out when its oldest content reaches `L`, complete or not. The
codec's own look-ahead is not counted. A stream that produces little (a
lossless codec on digital silence, a still image) still writes at least once
per `L`. On a sink that is not live, no bound applies.

## 4. The segmenting face

The segmenting face is offered by a factory of its own, registered apart
from the stream face's (§1 item 6) and built with the segment duration: a
segmented output looks the format up there, and "segmentable" means that
lookup answers. A `segmented(D)` destination is the input of that factory,
and a declared capability for the check of §5.3. How the writer carries
ID3 (its mode) is a declaration of the format, read at evaluation, never a
call on a writer. The HLS output owns its rules: the calls, their order,
split requests, split times, init segments, ID3 and the per-variant
properties ([encoder-contract.md](../../outputs/hls/spec/encoder-contract.md)).
Its pieces map onto this file's markers (the HLS contract's own names in
[encoder-contract.md §1](../../outputs/hls/spec/encoder-contract.md#1-role)):

| Segmenting face        | Stream face                                                    |
| ---------------------- | -------------------------------------------------------------- |
| `Init (init, pieces)`  | the first `stream_start`; `init` is its header                 |
| `Bytes b`              | bytes                                                          |
| `Split {time}`         | `access_point(time)`                                           |
| `Restart {time, init}` | `stream_start(header_length, time)`; `init` is its header      |
| the end of the run     | `end(clean)`: the final bytes and the access points among them |

On a segmented sink, the writer:

- makes every access point a position where a segment can start (a fragment
  boundary in a fragmented container, a new PAT and PMT in MPEG-TS);
- places access points at the sink's split requests as far as its streams
  allow (within one codec frame for all-key audio, at key frames only for
  video), per the HLS contract;
- hands each stream's header as the init segment, never repeated in the
  segments that follow.

A writer whose configuration is not segmentable does not offer this face
(§5.2). HLS timed ID3 is an operation of the HLS contract; it is not the
in-band metadata of §9.

## 5. Capabilities and the checks at evaluation

### 5.1 What an output declares

At construction, from its parameters alone, an output declares:

- the capabilities its sinks will have ([sink.md §1](sink.md#1-capabilities));
- whether it serves readers that start mid-stream
  ([shared-stream.md](../../outputs/spec/shared-stream.md)).

Each caller file's §1 states its declarations. The expected ones
(informative; the caller files own them):

| Output                             | Capabilities                                                                   | Readers mid-stream |
| ---------------------------------- | ------------------------------------------------------------------------------ | ------------------ |
| `output.file`, `append=false`      | seekable, fresh; `delegated(path)` for a configuration that needs no byte sink | no                 |
| `output.file`, `append=true`       | seekable, continuing; refused for a configuration that needs no byte sink      | no                 |
| `output.external`, `pipe`          | fresh                                                                          | no                 |
| `output.icecast`                   | fresh, live                                                                    | no                 |
| `output.harbor`, shared            | fresh, live                                                                    | yes                |
| `output.harbor`, dedicated         | fresh, live                                                                    | no                 |
| `output.srt`, listener             | fresh, live                                                                    | yes                |
| `output.srt`, caller               | fresh, live                                                                    | no                 |
| `output.url`                       | delegated                                                                      | no                 |
| `output.file.hls` and its wrappers | segmented                                                                      | no                 |

### 5.2 What a configuration declares

Each configuration declares the following, without building a writer, as
functions of the format value and, where the writer adapts to its
destination (fragmenting an MP4 for a segmented sink), of the declared
capabilities:

| Declaration        | Meaning                                                                                                                 |
| ------------------ | ----------------------------------------------------------------------------------------------------------------------- |
| streamable         | as defined above                                                                                                        |
| needs seekable     | needs a seekable destination                                                                                            |
| can continue       | as defined above                                                                                                        |
| in-band metadata   | offers `metadata` (§9); native `%ogg`, and `%external` with `restart_on_metadata`                                       |
| segmentable        | the segmenting registry answers the format (§4)                                                                         |
| needs no byte sink | produces no bytes for a sink (a muxer that writes its own files or devices): only a delegated destination takes it (§6) |
| accepts delegated  | can open a delegated destination itself (§6)                                                                            |

Each implementation file's §2 states its declarations per configuration.

### 5.3 The check

When an output is constructed, during script evaluation, it checks the
format against its declarations. Each refusal is an invalid-value error on
the format argument that names what the configuration needs and what the
output offers:

| Output declares                     | Configuration             | Result                                                                                                                                                 |
| ----------------------------------- | ------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------ |
| readers mid-stream                  | not streamable            | refused; the message names the output's alternative where it has one (a dedicated encoder)                                                             |
| any capabilities without `seekable` | needs seekable            | refused                                                                                                                                                |
| continuing                          | cannot continue           | refused; the message names the format, says it cannot be appended to, and names the format that can where there is one (`%ogg(%flac)` for native FLAC) |
| no delegated destination            | needs no byte sink        | refused                                                                                                                                                |
| delegated                           | does not accept delegated | refused                                                                                                                                                |
| segmented                           | not segmentable           | refused, as the HLS tree states                                                                                                                        |

**Streamable on the script's word.** A configuration whose writer cannot
vouch for its streamability (an FFmpeg muxer it does not know, `%external`)
is not streamable by default. An explicit `streamable=true` on `%ffmpeg` or
`%external` declares it streamable: the claim is the script's, which
answers for readers starting at the writer's access points, and it is never
inferred. On a configuration that needs a seekable destination it is
refused. Each of the two implementation files states the access points it
then marks.

**Defaults adapt, explicit settings are checked.** The writer adapts every
default of its configuration to the declared capabilities by itself
(§3.5): it drops its preambles on a live or continuing sink, writes the
streaming form where it cannot seek, extends an existing stream where it
continues one. The script never has to change a format to suit an output.
A parameter the script set explicitly
([formats.md §3](formats.md#3-the-parameter-specification)) is never
adapted: when it contradicts the declared capabilities, it is refused the
same way as the rows above. Examples: a preamble set explicitly
(`%mp3(id3v2=…)`, `%ffmpeg` mp3 with `id3v2_version` above 0 or
`write_xing=1`) for a live, continuing or segmented output; a protocol or truncation
option where the output owns the file. The same parameter left at its
default is adapted, never refused. Each implementation file's §2 lists its
parameters of this kind.

Degraded but valid results are not errors: the streaming form on a sink
that cannot seek, a format with an empty header on a live output, an
appended file whose first header keeps placeholders. The output logs the
form it uses once, at `info`, when it starts. Errors are kept for
configurations whose result would be empty or invalid, and for explicit
settings the destination contradicts.

An explicit option that holds media longer than a live sink's bound `L`
(§3.9) is refused on that sink the same way: each implementation file's §2
lists its options of this kind.

### 5.4 At open

A sink confirms its capabilities when it opens, before the writer is built
([sink.md §1](sink.md#1-capabilities)). A path declared seekable that turns
out to be a FIFO or a device loses `seekable`: the writer uses the streaming
form, logged once at `info`. A continuing sink whose file is absent or
empty is confirmed `fresh`: there is no stream to extend, and the writer
writes a whole stream with its header. An append to a FIFO or a device is
confirmed `fresh` and not seekable. A configuration that needs a seekable
destination cannot be honoured there: that is a configuration error known
not to recover, a hard error when it shows
([faults.md §2](../../../spec/faults.md#2-classes)), naming the output, the
path and the configuration's need.

## 6. The delegated destination

1. A delegated destination is a protocol URL (`output.url`), or the path or
   name a muxer that needs no byte sink takes as its argument. Only a writer
   whose configuration accepts delegated takes one; the FFmpeg writer is the
   one implementation ([FFmpeg encoder.md](../../optionals/ffmpeg/spec/encoder.md)).
2. The writer is built with `delegated(u)` and an interrupt the output
   provides. It opens `u`, writes and closes it itself, under the output's
   lifecycle: the output decides when to start, stop and retry, and its
   interrupt aborts any blocking open or write. The writer writes no bytes
   and no markers into any sink.
3. A delegated writer runs on a task of its own, as work allowed to block.
   The output's streaming work hands it each piece and metadata through a
   bounded queue and never waits for it. Every call of that writer runs on
   that task, in the order handed.

   | Parameter               | Recommended                                | Protects                                                      |
   | ----------------------- | ------------------------------------------ | ------------------------------------------------------------- |
   | `delegated_queue_bound` | the output's write timeout, in stream time | Memory, and the age of what reaches a destination that stalls |

   While the destination is being opened, no byte of its stream exists yet:
   past the bound, the output drops the oldest pieces it holds, with a
   warning that states the stream time dropped. Once the destination is
   open, past the bound it cannot keep up: that is an output fault (§8), and
   the output's interrupt aborts the writer's pending call.

4. An output that delegates a destination keeps what it owns of it: naming,
   the directories it creates, its callbacks once the writer ended, its
   restart policy. A delegated writer never restarts its container in
   place: where its container would restart, the writer fails with an
   output fault and the output builds a new writer under its policy.
5. A delegated destination offers no in-band metadata (§9).

## 7. Concurrency

1. **One caller.** The output's streaming work calls a writer, and nothing
   else does. That work is the output's cycle and its start and stop as
   [output.md §2](../../outputs/spec/output.md#2-states) runs them, and, for a
   delegated writer, its own task (§6). Two calls on one writer never
   overlap, so a writer needs no lock of its own for its calls.
2. **Requests from other threads.** A script, a server command or a reader's
   connection never calls a writer: it posts a request the output's
   streaming work carries out between pieces
   ([concurrency.md §2](../../../spec/concurrency.md#2-the-streaming-thread)).
   A stop or a reopen asked during a frame takes effect before the next
   piece, never inside one. What a reader starting mid-stream needs comes
   from the markers the shared stream recorded, never from a call on the
   writer.
3. **No waiting.** A writer's calls never wait for a peer, a disk or a
   process: the sink's operations return at once
   ([sink.md §3](sink.md#3-never-blocking-the-streaming-thread)). A writer
   that does I/O of its own (a child process's pipes, a delegated
   destination) does it as work allowed to block, behind a bounded queue, as
   §6 does. A clean end that seeks or reads back runs as work allowed to
   block (§3.6); once `end` is handed to that work, the streaming work calls
   the writer no more.
4. **A writer's own work.** Work a writer runs besides its calls (reading a
   process's output) owns the state it shares with the calls under
   [concurrency.md §3](../../../spec/concurrency.md#3-shared-state). It never
   writes into the sink: what it produced is written by the writer's next
   call, or by `end`, in stream order.
5. **Writers in parallel.** Writers of different outputs MAY run on several
   domains at once. A writer's bytes never depend on another writer. State a
   codec library shares between writers is safe to reach from several
   domains, and identifiers that must be unique within a stream are unique:
   Ogg serial numbers are distinct across every link a writer writes.

## 8. Faults

A writer reports every fault as a value that carries its class
([faults.md §2](../../../spec/faults.md#2-classes)), the cause, the writer's
name, and for a configuration error the parameter and the format's
position. No untyped failure leaves a writer; a failure the writer cannot
classify is an output fault. The value MAY be raised as an exception that
carries it: every writer's calls then go through one enforcement point,
which classes any other failure as an output fault.

| Fault                                                                                                                               | Class               | Outcome                                                                                                                                                                                                               |
| ----------------------------------------------------------------------------------------------------------------------------------- | ------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| a format refused at type checking or evaluation ([formats.md §7](formats.md#7-when-a-format-is-validated), §5.3)                    | configuration error | the script fails before it starts                                                                                                                                                                                     |
| a library or process refuses the configuration at creation, deterministically from the configuration and the build                  | configuration error | hard error when it shows ([faults.md §2](../../../spec/faults.md#2-classes)), naming the output, the parameter and the format's position. Never silent: a writer that cannot encode never yields an empty destination |
| a sink confirms capabilities the configuration cannot use (§5.4)                                                                    | configuration error | hard error                                                                                                                                                                                                            |
| creation fails for a cause that can clear (a delegated destination unreachable, a process that cannot start for lack of a resource) | output fault        | the output's restart policy                                                                                                                                                                                           |
| one input item the codec or the muxer refuses                                                                                       | item fault          | the item is dropped with a warning and counted ([faults.md §4](../../../spec/faults.md#4-item-faults-escalate))                                                                                                       |
| `max_consecutive_faults` item faults in a row on one writer                                                                         | output fault        | the writer is in an error state no data repairs; the output restarts under its policy with a new writer                                                                                                               |
| the sink fails ([sink.md §7](sink.md#7-faults))                                                                                     | output fault        | the writer is ended abandoned; the output's restart policy                                                                                                                                                            |
| a writer's own process or library fails while streaming                                                                             | output fault        | the writer becomes `failed`                                                                                                                                                                                           |
| a drain, trailer or finalisation step fails at a clean end                                                                          | not escalated       | logged at `severe`; the end goes on (§3.6)                                                                                                                                                                            |
| a call outside §2.3                                                                                                                 | internal error      | fails loudly; a defect                                                                                                                                                                                                |

- A call that reports an output fault leaves the writer `failed` (§2.3).
- Every output fault of a writer or of its sink reaches the output's one
  restart policy exactly once, whatever layer raised it. The outcome (retry
  after a delay, stop, fail) is the policy's, and it is observable through
  the output's state and callbacks. Each caller file's §8 states its policy.
- A data fault never stops an output.
- Copied FFmpeg streams have one more case, unrecoverable, owned by the
  FFmpeg tree
  ([FFmpeg encoder.md §13.3](../../optionals/ffmpeg/spec/encoder.md#133-incompatible-copy-source)).

## 9. In-band metadata

1. A writer offers `metadata` only when its configuration declares in-band
   metadata (§5.2). The native Ogg writer does: a new link per offer, whose
   header carries the metadata. `%external` with `restart_on_metadata`
   does in a narrower sense: each offer starts a new stream (a new
   process), and the metadata's values never reach the process
   ([external.md §7](external.md#7-metadata)). No other writer offers it,
   the FFmpeg writer included, whatever its muxer.
2. `metadata(m)` is called between pieces. It writes nothing and drains
   nothing: the writer records `m`, and at the next piece ends the current
   stream (for Ogg: drains and ends every logical stream of the current
   link) and writes a stream start whose header carries `m`. Each new
   stream decodes alone, carries every track of the format, and ends
   validly whatever it received, nothing included; the implementation file
   states how.
3. An offer whose exported values are empty, or equal to those the current
   stream carries, starts no stream. Reason: a chained stream is valid but
   poorly supported by players, and a link that carries nothing new only
   costs them a restart.
4. A writer whose stream header can carry metadata (Vorbis comments, FLAC
   metadata blocks, an ID3v2 tag on a file, container tags) writes its
   initial metadata there under the tag rule of §3.5 item 7. A tag with no
   entry is not written where the format allows omitting it.
5. Out-of-band metadata (ICY blocks, server updates, HLS timed ID3) is the
   output's and its sink's. A writer is never told about it.
6. Each user of this interface decides whether it offers metadata in band to
   a writer that accepts it, and MAY let the script choose with a parameter
   (caller files §4). By default, an output that carries in-band metadata
   where the writer offers it keeps doing so.

## 10. Implementation files

Every writer has an implementation file that answers each section below,
under the same number. A section that does not apply is answered
"Nothing.". The common rule is the clause linked; the file states only its
own.

| §   | Section                    | Clause                                                                                                                                                                                                                    |
| --- | -------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1   | Format and options         | The parameter specification ([formats.md §3](formats.md#3-the-parameter-specification)): options, defaults, ranges, errors, and what is checked at each phase ([formats.md §7](formats.md#7-when-a-format-is-validated)). |
| 2   | Declarations               | §5.2 per configuration; the defaults it adapts to capabilities, and the explicit parameters refused against them (§5.3).                                                                                                  |
| 3   | Creation                   | §2.2: inputs used, library and process checks, the form chosen per capabilities, creation faults.                                                                                                                         |
| 4   | Input                      | §3.3: fields read, units, remainders, rate conversion, image adaptation.                                                                                                                                                  |
| 5   | Bytes and markers          | §3.4: the byte layout; each stream start and its header; the access points marked.                                                                                                                                        |
| 6   | Header forms and preambles | §3.5: file form, streaming form, the continuing row and the header update at end, preambles, finalisation.                                                                                                                |
| 7   | Metadata                   | §2.2, §3.5 items 7–8 and §9: initial metadata and the tags it becomes per sink; encoder identification; the in-band offer, or "Nothing.".                                                                                 |
| 8   | Delay and padding          | §3.7: the values, where they are signalled, at every stream start.                                                                                                                                                        |
| 9   | End                        | §3.6: the stages held and their drain order, final bytes, abandoned end.                                                                                                                                                  |
| 10  | Properties                 | §3.7 and §3.8: `bitrate`, `delay`, `padding`, and from when each is known.                                                                                                                                                |
| 11  | Live latency               | §3.9: what holds bytes, and how the bound is met.                                                                                                                                                                         |
| 12  | Segmenting face            | §4: the HLS implementation file that answers it, or "Nothing.".                                                                                                                                                           |
| 13  | Delegated destination      | §6, or "Nothing.".                                                                                                                                                                                                        |
| 14  | Concurrency                | §7: work of its own, state shared with other writers.                                                                                                                                                                     |
| 15  | Faults                     | §8: every fault the writer reports, and its class.                                                                                                                                                                        |
| 16  | Tests                      | The conformance checks of the writer, drawn from its sections.                                                                                                                                                            |

Implementation files: [WAV](wav.md), [AVI](avi.md), [external](external.md),
[LAME MP3](mp3.md), [Shine](shine.md), [FDK-AAC](fdkaac.md),
[native FLAC](flac.md), [Ogg](ogg.md) with its own stream-codec contract
([ogg-streams.md](ogg-streams.md)), and [FFmpeg](ffmpeg.md) (its codecs and
container in the [FFmpeg tree](../../optionals/ffmpeg/spec/encoder.md)).
[ndi.md](ndi.md) owns `%ndi` and `output.ndi`; it answers §1, and "Nothing."
elsewhere, since `%ndi` is not a writer (§1).

## 11. Caller files

Every output that drives a writer has a caller file that answers each
section below, under the same number; "Nothing." where it does not apply. A
caller with a tree of its own (harbor, SRT, HLS, the FFmpeg I/O) answers in
its caller file by linking the sections of its tree.

| §   | Section                   | Clause                                                                                                                                                                             |
| --- | ------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1   | Formats and declarations  | The factory lookup and its error; the capabilities and the readers mid-stream it declares (§5.1); the checks of §5.3 and their messages.                                           |
| 2   | The sink                  | What the sink is, what it owns, its bounds, its confirmation at open ([sink.md](sink.md)).                                                                                         |
| 3   | Writer lifetime           | When a writer is built and ended, one per destination stream (§2.1); its initial metadata (§2.2); a destination that got no piece.                                                 |
| 4   | Pieces and metadata       | The pieces it hands; in-band or out-of-band metadata, any script parameter choosing it and its default (§9); out-of-band placement ([sink.md §5](sink.md#5-out-of-band-metadata)). |
| 5   | Markers and readers       | What its sink does with stream starts, access points and `end`; its use of the shared stream or of the segmenting face.                                                            |
| 6   | End                       | Clean and abandoned ends; delivery of the final bytes; finalisation and the callbacks that follow it.                                                                              |
| 7   | Reopen, reconnect, append | New destination streams and new writers; delays; continuity across splits (§2.1).                                                                                                  |
| 8   | Faults and restart policy | Every writer and sink fault, its class, and the one restart policy they reach (§8).                                                                                                |
| 9   | Concurrency               | Its streaming work, the requests other threads post, the work allowed to block (§7).                                                                                               |
| 10  | Tests                     | The conformance checks of the caller.                                                                                                                                              |

Caller files: [the encoded-output base](output-encoded.md),
[output.file](output-file.md), [output.external and pipe](output-pipe.md),
[output.icecast](output-icecast.md), [output.harbor](output-harbor.md),
[output.srt](output-srt.md), [output.url](output-url.md),
[the HLS output](output-hls.md).
