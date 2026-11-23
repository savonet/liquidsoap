# The `%ffmpeg` writer at the seam

Part A. How the `%ffmpeg` encoder is a container writer
([encoder.md](encoder.md)): how its muxer's header, packets and trailer map
onto the stream face, the I/O it gives the muxer over a sink or a delegated
destination, its declarations per configuration, the defaults it adapts to
its destination and the explicit options refused against it, its markers
and entry header, and its faults at the seam. Its sections answer
[encoder.md §10](encoder.md#10-implementation-files) under the same numbers.

It relies on the FFmpeg tree, which owns, and this file does not restate:

- the `%ffmpeg(...)` language, typing, rendering, extension and MIME:
  [encoder-format.md](../../optionals/ffmpeg/spec/encoder-format.md);
- the building of the container and its streams, option consumption, frame
  encoding, the stream-encoder contract, draining, copy streams and
  container restarts at a copy source switch:
  [FFmpeg encoder.md](../../optionals/ffmpeg/spec/encoder.md) (**FE** below),
  [encoder-internal.md](../../optionals/ffmpeg/spec/encoder-internal.md),
  [encoder-copy.md](../../optionals/ffmpeg/spec/encoder-copy.md);
- every timestamp: [timestamps.md](../../optionals/ffmpeg/spec/timestamps.md);
- `output.url`'s states, interrupt and error classification:
  [io.md](../../optionals/ffmpeg/spec/io.md); the MIME table, used by the
  Icecast and harbor outputs too, is
  [encoder-format.md §12](../../optionals/ffmpeg/spec/encoder-format.md);
- the container operations of the bindings:
  [avformat.md](../../../modules/synced/ffmpeg/spec/avformat.md);
- the segmenting face: [HLS encoder-ffmpeg.md](../../outputs/hls/spec/encoder-ffmpeg.md).

It also relies on [sink.md](sink.md), [formats.md](formats.md),
[shared-stream.md](../../outputs/spec/shared-stream.md),
[draining.md](../../source/spec/draining.md) and
[faults.md](../../../spec/faults.md).

**Muxer** below is the FFmpeg output format a configuration names with
`format=`, by its FFmpeg short name. The **ISO family** is `mp4`, `mov`,
`ipod` and `ismv`. A muxer **needs no file** when FFmpeg flags it as opening
no I/O of its own: devices, and muxers that write their own files, such as
`hls`, `dash`, `segment`, `tee` and `image2`.

## 1. Format and options

1. The parameter specification is
   [encoder-format.md](../../optionals/ffmpeg/spec/encoder-format.md). A
   format value holds no destination: the destination is the sink the output
   provides, or the delegated destination it names
   ([encoder.md §6](encoder.md#6-the-delegated-destination)).
2. A container option is **explicit** exactly when the script wrote it. An
   option the script did not write is a default, whatever value the muxer
   gives it ([formats.md §3](formats.md#3-the-parameter-specification)). The
   defaults this writer adapts, and the explicit options it refuses, are
   listed in §2.
3. The container options are consumed, in order, by FFmpeg's generic
   container options and the muxer's private options; on a delegated
   destination only, then by the protocol that opens it. `streamable` is
   not a container option: it is the writer's (§2), and no FFmpeg layer
   sees it
   ([avformat.md §9](../../../modules/synced/ffmpeg/spec/avformat.md#9-options)).
   On a sink, a protocol option (`truncate`, `blocksize`, `seekable`,
   `follow`, `pkt_size`, and every other option of FFmpeg's protocols) has
   no consumer: it is refused, and the error names it as an option of a
   destination the output owns. Reason: on a sink the output opens, names,
   truncates and appends; an option that claims to do so in the format would
   be silently ignored.
4. Every leftover, container and stream alike, is reported in one error
   ([FE §4](../../optionals/ffmpeg/spec/encoder.md#4-option-handling)).
5. A `format` that names no muxer of the build is refused, on every
   destination: no other muxer is ever substituted for it. Without
   `format`, the muxer is the one FFmpeg guesses from the destination's
   name, when the output is constructed:
   - `output.file` with a constant path: guessed from its extension; an
     extension no muxer claims is refused, naming it. With a path that is
     not constant (a getter, an interpolated name), the name is known only
     at open, and the format is refused, asking for `format=`
     ([output-file.md §1.4](output-file.md#14-checks-at-evaluation));
   - a delegated destination: guessed from the URL; none guessed is
     refused;
   - every other sink: refused, there is no name to guess from.

   The guessed muxer is fixed at construction, and every check of §2 runs
   with it.

6. Phases ([formats.md §7](formats.md#7-when-a-format-is-validated)): the
   muxer lookup and the option names are checked at evaluation at the latest,
   without opening anything (FE §4 states which run at type checking); the
   checks against the destination (items 3 and 5, §2) run when the output is
   constructed. None waits for the first piece.

## 2. Declarations

Per configuration, as functions of the format value and of the declared
capabilities ([encoder.md §5.2](encoder.md#52-what-a-configuration-declares)):

| Muxer                                                                                                        | Streamable                                             | Needs seekable | Can continue                        | Segmentable |
| ------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------ | -------------- | ----------------------------------- | ----------- |
| `mp3`                                                                                                        | yes                                                    | no             | yes: MPEG audio frames, no preamble | yes         |
| `adts`, `ac3`, `eac3`                                                                                        | yes                                                    | no             | yes: frames only                    | yes         |
| `mpegts`                                                                                                     | yes                                                    | no             | yes: packets only                   | yes         |
| `ogg`, `oga`, `ogv`, `opus`, `spx`                                                                           | yes                                                    | no             | yes: a new chained stream           | no          |
| `matroska`, `webm`                                                                                           | yes: clusters cut at key frames, header replayed (§5)  | no             | no                                  | no          |
| ISO family, `movflags` not explicit                                                                          | yes: fragmented on every sink but a seekable fresh one | no             | no                                  | yes         |
| ISO family, explicit `movflags` with `empty_moov` and `frag_keyframe` or `frag_custom`, without `delay_moov` | yes                                                    | no             | no                                  | yes         |
| ISO family, any other explicit `movflags` that fragments                                                     | no                                                     | no             | no                                  | yes         |
| ISO family, explicit `movflags` that does not fragment                                                       | no                                                     | yes            | no                                  | no          |
| `wav`, `flac`                                                                                                | yes                                                    | no             | no: use `%wav` or `%flac`           | no          |
| `webvtt`                                                                                                     | no                                                     | no             | no                                  | yes         |
| any other muxer                                                                                              | no; yes with an explicit `streamable=true`             | no             | no                                  | no          |

- **In-band metadata**: none, for every configuration
  ([encoder.md §9](encoder.md#9-in-band-metadata)). The Ogg muxers carry
  their initial metadata only (§7).
- **Needs no byte sink**: exactly the muxers that need no file.
- **Accepts delegated**: every configuration. On a delegated destination no
  configuration declares needs seekable: the protocol decides, once open
  (§13).
- A muxer not in the table has no declaration this writer can vouch for: it
  is not streamable, cannot continue and is not segmentable. A muxer that
  turns out to need a seekable destination it was not given fails at its
  header write (§15).
- **`streamable=true`** (bool, default unset) declares any configuration
  streamable on the script's word
  ([encoder.md §5.3](encoder.md#53-the-check)); it is refused on a
  configuration that needs seekable. On a muxer of the table it changes
  nothing that row does not already do. On another muxer the writer marks
  the access points of §5 item 4's last row. `streamable=false` declares
  the configuration not streamable.
- **Cannot continue** for `wav`, `flac`, Matroska and the ISO family: the
  muxer writes a whole header at its start and cannot write media alone
  after an existing stream. Migration for an appended file: `%wav` or
  `%flac`, which continue a stream
  ([encoder.md §3.5](encoder.md#35-header-forms-and-preambles)).
- Streamable is the writer's promise to delimit an entry header and mark
  access points (§5). It is declared only for muxers whose header is
  complete once their header write returns, and whose units the writer can
  locate.

**Defaults adapted to the capabilities.** The writer applies them itself; they
are never added to the format value and never printed.

| Muxer      | Option                        | Adapted value                                                                                                                                                    |
| ---------- | ----------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `mp3`      | `id3v2_version`, `write_xing` | `id3v2_version`: the muxer's default on a file sink, 0 on every other sink; `write_xing`: the muxer's default on a file sink, 0 elsewhere (the frame needs seek) |
| ISO family | `movflags`                    | on every sink but a seekable fresh one: `+frag_custom+empty_moov+default_base_moof`, a fragmented stream whose fragments the writer cuts (§5)                    |
| Ogg muxers | `page_duration`               | at most the live bound `L` on a `live(L)` sink                                                                                                                   |
| every one  | `max_interleave_delta`        | at most `L` on a `live(L)` sink, when interleaving is on                                                                                                         |
| every one  | segmenting defaults           | on a `segmented(D)` sink, those of [FE §3](../../optionals/ffmpeg/spec/encoder.md#3-building-an-instance) step 2, with that `D`                                  |

**Explicit options refused against the declared capabilities**, when the
output is constructed ([encoder.md §5.3](encoder.md#53-the-check)):

| Explicit option                                                                                                                                                                                                   | Refused on                    |
| ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------- |
| `mp3`: `id3v2_version` above 0, `write_xing` set to 1                                                                                                                                                             | a `live` or `continuing` sink |
| `mp3`: `write_id3v1` set to 1                                                                                                                                                                                     | a `continuing` sink           |
| ISO family: `movflags` with `faststart`                                                                                                                                                                           | every sink without `seekable` |
| an option that holds media longer than `L`: Ogg `page_duration`, `max_interleave_delta` (or 0, no bound, with interleaving on), the ISO family's `frag_duration`, Matroska's `cluster_time_limit`, each above `L` | a `live(L)` sink              |
| `streamable=true` on a configuration that needs seekable                                                                                                                                                          | every destination             |
| a protocol option (§1 item 3)                                                                                                                                                                                     | every sink                    |

The other rows of [encoder.md §5.3](encoder.md#53-the-check) apply to the
declarations above: a configuration that is not streamable on an output
serving readers mid-stream, that needs seekable on a sink without
`seekable`, that cannot continue on a `continuing` sink, that needs no byte
sink on a sink, or that is not segmentable on a segmented sink, is refused.

## 3. Creation

### 3.1 Inputs and order

A writer is built from the configuration, the confirmed capabilities, the
sink or the delegated destination with the output's interrupt, the output's
name, the initial metadata and the format's position
([encoder.md §2.2](encoder.md#22-creation)). In order:

1. The container options are the format's options, plus the defaults of §2
   adapted to the **confirmed** capabilities, plus the stream defaults of
   [FE §3](../../optionals/ffmpeg/spec/encoder.md#3-building-an-instance)
   step 1. A capability lost at confirmation adapts the defaults like any
   other: an MP4 whose `movflags` are not explicit, written to a path that
   turns out to be a FIFO, is fragmented
   ([encoder.md §5.4](encoder.md#54-at-open)).
2. The muxer is opened over the I/O of §3.2, or on the delegated destination
   (§13).
3. The stream encoders are built and their options checked
   ([FE §3](../../optionals/ffmpeg/spec/encoder.md#3-building-an-instance)
   steps 3.5–3.6, [FE §4](../../optionals/ffmpeg/spec/encoder.md#4-option-handling)).
4. The container metadata is set from the initial metadata (§7).

Creation writes nothing into the sink: no muxer stream exists yet, and the
header is written with the first piece (§5). A creation that fails part way
releases what it built.

### 3.2 The I/O over a sink

The muxer writes through I/O the writer builds on the sink
([sink.md §2](sink.md#2-operations)):

| Muxer I/O        | Sink                                                                                                                      |
| ---------------- | ------------------------------------------------------------------------------------------------------------------------- |
| write            | `write`                                                                                                                   |
| seek, size query | `seek`, `size`; offered only on a sink confirmed `seekable` and `fresh`                                                   |
| read             | `read`; offered only on a sink confirmed `seekable` and `fresh`, and only while the writer ends, as work allowed to block |

- A muxer given no seek sees a destination that cannot seek, and writes its
  streaming form by itself (§6). On a `continuing` sink no seek is offered:
  the muxer never reaches back into the existing stream.
- A read the muxer asks for outside the end is refused to the muxer as an
  I/O error.
- The I/O layer's buffer is written into the sink at the end of every call
  of the stream face, so that every byte a call produced is in the sink when
  it returns ([encoder.md §3.2](encoder.md#32-order-of-a-calls-output)). The
  muxer's own queues (interleaving, a fragment or cluster being filled) are
  not flushed by this; §11 bounds them.
- A muxer that needs no file is never opened over a sink (§2).

## 4. Input

[FE §5](../../optionals/ffmpeg/spec/encoder.md#5-encoding-a-frame) and its
stream encoders own what each field reads, frame sizes, rate conversion,
image scaling and pixel conversion
([encoder-internal.md](../../optionals/ffmpeg/spec/encoder-internal.md)), and
the readiness of copy streams. At the seam:

- Every piece has at least one sample or one image period
  ([encoder.md §3.3](encoder.md#33-frames-in)). The muxer streams are created
  at the first piece at which every copy field has known parameters (FE §5
  step 1); the pieces before it are held, not dropped.
- No byte, marker or timestamp depends on the piece boundaries.

## 5. Bytes and markers

Every byte is the muxer's. The writer decides when the header is written,
where units are cut, and which markers it places.

1. **Header.** Right after the muxer streams are created, before any frame
   or packet reaches the muxer, the writer writes the header and flushes the
   I/O. The `stream_start(header_length, t)` marker precedes those bytes,
   and `header_length` is exactly the number of bytes that header write
   produced. Reason: a codec with a delay returns no packet for its first
   frames, so a header taken at the first packet, or at whichever call
   happened to flush it, is empty or misplaced. The header write is the one
   moment at which the muxer's header is complete and nothing else has been
   written.
2. **Entry header.** For the streamable configurations of §2, the bytes of
   the header write are the whole entry header: the EBML header, segment
   information and tracks of Matroska; the file type and empty movie box of
   a fragmented ISO stream; the header pages of Ogg; the RIFF header of WAV;
   `fLaC` and its metadata blocks; nothing for `mp3` without preamble,
   `adts`, `ac3`, `eac3` and `mpegts`. A preamble (§6) follows the stream
   start and is not part of the header.
3. **Cutting units.** The writer makes the muxer end its current unit where
   an access point must begin:
   - Matroska and the fragmented ISO family: before every key packet of a
     video stream, and before the next packet once the unit holds
     `fragment_duration` of media (§11);
   - `mpegts`: the muxer writes its tables (PAT and PMT) again before the
     next key packet of every video stream, and, for audio only, before the
     next packet once `ts_table_interval` has elapsed since the last tables.

   | Parameter           | Recommended | Protects                                                        |
   | ------------------- | ----------- | --------------------------------------------------------------- |
   | `ts_table_interval` | 0.1 s       | The time a reader starting mid-stream waits for its first table |

4. **Access points.** The writer marks `access_point(t)` at:

   | Muxer                                 | Access point                                                                                                                                                                                                        |
   | ------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
   | `mp3`, `adts`, `ac3`, `eac3`          | each frame                                                                                                                                                                                                          |
   | `mpegts`                              | the tables written by item 3, when every stream's next packet is a key packet                                                                                                                                       |
   | Ogg muxers                            | each page that begins a packet                                                                                                                                                                                      |
   | Matroska                              | each cluster whose first block in every video stream is a key frame                                                                                                                                                 |
   | fragmented ISO family                 | each fragment that begins with a key frame in every video stream                                                                                                                                                    |
   | `wav`, `flac`                         | each packet's first byte                                                                                                                                                                                            |
   | another muxer, with `streamable=true` | the first byte the muxer writes for each key packet of every video stream, or, audio only, for each packet, where the writer can locate it from the I/O calls; the claim that a reader starts there is the script's |

   The writer finds these positions from what it asked the muxer to do (a
   cut, a table rewrite), or by reading the framing of the bytes it writes
   (frame sync words, Ogg capture patterns, box and element identifiers);
   it MAY do either. `t` is the media time of the first media after the
   position, from the packets' timestamps
   ([timestamps.md](../../optionals/ffmpeg/spec/timestamps.md)).

5. **Further streams.** A container restart at a copy source switch
   ([FE §13](../../optionals/ffmpeg/spec/encoder.md#13-container-restart))
   ends the current stream and starts another in the same sink only where
   streams of that muxer may follow one another in one byte stream: the Ogg
   muxers, as a chained stream (RFC 3533). The new stream has its own
   `stream_start` and header, written as in item 1. No other event starts a
   stream inside a writer; where no restart is in place, §15 applies.
6. **End.** `end` follows the trailer and the last flush (§9).

## 6. Header forms and preambles

1. **File form.** On a sink confirmed `seekable` and `fresh`, the muxer is
   given seek (§3.2) and writes its file form: the ISO family's movie box at
   the end, or at the front with `faststart` by reading back at the end;
   Matroska sizes, cues and duration; the `mp3` encoder-information frame
   with its counts, delay and padding; WAV sizes; FLAC total samples and MD5
   signature. Where the muxer writes a streaming form first and rewrites it
   with its trailer (Matroska, WAV, FLAC, `mp3`), a file whose writer was
   abandoned keeps a valid header with placeholders. A non-fragmented ISO
   file whose writer was abandoned has no movie box: that is the nature of
   that form, and an explicit fragmenting `movflags` is the script's way to
   avoid it.
2. **Streaming form.** On every other sink, the muxer has no seek and writes
   its streaming form: Matroska unknown sizes and no cues, WAV sizes meaning
   "to the end", a FLAC total of 0 and an MD5 of zeroes, no `mp3`
   encoder-information frame, a fragmented ISO stream (§2). The streaming
   form is the stream's header, hence the entry header of its access points
   (§5).
3. **Continuing.** On a `continuing` sink, the configurations that can
   continue (§2) write:

   | Muxer                           | The continuing writer writes              | Existing header updated at a clean end    |
   | ------------------------------- | ----------------------------------------- | ----------------------------------------- |
   | `mp3`                           | MPEG audio frames only, no preamble       | nothing                                   |
   | `adts`, `ac3`, `eac3`, `mpegts` | media only                                | nothing                                   |
   | Ogg muxers                      | a new chained stream, with its own header | nothing: each link is complete on its own |

   This writer declines the SHOULD of
   [encoder.md §3.5](encoder.md#35-header-forms-and-preambles) item 3,
   which asks a writer to update the existing header's counts, for the
   reason that table states: the muxer writes the header of the stream it
   starts and no other, so an existing encoder-information frame is not one
   this writer can rewrite. An appended `mp3` file keeps the counts of its
   first stream.

4. **Preambles.** The `mp3` muxer's ID3v2 tag and encoder-information frame
   are preambles, under
   [encoder.md §3.5](encoder.md#35-header-forms-and-preambles) item 5: on
   by default on a file sink only, as the muxer's defaults have them; off
   by default elsewhere (§2), written on another fresh sink (a FIFO, a
   process) when explicit, refused on a `live`, `continuing` or segmented
   sink when explicit. The encoder-information frame needs seek, and is
   written in the file form only.

## 7. Metadata

- **Initial metadata.** On a file sink, the format's own `metadata`
  entries, overridden key by key by the initial metadata
  ([FE §6](../../optionals/ffmpeg/spec/encoder.md#6-metadata) step 1), are
  set as the container's metadata before the header is written; on every
  other sink, the format's own entries only, the initial metadata being
  tags the tag rule leaves out
  ([encoder.md §3.5](encoder.md#35-header-forms-and-preambles), item 7).
  The muxer writes them where its header carries metadata: Vorbis comments
  and OpusTags in Ogg, Matroska tags, ISO metadata boxes, the ID3v2 tag
  when that preamble is on. The format's own entries are written on every
  sink: they are explicit, and only a preamble carrying them is refused
  where a preamble is (§2).
- **Encoder identification.** The muxer's own `encoder` tag (FFmpeg's
  library and version) stands for `ENCODER=` (encoder.md §3.5 item 8).
- **In-band offer.** Nothing. The writer offers no `metadata` call, whatever
  its muxer, the Ogg muxers included
  ([encoder.md §9](encoder.md#9-in-band-metadata)). A metadata change after
  the stream started is the output's to carry out of band, or to leave.

## 8. Delay and padding

- The delay of an encoded stream is its codec's initial padding, read when
  the codec opens. Its padding is known at the stream's end: the samples its
  last frame holds beyond the input. Both reach the muxer in the stream's
  parameters, and the stream's timestamps carry the delay as
  [timestamps.md](../../optionals/ffmpeg/spec/timestamps.md) states. The muxer
  writes them where its container has a field
  ([encoder.md §3.7](encoder.md#37-delay-and-padding)): Ogg Opus pre-skip and
  the last granule position, the ISO family's edit list, Matroska's codec
  delay and discard padding, the `mp3` encoder-information frame (file form
  only).
- A copy stream carries the values its source signals, in its parameters.
- Every stream start (a new writer, a restart of §5 item 5) starts new
  codecs, and its values are those of the new stream.

## 9. End

`end(clean)`:

1. If no muxer stream was ever created (no piece, or copy fields that never
   got known parameters), nothing is written, `end` included
   ([encoder.md §2.3](encoder.md#23-states)).
2. Every stream encoder drains, upstream first and in track order: the
   queue short of a codec frame, the rate converter and the scaler, the
   codec, then the packets the copy streams hold
   ([FE §7](../../optionals/ffmpeg/spec/encoder.md#7-end),
   [FE §12.8](../../optionals/ffmpeg/spec/encoder.md#128-flush-and-end)).
3. The trailer is written: on a seekable sink the muxer seeks back to write
   its file form, and may read back (§6 item 1); on every sink it writes its
   final unit (the last fragment or cluster, the end-of-stream pages).
4. The I/O is flushed into the sink, then `end` is written.

On a seekable sink, `end(clean)` runs as work allowed to block
([encoder.md §3.6](encoder.md#36-draining-and-final-bytes)). A drain, trailer
or flush step that fails is logged at `severe` with the writer's name and the
cause, and the end goes on with the next step.

`end(abandoned)` releases the codecs and the muxer without draining, without
a trailer, and writes nothing more, `end` included. The bytes the muxer
produced before the fault that led to it are already in the sink (§3.2).

## 10. Properties

- `bitrate`: the sum, over the non-drop streams, of each stream's bit rate
  fixed by the configuration, that is an encoded stream's explicit `b` codec
  option, read as FFmpeg reads an integer option (`"128k"` is 128000). It is
  answered from creation when every stream has one, and equals the format
  helper's answer ([formats.md §9](formats.md#9-helpers)). Otherwise it is
  none until every stream reports a rate once its muxer stream exists (a
  copy stream from its parameters, an encoded stream from its codec), then
  that sum.
- `delay`: per §8, known once the stream's codec is open; none before the
  first piece.
- `padding`: per §8, known at the stream's end.

## 11. Live latency

What holds encoded data, besides the codecs' own look-ahead:

| Stage                              | Bound                                                               |
| ---------------------------------- | ------------------------------------------------------------------- |
| the I/O buffer                     | nothing held: written into the sink at the end of every call (§3.2) |
| the interleaving queue             | `max_interleave_delta`, at most `L` on a `live(L)` sink (§2)        |
| an Ogg page being filled           | `page_duration`, at most `L` on a `live(L)` sink (§2)               |
| a fragment or cluster being filled | cut by the writer at most every `fragment_duration` (§5)            |

| Parameter           | Recommended                              | Protects                                                                                      |
| ------------------- | ---------------------------------------- | --------------------------------------------------------------------------------------------- |
| `fragment_duration` | 1 s, and at most `L` on a `live(L)` sink | Memory the muxer holds for one unit, and the delay before a reader receives that unit's bytes |

A unit cut because it reached `fragment_duration` without a key frame is not
an access point. On a sink that is not live, the bound still applies, for
memory.

## 12. Segmenting face

The segmenting face is
[HLS encoder-ffmpeg.md](../../outputs/hls/spec/encoder-ffmpeg.md), over
[FE §8](../../optionals/ffmpeg/spec/encoder.md#8-hls). A writer built with a
`segmented(D)` sink applies the segmenting defaults of
[FE §3](../../optionals/ffmpeg/spec/encoder.md#3-building-an-instance) step 2
with that `D`, and offers the face for the segmentable configurations of §2.

## 13. Delegated destination

The FFmpeg writer is the one writer that accepts a delegated destination
([encoder.md §6](encoder.md#6-the-delegated-destination)).

1. Built with `delegated(u)` and the output's interrupt, on the output's
   task, it opens `u` with the muxer of §1 item 5, the protocol consuming its
   options (§1 item 3). A muxer that needs no file opens no I/O: `u` is only
   its argument. Files and connections a muxer opens itself, next to `u`,
   are opened through FFmpeg under the same interrupt.
2. The interrupt covers every blocking call of the destination for the
   writer's whole life: the open, every write, the trailer, and the I/O the
   muxer opens itself. It also returns true once a write in progress has
   lasted the output's write timeout
   ([io.md §3](../../optionals/ffmpeg/spec/io.md#3-interrupting-blocking-libav-calls)).
3. The writer gives no meaning of its own to the form of `u`. `output.url`
   never hands it a `file:` URL or a local path: it refuses them at
   evaluation ([output-url.md §1](output-url.md#1-formats-and-declarations)).
   A local path reaches the writer only as the argument of a muxer that
   needs no file, from `output.file`. It never appends, never reopens `u`,
   and never restarts its container in place: where FE §13 would restart,
   the writer fails with an output fault and the output builds a new writer
   under its policy.
4. Header forms follow the protocol: a seekable protocol gets the file form,
   another the streaming form. The writer MAY adapt the defaults of §2 to the
   protocol's seekability once `u` is open, before the header write; a muxer
   that needs seeking the protocol does not offer fails at its header write
   (§15).
5. No sink, no markers, no in-band metadata.

## 14. Concurrency

- Every call comes from one caller: the output's streaming work, or the
  output's task for a delegated writer
  ([encoder.md §7](encoder.md#7-concurrency)). The writer needs no lock of
  its own. FFmpeg refuses concurrent use of one container; the one-caller
  rule is what keeps that from happening.
- The writer runs no work of its own besides its calls.
- A clean end on a seekable sink, and every call of a delegated writer, run
  as work allowed to block.
- Writers of different outputs share no muxer, codec or I/O state, and MAY
  run on several domains at once. Hardware device contexts shared between
  codecs are the FFmpeg tree's
  ([encoder-internal.md](../../optionals/ffmpeg/spec/encoder-internal.md)).

## 15. Faults

Classes of [faults.md §2](../../../spec/faults.md#2-classes). The seam's table
is [encoder.md §8](encoder.md#8-faults); the FFmpeg tree's is
[FE §5.2](../../optionals/ffmpeg/spec/encoder.md#52-data-faults-and-output-faults).

| Fault                                                                                                                                                 | Class                                                                              | Outcome                                                                   |
| ----------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------- | ------------------------------------------------------------------------- |
| unknown muxer, `format` missing where no muxer can be guessed, leftover options, a protocol option on a sink (§1)                                     | configuration error                                                                | refused at evaluation; one error names every leftover                     |
| a declaration or an explicit option that the output's capabilities refuse (§2)                                                                        | configuration error                                                                | refused when the output is constructed                                    |
| the muxer's open or header write refused for a cause that follows from the configuration and the confirmed capabilities, on the output's first writer | configuration error                                                                | hard error, naming the output, the muxer and FFmpeg's reason              |
| the same on a later writer (a reopen, a reconnect); a delegated open that fails for any other cause                                                   | output fault                                                                       | creation fails, or the writer is `failed`; the output's restart policy    |
| a frame or packet the codec or the muxer refuses                                                                                                      | item fault                                                                         | dropped with a warning and counted (FE §5.2)                              |
| `max_consecutive_faults` item faults in a row on one field                                                                                            | as FE §5.2                                                                         | on an encoded field, an output fault                                      |
| any other failure during a call, of whatever kind                                                                                                     | output fault                                                                       | the writer is `failed`                                                    |
| an incompatible copy source with no restart in place (§5 item 5)                                                                                      | as [FE §13.3](../../optionals/ffmpeg/spec/encoder.md#133-incompatible-copy-source) | unrecoverable on a sink; an output fault on a delegated destination (§13) |
| the sink fails                                                                                                                                        | output fault                                                                       | the writer is ended abandoned                                             |
| a drain, trailer or flush step fails at a clean end                                                                                                   | not escalated                                                                      | logged at `severe`; the end goes on (§9)                                  |

- **A header write is never an item fault.** Its failure fails the writer,
  whatever packet or frame triggered it. Reason: every later packet needs
  the same header, so a header that cannot be written is a stream that
  writes nothing, and a retry per packet hides that behind one log line, for
  ever.
- **A run of item faults ends only with an item the muxer accepted.** A call
  that hands the muxer nothing (a codec still filling its first frame)
  neither counts in a run nor ends it.
- A call that reports an output fault leaves nothing a later call could act
  on: the writer is `failed`, and a restart that could not build its new
  container leaves no closed container current.

## 16. Tests

| Check                                                                                                                                                                                           | Binding                                                                                                                           |
| ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------- |
| A reader of a shared `output.harbor` mount and a peer of an `output.srt` listener, starting after 1 s, for Matroska with AAC, with MP3, with Opus and with PCM, and for fragmented MP4 with AAC | the bytes start with the entry header, then an access point; FFmpeg decodes them with `-xerror`                                   |
| `%ffmpeg(format="mp4", %audio(codec="aac"))` to `output.external`, and to `output.file` on a FIFO path                                                                                          | a fragmented ISO stream that decodes; bytes reach the process within `fragment_duration`                                          |
| The same with `movflags="+faststart"` to `output.external`, `output.icecast` and `output.file(append=true)`                                                                                     | refused at evaluation, naming the need for a seekable destination                                                                 |
| The same with `movflags="+faststart"` to `output.file`                                                                                                                                          | the movie box precedes the media box                                                                                              |
| A muxer whose header write always fails, on the output's first writer                                                                                                                           | the script fails with a hard error naming the output; no output runs writing nothing                                              |
| `blocksize=4096` or `truncate=0` in an Ogg `%ffmpeg` format given to `output.file`                                                                                                              | refused at evaluation, naming the option as a protocol option                                                                     |
| An Ogg `%ffmpeg` format, with each of `ogg`, `oga` and `opus`, and three metadata changes, to `output.file` and to `output.harbor`                                                              | one logical stream; on the file it carries the initial title, on the mount no title; no failure                                   |
| `output.url` with a `file:` URL, and with a plain path                                                                                                                                          | refused at evaluation, naming `output.file`                                                                                       |
| `format="nosuchmuxer"` with `output.file` on `x.mp3`, and with `output.external`                                                                                                                | refused at evaluation in both                                                                                                     |
| No `format=`, `output.file` on `"x.mka"`; on `"x.nosuch"`; on a path getter                                                                                                                     | Matroska written; refused naming the extension; refused asking for `format=`                                                      |
| An unknown muxer to a shared `output.harbor` mount, without and with `streamable=true`                                                                                                          | refused, naming `streamable=true`; accepted                                                                                       |
| `frag_duration` or `cluster_time_limit` above `L`, explicit, to `output.icecast`                                                                                                                | refused at evaluation                                                                                                             |
| `foo=3` and a stream option `bar="x"` in one format                                                                                                                                             | one error naming both, at evaluation                                                                                              |
| `%ffmpeg(format="mp3", …)` to `output.srt`, `output.harbor` and `output.icecast`                                                                                                                | no ID3v2 tag and no encoder-information frame anywhere in the bytes sent                                                          |
| The same to `output.file`                                                                                                                                                                       | the bytes start with an ID3v2 tag; the file holds an encoder-information frame with counts, delay and padding                     |
| The same to `output.external`, without and with `id3v2_version=3`                                                                                                                               | no ID3v2 tag; an ID3v2 tag                                                                                                        |
| The same with `id3v2_version=3` to `output.harbor`                                                                                                                                              | refused at evaluation                                                                                                             |
| 2 s of 128 kb/s MP3 to `output.file`, its size sampled every 0.5 s                                                                                                                              | the file grows at every sample                                                                                                    |
| One minute of `mp3`, `adts`, `mpegts` and Ogg appended to a file holding one minute of the same configuration                                                                                   | decodes as two minutes; the Ogg file as two links                                                                                 |
| `wav`, `flac`, `matroska` and MP4 to `output.file(append=true)`                                                                                                                                 | refused at evaluation                                                                                                             |
| `output.url` to a peer that accepts and never reads; a stop during that write; a muxer writing its own files next to the URL on a stalled mount                                                 | each write ends within the write timeout; the stop returns at once and runs no `on_error`                                         |
| `output.icecast` with `%ffmpeg(format="mp3", %audio(codec="libmp3lame", b="128k"))`                                                                                                             | the first request head announces 128 kb/s                                                                                         |
| Audio-only Matroska and fragmented MP4 on a `live(1 s)` sink                                                                                                                                    | no second passes without bytes reaching the sink                                                                                  |
| A click at a known sample, encoded with AAC and with Opus into MP4, Matroska and Ogg files                                                                                                      | a decoder honouring the signalled delay and padding places the click within one sample, and the decoded length equals the input's |
| A copy source switch to other extradata in an Ogg `%ffmpeg` stream on a sink, and on `output.url`                                                                                               | on the sink, a new chained stream with its own `stream_start`; on `output.url`, an output fault and a new writer                  |
| An AAC stream whose every packet the muxer refuses                                                                                                                                              | the output fault is reached after `max_consecutive_faults` packets                                                                |
