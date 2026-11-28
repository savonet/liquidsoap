# The `%ffmpeg` encoder: container and stream-encoder contract

This file owns the building of one `%ffmpeg` container writer: how the
container is opened, how its streams are created and fed, how options are
checked, how its container metadata is set, how it drains and ends, how it
restarts its container at a copy source switch, how it cuts HLS segments on a
segmented sink, and the contract every stream encoder meets (§12). The
writer's place at the seam (its declarations, the defaults it adapts, its
markers, header forms, I/O over a sink, delegated destination and faults) is
owned by [the `%ffmpeg` writer at the seam](../../../encoder/spec/ffmpeg.md),
under the [encoder contract](../../../encoder/spec/encoder.md) and the
[sink contract](../../../encoder/spec/sink.md); this file links them and does
not restate them. It relies on the format record of
[encoder-format.md](encoder-format.md), on the output container operations of
the bindings ([avformat.md §4.4](../../../../modules/synced/ffmpeg/spec/avformat.md#44-output)),
and on the draining rule of [draining.md](../../../source/spec/draining.md).
Stream implementations are in [encoder-internal.md](encoder-internal.md) and
[encoder-copy.md](encoder-copy.md); every timestamp rule is in
[timestamps.md](timestamps.md).

## 1. Entry points and call order

Under the generic registry
([encoder.md §1](../../../encoder/spec/encoder.md#1-registry)), the `ffmpeg`
plug-in answers exactly the `%ffmpeg` formats. Its factory answers the
declarations of
[ffmpeg.md §2](../../../encoder/spec/ffmpeg.md#2-declarations) and the format
helpers without building anything, and builds a writer from:

| Input            | Meaning                                                                                                                                 |
| ---------------- | --------------------------------------------------------------------------------------------------------------------------------------- |
| configuration    | the format value, every deferred value resolved                                                                                         |
| capabilities     | those the sink confirmed ([sink.md §1](../../../encoder/spec/sink.md#1-capabilities))                                                   |
| destination      | the sink, or `delegated(u)` with the output's interrupt ([encoder.md §6](../../../encoder/spec/encoder.md#6-the-delegated-destination)) |
| name             | names the writer in logs                                                                                                                |
| initial metadata | [encoder.md §2.2](../../../encoder/spec/encoder.md#22-creation) (§6)                                                                    |
| position         | the format's script position, for errors                                                                                                |

The interrupt covers every blocking I/O of a delegated destination
([io.md §3](io.md#3-interrupting-blocking-libav-calls)). Building runs §3 and
answers the writer or a fault (§5.2).

The writer offers the stream face of
[encoder.md §3](../../../encoder/spec/encoder.md#3-the-stream-face):

| Operation                     | Effect                                                                                           |
| ----------------------------- | ------------------------------------------------------------------------------------------------ |
| `write piece`                 | encodes the piece (§5)                                                                           |
| `end clean`, `end abandoned`  | ends the writer (§7)                                                                             |
| `bitrate`, `delay`, `padding` | property reads ([ffmpeg.md §10](../../../encoder/spec/ffmpeg.md#10-properties), §12.9)           |
| `insert_id3`                  | on a `segmented(D)` sink with the `in_stream` ID3 mode only: the HLS contract's timed ID3 (§8.5) |

It offers no metadata call (§6). Every call comes from one caller
([encoder.md §7](../../../encoder/spec/encoder.md#7-concurrency)).

A writer is in one of three states, those of
[encoder.md §2.3](../../../encoder/spec/encoder.md#23-states):

| State     | Entered by             | Left by                                                                                                                      |
| --------- | ---------------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| `writing` | §3                     | `end` → `ended`; an output fault → `failed`; a container restart at a source switch (§13) stays `writing` on a new container |
| `failed`  | an output fault (§5.2) | `end` → `ended`                                                                                                              |
| `ended`   | §7                     | none                                                                                                                         |

Within `writing`, the muxer streams exist from the first piece of §5 step 1;
before it, nothing has been written. In `failed`, `end` releases the
container without draining
([draining.md §5](../../../source/spec/draining.md#5-discarded-on-purpose)).
A call outside the table of encoder.md §2.3 is an internal error.

## 2. What the writer relies on from callers

- Every piece holds at least one sample or one image period
  ([encoder.md §3.3](../../../encoder/spec/encoder.md#33-frames-in)).
- Metadata reaches the writer only as its initial metadata, at creation. No
  call hands it metadata afterwards (§6).
- The content of every field the format names is present in each piece with
  the content type of [encoder-format.md §10](encoder-format.md#10-content-type-implied-by-a-format).

## 3. Building an instance

In order:

1. **Stream defaults.** Every encoded track's codec options are copied, and
   the global-header flag is added to `flags`: combined with an existing value
   the way `q` combines `QSCALE`
   ([encoder-format.md §8](encoder-format.md#8-q-and-qscale)), unless the
   written `flags` names `global_header` itself (`+global_header` or
   `-global_header`). Reason: global headers put the codec's configuration in
   the stream's codec parameters, where the muxer and the codec string of
   §8.6 read it, whatever the container.
2. **Segmenting defaults**, only on a `segmented(D)` sink, `D` its target
   duration
   ([sink.md §1](../../../encoder/spec/sink.md#1-capabilities)). Like
   every default this writer adapts to its capabilities
   ([ffmpeg.md §2](../../../encoder/spec/ffmpeg.md#2-declarations)), each is
   applied by the writer, never added to the format value nor printed, and
   an option the format writes is kept:
   - `format` of the ISO base media family (`mp4`, `mov`, `ipod`,
     `ismv`): container option `movflags` defaults to
     `"+frag_custom+dash+delay_moov"` (fragments are cut only when the
     encoder asks, §8.2).
   - `format="mpegts"`: container option `avoid_negative_ts` defaults to
     `"disabled"`. Reason: the muxer would otherwise shift the timeline by
     the first packet's negative timestamp, which is the codec's priming,
     so variants of one output with different codecs (AAC-LC, HE-AAC) would
     carry different timestamps for the same media time. MPEG-TS stores
     timestamps after its own positive delay.
   - Every encoded video track: `+cgop` (closed GOP) is added to `flags`,
     combined the way step 1 combines the global-header flag, unless the
     written `flags` names `cgop` or is a number. Reason: every key frame is
     a place where the HLS output may cut (§8.7), and must let a decoder
     start there. In an open GOP, libx265 makes its key frames CRA pictures,
     so a player starting at such a segment drops its leading pictures.
   - Every encoded video track: codec options `g` and `keyint_min` default
     to `round(D × r / n)` frames, `r` the track's frame rate, `D` in
     seconds and `n = max(1, ⌈D / 2⌉)`. A raw track with
     no `framerate` gets its rate from its first frame
     ([encoder-internal.md §4.6](encoder-internal.md#46-frame-timing-both-video-modes)),
     so its `g` and `keyint_min` are sized when its stream is created; a
     later change of the source's rate keeps them. With `libx264`
     and `libx264rgb`, `sc_threshold` defaults to 0. Each is set only when
     the format does not write it. Reason for naming the codecs:
     `sc_threshold` is a private option of those encoders, and another
     encoder rejects it as unused.
     Reason: the HLS output cuts at key frames and places none
     ([HLS segmenting §4.1](../../../outputs/hls/spec/segmenting.md#41-key-frame-variants)),
     so key frames at a fixed interval dividing `D` give segments of `D`;
     at most 2 s apart, a cut waits at most one interval; without scene
     cuts, no key frame falls in between. An encoder that does not read
     these options places its key frames itself, and the HLS output warns
     when they do not fit `D`.
   - `interleaved=Default` becomes `False`; an explicit value is kept.
     Reason: an interleaving muxer holds packets until every stream has
     written, which empties the first segment when a stream is sparse (the
     timed-ID3 stream of §8.5).
3. **Container** (also run again by a restart, §13):
   1. Container options = a copy of the format's `opts`. Two keys given as
      names are converted: `sample_fmt` to FFmpeg's sample-format id and
      `channel_layout` to FFmpeg's canonical description. An unknown name or a
      non-string value is an error of kind `encoder`.
   2. The muxer is the one
      [ffmpeg.md §1](../../../encoder/spec/ffmpeg.md#1-format-and-options)
      item 5 fixes when the output is constructed: the muxer `format`
      names, a name no muxer of the build has being a configuration error on
      every destination; without `format`, the muxer FFmpeg guesses from the
      extension of a constant `output.file` path, or from the URL of a
      delegated destination; with neither, a configuration error. No other
      muxer is ever substituted.
   3. `interleaved`: `Default` → true when the format has more than one
      non-drop track; `True`/`False` as written. Reason: a single-stream
      output has nothing to interleave. With interleaving on, a sparse
      stream (subtitles) delays the other streams by at most the muxer's
      `max_interleave_delta`, a container option the user may set, bounded
      on a live sink
      ([ffmpeg.md §11](../../../encoder/spec/ffmpeg.md#11-live-latency)).
   4. Open, with the container options of step 3.1 plus the defaults
      [ffmpeg.md §2](../../../encoder/spec/ffmpeg.md#2-declarations) adapts
      to the confirmed capabilities
      ([ffmpeg.md §3.1](../../../encoder/spec/ffmpeg.md#31-inputs-and-order)):
      - on a sink: the muxer is opened over the I/O the writer builds on the
        sink ([ffmpeg.md §3.2](../../../encoder/spec/ffmpeg.md#32-the-io-over-a-sink)):
        write always; seek and size only on a sink confirmed `seekable` and
        `fresh`; read only on such a sink, while the writer ends. Opening
        writes nothing into the sink.
      - on `delegated(u)`: the bindings open `u` themselves, with the muxer
        of step 3.2 and the output's interrupt
        ([ffmpeg.md §13](../../../encoder/spec/ffmpeg.md#13-delegated-destination)).
        Nothing is written into any sink.
   5. Stream encoders: every non-drop track builds its stream encoder, in
      track order (§12). No muxer stream is added yet (§5). An encoded track
      with no `codec` is an error of kind `encoder` naming the field.
   6. Options are checked (§4).
4. **Container metadata**: set from the initial metadata (§6).

Every failure answers a fault and releases what was built
([encoder.md §2.2](../../../encoder/spec/encoder.md#22-creation)). Its class
is that of [ffmpeg.md §15](../../../encoder/spec/ffmpeg.md#15-faults): a
refusal that follows from the configuration and the confirmed capabilities
is a configuration error on the output's first writer and an output fault on
a later one; a delegated open that fails for any other cause is an output
fault (§5.2).

### 3.1 Conformance

- `%ffmpeg(%audio(codec="aac"), %video.drop)` opens a non-interleaved
  container.
- A stream with `q=2` and no `flags` gets both the qscale and the global-header
  flag.

## 4. Option handling

There are two option sets:

- **Container options** (format `opts`, plus the adapted defaults of §3
  step 2 and of
  [ffmpeg.md §2](../../../encoder/spec/ffmpeg.md#2-declarations)): consumed
  by the bindings' open, by FFmpeg's generic container options, then the
  muxer's private options, then, on a delegated destination only, the I/O
  protocol. On a sink no protocol consumes options: a protocol option there
  is a leftover, which the error names as an option of a destination the
  output owns
  ([ffmpeg.md §1](../../../encoder/spec/ffmpeg.md#1-format-and-options)
  item 3).
- **Stream options** (track `opts`, plus `flags`): consumed by the stream
  encoder's creation (codec, device context, generic codec options).

Every option MUST be consumed. All leftovers, container and stream alike, are
reported in **one** error of kind `encoder` at the format's position, that
names each leftover key with its scope (the container, or the field). Example
message: `Unrecognized options: container: foo=3; audio: bar="x"`.

The check runs at evaluation, when the output is constructed, without
opening anything
([formats.md §7](../../../encoder/spec/formats.md#7-when-a-format-is-validated)).
A literal `format` naming no muxer of the build is refused at type checking;
every option name is checked at evaluation, since what the build's libraries
accept is known only there. Container option names are checked against the
options the muxer, FFmpeg's generic container options and, on a delegated
destination, its protocol list: the URL is a constant string, known at
evaluation. Stream option names are checked against the options
the codec, its device context and FFmpeg's generic codec options list
([avutil.md §9.3](../../../../modules/synced/ffmpeg/spec/avutil.md#93-listing-options)).
Stream encoders whose muxer stream is created at the first piece (§5) check
again at stream creation, where a leftover raises the same error. Reason: a
bad option is a configuration error and is reported before the output
starts, not when the first piece arrives.

A format whose options are all valid but unusable together fails at FFmpeg's
open or at stream creation with FFmpeg's error, classed by §5.2.

### 4.1 Conformance

- `%ffmpeg(format="adts", foo=3, %audio(codec="aac", bar="x"))` fails at
  evaluation with one error naming both `foo` and `bar`.
- `%ffmpeg(%video(codec="libx264", nosuchopt=1))` fails at evaluation,
  before any writer is built.
- `%ffmpeg(format="ogg", truncate=0, %audio(codec="libvorbis"))` given to
  `output.file` fails at evaluation, naming `truncate` as a protocol option.

## 5. Encoding a frame

`write piece`:

1. At the container's first piece in which every copy field has a run with
   known parameters ([content.md §4.2](content.md#42-run-layer)), every
   stream encoder creates its muxer stream (§12.2), **in track order**.
   Without copy fields, that is the container's first piece. Pieces before
   it write nothing for any stream, so encoded streams do not start ahead of
   the copied ones; their content is held, not discarded, up to
   `pending_pieces_bound` of memory
   ([ffmpeg.md §4](../../../encoder/spec/ffmpeg.md#4-input)). Past that
   bound the oldest held pieces are dropped, with one warning per wait
   giving the stream time dropped. The wait is
   bounded by `max_keyframe_wait`
   ([encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start)):
   past it, the writer fails with an output fault naming the copy fields
   without parameters ([faults.md §2](../../../../spec/faults.md#2-classes)), and the
   output's restart policy applies. Copy outputs are an expert feature: the
   script author who feeds one is expected to supply its streams, and a
   container missing a declared track is not produced. Reason: stream
   parameters of video and copy tracks come from the first piece, and
   creating every stream at the same moment makes the container's stream
   order the written track order.
2. Right after the streams are created, before any frame or packet reaches
   the muxer, the writer writes the header explicitly with the bindings'
   header write, and flushes the I/O, with the `stream_start` marker before
   those bytes
   ([ffmpeg.md §5](../../../encoder/spec/ffmpeg.md#5-bytes-and-markers)
   item 1).
3. Every stream encoder's `encode` runs on the piece, in track order. After
   the field loop, the copy tracks' end-of-piece step runs: source groups
   are evaluated and every group whose members are all ready starts
   ([encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start)).
4. Writes queued until the first media packet (§8.5) run in queue order.
5. On a sink, the I/O buffer is written into the sink before the call
   returns
   ([ffmpeg.md §3.2](../../../encoder/spec/ffmpeg.md#32-the-io-over-a-sink));
   on a delegated destination the muxer writes it itself.

### 5.2 Data faults and output faults

Every fault the writer detects belongs to one class of
[faults.md §2](../../../../spec/faults.md#2-classes) and is reported as a value
([encoder.md §8](../../../encoder/spec/encoder.md#8-faults)); the classes at
the seam are [ffmpeg.md §15](../../../encoder/spec/ffmpeg.md#15-faults). The
writer is part of an output, so it has no source to end: its faults are
configuration errors, item faults or output faults.

| Fault                                                                                                                                                                                                                                                                                    | Class                                               | Outcome                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| bad option, unknown codec or muxer, missing codec, invalid argument (§3, §4, [encoder-format.md §13](encoder-format.md#13-errors-summary))                                                                                                                                               | configuration error                                 | refused at type checking or evaluation: the script fails before streaming                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| the muxer's open or header write, or a stream's creation, refused deterministically from the configuration and the confirmed capabilities (codec or muxer lookup, a parameter rejected for the configured format, a muxer that needs seeking it was not given), hardware device included | refused by the configuration, classed by the output | the writer reports the refusal as its own kind of fault and becomes `failed`. The output, which alone knows whether this is its first writer, classes it: on the first writer, a configuration error, a hard error ([faults.md §2](../../../../spec/faults.md#2-classes)) naming the output, the muxer or the field, and FFmpeg's reason, so the script fails and no output runs writing nothing; on a later writer (reopen, reconnect, restart: hardware session caps, device resets), an output fault, and the output restarts under its policy |
| a stream that cannot be created because of the source's data (copy parameters the muxer rejects)                                                                                                                                                                                         | output fault                                        | as below                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| the codec refuses a frame, the muxer or its bitstream filter refuses a packet, a subtitle is too large, a timed-ID3 tag fails                                                                                                                                                            | item fault                                          | the item is dropped with a warning; the stream, the container and the other tracks go on                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| `max_consecutive_faults` item faults in a row on one field                                                                                                                                                                                                                               | item fault                                          | copy field: the copied source's group restarts on its next keyframe ([encoder-copy.md §10](encoder-copy.md#10-options-and-errors)); encoded field: an output fault, so the output restarts under its policy with a new writer                                                                                                                                                                                                                                                                                                                     |
| the sink fails, a delegated destination fails (its open, a write, a write past the output's timeout), the header write fails for any other cause, a restart cannot build its container                                                                                                   | output fault                                        | writer `failed` (§1)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| an incompatible copy source with no clean restart (§13.3)                                                                                                                                                                                                                                | as §13.3                                            | unrecoverable on a sink; an output fault on a delegated destination                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| any other failure during a call, of whatever kind                                                                                                                                                                                                                                        | output fault                                        | writer `failed`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| a drain, trailer or flush step fails at `end clean` (§7)                                                                                                                                                                                                                                 | not escalated                                       | logged at `severe`; the end goes on                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |

- **A header write is never an item fault**, whatever packet or frame
  triggered it: its failure fails the writer, or is the configuration error
  above.
- Every output fault leaves the writer `failed`, whatever its kind; a later
  call acts on nothing the failure left behind.

Item faults are counted per field against `max_consecutive_faults`
([faults.md §4](../../../../spec/faults.md#4-item-faults-escalate)). **A run
ends only with an item the codec or the muxer accepted**: a call that hands
the muxer nothing (a codec still filling its first frame) neither counts in
a run nor ends it. A data fault never stops the output. A run past the bound
on a copy field restarts that source's group (per tenure,
[encoder-copy.md §10](encoder-copy.md#10-options-and-errors)); later runs
restart it again. On an encoded field the run is an output fault: the output
restarts under its policy, which builds a new writer with new codecs.
Reason: a codec that keeps refusing frames is in an error state, which no
further data repairs. The first drop of each kind is logged as a warning;
repeats are logged with their count at most once per `fault_log_interval`
([faults.md §4](../../../../spec/faults.md#4-item-faults-escalate)).

An output fault reaches the output's one restart policy exactly once
([encoder.md §8](../../../encoder/spec/encoder.md#8-faults); for `output.url`,
[io.md §11.3](io.md#113-errors)). The clock and the other outputs go on.

### 5.1 Conformance

- `%ffmpeg(%video(codec="libx264"), %audio(codec="aac"))` gives stream 0
  video and stream 1 audio.

## 6. Metadata

The container's metadata is set once, at creation, before the header is
written:

1. The entries of the initial metadata are added onto the format's own
   `metadata`: on a shared key the initial metadata's value wins, and the
   format's other entries are kept. On a sink where the tag rule writes no
   tags from the initial metadata, only the format's own entries are set
   ([ffmpeg.md §7](../../../encoder/spec/ffmpeg.md#7-metadata)).
2. The list replaces the container's metadata.

A metadata change after creation never reaches the writer (§2). No
container carries it in band, and no new Ogg link is started for it, the
Ogg muxers included
([encoder.md §9](../../../encoder/spec/encoder.md#9-in-band-metadata)). An
output carries it out of band where it has a channel (Icecast metadata
updates, HLS timed ID3, §8.5), or leaves it.

### 6.1 Conformance

- An Ogg Vorbis `%ffmpeg` `output.file` with a metadata change mid-stream
  gives one logical stream that carries the initial title, and no second
  link.

## 7. End

`end clean`, from `writing`, as
[ffmpeg.md §9](../../../encoder/spec/ffmpeg.md#9-end) orders it:

1. When no muxer stream was ever created, nothing is written, `end`
   included.
2. Every stream encoder drains (§12.8), in track order; a copy stream writes
   the packets it holds ([encoder-copy.md §8](encoder-copy.md#8-flush-and-end)).
   Each stage's drained output reaches the muxer ([draining.md §3](../../../source/spec/draining.md#3-order)).
3. The container is closed. Closing drains every codec and writes the
   trailer ([avformat.md §2.1 Release](../../../../modules/synced/ffmpeg/spec/avformat.md#21-container--input-container-output-container)).
4. On a sink, the I/O is flushed into the sink, then `end` is written.

A drain, trailer or flush step that fails is logged at `severe` with the
writer's name and the cause, and the end goes on with the next step; it is
not escalated ([draining.md §5](../../../source/spec/draining.md#5-discarded-on-purpose)).

`end abandoned`, and `end` from `failed`, release the codecs and the
container with the bindings' abandon
([avformat.md §2.1 Release](../../../../modules/synced/ffmpeg/spec/avformat.md#21-container--input-container-output-container)):
no drain, no trailer, nothing more written, `end` included. The bytes
written before are already in the sink.

### 7.1 Conformance

- One second of 44100 Hz stereo sine through
  `%ffmpeg(format="adts", %audio(codec="aac"))` gives as many AAC packets as
  the FFmpeg command line gives for the same input (45), and the decoded
  output holds at least the 44100 input samples.
- A re-encoded video file of N frames at the output frame rate gives N frames.

## 8. HLS

Only on a `segmented(D)` sink. The segmenting face
([encoder.md §4](../../../encoder/spec/encoder.md#4-the-segmenting-face)) is
its own interface and registry, beside the stream face: the HLS output
builds it with the segment duration `D`, and a configuration is segmentable
exactly when that registry answers it. Its events are the markers of the
stream face, under their own names:
the writer's first `stream_start` delimits the init segment (§8.3), every
accepted split point is an `access_point` (§8.2, §8.4), a container restart
is a `stream_start` with the new container's header (§8.4), and `end clean`
writes the final bytes. §8.7 states what the HLS output does with them.

### 8.1 ID3 mode

The HLS ID3 mode
([HLS encoder-contract §3](../../../outputs/hls/spec/encoder-contract.md#3-id3-mode))
is a declaration of the configuration: a function of the muxer and of the
HLS output's ID3 setting, answered without building a writer. No writer is
built at evaluation. By the muxer's name:

| Muxer                        | ID3 setting | Declaration                                                                                                                |
| ---------------------------- | ----------- | -------------------------------------------------------------------------------------------------------------------------- |
| `mpegts`                     | not `false` | `in_stream`: the writer adds a timed-ID3 data stream (§8.5)                                                                |
| `mpegts`                     | `false`     | `none`                                                                                                                     |
| `adts`, `mp3`, `ac3`, `eac3` | `false`     | refused at evaluation: packed audio requires ID3                                                                           |
| `adts`, `mp3`, `ac3`, `eac3` | otherwise   | `prepended`: `insert_id3` returns a raw ID3 tag to prepend to the segment (the generic HLS ID3 builder, version default 3) |
| other                        | `true`      | refused at evaluation: the format carries no HLS ID3 metadata                                                              |
| other                        | not `true`  | `none`                                                                                                                     |
| none (no muxer)              | any         | refused at evaluation: a format is required                                                                                |

Reason for the packed-audio row: an HLS packed-audio segment carries its
timestamp in an ID3 tag (RFC 8216 §3.4).

### 8.2 Keyframe hooks

A **split point** is a position in the writer's output bytes such that, for
every participating stream, the first packet of that stream written after the
position is a key packet. Participating streams are the muxer streams of
non-drop tracks; the timed-ID3 data stream does not participate. A subtitle
packet is decodable on its own: subtitle streams participate and every
subtitle packet counts as a key packet. Reason: a segment is fetched and
decoded on its own, so every stream in it starts at a key packet.

Each stream encoder gives every packet to the instance's hook, with the
packet's key flag, its media time ([§12.7](#127-keyframes)) and the write that
hands it to the muxer; the hook decides when that write runs. Outside HLS it
runs at once.

In HLS mode the muxer is not interleaved and the instance orders the packets
itself. Encoders hand packets over with their own latency: libx264's
lookahead and B-frames put video packets up to seconds behind audio ones. A
packet is written once every participating stream that is not all-key has
handed over packets up to its media time; packets are written earliest first,
each stream in its own order, and at equal times a stream that is not all-key
goes first. A packet held for longer than `settings.ffmpeg.copy.max_keyframe_wait`
behind the latest one handed over is written anyway. Timed-ID3 tags (§8.5)
are ordered with the packets, at their time, without gating them. Every held
packet is written before a container closes; one whose write fails is logged
and dropped. Reason: a split point at a key packet
then has every stream's earlier packets before it and none of the later
ones, so all streams of a segment start at its split time (#5719). The steps
below see packets as they are written.

A stream is **all-key** while every packet it has written is flagged key
(audio, subtitles, intra-only codecs). Candidates are taken only where a cut
can be useful:

1. Before a key packet of a participating stream that is not all-key, and
   before the first packet at or after each split request of the sink
   (§8.4), the hook flushes the muxer, so that the buffer holds
   exactly what precedes the packet, and records that position as a
   candidate, with the packet's media time as the split point's time.
2. A candidate is accepted when every other participating stream's first
   packet after it is a key packet, and dropped as soon as one is not. A
   stream that writes nothing until its next key packet (a copy stream
   waiting for a keyframe) or that received no data since the candidate (a
   sparse subtitle stream, a field with no content) does not hold the
   decision. When
   every other participating stream flags each packet as key (audio,
   subtitles, intra-only codecs), acceptance is immediate in practice.
   A candidate decided by a key packet that takes a candidate of its own is
   dropped in favour of that one: the two are one split point, and keeping
   both would leave a segment of a few packets between them.
3. Bytes written after an undecided candidate are held by the writer and
   written into the sink in order once it is decided, the candidate's
   `access_point` before them when it is accepted; no byte is dropped.

The muxer is flushed at each candidate and the writer asks for no other
muxer flush; moving the I/O buffer into the sink at the end of a call
(§5 step 5) is an I/O flush only, which leaves the muxer's queues as they
are. With fragmented MP4 (`frag_custom`) a flush ends a fragment, so a
segment holds one fragment per candidate it contains.

### 8.3 Init segment

The init segment is the header the writer's first `stream_start` delimits:
the bytes of the explicit header write (§5 step 2), which the muxer writes
before its first media data (fragmented MP4: `ftyp` and `moov`). With the
default `delay_moov`, kept because copied and primed streams need their
first packet's parameters in `moov`, the muxer writes `moov` with the first
fragment: the header then ends at the first `styp`, `sidx` or `moof` box,
found by scanning the boxes written
([ffmpeg.md §5](../../../encoder/spec/ffmpeg.md#5-bytes-and-markers) item 1). For a
format without one (MPEG-TS, ADTS, MP3) it is empty. The HLS output accepts
only the segment formats of RFC 8216
([HLS variants §2](../../../outputs/hls/spec/variants.md#2-building-a-variant)).

Since the header is written at stream creation, the init segment is known at
the piece at which the streams are created; no piece's data is lost before
it. For MP4 a media segment starts with its first fragment's boxes (`sidx`
when the muxer writes one, `moof`). An init segment MUST NOT be empty for a
format that has a header.

### 8.4 Split points and restarts

The sink asks for split points at media times (the HLS contract's
`boundary`): the next media time at which the HLS output wants one (§8.7).
The writer writes, in byte order:

- bytes no longer held (§8.2);
- `access_point(time)` at each accepted split point, with its media time.
  For MP4, the bytes after it start a fragment;
- at a container restart (§13) during a call, `stream_start(header_length,
time)` followed by the new container's header, its init segment (empty for
  a format without one). It is also a split point. The bytes after it stay
  held until the new header is written.

Encoded video streams place their key frames by their own key-frame
interval (§3 step 2); a split request does not make a key frame.

Holding (§8.2 step 3) only delays bytes.

### 8.5 Timed ID3 in mpegts

With the `in_stream` ID3 mode (§8.1), stream creation adds a data stream of codec `timed_id3`, time base `1/audio_rate`.
`insert_id3 ~time m` builds an ID3v2 tag (version
default 3), makes a packet with pts = dts at `time` on the media timeline (output
position minus `skipped`) as given by
[timestamps.md §8](timestamps.md#8-timed-id3-hls-mpegts), and writes it to that
stream. The timed-ID3 stream is generated by the encoder: it is not a member
of any copy group and does not count in the whole-output switch test
([timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase)). Until the first media packet has reached the muxer, the write is queued;
the queue runs after the piece that wrote that packet, in order. Reason: a muxer fixes its shift of
negative timestamps (codec priming) from the first packet it receives, so
the media streams must reach it first. It returns none (the tag is in the stream, not
prepended). `insert_id3` is an operation of the HLS contract, not in-band
metadata ([encoder.md §4](../../../encoder/spec/encoder.md#4-the-segmenting-face)).

### 8.6 Properties

| Query                     | Answer                                                                                                                     |
| ------------------------- | -------------------------------------------------------------------------------------------------------------------------- |
| `codec_attrs ()`          | the codec strings of the participating streams, joined by `,` in track order; none when any stream has none                |
| `independent_segments ()` | true when no participating stream is a copied stream whose codec is not intra-only (§8.4)                                  |
| `bitrate ()`              | the sum of the participating streams' bit rates; none when any stream reports none (a partial sum understates the variant) |
| `video_size ()`           | the single stream that reports a size; none with zero or several                                                           |

A stream's codec string is the bindings' `codec_attr` of the stream
([avformat.md §4.4](../../../../modules/synced/ffmpeg/spec/avformat.md#44-output)).
When the bindings give none, the stream falls back on its codec id: AAC →
`mp4a.40.2`, MP3 → `mp4a.40.34`; otherwise none. When any stream has no codec
string, `codec_attrs` is none and the encoder logs a warning naming the
stream; the HLS output then omits `CODECS` ([HLS variants §3](../../../outputs/hls/spec/variants.md#3-derived-properties)). Reason: the HLS `CODECS` attribute MUST list every format present in
the variant (RFC 8216 §4.3.4.2); a partial list is worse than none.

Streams exist from the first piece (§5), so the properties are known once
the first `stream_start` is written. The HLS output reads them after that.

### 8.7 Segmenting (HLS output side)

The HLS output owns the segment grid, the cut rules, the target duration
and output-wide cuts
([HLS segmenting](../../../outputs/hls/spec/segmenting.md)); the encoder
seam is [HLS encoder-contract](../../../outputs/hls/spec/encoder-contract.md).
What this writer provides to them:

1. **Requested times.** With every piece the segmented sink requests the
   next media time at which it wants a split point: the next grid
   boundary, or an earlier pending sync time. Media time is the output
   position minus `skipped` ([timestamps.md §1.4](timestamps.md#14-the-output-timeline)),
   and MUST equal the HLS output's media time. When every stream is
   all-key, the hook takes a candidate before the first packet at or after
   it (§8.2), so the variant has a split point at each requested time,
   within one packet. The candidate is taken at the first
   packet at or after the request even when no stream is video and the
   request has not changed since the previous piece. A request stays
   pending until a candidate is taken at it, also after a later request
   arrives: packets trail the media time by the codec delay, and every
   requested time gets its candidate, in order.
2. **Split points** are the only places a segment may be cut (§8.2). With
   MPEG-TS, the writer makes the muxer write a PAT and a PMT at each
   candidate (the muxer's own header resend), so every segment starts with
   them (RFC 8216 §3.2).
3. **Restarts** are stream starts (§8.4, §13).

### 8.8 Conformance

- MPEG-TS, AAC and H.264 with keyframes every second and 2 s segments: the
  first packet of every stream in every segment is a key packet, and no
  segment ends with a video key packet.
- A video with keyframes every 4 s and 1 s segments, re-encoded with the
  same key-frame interval, then copied: every segment starts with a video
  key frame and lasts 4 s, and the output warns that segments exceed the
  target duration.
- The same video re-encoded without a key-frame interval in the format:
  every segment lasts 1 s within one frame and starts with a key frame.
- A muxer that rejects every packet of one encoded field: the output keeps
  streaming the other fields, and the drops are logged with their count at
  most once per `fault_log_interval`, until `max_consecutive_faults` drops in
  a row restart the output.
- An HLS variant with an encoded subtitle track is cut into segments.
- Two variants, HE-AAC and AAC-LC, of one source: after fifty segments, the
  first PTS of segment _n_ is equal across variants to within one codec frame.
- An AAC MPEG-TS HLS output with 2 s segments: the first ten segments sum to
  20 s within one internal frame.
- An MP4 variant: the init segment is non-empty and the first media
  segment holds the first frame.

## 9. Delegated destination

The writer accepts a delegated destination
([ffmpeg.md §13](../../../encoder/spec/ffmpeg.md#13-delegated-destination)):
the bindings open it with the muxer of §3 step 3.2 and the output's
interrupt, and nothing is written into any sink. `output.url` is the one
output that delegates a protocol URL; its format check, states, errors and
restarts are [io.md §1.4](io.md#14-outputurl) and
[io.md §11](io.md#11-outputurl). `output.file` delegates only the path a
muxer that needs no file takes as its argument
([encoder.md §5.1](../../../encoder/spec/encoder.md#51-what-an-output-declares)).
No output names a destination in the format value
([encoder-format.md §1](encoder-format.md#1-shape-of-a-format-value)).

## 10. Readers starting mid-stream

- A reader of a shared `output.harbor` mount or a peer of an `output.srt`
  listener starts at the writer's stream starts and access points, which the
  shared stream records
  ([shared-stream.md](../../../outputs/spec/shared-stream.md)); its entry
  header is the one the shared stream recorded from the `stream_start`
  marker of §5 step 2, never asked of the writer.
- Whether a configuration is streamable is a declaration
  ([ffmpeg.md §2](../../../encoder/spec/ffmpeg.md#2-declarations)), with the
  access points of
  [ffmpeg.md §5](../../../encoder/spec/ffmpeg.md#5-bytes-and-markers). A
  muxer this writer cannot vouch for is streamable only with an explicit
  `streamable=true`
  ([encoder-format.md §3](encoder-format.md#3-top-level-arguments)).
- The MIME type and the Icecast stream info of a `%ffmpeg` format are
  [io.md §12](io.md#12-other-outputs-special-casing-ffmpeg).

**Conformance.**

- An Ogg/Opus `%ffmpeg` reader of a shared `output.harbor` mount, starting
  after the burst no longer holds the stream's start, receives `OpusHead`
  first, and its stream decodes.
- After a restart at a copy source switch in an Ogg `%ffmpeg` stream on a
  shared mount, a reader that starts with a burst shorter than the stream
  so far receives a link's header pages first, and no page of an earlier
  link after them.

## 11. Failure behaviour summary

Classes as in §5.2 and [faults.md §2](../../../../spec/faults.md#2-classes).

| Situation                                        | Behaviour                                                                                                            |
| ------------------------------------------------ | -------------------------------------------------------------------------------------------------------------------- |
| failed creation (§3, §4)                         | a fault: configuration error; output fault on a later writer, or for a delegated open that fails for any other cause |
| item fault in `write` (§5.2)                     | item dropped with a warning; streaming goes on                                                                       |
| output fault in `write` (§5.2)                   | writer `failed` (§1)                                                                                                 |
| drain, trailer or flush error at `end clean`     | not escalated; logged at `severe`; the end goes on                                                                   |
| copy source with other header properties (§13.3) | rejected at open; past the merge, accepted (in-band container), restart where defined, else §13.3                    |

What an output does with a fault is its restart policy's
([encoder.md §8](../../../encoder/spec/encoder.md#8-faults)). `output.url`
classifies a failed open or a failed stream creation as permanent
(configuration) or transient and acts on it in
[io.md §11.3](io.md#113-errors).

## 12. Stream-encoder contract

Each non-drop track has one stream encoder. Its implementations follow this
numbering: [encoder-internal.md](encoder-internal.md),
[encoder-copy.md](encoder-copy.md). Each implementation file answers every
clause. The inline encoders ([inline-codecs.md](inline-codecs.md)) do not
implement this contract; they reuse the format record, the audio frame writer
of [encoder-internal.md §5.1](encoder-internal.md#51-audio), and the internal
resampler and scaler.

### 12.1 Interface

A stream encoder is built from
`(field, track mode, options, hook, output container)` and provides:

| Operation         | Meaning                                                                               |
| ----------------- | ------------------------------------------------------------------------------------- |
| `mk_stream piece` | called once per container, with its first piece (§5 step 1); creates the muxer stream |
| `encode piece`    | consume the field's content of the piece                                              |
| `flush ()`        | drain every stage the stream encoder holds into the muxer (§12.8)                     |
| `codec_attr ()`   | RFC 6381 codec string, or none                                                        |
| `bitrate ()`      | bit rate, or none                                                                     |
| `video_size ()`   | (width, height), or none                                                              |
| `delay ()`        | the stream's signalled delay, in samples at the codec's rate, or none                 |
| `padding ()`      | the stream's signalled padding, known at its end, or none                             |

### 12.2 Stream creation

At `mk_stream`, from what parameters; which checks run at build (§4). Every
stream is created at the container's first frame, in track order. When the
stream's parameters can differ between sources, how a difference is detected
and reported: an incompatible source is reported to the writer, which
restarts or fails (§13).

### 12.3 Input

What the stream reads from the piece's field. Every piece holds at least one
sample or one image period (§2).

### 12.4 Conversion

What is done to the content before the muxer, and which stages hold data.

### 12.5 Timestamps

Owned by [timestamps.md](timestamps.md); each implementation links its section.

### 12.6 Writing

Which bindings write call, with which time base.

### 12.7 Keyframes

Every packet goes through the writer's hook (§8.2) with its key flag before
the muxer receives it.

### 12.8 Flush and end

Every stage the stream encoder holds, and how `flush` drains it
([draining.md](../../../source/spec/draining.md)): upstream first, into the muxer, before the
container closes. What the container close drains (the codec). What a
restart (§13) does.

### 12.9 Properties

What `codec_attr`, `bitrate`, `video_size`, `delay` and `padding` report and
from when; the writer's own properties are
[ffmpeg.md §8, §10](../../../encoder/spec/ffmpeg.md#10-properties).

### 12.10 Options and errors

Which options it consumes; every error it raises.

### 12.11 Logs

Every message, by level.

## 13. Container restart

A **container restart** ends the current container and continues the
writer in a new one, in the same sink, as a new stream with its own
`stream_start` and header
([ffmpeg.md §5](../../../encoder/spec/ffmpeg.md#5-bytes-and-markers)
item 5). It is triggered only by an incompatible copy source
([encoder-copy.md §12.7](encoder-copy.md#127-parameters-across-sources),
§13.3); a metadata change never restarts a container (§6).

### 13.1 Containers with a clean restart

| Container and target                                     | Restart                                                                                                                                            |
| -------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------- |
| Ogg, on a sink                                           | chaining: a new stream whose logical bitstreams follow in the same sink                                                                            |
| any muxer on a `segmented(D)` sink                       | a new segment that starts a new container, after a discontinuity, with its own init segment when the format has one (§8.4, §8.7)                   |
| any muxer on a delegated destination                     | none in place: the writer fails with an output fault and the output builds a new writer under its restart policy ([io.md §11.3](io.md#113-errors)) |
| anything else (MP4, Matroska, MPEG-TS, FLV to a file, …) | none                                                                                                                                               |

Reason for Ogg: a chained Ogg stream is valid Ogg (RFC 3533), so the
output stays conformant, and players are expected to support it.

### 13.2 What a restart does

1. Every stream encoder drains (§12.8), in track order.
2. The container is closed (codecs drained, trailer written).
3. A new container is built by §3 step 3, with new stream encoders, and the
   container metadata of §6 is set on it. After a restart on a segmented
   sink in the ISO base media
   family, its `movflags` also carry `+frag_discont`. Reason: the muxer
   otherwise starts the new container's fragment times at 0, whatever the
   packet timestamps.
4. The new container's streams are created (§5 step 1) from the current
   content of each field (a copy stream: the parameters of its current run,
   [content.md §4.2](content.md#42-run-layer)), at once (§13.3), and its
   header is written with its `stream_start` (§5 step 2).
5. In the new container, each copy stream writes nothing before a key packet
   unless its codec is intra-only; packets dropped this way are counted and
   logged at info level. Each encoded stream starts from a new codec, so with
   a key frame.

A restart at a source switch cuts the piece at the switch position: the
first run whose parameters the container cannot follow, over every copy
track, found before the piece is encoded. Every track writes the part before
it into the old container and the part after it into the new one, so no
packet of the new source goes into the old container, every stream of the
new container starts at the switch, and no packet of the switching piece is
lost.

**Timelines.** An Ogg link starts its own timeline: the new stream encoders
count from 0. A restart on a segmented sink keeps the variant's timeline
([timestamps.md §2](timestamps.md#2-lifetimes-what-resets-what)): each new
stream encoder takes over the output counters of the one it replaces (the
sample count of an audio stream, the output frame number of a video stream,
the output position of a subtitle stream). DTS stays strictly increasing per
stream across the restart: the new codec's first packet DTS is at or after
the previous segment's end on the variant timeline. The counters therefore
shift forward by the new codec's initial delay, read when the codec opens
(audio: its priming, AAC 1024 samples; video: its reordering delay), so the
first frame's pts is the predecessor's next pts plus that delay. Priming does
not overlap the previous segment. The discontinuity before the new segment
(§8.7) tells the player to reset its decoder there
([timestamps.md §2](timestamps.md#2-lifetimes-what-resets-what)).
Copy per-source state across a restart:
[encoder-copy.md §12.5](encoder-copy.md#125-lifetime-of-per-source-state).

### 13.3 Incompatible copy source

Copy compatibility is split by what a container can survive
([content.md §3.1](content.md#31-ffmpegcopy) owns the property lists):

- **Type properties** (media, codec id, bitstream form such as AVCC against
  Annex B or ADTS against raw AAC, dimensions, sample rate, channel layout,
  and pixel or sample format when both are set): no container survives a
  change. The type merge rejects such a source when it opens; the encoder
  never sees it.
- **Header properties** (extradata, profile, level): the type merge rejects
  them too, so the first source of a track commits them. They reach the
  encoder only through a path that bypasses the merge; there the copy
  stream encoder judges them at a source switch, by the container
  ([encoder-copy.md §12.7](encoder-copy.md#127-parameters-across-sources)).
- **Descriptive properties** (colour range, primaries, transfer, colour
  space, chroma location, field order, header side data such as the display
  matrix, HDR mastering and content light): a difference logs a warning
  naming both values. The stream keeps the first header. They never restart,
  stop or reject.

At a switch, a difference the container carries in band, by the table of
[content.md §3.1.1](content.md#311-where-copy-parameters-are-merged), is
written as it comes. Any other one restarts the container where the target
defines a restart (§13.1); otherwise, under the relaxed setting, the packets
go to the muxer, which judges them; otherwise the output stops.

- A restart logs both parameter sets at warning level.
- A stop fails the writer with an unrecoverable error
  ([clock.md §11](../../../clock/spec/clock.md#11-failure)) whose message
  names the field, the property that differs and both parameter sets, and
  says to encode the sources with identical settings. The clock logs it
  without a backtrace: the one data-driven case the user chose to stop an
  output (D2).
- With `settings.ffmpeg.content.copy.relaxed_compatibility_check` on, the
  type merge reduces to media and codec id. This check still restarts where
  the target can; elsewhere the muxer is the judge: a packet it rejects is an
  item fault (§5.2).

Consequence: the merge applies the same table before a source opens, with
the containers the encoders typed their copy tracks with
([encoder-format.md §10](encoder-format.md#10-content-type-implied-by-a-format)):
a source with another change is rejected when it opens. HLS and `output.url`
restarts are not known from the type: those outputs follow such a change only
with the relaxed setting.

### 13.4 Conformance

- A restart at a source switch in an Ogg output written to `output.file` and
  to `output.harbor`: both give a valid chained Ogg stream whose links decode
  in order, each link after a `stream_start` of its own.
- A restart at a source switch in an Ogg output on a sink: every packet of
  the new source in the switching piece is in the new link.
- The same switch on `output.url`: an output fault, and a new writer under
  the output's policy.
- An HLS variant with re-encoded AAC and copied video, restarted at a copy
  switch: the audio pts of the new segment continue the previous segment's
  timeline.
- A muxer that rejects one packet: the packet is missing, a warning names the
  field, and the output keeps streaming.
- An MPEG-TS file output copying H.264 from a source, then from a source with
  other extradata in Annex B form: the output accepts the change and decodes
  across the switch.
- An MP4 file output with the same sources: the output stops at the switch
  with an error naming both parameter sets, and every packet before the
  switch is in the file.
- The same as an fMP4 HLS variant: a new segment, after a discontinuity,
  starts at the second source's first key frame with its own init segment and
  decodes on its own.
- A playlist of files that differ only in rotation, HDR metadata or colour
  properties, copied to Matroska: every file plays, and each difference logs
  one warning.
