# The `%ffmpeg` encoder: container and stream-encoder contract

This file owns the lifecycle of one `%ffmpeg` encoder instance: how the
container is opened, how its streams are created and fed, how options are
checked, how metadata, draining, stopping and container restarts work, how an
instance cuts HLS segments and what the HLS output does with them, and the
contract every stream encoder meets (§12). It relies on the format record of
[encoder-format.md](encoder-format.md), on the output container operations of
the bindings ([avformat.md §4.4](../../../../modules/synced/ffmpeg/spec/avformat.md#44-output)),
on the draining rule of [draining.md](draining.md), and on the generic encoder
interface of Liquidsoap (stated in §2 as far as this encoder relies on it).
Stream implementations are in [encoder-internal.md](encoder-internal.md) and
[encoder-copy.md](encoder-copy.md); every timestamp rule is in
[timestamps.md](timestamps.md).

## 1. Entry points and call order

The generic encoder layer asks every registered encoder plug-in whether it
accepts a format; the `ffmpeg` plug-in accepts exactly the `%ffmpeg` formats.
Accepting returns a factory:

```
factory : ?hls:bool -> ?interrupt:(unit -> bool) -> pos:position option ->
  name:string -> metadata -> encoder
```

`hls` defaults to `false`; `name` names the instance in logs. `interrupt`
is passed to the bindings' open of a `Url` target and covers every blocking
I/O of that container, so an output can abort a blocking open or write at
shutdown ([io.md §3](io.md#3-interrupting-blocking-libav-calls)). Calling the
factory builds the instance (§3), then calls `encode_metadata` with the given
metadata (§6).

The returned `encoder` has:

| Operation           | Called by                                  |
| ------------------- | ------------------------------------------ |
| `encode frame`      | every output, once per (sub-)frame of data |
| `encode_metadata m` | outputs, when a frame carries metadata     |
| `header ()`         | Harbor, at listener connect (§10)          |
| `stop ()`           | outputs, when they stop or reopen          |
| `hls.*`             | the HLS output only (§8)                   |

The generic layer serialises the operations of one instance: two operations
of the same instance never run at the same time.

An instance is in one of four states:

| State     | Entered by                                      | Left by                                                                                                                                                                                         |
| --------- | ----------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Built`   | §3                                              | the first `encode` (§5) → `Started`                                                                                                                                                             |
| `Started` | §5 step 1; a restart at a source switch (§13.2) | `stop` → `Stopped`; an output fault → `Failed`; a metadata restart (§13) → `Built` on a new container; a restart at a source switch → `Started` on a new container, its streams created at once |
| `Failed`  | an output fault (§5.2)                          | `stop` → `Stopped`                                                                                                                                                                              |
| `Stopped` | §7                                              | none                                                                                                                                                                                            |

In `Failed`, every operation except `stop` raises the recorded error again;
`stop` releases the container without draining
([draining.md §5](draining.md#5-discarded-on-purpose)). In `Stopped`, `stop`
does nothing and every other operation is an error.

## 2. What the encoder relies on from callers

- Frames passed to `encode` are the output's frames or sub-frames. A frame of
  length 0 carries no data; it can still trigger stream creation (§5).
- Metadata is passed before the data of the same sub-frame (the outputs'
  default "metadata first" order).
- The content of every field the format names is present in each frame with
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
2. **HLS defaults**, only when `hls` is true:
   - `format="mp4"`: container option `movflags` defaults to
     `"+frag_custom+dash+delay_moov"` (fragments are cut only when the
     encoder asks, §8.2).
   - `interleaved=Default` becomes `False`; an explicit value is kept.
     Reason: an interleaving muxer holds packets until every stream has
     written, which empties the first segment when a stream is sparse (the
     timed-ID3 stream of §8.5).
3. **Container** (also run again by a restart, §13):
   1. Container options = a copy of the format's `opts`. Two keys given as
      names are converted: `sample_fmt` to FFmpeg's sample-format id and
      `channel_layout` to FFmpeg's canonical description. An unknown name or a
      non-string value is an error of kind `encoder`.
   2. The muxer is guessed from `format` (short name), the `Url` target as
      file name, and `mime_type`. None found is allowed for `Url` (FFmpeg
      guesses from the URL).
   3. `interleaved`: `Default` → true when the format has more than one
      non-drop track; `True`/`False` as written. Reason: a single-stream
      output has nothing to interleave. With interleaving on, a sparse
      stream (subtitles) delays the other streams by at most the muxer's
      `max_interleave_delta`, a container option the user may set.
   4. Open:
      - `Stream` output: no muxer found is an error of kind `encoder` at the
        format's position (example messages: `Format is required!` when
        `format` is none, else
        `No ffmpeg format could be found for format="<f>"`). The container writes into an in-memory buffer through a
        write callback. A seek callback over that buffer is given only for
        HLS with `format="mp4"`.
      - `Url u` output: the bindings open `u` themselves, with the guessed
        muxer if any and the factory's `interrupt`. Nothing is written to
        the buffer.
   5. Stream encoders: every non-drop track builds its stream encoder, in
      track order (§12). No muxer stream is added yet (§5). An encoded track
      with no `codec` is an error of kind `encoder` naming the field.
   6. Options are checked (§4).
4. **Initial metadata**: `encode_metadata` with the metadata given to the
   factory (§6).

Every failure raises out of the factory and releases what was built. A failed
open of a `Url` target is an output fault; every other failure here is a
configuration error ([faults.md §2](faults.md#2-classes), §5.2).

### 3.1 Conformance

- `%ffmpeg(%audio(codec="aac"), %video.drop)` opens a non-interleaved
  container.
- A stream with `q=2` and no `flags` gets both the qscale and the global-header
  flag.

## 4. Option handling

There are two option sets:

- **Container options** (format `opts`, plus the HLS and output defaults of
  §3 and §9): consumed by the bindings' open (generic container options, muxer
  private options, then the I/O protocol).
- **Stream options** (track `opts`, plus `flags`): consumed by the stream
  encoder's creation (codec, device context, generic codec options).

Every option MUST be consumed. All leftovers, container and stream alike, are
reported in **one** error of kind `encoder` at the format's position, that
names each leftover key with its scope (the container, or the field). Example
message: `Unrecognized options: container: foo=3; audio: bar="x"`.

The check runs at build (§3 step 3.6), before the first byte of media is
written. Stream encoders whose muxer stream is created at the first frame
(§5) check their option **names** at build against the options the codec,
its device context and FFmpeg's generic codec options list
([avutil.md §9.3](../../../../modules/synced/ffmpeg/spec/avutil.md#93-listing-options)),
and check again at stream creation, where a leftover raises the same error.
Reason: a bad option is a configuration error and is reported when the output
starts, not when the first frame arrives.

A format whose options are all valid but unusable together fails at FFmpeg's
open or at stream creation with FFmpeg's error.

### 4.1 Conformance

- `%ffmpeg(format="adts", foo=3, %audio(codec="aac", bar="x"))` fails at
  output start with one error naming both `foo` and `bar`.
- `%ffmpeg(%video(codec="libx264", nosuchopt=1))` fails at output start,
  before any frame is encoded.

## 5. Encoding a frame

`encode frame`:

1. At the container's first frame in which every copy field has a run with
   known parameters ([content.md §4.2](content.md#42-run-layer)), every
   stream encoder creates its muxer stream (§12.2), **in track order**.
   Without copy fields, that is the container's first frame, even of length 0. Frames before it write nothing for any stream, so encoded streams do
   not start ahead of the copied ones; their content is held, not
   discarded. The wait is bounded by `max_keyframe_wait`
   ([encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start)):
   past it, the output fails with an output fault naming the copy fields
   without parameters ([faults.md §2](faults.md#2-classes)), and its restart
   policy applies. Copy outputs are an expert feature: the script author
   who feeds one is expected to supply its streams, and a container missing
   a declared track is not produced. Reason: stream parameters of video
   and copy tracks come from the first frame, and creating every stream at
   the same moment makes the container's stream order the written track
   order.
2. Every stream encoder's `encode frame` runs, in track order.
   After the field loop, the copy tracks' end-of-frame step runs: source
   groups are evaluated and every group whose members are all ready starts
   ([encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start)).
3. When the container header has been written and writes are queued (§8.5),
   they run in queue order.
4. The bytes the buffer holds are taken and returned (empty for a `Url`
   output).

The container header is written by the bindings on the first packet or frame
written to any stream.

### 5.2 Data faults and output faults

Every fault the encoder detects belongs to one class of
[faults.md §2](faults.md#2-classes). The encoder is part of an output, so it
has no source to end: its faults are configuration errors, item faults or
output faults.

| Fault                                                                                                                                                                                                           | Class               | Outcome                                                                                                                                                                                                                                                                             |
| --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| bad option, unknown codec or muxer, missing codec, invalid argument (§3, §4, [encoder-format.md §13](encoder-format.md#13-errors-summary))                                                                      | configuration error | raised at build: the output fails to start and the script fails before streaming                                                                                                                                                                                                    |
| a stream that cannot be created, deterministically from the configuration (codec or muxer lookup, a parameter rejected for the configured format), or at the output's first start-up (hardware device included) | configuration error | hard error ([faults.md §2](faults.md#2-classes)): the script fails                                                                                                                                                                                                                  |
| a stream that cannot be created at a later rebuild (restart, reopen, HLS restart): hardware session caps, device resets                                                                                         | output fault        | as below: the output restarts under its policy                                                                                                                                                                                                                                      |
| a stream that cannot be created because of the source's data (copy parameters the muxer rejects)                                                                                                                | output fault        | as below                                                                                                                                                                                                                                                                            |
| the codec refuses a frame, the muxer or its bitstream filter refuses a packet, a subtitle is too large, a timed-ID3 tag fails                                                                                   | item fault          | the item is dropped with a warning; the stream, the container and the other tracks go on                                                                                                                                                                                            |
| `max_consecutive_faults` item faults in a row on one field                                                                                                                                                      | item fault          | copy field: the copied source's group restarts on its next keyframe ([encoder-copy.md §10](encoder-copy.md#10-options-and-errors)); encoded field: that stream's codec is rebuilt (§12.8), since a codec in an error state is not a data fault; a failed rebuild is an output fault |
| the byte sink fails (I/O error, failed open of a `Url` target), the header write fails, a restart cannot build its container                                                                                    | output fault        | instance `Failed` (§1); the error propagates out of `encode`                                                                                                                                                                                                                        |
| an incompatible copy source with no clean restart (§13.3)                                                                                                                                                       | output fault        | as above                                                                                                                                                                                                                                                                            |
| a drain or close fails in `stop` (§7)                                                                                                                                                                           | output fault        | logged at error level; teardown continues                                                                                                                                                                                                                                           |
| no split point by the HLS bound (§8.7)                                                                                                                                                                          | output-side wait    | the segment is cut without the condition, with a warning ([faults.md §5](faults.md#5-waiting))                                                                                                                                                                                      |

Item faults are counted per field against `max_consecutive_faults`
([faults.md §4](faults.md#4-item-faults-escalate)). A data fault never stops
the output. A run past the bound on a copy field restarts that source's
group (per tenure, [encoder-copy.md §10](encoder-copy.md#10-options-and-errors));
later runs restart it again. On an encoded field the stream's codec is
drained as far as it can be and rebuilt with the same parameters, continuing
the stream's counters (as across an HLS restart, §13.2); if the rebuild fails,
the output fault restarts the output. The first drop of each kind is logged as a warning; repeats are
logged with their count at most once per `fault_log_interval`
([faults.md §4](faults.md#4-item-faults-escalate)).

An output fault reaches the script through the output's restart policy and
callbacks ([faults.md §2](faults.md#2-classes),
[io.md §11.3](io.md#113-errors)). The clock and the other outputs go on.

### 5.1 Conformance

- `%ffmpeg(%video(codec="libx264"), %audio(codec="aac"))` gives stream 0
  video and stream 1 audio.

## 6. Metadata

`encode_metadata m`:

1. The entries of `m` are added onto the format's own `metadata`: on a
   shared key the value from `m` wins, and the format's other entries are
   kept.
2. If the container header is not written yet: the list replaces the
   container's metadata.
3. Else, when the container defines a metadata restart (Ogg, §13.1): the
   container is restarted (§13) and the list is set on the new container.
   Reason: Ogg carries a metadata update by chaining a new logical bitstream,
   which a reader decodes as a new track.
4. Else the update is not carried in the container. This is intended: the
   container has no in-stream update, and an output carries metadata by its
   own means where it has one (Icecast metadata updates, HLS timed ID3,
   §8.5). The encoder logs the dropped update at debug level.

The bytes produced by a restart stay in the buffer and are returned by the
next `encode` or `stop`.

### 6.1 Conformance

- An Ogg Vorbis `output.file` with a metadata change mid-stream gives a
  chained Ogg file whose second link carries the new title.

## 7. Stop

`stop ()`, from `Started` or `Built`:

1. Every stream encoder drains (§12.8), in track order; a copy stream writes
   the packets it holds ([encoder-copy.md §8](encoder-copy.md#8-flush-and-end)).
   Each stage's drained output reaches the muxer ([draining.md §3](draining.md#3-order)).
2. The container is closed. Closing drains every codec and writes the
   trailer ([avformat.md §2.1 Release](../../../../modules/synced/ffmpeg/spec/avformat.md#21-container--input-container-output-container)).
3. The buffer is returned.

A drain or close that fails is logged at severe level with the error and
teardown continues ([draining.md §5](draining.md#5-discarded-on-purpose)); the
bytes produced so far are returned. A container that never received a packet
is closed without header or trailer. From `Failed`, `stop` releases the
container without draining and returns nothing.

### 7.1 Conformance

- One second of 44100 Hz stereo sine through
  `%ffmpeg(format="adts", %audio(codec="aac"))` gives as many AAC packets as
  the FFmpeg command line gives for the same input (45), and the decoded
  output holds at least the 44100 input samples.
- A re-encoded video file of N frames at the output frame rate gives N frames.

## 8. HLS

Only with `hls=true`. The HLS output calls `init`, then `init_encode` until it
returns an init segment, then `split_encode` for every later frame, and
`stop` at the end. §8.7 states what the HLS output does with the results.

### 8.1 `init ?id3_enabled ?id3_version ()`

By the muxer's name:

| Muxer                        | `id3_enabled` | Result                                                                                                                        |
| ---------------------------- | ------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| `mpegts`                     | not `false`   | a timed-ID3 data stream is added (§8.5); returns `true`                                                                       |
| `mpegts`                     | `false`       | returns `false`                                                                                                               |
| `adts`, `mp3`, `ac3`, `eac3` | `false`       | error: packed audio requires ID3                                                                                              |
| `adts`, `mp3`, `ac3`, `eac3` | otherwise     | `insert_id3` returns a raw ID3 tag to prepend to the segment (the generic HLS ID3 builder, version default 3); returns `true` |
| other                        | `true`        | error: the format carries no HLS ID3 metadata                                                                                 |
| other                        | not `true`    | returns `false`                                                                                                               |
| none (no muxer)              | any           | error: a format is required                                                                                                   |

Reason for the packed-audio row: an HLS packed-audio segment carries its
timestamp in an ID3 tag (RFC 8216 §3.4).

### 8.2 Keyframe hooks

A **split point** is a position in the instance's output bytes such that, for
every participating stream, the first packet of that stream written after the
position is a key packet. Participating streams are the muxer streams of
non-drop tracks; the timed-ID3 data stream does not participate. A subtitle
packet is decodable on its own: subtitle streams participate and every
subtitle packet counts as a key packet. Reason: a segment is fetched and
decoded on its own, so every stream in it starts at a key packet.

Each stream encoder calls the instance's hook before it gives a packet to the
muxer, with the packet's key flag and media time ([§12.7](#127-keyframes)).
A stream is **all-key** while every packet it has written is flagged key
(audio, subtitles, intra-only codecs). Candidates are taken only where a cut
can be useful:

1. Before a key packet of a participating stream that is not all-key, and
   before the first packet at or after each grid boundary given by the HLS
   output (§8.4), the hook flushes the muxer, so that the buffer holds
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
3. Bytes written after an undecided candidate are held by the instance and
   returned in order once it is decided; no byte is dropped.

The muxer is flushed at each candidate and the instance asks for no other
flush. With fragmented MP4 (`frag_custom`) a flush ends a fragment, so a
segment holds one fragment per candidate it contains.

### 8.3 `init_encode frame`

The init segment is the container's header: the bytes the muxer writes before
its first media data (fragmented MP4: `ftyp` and `moov`; WebM: the EBML
header, segment information and tracks). For a format without one (MPEG-TS,
ADTS, MP3) it is none.

`init_encode ~boundary frame` encodes the frame (§5). It returns
`Init (init, pieces)` once the header is known, where `pieces` (§8.4) hold
every byte after the header; for MP4 they start with the `styp` box. Until the header is known it returns
`Pending`, and the bytes stay in the buffer for the next call: no frame's
data is lost while the HLS output retries. An init segment MUST NOT be empty
for a format that has a header.

The `styp` box (24 bytes):
`00 00 00 18 's' 't' 'y' 'p' 'm' 's' 'd' 'h' 00 00 00 00 'm' 's' 'd' 'h' 'm' 's' 'i' 'x'`
— size 24, major brand `msdh`, minor version 0, compatible brands `msdh`, `msix`.
It starts every MP4 media segment.

### 8.4 `split_encode frame`

```
split_encode : boundary:time -> force:bool -> frame -> piece list
piece = Bytes of bytes
      | Split of { time : time; forced : bool }
      | Restart of { time : time; init : bytes option }
```

`boundary` is the next grid boundary of the HLS output, in media time (§8.7).
The frame is encoded (§5) and its output is returned as pieces, in byte
order:

- `Bytes`: output bytes no longer held (§8.2).
- `Split`: an accepted split point between the bytes before and after it,
  with its media time. For MP4, the bytes after it start with the `styp`
  box.
- `Restart`: a container restart (§13) during the call. It is a split point
  that also starts a new container, with `init` its init segment (none for
  formats without one). It is returned in the call in which the new init
  segment is known (§8.3); the bytes after it stay held until then.

Each encoded video stream makes its first image at or after `boundary` a key
frame (§12.7), so encoded variants have a split point at each boundary.

With `force` true, the instance flushes the muxer before the frame is
encoded and returns a `Split` with `forced` true at that position, without
its condition: some stream of the next segment then starts with a packet
that is not a key packet. An instance that has made an encoded stream's
image at `boundary` a key frame and has not yet seen that key packet ignores
`force`: the split point is coming, at the boundary, and the encoder's delay
only holds it back. Reason: `force` is decided on the media time given to the
encoder, which runs ahead of the packets by the codec's delay (lookahead,
B-frames), and a split at the boundary keeps the segment within the target
duration whatever that delay. An instance with a restart whose `Restart` piece
is not yet returned ignores `force` as well: the restart is the split point,
at the switch, and the segment before it ends there.

Holding (§8.2 step 3) only delays bytes, and never beyond a forced split.

### 8.5 Timed ID3 in mpegts

`init` adds a data stream of codec `timed_id3`, time base `1/audio_rate`.
`insert_id3 ~frame_position ~sample_position m` builds an ID3v2 tag (version
default 3), makes a packet with pts = dts on the media timeline (output
position minus `skipped`) as given by
[timestamps.md §8](timestamps.md#8-timed-id3-hls-mpegts), and writes it to that
stream. The timed-ID3 stream is generated by the encoder: it is not a member
of any copy group and does not count in the whole-output switch test
([timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase)). Until the first media packet has reached the muxer, the write is queued;
the queue runs after the frame that wrote that packet, in order. Reason: a muxer fixes its shift of
negative timestamps (codec priming) from the first packet it receives, so
the media streams must reach it first. It returns none (the tag is in the stream, not
prepended).

### 8.6 Properties

| Query                     | Answer                                                                                                      |
| ------------------------- | ----------------------------------------------------------------------------------------------------------- |
| `codec_attrs ()`          | the codec strings of the participating streams, joined by `,` in track order; none when any stream has none |
| `independent_segments ()` | true when no participating stream is a copied stream whose codec is not intra-only (§8.7 item 6)            |
| `bitrate ()`              | the sum of the bit rates the streams report; none if none                                                   |
| `video_size ()`           | the single stream that reports a size; none with zero or several                                            |

A stream's codec string is the bindings' `codec_attr` of the stream
([avformat.md §4.4](../../../../modules/synced/ffmpeg/spec/avformat.md#44-output)).
When the bindings give none, the stream falls back on its codec id: AAC →
`mp4a.40.2`, MP3 → `mp4a.40.34`; otherwise none. When any stream has no codec
string, `codec_attrs` is none and the encoder logs a warning naming the
stream; the HLS output then omits `CODECS` (§8.7 item 7). Reason: the HLS `CODECS` attribute MUST list every format present in
the variant (RFC 8216 §4.3.4.2); a partial list is worse than none.

Streams exist from the first frame (§5), so the properties are known once
`init_encode` has returned `Init`. The HLS output reads them after that.

### 8.7 Segment grid (HLS output side)

The HLS output owns the segment grid; this section is its contract with the
encoder, because the grid is only correct together with §8.4.

1. **Grid.** Boundary _k_ is at `k × segment_duration` of media time (the
   output position minus `skipped`,
   [timestamps.md §1.4](timestamps.md#14-the-output-timeline)), counted from
   the variant's start, in absolute terms. A boundary is never computed
   from the previous segment's actual end.
2. **Request.** With every frame, the HLS output passes the next boundary
   _k_ to `split_encode`.
3. **Cut.** The HLS output appends pieces to the open segment and keeps the
   bytes after the segment's last split point pending. Once a split point
   with time after _k_ arrives, or the stream ends, the segment is cut at the
   last split point at or before _k_ inside the segment, and the pending
   bytes after it start the next segment. When the segment holds no split
   point at or before _k_, the cut is the first split point after _k_. A
   segment is never cut anywhere but at a split point.
4. **Leftovers.** The duration of a segment, and its `EXTINF`, is the media
   time between its two cuts. A segment that ends early or late is shorter
   or longer, and the next one correspondingly longer or shorter: the grid
   does not move. Reason: codec frames do not match internal frames
   (2048-sample HE-AAC against 960-sample frames), and a grid that moved by
   the leftover would drift by a codec-dependent amount, so variants of one
   source would drift apart.
5. **Extra cuts.** The HLS output MAY cut at any other accepted split point
   to open a segment for pending metadata or an inserted tag. An extra cut
   does not move later boundaries. The HLS output calls `insert_id3` when it
   opens a segment, with the segment's start position, so each segment that
   needs a tag carries it from its start (§8.1, §8.5).
6. **Bound.** The announced target duration is fixed for the playlist and is
   at least `segment_duration`. RFC 8216 requires every `EXTINF`, rounded to
   the nearest integer, to be at most the target duration. The wait for a
   split point is bounded by `max_split_wait`: the time until the open
   segment's rounded duration would exceed the target duration. When the
   next frame would pass that bound and the segment holds no split point, the
   HLS output passes `force` true. When a forced `Split` comes back, it logs a
   warning naming the variant, and reports it through the output's
   callbacks ([faults.md §5](faults.md#5-waiting)). A forced segment does not
   start at a key packet of every stream, so a variant that can be forced
   MUST NOT announce `EXT-X-INDEPENDENT-SEGMENTS`: the HLS output announces
   it only for variants with no copied stream whose codec is not intra-only
   (encoded streams make key frames at the boundaries).
7. **Codec strings.** When `codec_attrs` is none (§8.6), the HLS output
   omits `CODECS` for that variant with a warning (RFC 8216 makes it a
   SHOULD).
8. **Restart.** On `Restart`, the HLS output ends the segment with `before`,
   marks the next segment as a discontinuity, gives it the new init segment
   when there is one, and starts it with `after`.

### 8.8 Conformance

- MPEG-TS, AAC and H.264 with keyframes every second and 2 s segments: the
  first packet of every stream in every segment is a key packet, and no
  segment ends with a video key packet.
- A video with keyframes every 4 s and 1 s segments, re-encoded: every
  segment starts with a key frame and no `EXTINF` exceeds the target duration.
- The same video copied: no `EXTINF` exceeds the target duration, each
  forced cut logs a warning, and the playlist does not announce
  `EXT-X-INDEPENDENT-SEGMENTS`.
- A muxer that rejects every packet of one encoded field: the output keeps
  streaming the other fields, and the drops are logged with their count at
  most once per `fault_log_interval`.
- An HLS variant with an encoded subtitle track is cut into segments.
- Two variants, HE-AAC and AAC-LC, of one source: after fifty segments, the
  first PTS of segment _n_ is equal across variants to within one codec frame.
- An AAC MPEG-TS HLS output with 2 s segments: the first ten segments sum to
  20 s within one internal frame.
- A WebM and an MP4 variant: the init segment is non-empty and the first media
  segment holds the first frame.

## 9. Outputs with a URL or file target

| Output        | Effect on the format                                                                                                                                                                                                                                                                                                         |
| ------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `output.file` | `output = Url "file:<path>"`, container option `truncate` = 0 when `append=true`, else 1. The output itself creates the file with its permissions and closes it; the encoder writes it, appending when `truncate` = 0 (FFmpeg's `file` protocol has no append mode: `truncate` = 0 alone writes from the start of the file). |

With a `Url` target the encoder returns empty bytes; the output writes nothing
itself. `output.file` with `reopen_*` builds a new instance per file.
`output.url` also sets a `Url` target; its format check, errors and restarts
are in [io.md §1.4](io.md#14-outputurl) and [io.md §11](io.md#11-outputurl).

## 10. Icecast and Harbor

- MIME type, the mp3 container options and the Icecast stream info of a
  `%ffmpeg` format are in [io.md §12](io.md#12-other-outputs-special-casing-ffmpeg).
- `header ()` returns the init segment of the current container (§8.3):
  empty for a format without a header, the header bytes otherwise, and after
  an Ogg restart the new link's header. Harbor sends it to each new listener.
  A listener of a shared encoder then joins at the next `encode` boundary,
  which is decodable for Ogg (whole pages) and for formats without a header.
  For Matroska, WebM and MP4 a reader needs to start at a cluster or fragment
  boundary, which the encoder does not mark;
  `dedicated_encoder=true` gives each listener its own container.

## 11. Failure behaviour summary

Classes as in §5.2 and [faults.md §2](faults.md#2-classes).

| Situation                                        | Behaviour                                                              |
| ------------------------------------------------ | ---------------------------------------------------------------------- |
| factory failure (§3, §4)                         | raises; configuration error, or output fault for a failed `Url` open   |
| data fault in `encode` (§5.2)                    | item dropped with a warning; streaming goes on                         |
| output fault in `encode` (§5.2)                  | raises; instance `Failed` (§1)                                         |
| drain or close error in `stop`                   | output fault; logged at error; teardown continues                      |
| metadata after the header, no restart (§6)       | not carried; logged at debug                                           |
| copy source with other header properties (§13.3) | accepted (in-band container), restart where defined, else output fault |
| no split point by the HLS bound (§8.7)           | forced cut with a warning (output-side wait)                           |

What an output does with a raised error is the output's. `output.url`
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

| Operation            | Meaning                                                                                           |
| -------------------- | ------------------------------------------------------------------------------------------------- |
| `mk_stream frame`    | called once per container, with its first frame (§5 step 1); creates the muxer stream             |
| `encode frame`       | consume the field's content of the frame                                                          |
| `flush ()`           | drain every stage the stream encoder holds into the muxer (§12.8)                                 |
| `request_keyframe t` | make the first encoded image at or after media time `t` a key frame, where the stream can (§12.7) |
| `codec_attr ()`      | RFC 6381 codec string, or none                                                                    |
| `bitrate ()`         | bit rate, or none                                                                                 |
| `video_size ()`      | (width, height), or none                                                                          |

### 12.2 Stream creation

At `mk_stream`, from what parameters; which checks run at build (§4). Every
stream is created at the container's first frame, in track order. When the
stream's parameters can differ between sources, how a difference is detected
and reported: an incompatible source is reported to the instance, which
restarts or fails (§13).

### 12.3 Input

What the stream reads from the frame field. A frame of length 0 carries no
data.

### 12.4 Conversion

What is done to the content before the muxer, and which stages hold data.

### 12.5 Timestamps

Owned by [timestamps.md](timestamps.md); each implementation links its section.

### 12.6 Writing

Which bindings write call, with which time base.

### 12.7 Keyframes

Every packet goes through the instance's hook (§8.2) with its key flag before
the muxer receives it. What `request_keyframe` does.

### 12.8 Flush and end

Every stage the stream encoder holds, and how `flush` drains it
([draining.md](draining.md)): upstream first, into the muxer, before the
container closes. What the container close drains (the codec). What a
restart (§13) does.

### 12.9 Properties

What `codec_attr`, `bitrate`, `video_size` report and from when.

### 12.10 Options and errors

Which options it consumes; every error it raises.

### 12.11 Logs

Every message, by level.

## 13. Container restart

A **container restart** ends the current container and continues the
instance in a new one, in the same byte output. It is triggered by an
in-stream metadata update in Ogg (§6) or by an incompatible copy source
([encoder-copy.md §12.7](encoder-copy.md#127-parameters-across-sources), §13.3).

### 13.1 Containers with a clean restart

| Container and target                                     | Restart                                                                                                                                                |
| -------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Ogg, `Stream` output                                     | chaining: the new container's logical bitstreams follow in the same buffer                                                                             |
| Ogg, `Url` output with a `file:` URL (`output.file`)     | chaining: the file is reopened for appending and the new container appends to it                                                                       |
| any muxer under HLS (`hls=true`)                         | a new segment that starts a new container, after a discontinuity, with its own init segment when the format has one (§8.4, §8.7)                       |
| any muxer of `output.url`                                | a reconnect: the instance fails with an output fault and `output.url` builds a new instance under its restart policy ([io.md §11.3](io.md#113-errors)) |
| anything else (MP4, Matroska, MPEG-TS, FLV to a file, …) | none                                                                                                                                                   |

Reason for Ogg files: a chained Ogg file is valid Ogg (RFC 3533), so the
output stays conformant, and players are expected to support it.

### 13.2 What a restart does

1. Every stream encoder drains (§12.8), in track order.
2. The container is closed (codecs drained, trailer written).
3. A new container is built by §3 step 3, with new stream encoders, and the
   current metadata is set on it.
4. The new container's streams are created (§5 step 1) from the current
   content of each field (a copy stream: the parameters of its current run,
   [content.md §4.2](content.md#42-run-layer)): at the next frame for a
   metadata restart, at once for a restart at a source switch (§13.3).
5. In the new container, each copy stream writes nothing before a key packet
   unless its codec is intra-only; packets dropped this way are counted and
   logged at info level. Each encoded stream starts from a new codec, so with
   a key frame.

A restart at a source switch cuts the frame at the switch position: the
first run whose parameters the container cannot follow, over every copy
track, found before the frame is encoded. Every track writes the part before
it into the old container and the part after it into the new one, so no
packet of the new source goes into the old container, every stream of the
new container starts at the switch, and no packet of the switching frame is
lost.

**Timelines.** An Ogg link starts its own timeline: the new stream encoders
count from 0. An HLS restart keeps the variant's timeline
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
- **Header properties** (extradata, profile, level): judged here, at a
  source switch, by the container. The copy stream encoder compares them
  ([encoder-copy.md §12.7](encoder-copy.md#127-parameters-across-sources)).
- **Descriptive properties** (colour range, primaries, transfer, colour
  space, chroma location, field order, header side data such as the display
  matrix, HDR mastering and content light): a difference logs a warning
  naming both values. The stream keeps the first header. They never restart,
  stop or reject.

Containers by where they keep header properties:

| Container                                                   | Header properties         | A change at a switch                                                       |
| ----------------------------------------------------------- | ------------------------- | -------------------------------------------------------------------------- |
| MP4/MOV, Matroska/WebM, FLV, Ogg                            | stored once in the header | a restart where the target defines one (§13.1); otherwise the output stops |
| MPEG-TS (HLS-TS included), ADTS, Annex B elementary streams | carried in band           | accepted: the packets carry their own configuration                        |

- A restart logs both parameter sets at warning level.
- A stop fails the instance with an error of kind `encoder` that names the
  field and prints both parameter sets, logged at error level: an output
  fault ([faults.md §2](faults.md#2-classes)), the one data-driven case the
  user chose to stop an output.
- With `settings.ffmpeg.content.copy.relaxed_compatibility_check` on, the
  type merge and this check both reduce to media and codec id. Every other
  difference goes to the muxer, which is the judge: a packet it rejects is
  an item fault (§5.2).

Consequence: a live relay whose remote encoder restarts with new extradata
keeps working. An in-band output accepts the change; a header-storing output
restarts or stops by the table.

### 13.4 Conformance

- An Ogg Vorbis output written to a file and to a `Stream` output, with a
  metadata change: both give a valid chained Ogg stream whose links decode in
  order.
- A restart at a source switch in an Ogg `Stream` output: every packet of
  the new source in the switching frame is in the new link.
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
