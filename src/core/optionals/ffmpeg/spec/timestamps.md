# Encoder timestamps (PTS/DTS)

This file owns every rule by which the `%ffmpeg` encoder gives timestamps to
what it writes: where each timestamp comes from, every time-base conversion
and its rounding, every per-stream and per-source offset with the events that
set and reset it, how monotonic order is kept or repaired, and what the muxer
is told. It relies on the stream encoders of
[encoder-internal.md](encoder-internal.md) and
[encoder-copy.md](encoder-copy.md), on the container lifecycle of
[encoder.md](encoder.md), on the content layout (chunk-relative positions in
main ticks, per-chunk source id and time base) of [content.md](content.md),
and on the decoder's timestamps ([decoder.md](decoder.md)) for what arrives in
raw and copy content. Inline encoders and filters describe their own
timestamps and link here.

## 1. Units and conversions

### 1.1 Time bases in play

| Name             | Value                                                              | Used for                                                         |
| ---------------- | ------------------------------------------------------------------ | ---------------------------------------------------------------- |
| main ticks       | `1/main_rate` ([common.md §4](common.md#4-liquidsoap-time-bases))  | content positions and lengths; copy timestamps; subtitle offsets |
| audio sample     | `1/audio_rate` ([common.md §4](common.md#4-liquidsoap-time-bases)) | timed-ID3 data stream                                            |
| video frame      | `1/video_rate` ([common.md §4](common.md#4-liquidsoap-time-bases)) | internal video frame numbers                                     |
| target audio     | `1/samplerate` of the track                                        | audio encoder input                                              |
| target video     | `1/framerate` of the track                                         | video encoder input                                              |
| subtitle         | `1/1000000`                                                        | subtitle input                                                   |
| chunk time base  | carried by each raw or copy chunk                                  | source packet and frame timestamps                               |
| stream time base | the muxer's choice, set when the header is written                 | what is stored in the file                                       |

With the default rates of [common.md §4](common.md#4-liquidsoap-time-bases), a 0.02 s frame is 882
main ticks.

### 1.2 Encoder-side conversion

`convert(v, src, dst)` below is the layer's time-base conversion of
[common.md §5](common.md#5-time-base-conversion) (truncating toward zero).

### 1.3 Bindings-side conversion

`write_packet stream tb packet` converts pts, dts and duration from `tb` to
the stream time base; `write_frame` converts packets from the encoder's time
base to the stream's. Both round to nearest as FFmpeg's rescaling does
([avformat.md §4.4](../../../../modules/synced/ffmpeg/spec/avformat.md#44-output)). A
muxer may replace the stream time base when it writes the header, so the
stored values are in that time base (mpegts 1/90000, matroska 1/1000, nut the
codec's).

## 2. Lifetimes: what resets what

| Event                                                                                                                                                   | Internal / raw audio     | Internal / raw video                                             | Subtitle | Copy                                                                                                                                                 |
| ------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------ | ---------------------------------------------------------------- | -------- | ---------------------------------------------------------------------------------------------------------------------------------------------------- |
| new encoder instance (output start, reopen, new file, `output.url` reconnect, Harbor dedicated listener)                                                | from 0                   | from 0                                                           | from 0   | per-track and per-source state new                                                                                                                   |
| Ogg in-stream metadata ([encoder.md §6](encoder.md#6-metadata))                                                                                         | from 0 (new container)   | from 0 (new container)                                           | from 0   | per-track state new; per-source store kept (§6.5)                                                                                                    |
| metadata, any other case                                                                                                                                | nothing                  | nothing                                                          | nothing  | nothing                                                                                                                                              |
| track boundary in the source                                                                                                                            | nothing                  | nothing                                                          | nothing  | nothing (only the source id matters)                                                                                                                 |
| new source id in the content: new decoder, seek in the file decoder, reconnect, filter restart ([content.md §5](content.md#5-stream_idx-and-time_base)) | nothing                  | raw: new frame-rate converter, re-anchored (§4.3); internal: n/a | n/a      | source switch: rebase (§6.2), then a keyframe wait when `wait_for_keyframe` is set ([encoder-copy.md §12.2](encoder-copy.md#122-accepting-a-packet)) |
| same source id, timestamps restart lower (inline encoder rebuilt after sleep, [inline-codecs.md §9](inline-codecs.md#9-timestamps-inline-part))         | nothing                  | nothing                                                          | n/a      | no switch, no rebase: every packet with `last_start + dts_m ≤ last_dts` is dropped (§6.3) until the new timestamps pass the old ones                 |
| new pixel format or chunk time base (raw video)                                                                                                         | —                        | new converter, re-anchored                                       | —        | —                                                                                                                                                    |
| new audio parameters (raw audio)                                                                                                                        | nothing (resampler kept) | —                                                                | —        | —                                                                                                                                                    |
| HLS segment boundary                                                                                                                                    | nothing                  | nothing                                                          | nothing  | nothing                                                                                                                                              |
| `stop`                                                                                                                                                  | encoder drained by close | converter drained, encoder drained by close                      | nothing  | nothing                                                                                                                                              |

Timestamps of one instance and one container therefore start at 0 and run
for the container's lifetime; HLS segments of one variant share one timeline.

## 3. Audio (internal and raw)

The encoder ignores every timestamp of its input; audio timestamps count
samples.

1. Conversion yields frames at the target rate ([encoder-internal.md §4.1–4.2](encoder-internal.md#41-audio-internal)).
2. Without re-framing (variable-frame-size codec): each frame gets
   pts = `convert(S, 1/samplerate, T)` where `S` is the number of samples
   given to the encoder before it, and `T` is the stream's time base read when
   the stream was created, which is the encoder's `1/samplerate`. So pts = `S`.
3. With re-framing: each converted frame gets pts from a first sample counter
   in the same way before entering the re-framer (whose input time base is
   `T`); each frame the re-framer delivers gets pts from a second counter over
   delivered samples. The encoder sees `0, F, 2F, …` with `F` the frame size.
4. The codec produces packets whose timestamps it derives from these
   (encoder delay included: AAC's first packet has pts −1024 at 48 kHz, from
   the implementation); the bindings convert them to the stream time base.

Consequences:

- Strictly increasing, gap-free pts by construction; nothing checks them.
- A gap, overlap or jump in a raw source's timestamps is erased: raw audio is
  laid end to end.
- Audio time equals the number of samples encoded; it stays in step with the
  other streams only as long as the content delivers exactly the expected
  number of samples per frame.

## 4. Video (internal and raw)

### 4.1 Frame timestamps entering the converter

| Mode     | Input pts                                   | Input time base       | Source id in the converter key |
| -------- | ------------------------------------------- | --------------------- | ------------------------------ |
| internal | image number since the stream began, from 0 | `1/video_rate`        | constant (1)                   |
| raw      | the decoded frame's pts, as carried         | the chunk's time base | the chunk's source id          |

### 4.2 The frame-rate converter

Each converter is the Fps helper graph of [filters.md §16.2](filters.md#162-fps),
built with the start offset `A` of §4.3.

Each frame delivered gets pts = `convert(pts, output tb, T)`, `T` = the
stream time base read at `mk_stream` (the encoder's `1/framerate`), and the
frames-written counter `W` increases by 1. Then `write_frame`.

### 4.3 Building and re-anchoring a converter

A converter is built for the first frame and for every frame whose (pixel
format, time base, source id) differs from the current converter's. Its
anchor is `A = convert(W, 1/framerate, input tb)`: the number of frames
already written, expressed in the input's time base. So the first frame of a
new source continues the output grid where the previous one stopped,
whatever its own pts. Reason (history, #2978, then #5525): raw content from a
new source restarted the PTS and broke continuity; the anchor was later
converted to the input time base because it counts output frames.

The previous converter is dropped without being drained: frames it holds (the
`fps` filter keeps one) are not written.

### 4.4 Within one converter

Non-increasing or missing input pts are passed to FFmpeg's `setpts` and `fps`
filters as they are; the encoder checks nothing. Monotonic output relies on
`fps` producing a monotonic grid ([findings](findings/encoder.md)).

### 4.5 Encoder output

The codec reorders frames (B-frames): packet dts start below pts. Example
(from the implementation, internal 30 fps H.264 in mpegts): first video packet
dts = −2/30 s relative to its pts 0.

## 5. Subtitles

For each subtitle at chunk position `p` in a frame:

- pts = `convert(O + p, 1/main_rate, 1/1000000)`, `O` = the sum of the lengths
  of the frames of positive length this stream has encoded before.
- start and end display time = `trunc(seconds(start or end) × 1000)` ms
  (relative to pts).
- the packet's timestamp and duration follow
  [avformat.md §4.4 `write_subtitle_frame`](../../../../modules/synced/ffmpeg/spec/avformat.md#44-output).

`O` follows Liquidsoap time, so subtitle times are positions in the output
stream. Nothing checks their order.

## 6. Copy streams, per track

Notation: for a packet of a chunk with time base `tb`, `pts_m`, `dts_m`,
`dur_m` are its pts, dts and duration converted to main ticks (§1.2), each
none when absent. `R` is the track's current source record
([encoder-copy.md §12.1](encoder-copy.md#121-model)).

### 6.1 Order of operations

For each packet the track accepts ([encoder-copy.md §12.2](encoder-copy.md#122-accepting-a-packet)):

1. convert to main ticks;
2. monotonicity check (§6.3) — may drop;
3. if the source has not begun for this track: begin (§6.2);
4. `pts' = pts_m + R.last_start`, `dts' = dts_m + R.last_start` (none stays
   none);
5. if `dts'` is some: `R.position = max(R.position, dts' + (dur_m or 1))`;
6. `last_dts = dts'` (none when the packet has none);
7. keyframe hook ([encoder-copy.md §7](encoder-copy.md#7-keyframes));
8. a duplicate with pts `pts'`, dts `dts'`, duration `dur_m` is written in
   main ticks.

`R.last_start` is read at step 4 for every packet, so a later raise of the
shared value (§7) applies to the following packets.

### 6.2 Beginning a source (rebase)

With `last_position` = the previous record's `position` taken at the switch
(0 before the first source) and `head` from
[encoder-copy.md §12.4](encoder-copy.md#124-stream-head-of-a-sparse-source):

```
head_len   = p + (n if head = Pending (s, n) for this source, else 0)
candidate  = last_position − (dts_m or 0) + head_len
R.last_start = max(R.last_start, candidate)     (shared, §7)
```

`p` is the packet's position in its chunk. The first accepted packet of the
source is thus written at `dts' = last_position + head_len` (when no sibling
raised `last_start`): right after everything previously written from the
previous source, plus the time the new source spent before this packet in
Liquidsoap's timeline. Reason (code comment): the next source starts at
exactly the dts where the last one ended, accounting for the first packet's
dts and for empty data preceding it in sparse streams (history: #4860 added
the head for subtitles; #3628 introduced the per-source `position`).

The time between the previous source's last packet and the switch (trailing
empty chunks) is not counted. Packets dropped while waiting for a keyframe do
not count either: the first keyframe is placed right after the previous
source's end, with only its own chunk position as head.

### 6.3 Monotonicity check

Before step 3, with `o` = `R.last_start` if the source has begun, else
`last_position − (dts_m or 0)`:

| `dts_m` | `last_dts` | Condition   | Result                                                               |
| ------- | ---------- | ----------- | -------------------------------------------------------------------- |
| none    | any        | —           | accepted; logs `Packet has no dts!` at important                     |
| some d  | some l     | `o + d ≤ l` | dropped; logs `Dropping packet with non-monotomic dts!` at important |
| some d  | none       | —           | accepted                                                             |
| some d  | some l     | `o + d > l` | accepted                                                             |

So written dts are strictly increasing in main ticks, per track, except
after a packet without dts, which resets `last_dts` to none and lets the next
packet through unchecked. Only dts is checked; pts is neither checked against
dts nor against previous pts. A dropped packet is dropped from the output
entirely (it is not shifted). Reason (history, 19f6d3de6): packets without
dts were dropped at first; they are accepted because some streams carry
none.

Before the source has begun the check uses `last_position` without the head
and without a sibling's `last_start`, so the value checked can be lower than
the value written; both are above `last_dts` since `last_position` >
`last_dts`.

### 6.4 Missing values

| Missing  | Effect                                                                                                  |
| -------- | ------------------------------------------------------------------------------------------------------- |
| dts      | accepted (§6.3); rebase uses 0 for it; `position` not advanced; `last_dts` cleared; written without dts |
| pts      | written without pts                                                                                     |
| duration | `position` advanced by 1 tick; written without duration                                                 |

### 6.5 Restarts

An Ogg restart builds new copy tracks with `last_position` 0 and no
`last_dts`, which start the new container from 0 — unless their first lookup
finds a record of the same source still held in the encoder's store, in which
case its larger `last_start` is kept by the max and the new container's
timestamps continue from the old offset. A record no track holds is forgotten
at an unspecified time, so either can happen
([encoder-copy.md §12.5](encoder-copy.md#125-release-and-lifetime-of-source-records), [findings](findings/encoder.md)).

## 7. Copy streams, across tracks

Tracks on the same source share `R`:

- `R.position` is the greatest end written by any of them; the first track to
  switch away reads it at that moment, so a sibling's packets written later
  in the same frame are not in its `last_position`.
- `R.last_start` is the greatest candidate of the tracks that have begun.
  Once all tracks of the source have begun, they share one offset, so the
  source's own relation between its streams (e.g. audio dts −23 ms against
  video dts −80 ms at its start) is kept.
- A track that begins before a sibling with a larger candidate writes its first
  packets with its own smaller offset; when the sibling begins, the offset
  rises and the first track's next packet jumps forward by the difference.
  (Comment beside the code: "always have the greatest possible offset".)

Tracks on different sources share nothing: each rebases on its own previous
source's end. The encoder keeps no relation between them.

Interleaving across streams is the muxer's: with `interleaved` true the
bindings use FFmpeg's interleaving writer, which orders packets by dts across
streams; with false, packets go to the muxer in call order (field order,
[encoder.md §5](encoder.md#5-encoding-a-frame)).

## 8. Timed ID3 (HLS, mpegts)

`insert_id3 ~frame_position ~sample_position` (positions of the HLS output:
whole frames and remaining main ticks since the output started):

```
pts = dts = audio_of_main(frame_position × frame_size + sample_position)
```

in `1/audio_rate`, `frame_size` in main ticks, `audio_of_main` truncating.
Written to the `timed_id3` data stream with that time base; queued until the
container header exists. It follows Liquidsoap's output time, which matches
the internal audio timeline only when audio starts at the same moment and
runs at the global rate.

## 9. What the muxer is told

| Writer       | Time base passed             | Values                            |
| ------------ | ---------------------------- | --------------------------------- |
| audio, video | encoder time base (bindings) | codec packets from §3 / §4 frames |
| subtitle     | `1/1000000`                  | §5                                |
| copy         | `1/main_rate`                | §6                                |
| timed ID3    | `1/audio_rate`               | §8                                |

The muxer applies its own rules on top (FFmpeg rejects non-increasing dts
for strict muxers with an error, which propagates out of `encode`). A strictly
increasing dts in main ticks can become equal after rounding to a coarser
stream time base ([findings](findings/encoder.md)).

## 10. Summary: how order is kept

| Stream    | Source of order                | Checked                       | Repair                                           | Log                    |
| --------- | ------------------------------ | ----------------------------- | ------------------------------------------------ | ---------------------- |
| audio     | sample counter                 | no                            | input timestamps ignored                         | none                   |
| video     | frame counter + `fps` grid     | no                            | re-anchor on source/format change                | none                   |
| subtitle  | Liquidsoap position            | no                            | none                                             | none                   |
| copy      | source dts + per-source offset | dts only, strictly, per track | rebase on source switch; drop non-increasing dts | important (2 messages) |
| timed ID3 | Liquidsoap position            | no                            | none                                             | none                   |

## 11. Worked examples

All from the implementation: two 2-second files made with the FFmpeg CLI,
each AAC 44.1 kHz stereo (time base 1/44100, first packet dts −1024) and
H.264 25 fps with B-frames (time base 1/12800, first packet dts −1024 =
−0.08 s, pts 0), played by `sequence([once(single(A)), once(single(B))])` into
`output.file(%ffmpeg(format="mpegts", …copy tracks…))`, read back with
`ffprobe -show_packets`, times relative to the file's first dts. Main rate 44100.

### 11.1 `%ffmpeg(format="mpegts", %audio.copy, %video.copy)` (video fed first)

Start of A:

- video first packet: `dts_m` = −1024 × 44100/12800 = −3528; `candidate` =
  0 + 3528 + 0 = 3528; written dts 0, pts 3528 (0.0800 s).
- audio first packet: `dts_m` = −1024; `candidate` = 1024; `last_start` =
  max(3528, 1024) = 3528; written 2504 (0.0568 s).

End of A: the last audio packet that reaches the encoder (source dts 87040;
A's final packet, dts 88064, is not delivered,
[decoder.md §13.1](decoder.md#131-end-of-input)) is
written at 87040 + 3528 = 90568 (2.0537 s) and ends at 91592 (2.0769 s); the
last video
packet is written at dts 1.9600 s and ends at 2.0000 s. A's `position` is
therefore 91592.

Switch to B:

- video keyframe: `dts_m` = −3528, `candidate` = 91592 + 3528 + 0 = 95120;
  written dts 91592 (2.0769 s), pts 95120 (2.1569 s).
- audio first packet: `dts_m` = −1024, `candidate` = 91592 + 1024 = 92616;
  `last_start` = max(95120, 92616) = 95120; written 94096 (2.1337 s).

The audio has a hole of 94096 − 91592 = 2504 ticks (56.8 ms) at the switch;
the video is continuous in dts; B's internal A/V relation is kept.

### 11.2 `%ffmpeg(format="mpegts", %video.copy, %audio.copy)` (audio fed first)

Start of A: audio begins first with `candidate` 1024 → first audio packet
written at 0 (0.0000 s); the video then raises `last_start` to 3528, so the
second audio packet (source dts 0) is written at 3528 (0.0800 s) instead of
1024: a forward jump of 2504 ticks after the first packet.

Switch to B: the first audio packet of B is written at 91592 (2.0769 s),
the second at 95120 (2.1569 s), same jump; video keyframe at dts 2.0769 s.

### 11.3 Internal encode

`%ffmpeg(format="mpegts", %audio(codec="aac", samplerate=48000),
%video(codec="libx264", framerate=30))` over 1 s of `sine()` and
`video.testsrc()`: audio packets every 1920 (90 kHz) = 1024 samples, first
pts −1024/48000 s from video pts 0; video first dts −2/30 s, 30 frames.
