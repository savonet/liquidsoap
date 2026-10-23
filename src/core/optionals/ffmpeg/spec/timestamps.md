# Encoder timestamps (PTS/DTS)

This file owns every rule by which the `%ffmpeg` encoder gives timestamps to
what it writes: the output timeline every stream follows, where each
timestamp comes from, every time-base conversion, every per-source offset
with the events that fix it, how strictly increasing DTS is guaranteed per
output stream, and what the muxer is told. It relies on the stream encoders
of [encoder-internal.md](encoder-internal.md) and
[encoder-copy.md](encoder-copy.md) (switching, readiness, per-source groups),
on the container lifecycle of [encoder.md](encoder.md), on the content layout
(positions in main ticks, per-run source id and time base) of
[content.md](content.md), and on the conversion and absolute-origin rules of
[common.md](common.md#5-time-base-conversion). Inline encoders and filters
describe the timestamps they produce and link here.

## 1. Units and conversions

### 1.1 Time bases in play

| Name             | Value                                                              | Used for                                                    |
| ---------------- | ------------------------------------------------------------------ | ----------------------------------------------------------- |
| main ticks       | `1/main_rate` ([common.md §4](common.md#4-liquidsoap-time-bases))  | content positions and lengths; the output timeline; offsets |
| audio sample     | `1/audio_rate` ([common.md §4](common.md#4-liquidsoap-time-bases)) | timed-ID3 data stream                                       |
| target audio     | `1/samplerate` of the track                                        | audio encoder input                                         |
| target video     | `1/framerate` of the track                                         | video encoder input                                         |
| subtitle         | `1/1000000`                                                        | subtitle input                                              |
| run time base    | carried by each raw or copy run                                    | source packet and frame timestamps                          |
| stream time base | the muxer's choice, final once the header is written               | what is stored in the file                                  |

With the default rates of [common.md §4](common.md#4-liquidsoap-time-bases),
a 0.02 s frame is 882 main ticks.

### 1.2 Encoder-side conversion

Every conversion between time bases is the rescale of
[common.md §5](common.md#5-time-base-conversion): round to nearest, overflow
is an error. Every value is converted from its absolute timestamp, never
accumulated from converted steps
([common.md §6.0](common.md#60-absolute-origin)), so rounding never adds up.

### 1.3 Bindings-side conversion

`write_packet stream tb packet` converts pts, dts and duration from `tb` to
the stream time base; `write_frame` converts packets from the encoder's time
base to the stream's. Both round to nearest
([avformat.md §4.4](../../../../modules/synced/ffmpeg/spec/avformat.md#44-output)).
A muxer may replace the stream time base when it writes the header (mpegts
1/90000, matroska 1/1000, nut the codec's), so a rule about stored values
reads the stream time base after the header is written.

### 1.4 The output timeline

Every stream of one container follows one reference: the **output
position**, in main ticks since the timeline's origin. The origin is the
container's first frame, except across an HLS restart, which keeps the
variant's timeline: there the origin is the first frame of the encoder
instance ([encoder.md §13.2](encoder.md#132-what-a-restart-does)). The
position of an item is the sum of the lengths of the frames of positive
length encoded since the origin before its frame, plus its position in the
frame
(its run's start plus its position in the run,
[content.md §4.2](content.md#42-run-layer)).

- Re-encoded audio counts samples; one sample at the target rate is one
  sample of output position (§3).
- Re-encoded video and subtitles take their time from positions (§4, §5).
- Copied streams keep their source's own spacing within one source and are
  anchored on the output position at each switch, less the time skipped by
  whole-output switches (§6.2).
- Timed ID3 takes its time from the media timeline: the output position
  less `skipped` (§8).

Reason: an output that mixes copied and re-encoded streams, or copies streams
from different sources, keeps them in step only if they share a reference.
Time a copied stream skips at a switch (packets dropped before a keyframe,
the trailing silence of a sparse stream) then remains in the timeline
instead of moving that stream against the others. When no other stream
continues through a switch, nothing needs that time kept, and a
whole-output switch closes the keyframe wait instead (§6.2). The **media
timeline** is the output position less `skipped`; the HLS segment grid and
`EXTINF` run on it too ([encoder.md §8](encoder.md#8-hls)).

Conformance:

- Copy video and re-encode audio over a playlist of fifty files whose video
  starts mid-GOP: after the last join, audio and video of the last file start
  together to within one video frame, as they do in the file.

## 2. Lifetimes: what resets what

| Event                                                                                                                                                                                                                                                                                                                                                     | Re-encoded audio                                                                                                                                                                     | Re-encoded video                                  | Subtitle     | Copy                                                                                                                   |
| --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------- | ------------ | ---------------------------------------------------------------------------------------------------------------------- |
| new encoder instance (output start, reopen, new file, `output.url` reconnect, Harbor dedicated listener)                                                                                                                                                                                                                                                  | from 0                                                                                                                                                                               | from 0                                            | from 0       | from 0; no group                                                                                                       |
| Ogg restart (chaining, or an incompatible copy source; [encoder.md §13](encoder.md#13-container-restart))                                                                                                                                                                                                                                                 | from 0                                                                                                                                                                               | from 0                                            | from 0       | from 0; every group ends ([encoder-copy.md §12.5](encoder-copy.md#125-lifetime-of-per-source-state))                   |
| HLS restart (incompatible copy source; [encoder.md §13](encoder.md#13-container-restart))                                                                                                                                                                                                                                                                 | from the output position, shifted forward by the codec's priming so the first DTS is at or after the previous segment's end ([encoder.md §13.2](encoder.md#132-what-a-restart-does)) | the same                                          | nothing      | every group ends; each copy stream's end carries over; the next group anchors at or after it (§6.5)                    |
| metadata without restart, track boundary, HLS segment boundary                                                                                                                                                                                                                                                                                            | nothing                                                                                                                                                                              | nothing                                           | nothing      | nothing                                                                                                                |
| new source id ([content.md §5](content.md#5-stream_idx-and-time_base)): new decoder, seek, reconnect, declared timestamp discontinuity ([decoder.md §9.2](decoder.md#92-discontinuities-and-invalid-timestamps)), filter graph generation (one id per generation, shared by all its outputs, [filters.md §11.3](filters.md#113-build)), new inline record | counter continues                                                                                                                                                                    | nothing (time from positions)                     | nothing      | switch: group start ([encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start)), offset fixed once (§6.2) |
| new raw audio parameters                                                                                                                                                                                                                                                                                                                                  | conversion rebuilt, drained; counter continues                                                                                                                                       | —                                                 | —            | —                                                                                                                      |
| new raw video format                                                                                                                                                                                                                                                                                                                                      | —                                                                                                                                                                                    | converter rebuilt, drained; grid continues (§4.3) | —            | —                                                                                                                      |
| `stop`                                                                                                                                                                                                                                                                                                                                                    | drained                                                                                                                                                                              | drained                                           | nothing held | held packets written ([encoder-copy.md §8](encoder-copy.md#8-flush-and-end))                                           |

Timestamps of every stream start near 0 at the timeline's origin (§1.4) and
increase for the timeline's lifetime: an Ogg link starts its own timeline,
and HLS segments of one variant share one timeline, restarts included. A
re-encoded stream rebuilt by an HLS restart starts its counter at the output
position of the restart, in its own time base, shifted forward by its
priming so that its DTS stays strictly increasing (D5).
Drains follow [draining.md](draining.md#2-the-rule).

## 3. Audio (internal and raw)

Audio timestamps count samples; the encoder ignores input timestamps.

1. Conversion yields frames at the target rate
   ([encoder-internal.md §4.1–4.2](encoder-internal.md#41-audio-internal)).
2. Each frame given to the codec has pts = the number of samples given to the
   codec before it, in `1/samplerate`. With a fixed-frame-size codec, the
   re-framer delivers frames at `0, F, 2F, …`, `F` the frame size.
3. The codec derives packet timestamps from these, encoder delay included
   (AAC's first packet has pts −1024 at 48 kHz); the bindings convert them to
   the stream time base.

The sample counter stays on the output position, computed from absolute
positions. The encoder realigns its counter; it never invents samples. When
raw audio content has a gap in positions, the resampler is drained and the
counter jumps to the position of the next frame at the target rate, so the
stream carries a timestamp gap where the content had no audio. The re-framer
of a fixed-frame-size codec is not drained (the codec accepts a short frame
only as a stream's last): its partial frame is completed by the samples after
the gap, and the jump applies at that frame's start, so the gap lands less
than one codec frame early. No sample is invented or lost. When frames overlap in
position, the samples before the counter's position are dropped with a
warning. The mechanism is the conversion's
([encoder-internal.md §4.2](encoder-internal.md#42-audio-raw)). Reason: a
gap in the content is the content's; filling it with silence would stand in
for missing source content. A new source
id changes nothing for audio; a change of raw audio parameters rebuilds the
conversion after draining the old one, and the counter continues.

Conformance:

- Output pts are strictly increasing, and gap-free over gap-free content.
- Raw audio content with a 1 s gap: the output carries a pts gap equal to
  the content gap within one codec frame, and the sample count equals the
  input's.
- Re-encode decoded audio from a 44.1 kHz file followed by a 48 kHz file:
  output duration equals input duration to within one codec frame.
- After any run, the last audio pts differs from the output position by at
  most one codec frame plus the codec delay.

## 4. Video (internal and raw)

### 4.1 Frame timestamps entering the converter

Every frame enters the frame-rate converter with its **output position** as
its timestamp, in main ticks, internal and raw alike. Raw frames' own pts are
not read by the encoder: the producer already turned them into positions
([common.md §6.0](common.md#60-absolute-origin)). Frames at equal positions
keep their order.

Reason: positions are continuous across sources, seeks, reconnects and filter
generations, so a source switch needs no re-anchoring, and a missing or
decreasing pts inside one source cannot reach the converter.

### 4.2 The frame-rate converter

The converter is the Fps helper graph of
[filters.md §16.2](filters.md#162-fps), with input time base main ticks and
output time base `1/framerate`. Each frame it delivers gets the next
**output frame number** `W` (0, 1, 2, … for the stream's lifetime) as its
pts in `1/framerate`; then `write_frame`.

### 4.3 Building and re-anchoring a converter

A converter is built for the first frame and rebuilt when the frame format
changes (the set of properties that count as a format change is defined once,
by the content owner). Before a rebuild the old converter is drained
([draining.md](draining.md#2-the-rule)): every frame it holds is written.
The new converter continues the grid: no `setpts` re-anchoring is applied;
the `fps` filter gets `start_time = T`, the time of the next grid slot (frame
number `W`), so its first output frame is numbered `W` at earliest and a grid
slot already written is not written twice
([filters.md §16.2](filters.md#162-fps),
[encoder-internal.md §4.6](encoder-internal.md#46-frame-rate-conversion-both-video-modes)).
A source switch does not rebuild the converter.

At a gap in the raw video content's positions, the converter is drained and
rebuilt at the next grid slot after the gap, with the same start rule. It
never repeats a frame across the gap: the stream carries a timestamp gap
where the content had no picture, and no frame is invented
([faults.md §3](faults.md#3-source-faults-reach-the-script)).

### 4.4 Within one converter

Input timestamps are output positions, so they never decrease and are never
missing. The converter emits one frame per grid slot, per the Fps graph's
semantics; frames that fall in an already-filled slot are dropped by the
frame-rate conversion as intended.

### 4.5 Encoder output

The codec may reorder frames (B-frames): packet dts start below pts. Example:
internal 30 fps H.264 in mpegts, first video packet dts −2/30 s against its
pts 0.

Conformance:

- Re-encode a video whose resolution or pixel format changes twice, then a
  source switch: output pts increase by exactly one at every frame,
  including across each change, and every frame held by the converter at a
  change reaches the output.

## 5. Subtitles

For each subtitle at output position `P`:

- pts = `P` rescaled to `1/1000000`;
- start and end display times in milliseconds relative to pts;
- the packet's timestamp and duration follow
  [avformat.md §4.4 `write_subtitle_frame`](../../../../modules/synced/ffmpeg/spec/avformat.md#44-output).

Several subtitles at one position are legitimate (two cues shown at once). A
subtitle whose stored DTS would not exceed the previous one's is written one
unit of the stream time base after it; its display duration is kept.

## 6. Copy streams, per track

Notation, for a packet of a run with time base `tb`: `dts`, `pts`, `dur` are
its own values; `dts_m`, `pts_m`, `dur_m` are them rescaled to main ticks
(§1.2), none when absent. `O` is the offset of the track's group
([encoder-copy.md §12.1](encoder-copy.md#121-model)). A track's **end** is
the greatest `written dts + duration` it has written, in main ticks, or none
before its first packet. `P(x)` is the output position of packet `x`.

### 6.1 Order of operations

For each packet a track writes
([encoder-copy.md §12.2](encoder-copy.md#122-accepting-a-packet)):

1. Missing values are filled (§6.4).
2. Written values: `pts' = pts_m + O`, `dts' = dts_m + O`, duration `dur_m`.
   One offset applies to PTS and DTS, so the source's reordering delay is
   kept.
3. The monotonicity rule (§6.3) may drop the packet.
4. The track's end becomes `max(end, dts' + extent)`, `extent` = `dur_m`, or,
   when the packet has no duration, the span from its position to the next
   packet of its track or the end of its run.
5. The keyframe hook ([encoder-copy.md §7](encoder-copy.md#7-keyframes)), then
   the write.

### 6.2 Beginning a source (rebase)

When a group's offset is fixed
([encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start)),
each member `m` holding packets has a first held packet `f_m` (after the
trim) and a candidate:

```
candidate_m = max(P(f_m) − skipped, end_m) − dts_m(f_m)   (end_m absent: P(f_m) − skipped alone)
O           = max over those members of candidate_m
```

`O` is fixed once and applies to every packet of the source on every member,
late members included.

`skipped` belongs to the timeline (§1.4): it starts at 0 with the timeline
and grows only at a **whole-output switch**, a group whose members, when it
starts, are every stream of the output: every stream is a copy track and
every one switches to this source. At a whole-output switch where a member
waited for a keyframe ([encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start)), `skipped` grows, before the candidates are computed,
by the output position of the key packet that set the group's start time
`T` ([encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start))
minus the group's wait start. In every other case `skipped` is unchanged. It
is a sum of whole ticks, so it carries no rounding.

- Whole-output switch: the new source's content before its keyframe is
  skipped, every member's pre-start packets are trimmed to `T`, and the
  first packet lands at the previous end of the output. Players see no
  PTS/DTS gap. Reason: no other stream runs through the switch, so no
  stream needs that time kept to stay in sync.
- Any other stream continuing (a copy track on another source, or any
  re-encoded stream, which runs on the output position): `skipped` does not
  grow and the gap stays. Reason: the continuing streams keep playing
  through the wait, and the switched streams stay in sync with them only if
  their time stays in the timeline.
- An output with a re-encoded stream never has a whole-output switch, so its
  `skipped` stays 0 and every stream follows the output position exactly.

- `P(f_m) − skipped` anchors the source on the timeline (§1.4): its first
  packet lands at its place, so time the source spent before that packet (a
  sparse head, packets dropped before a keyframe outside a whole-output
  switch) is kept, and the trailing time of the previous source is kept too.
- `end_m` keeps each stream after its own previous packets: the first DTS of
  the source on that stream is at least its previous end, so a join neither
  overlaps the previous packet nor repeats a DTS.
- The maximum over members, decided once when every member's first packet is
  known (every non-sparse member's, [encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start)),
  keeps the source's own relation between its streams and never moves a
  stream after it has begun.

Each candidate uses its own member's end, not the latest end of every member:
when a producer's positions and timestamps disagree by a constant, the
offset then keeps that constant and does not grow at each join.

Conformance:

- Concatenate three files whose video uses B-frames, each with a different
  first DTS, through one copied stream: DTS strictly increasing across both
  joins; for every packet, PTS − DTS equals its source value to within one
  rounding; the first DTS after a join is at least the previous end; the
  packet count equals the sum of the inputs'; total duration equals the sum
  of the inputs to within one frame. The same with packets that carry no
  duration: no join overlaps the previous packet.
- Whole-output switch: an output that copies audio and video only, switching
  between two audio+video files cut 0.6 s before a keyframe: no DTS or PTS
  gap at the join on either stream; the new source starts at its keyframe on
  both streams; ten such joins leave no accumulated gap.
- Shared switch: copy video and re-encode audio, same files: the video has a
  gap of the keyframe wait at the join, and audio and video of the new source
  start together to within one video frame. The same with audio copied from
  a third, continuing source instead of re-encoded.
- Files in which audio is 200 ms shorter than video, both copied, ten joins:
  the first audio and video PTS of the last file differ by the same amount as
  in the file, to within one frame. Within each source, every stream's DTS
  steps equal its source's own steps, including the step between its first
  and second packet. The output is identical with the streams declared in
  either order.

### 6.3 Monotonicity check

Every copied output stream has strictly increasing stored DTS, where the
stored value is the written DTS rescaled to the stream time base (§1.3). The
cases that can break it each have their own rule:

| Case                                                                                                                                | Class ([faults.md §2](faults.md#2-classes)) | Result                                                                                                  |
| ----------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------- | ------------------------------------------------------------------------------------------------------- |
| within one tenure, after its first written packet, a packet whose `dts` is below the last written packet's `dts`, compared as times | item fault (invalid input)                  | dropped, logged at warning with the track, the source id and both values; counted; the output continues |
| within one tenure, a packet whose `dts` equals the last written packet's `dts`, compared as times                                   | item fault                                  | dropped, logged as "equal DTS dropped"; counted; the output continues                                   |
| a packet whose PTS is below its DTS                                                                                                 | item fault (invalid input)                  | the same                                                                                                |
| a packet needing more than `max_rounding_shift`                                                                                     | item fault                                  | the same                                                                                                |
| the first packets of a late member whose `dts'` falls below the track's end                                                         | none: join overlap                          | dropped, counted, logged at info                                                                        |
| a stored DTS equal to the previous one although `dts` increased (rounding to a coarser stream time base)                            | none: container precision                   | stored as previous + 1 unit                                                                             |

DTS values within a tenure are compared as times: each value times its run's
time base, exactly (or, equivalently for this rule, in main ticks before the
offset). A change of time base within one tenure is legal. A DTS that jumps
forward within one source by more than `max_keyframe_wait`
([encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start)) is
written as it is and logged at warning level with the jump; it is not
corrected.

Item faults are counted per tenure with the copy track's other item faults
([encoder-copy.md §10](encoder-copy.md#10-options-and-errors)); their
escalation is below.

The rounding rule keeps the stored value at most `max_rounding_shift` units
above the value rounding alone gives; a packet that needs more is dropped
and logged at warning with the track and the stream time base: the container
cannot represent that stream's spacing at that point. Recommended value: 1 unit. It protects the
timeline from a shift that grows packet after packet. When the shift lifts
DTS above PTS, PTS is lifted to DTS.

A decreasing DTS within one source is invalid media, as FFmpeg's muxers
treat it. An equal DTS is valid input for decoders, but the output stream's
DTS is strictly increasing (D5), so the packet cannot be written either. The
encoder repairs neither: it drops that packet, which contains the fault to
the smallest unit and keeps the output streaming. The
next packet is compared with the last written one. Across a switch, §6.2
guarantees the increase, so no switch event drops a packet here.

A source whose timestamps restart lower under the same id, with no declared
discontinuity, would lose every packet until its timestamps pass the old
ones. After `max_consecutive_faults` consecutive item faults on one tenure
([faults.md §4](faults.md#4-item-faults-escalate)), the
encoder treats the source as switched: the source's group restarts as a new
group on the same id, every member's tenure reopens with the usual keyframe
wait ([encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start)),
and the offset is fixed anew by this section's rebase. The packet that
completed the run is the first packet of the new tenure. A warning names the
source and the tracks, and the event is reported to the output. The bound
protects the output from a long silence on a source that is still
delivering data. The escalation stays inside the output, because the
encoder cannot end its upstream source's track. Every further run past the
bound restarts the group again; a data fault never stops the output
([faults.md §4](faults.md#4-item-faults-escalate)).

This rule covers copied packets only. Frames out of an `ffmpeg.filter` graph
are raw content: the filter stage repairs a decreasing video PTS of its own
output with a warning
([filters.md §13.3](filters.md#133-decreasing-video-timestamps-from-a-filter)),
and the encoder times raw frames by position (§4.1), so no filter output
reaches this check.

A packet whose PTS is below its DTS is invalid input and is dropped the same
way, logged with both values: a muxer rejects it, and no rewrite of either
value is faithful to the source. PTS need not increase from packet to packet (B-frames).

Conformance:

- A packet with no DTS, followed by a packet whose DTS is not greater than
  the last stored: the second is dropped with a warning; it never reaches
  the muxer, and the output keeps running.
- A source whose DTS goes back once mid-stream by a few packets: those
  packets are dropped with warnings, the output keeps running.
- A source whose DTS restarts at 0 mid-stream under the same id: after
  `max_consecutive_faults` drops, the source is rebased on the timeline with a
  warning and the output carries it again from its next keyframe.
- Copied DTS that differ by less than one unit of the output stream time base,
  isolated: the output keeps running.
- At no point does the muxer report a non-monotonic DTS.

### 6.4 Missing values

| Missing  | Filled with                                                                    |
| -------- | ------------------------------------------------------------------------------ |
| DTS      | stream without reordering: the packet's PTS; stream with reordering: see below |
| PTS      | the packet's DTS                                                               |
| both     | its output position minus `O` (so that it is written at its position)          |
| duration | written without duration; the extent of §6.1 step 4 is used for the end        |

A stream **has reordering** when its codec parameters declare a reorder
delay (B-frames). For such a stream a missing DTS is not filled from PTS,
which would put DTS out of decode order. The packet is given to the muxer
with DTS unset when the muxer derives DTS from the PTS sequence and that
delay; it is then not checked by §6.3, the track's previous DTS and end are
unchanged. Otherwise the packet is an item fault (§6.3).

The filled value is checked by §6.3 like any other. Filling never clears the
track's previous DTS, so the check after a packet without DTS is the same as
after any packet. Reason: packets without DTS are legitimate for some
demuxers, and FFmpeg's muxers derive a missing DTS from PTS the same way for
streams without reordering.

### 6.5 Restarts

A container restart ends every group with the old container
([encoder-copy.md §12.5](encoder-copy.md#125-lifetime-of-per-source-state)).
After an Ogg restart the timeline starts anew at 0 (with `skipped` back to
0); the new container's tracks have no end, so its first group is anchored
at its output position alone. After an HLS restart the variant's timeline
continues: each copy stream's end carries over to the new container's track
of the same field, so the first group anchors at or after it by §6.2, and
DTS stays strictly increasing across the restart (D5). In both cases the
result depends on no state of the old container's groups.

## 7. Copy streams, across tracks

- Tracks of one source share one offset, fixed once (§6.2). When the producer
  keeps one origin for the positions of all its streams
  ([content.md §7](content.md#7-timestamps-carried-in-content)), every packet
  of the source lands at its position, and no stream's first packet after a
  join precedes the latest end of the previous source's streams.
- Tracks of different sources share nothing but the timeline: each group is
  anchored on its own positions less the timeline's `skipped` (§6.2), so
  they stay in step through it. A switch of tracks of different sources is
  never whole-output, so it never moves one stream against another.
- Interleaving across streams is the muxer's: with `interleaved` true the
  bindings use FFmpeg's interleaving writer, which orders packets by DTS
  across streams; with false, packets go to the muxer in call order (field
  order, and held packets at the end of the frame,
  [encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start)).

## 8. Timed ID3 (HLS, mpegts)

`insert_id3 ~frame_position ~sample_position` (positions of the HLS output:
whole frames and remaining main ticks since the output started):

```
pts = dts = rescale(frame_position × frame_size + sample_position, main ticks → 1/audio_rate)
```

where the positions are on the media timeline (output position less
`skipped`, §1.4). Written to the `timed_id3` data stream in `1/audio_rate`;
queued until the container header exists. The encoder generates this
stream; it is not a member of any copy group, so it never prevents a
whole-output switch (§6.2).

## 9. What the muxer is told

| Writer       | Time base passed                                                                 | Values                            |
| ------------ | -------------------------------------------------------------------------------- | --------------------------------- |
| audio, video | encoder time base (bindings)                                                     | codec packets from §3 / §4 frames |
| subtitle     | `1/1000000`                                                                      | §5                                |
| copy         | the stream time base once the header is written; main ticks for the first packet | §6                                |
| timed ID3    | `1/audio_rate`                                                                   | §8                                |

Every stream's stored DTS is strictly increasing. For re-encoded audio and
video it follows from the counters; a codec whose packets are closer than one
unit of the stream time base cannot be stored by that container. That is a
configuration error ([faults.md §2](faults.md#2-classes)): the format asks
for a rate the container cannot carry. It can be known only once the muxer
has chosen the stream time base, at the header write of the first frame;
the encoder reports it there, before any data of the stream is written
([encoder.md §5.2](encoder.md#52-data-faults-and-output-faults)).

## 10. Summary: how order is kept

| Stream    | Reference                          | Across a source switch   | Within a source                            |
| --------- | ---------------------------------- | ------------------------ | ------------------------------------------ |
| audio     | sample counter on output position  | counter continues        | counter jumps over gaps, overlaps trimmed  |
| video     | output position + frame grid       | nothing to do            | grid; drained rebuild on format change     |
| subtitle  | output position                    | nothing to do            | equal positions written one unit apart     |
| copy      | source DTS + one offset per source | offset fixed once (§6.2) | invalid packets dropped; rounding ≤ 1 unit |
| timed ID3 | output position                    | nothing to do            | —                                          |

## 11. Worked examples

Illustrative, main rate 44100. Two 2-second files A and B, each AAC 44.1 kHz
(first packet DTS −1024 ticks) and H.264 25 fps with B-frames (first packet
DTS −0.08 s = −3528 ticks, PTS 0), played in sequence into
`%ffmpeg(format="mpegts", %audio.copy, %video.copy)`. The decoder places
each packet at the position of
[decoder.md §8.3](decoder.md#83-positions-and-sparse-advance), with one
mapping start `B` for every stream; here `B` is the earliest DTS, −3528
ticks, so a copied packet sits at its DTS minus `B`.

### 11.1 Container start

Frame 1 holds the first runs of A on both tracks; video's first packet is a
key packet. At the end of frame 1 both tenures are ready and the group
starts.

- video: `f` at position 0, DTS −3528, no end → candidate 3528;
- audio: `f` at position 2504, DTS −1024, no end → candidate 3528;
- `O` = 3528: video written DTS 0, PTS 3528; audio written 2504.

The same result holds with the fields declared in the other order.

Every packet of A reaches the encoder, the final audio packet included: the
decoder MUST deliver it at end of input
([decoder.md §13.1](decoder.md#131-end-of-input),
[draining.md §6](draining.md#6-conformance)). A's audio end in §11.2 counts
it.

### 11.2 Switch to B, cut mid-GOP

Say A's audio ends at 92616 (2.1001 s) and A's video at 88200, B starts at
position `S` = 92616, and B's first keyframe is 0.6 s into B.

- At the end of B's first frame, video is not ready; audio holds its packets.
- When the keyframe arrives (position `S` + 26460, DTS 22932), the group
  starts. `T` = the keyframe's PTS; audio packets with PTS below it are
  dropped.
- Both streams of the output are members: this is a whole-output switch.
  `skipped` grows by (`S` + 26460) − `S` = 26460.
- video candidate: max(`S` + 26460 − 26460, 88200) − 22932 = 69684; audio's
  first kept packet gives the same candidate (its position minus `skipped`
  minus its DTS); `O` = 69684.
- B's keyframe is written at DTS 92616 (2.1001 s), right after A's end; the
  first kept audio packet follows A's audio. No gap: only B's content before
  its keyframe is skipped.

### 11.3 Same switch with re-encoded audio

`%ffmpeg(format="mpegts", %audio(codec="aac"), %video.copy)` over the same
sequence. The re-encoded audio runs through the wait, so the switch is not
whole-output: `skipped` stays 0, the video candidate is
max(`S` + 26460, 88200) − 22932 = 96144, and B's keyframe is written at DTS
119076 (2.7001 s), its position. The video shows A's last picture for 0.6 s
while B's audio plays, and the two stay in sync.
