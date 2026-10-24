# Known complexity: timestamps, keyframe switching and per-source state

This file lists the difficulties that the history of the FFmpeg layer shows to
be easy to get wrong: keeping PTS and DTS monotonic on the encoding side, the
timestamp trouble in copy decoding, inline encoders, filter graphs and muxing
that fed into it, switching copied streams on keyframes, and the lifetime of
the state kept per source. Each entry states a situation, what an observer
saw, why the problem is hard in any language, and a check a new
implementation can be run against. It relies on the vocabulary of
[timestamps.md](timestamps.md) (stream identity, rebasing offset, main time
base) and [content.md](content.md) (chunks, positions, sparse streams). The
rules that answer each entry are listed in the Reconciliation table, and the
checks that test each entry in [tests.md §23](tests.md#23-known-complexity-checks).
§10–§13 come from defects the as-built pass found and the open reports they
explain.
Commit, issue and PR numbers are in the Trace appendix.

## 1. Rebasing a sequence of copied sources onto one timeline

**Situation.** A copy stream receives packets from one source, then from
another (a playlist, a `sequence`, a `fallback` switch, a reconnecting
input). Each source carries its own timestamps: they start anywhere (often
near zero, sometimes negative), use their own time base, and have PTS ahead
of DTS by a codec-dependent reordering delay.

**What went wrong.** Ten fixes over four years. A live RTMP relay to a large video platform stalled silently
after three to seven hours while the same files looped by the FFmpeg CLI ran
for days. Concatenated files came out shorter or longer than the sum of their
parts. Gaps or overlaps of one packet appeared at each join.

**Why it is hard.**

- Synthesising timestamps from the packet's position in the stream (PTS =
  DTS = position) throws away the reordering delay and breaks any codec with
  B-frames.
- Separate offsets for PTS and DTS, each computed from "where the previous
  value ended", drift apart: the PTS/DTS gap of the new source is no longer
  the one the codec encoded.
- The end of the previous source is its last DTS **plus that packet's
  duration**; using the last DTS alone overlaps by one packet, using a running
  sum of durations drifts whenever durations are missing or rounded.
- The new source's first DTS is only known when its first packet is
  _accepted_, which can be long after its first packet is _seen_ (see §3).
- Time bases differ per source; any offset computed in one source's time base
  and applied in another's shifts the timeline. One common base for all
  bookkeeping is the only safe choice.
- Nothing fails immediately: most muxers and players tolerate small errors,
  and the failure shows up hours later, at a strict ingest server.

**How to tell.** Concatenate three files whose video uses B-frames
(PTS ≠ DTS), each with a different first DTS, through a copy encoder with a
single copied stream to a file. Check, with a packet dump of the output: DTS
strictly increasing across both joins; for every packet, PTS − DTS equals the
value in its source file to within the rounding of one time-base conversion;
the first DTS after a join is at least the previous source's end (its last
DTS plus that packet's duration); the packet count equals the sum of the inputs' packet counts (no packet lost
at the end of a file, §10); total duration equals the sum of the inputs to
within one frame. Repeat with inputs whose packets carry no duration: no join
overlaps the previous source's last packet.

**Checked by.** [tests.md §23](tests.md#23-known-complexity-checks), row 1.

## 2. One offset for all streams of a source, aligned on the longest

**Situation.** Several copy streams (audio and video) come from the same
source and switch together. Their last packets do not end at the same time:
audio commonly ends a fraction of a second before video, or the reverse.

**What went wrong.** Three fixes. With one offset per stream, each track
change shifted audio relative to video by the difference of their end times;
over a playlist the A/V offset accumulated without bound. With the offset
taken from the stream that happened to switch first, the next source's audio
started before the previous source's video had ended.

**Why it is hard.** The offset must be decided when the first stream of the
new source starts, but the other streams of the previous source may not have
delivered their last packet yet, and the streams of the new source start at
different moments too (one may wait for a keyframe). The offset therefore
has to account for the end of every stream of the set, yet be settled before
any of them writes: raising it after a sibling has already started rebased
packets with the smaller value moves that sibling forward by the difference.

**How to tell.** Build a sequence of files in which audio is 200 ms shorter
than video, copy both. After ten joins, the first audio PTS and the first
video PTS of the last file differ by the same amount as in the source file,
to within one frame; no stream's first packet after a join precedes the
latest end of any stream of the previous source. Run it twice, with the
streams declared to the encoder in both orders: the two outputs are
identical. Within each source, every stream's DTS steps equal its source's
own steps, including the step between the first and second packet after a
join: no stream jumps forward once a sibling has started.

**Checked by.** [tests.md §23](tests.md#23-known-complexity-checks), row 2.

## 3. Switching copied streams on keyframes

**Situation.** A copied video stream (and the streams that go with it)
switches to a new source between two keyframes. The new source's first
packets depend on frames that were never sent. Some codecs are intra-only
(every packet decodable, most audio codecs); others need a keyframe. Streams
of the set may come from **the same source** (one file's audio and video) or
from **different sources** (audio from one file, video from another).

**What went wrong.** Four fixes and two user reports. Without waiting,
players froze or showed corrupt pictures after a switch until the next
keyframe; switching back from a live input to a file froze playback. With a
wait shared by every copied stream regardless of origin, audio taken from one
file and video from another produced an output with no audio stream at all:
the shared readiness never completed. A stream that ended before its first
keyframe left the whole set unready for good.

**Why it is hard.**

- The streaming model decides a switch before the new source's packets are
  seen; there is no lookahead to switch _on_ a keyframe, so the encoder has
  to discard packets after the fact.
- Readiness has two scopes. Streams from the same source must start together
  or the decodable picture lags the audio; streams from different sources
  have no common keyframe and must not wait for each other.
- Intra-only codecs are ready at once and must not hold the set back, yet
  they are part of the set the others wait with.
- A stream can leave the set before it ever became ready; whatever tracks
  readiness must then forget that stream without losing the others' state.
- The offset of §1 must be taken from the first _accepted_ packet, not the
  first one seen, or the discarded lead-in becomes a gap.
- Users want both behaviours: `%audio.copy(ignore_keyframe)` and
  `%video.copy(wait_for_keyframe)` exist because neither is right for every
  output.

**How to tell.**

- Same source: switch between two audio+video files mid-GOP with
  `wait_for_keyframe`, the second file's audio starting well before its
  first video keyframe. In the output, the first packet of the new source on
  every stream is at or after the new source's first video keyframe, and the
  first video packet is a keyframe. The result holds with the streams
  declared in either order, and with a third copied stream from an unrelated
  source added to the same output.
- Different sources: copy audio from one file and video from another. Both
  streams are present and non-empty in the output, and the audio's first
  packet is not delayed by the video's wait for a keyframe.
- Mixed: audio and video from source A, then audio from B and video from C.
  Each new stream starts on its own readiness; neither waits for the other.
- Early end: a source whose video ends before its first keyframe, followed by
  a normal one. The output resumes on the second source.
- Intra-only: an audio-only copy output never discards a packet on a switch.
- Continuing bitstream: an MPEG-TS input with a declared timestamp
  discontinuity whose packets continue to decode: no packet is discarded at
  the switch.

**Checked by.** [tests.md §23](tests.md#23-known-complexity-checks), row 3.

## 4. Lifetime of the per-source state

**Situation.** The state of §2 and §3 (shared offset, latest end, readiness)
is keyed by the identity of the source the packets come from, and must exist
exactly while some stream from that source is being muxed. Inline encoders
must also hand the same identity to every track they produce from one
source, so that a downstream copy encoder treats them as one set.

**What went wrong.** Three fixes. The audio and video produced by one inline
encode got different identities, so a downstream copy encoder treated them as
unrelated sources and neither shared an offset nor waited together. The
identity of a source was looked up by its memory address, which moves when
the runtime compacts memory: lookups silently missed and the two tracks of a
source drifted apart again. Readiness counting incremented only when a stream
was ready but decremented on every removal, so it could go negative and never
reach "all ready" again.

**Why it is hard.** The state's lifetime is not any one stream's: it starts
with the first stream of a source and ends with the last, while streams
switch at different moments. Keeping it forever leaks one entry per source
(a playlist creates sources without bound); dropping it early forgets the
offset or the readiness while a sibling still needs it, and the result
depends on when memory is reclaimed, so it is not reproducible. Additions and
removals come from different events (first packet, first keyframe, switch
away), so they are not naturally paired.

**How to tell.** Run a copy encoder over a playlist of a few thousand short
audio+video files with forced memory reclamation between tracks: memory held
by tracking state stays flat; every join still satisfies the checks of §1–§3.
Encode audio and video of one source inline, then copy both: the output
passes §2. Remove a stream before it is ready, many times: the next set still
becomes ready. Let a source's audio switch away while its video still waits
for a keyframe: the video's first packet gets the offset its audio used. Run
each scenario twice, forcing memory reclamation before the switch in one run
and after it in the other: the outputs are identical. This includes an
inline encoder put to sleep and woken again with a copy encoder downstream
(no packet dropped as non-monotonic), and an Ogg in-stream metadata restart
with copied streams (the new container's first timestamps do not depend on
the run).

**Checked by.** [tests.md §23](tests.md#23-known-complexity-checks), row 4.

## 5. Packets with no DTS or a non-increasing DTS

**Situation.** A copied packet arrives without a DTS, or with a DTS that,
after rebasing, is not greater than the last one written on that stream
(source glitch, overlap at a join, restart of an upstream encoder).

**What went wrong.** Three changes, one of them reverted the same day.
Dropping every packet without DTS emptied whole streams for sources that
never set one. Writing a non-increasing DTS made the muxer refuse the packet
and fail the output. Clearing the decoder-provided DTS on re-encoded frames
was tried and reverted.

**Why it is hard.** "No DTS" is legitimate for some demuxers and for packets
the muxer can time itself, so it cannot be treated as corruption. The
monotonicity test must run on the _rebased_ value with the offset that will
actually be applied, which, for the first packet of a new source, is not yet
the stored one. Equal DTS is rejected by muxers as well as decreasing DTS.

**How to tell.** Feed a copy stream without reordering packets lacking DTS:
they are written. Feed a stream with B-frames packets lacking DTS: the muxer
derives their DTS, or each is dropped as an item fault; none is written out
of decode order. Change the time base within one source: no packet is
dropped.
Feed it a packet whose rebased DTS equals the previous one: it is dropped
with a warning and the output keeps running. Feed a packet lacking DTS, then
one whose DTS is not greater than the last DTS written: the second is
dropped. Feed rebased DTS that differ by less than one unit of the output
container's time base: the output keeps running. At no point does the muxer
report a non-monotonic DTS.

**Checked by.** [tests.md §23](tests.md#23-known-complexity-checks), row 5.

## 6. Re-encoding: timestamps generated, then reset by a rebuilt stage

**Situation.** A re-encoded (not copied) stream gets its timestamps from a
counter (samples for audio, frames for video) rather than from its input.
Conversion stages in front of the encoder (frame-rate conversion, scaling)
are rebuilt when the input format or source changes, and the encoder or
decoder itself may be restarted.

**What went wrong.** Six fixes and one open report. Audio frames sent with
no PTS, and an output that crashed on track change with `Invalid argument`
while a downstream packager reported `dts 0 less than or equal to previous
dts`, date from the same weeks; the latter was fixed together with the
bindings. A restarted internal decoder produced PTS that went back to zero. Each format change
rebuilt the frame-rate stage, whose output restarted at zero: PTS
discontinuities and `dts less than previous` downstream. An initial-offset
expression clamped the wrong way shifted the first frames. Frames without PTS
had to fall back to the best-effort timestamp. The raw-content encoder is
still reported as needing the same rebasing as the copy encoder on source
switches.

**Why it is hard.** Every stage that holds a clock restarts it when rebuilt,
and rebuilding is the natural reaction to a format change. The counter that
makes output timestamps continuous has to outlive the stages, and each
rebuilt stage has to be told where to start, converted to its own time base.
Input timestamps are not reliable enough to inherit (missing, negative
start, restart on decoder reopen).

**How to tell.** Re-encode a video whose resolution or pixel format changes
twice, then a source switch: output PTS increase by exactly one frame
duration at every frame, including across each change, and every input
frame held by the stage at the change still reaches the output. Re-encode
audio across a track change: output PTS equal the cumulative sample count.
Re-encode decoded audio from a sequence of a 44.1 kHz and a 48 kHz file:
output duration equals input duration to within one codec frame. Decode a
video whose frames carry no presentation timestamp but a best-effort one:
frames are timed in order, not bunched or dropped.

**Checked by.** [tests.md §23](tests.md#23-known-complexity-checks), row 6.

## 7. Raw frames and filter graphs: durations from timestamps

**Situation.** Decoded (raw) frames carry timestamps, and their duration in
the stream is inferred from the next frame's timestamp. Frames pass through
content buffers (several frames may sit at one position), switch between
streams with different time bases, and enter FFmpeg filter graphs that may be
torn down and rebuilt when an input runs dry.

**What went wrong.** Five fixes and two reports. A sign error made every
inferred duration negative. Frames stored at the same position were reordered
by an unstable sort, and the content was rejected as
`Invalid ffmpeg content: non-monotonic PTS`, crashing the script. A switch
between streams in different time bases moved the position backwards and
triggered the same rejection. A filter graph with a lookahead (`loudnorm`)
behind a `buffer` that ran dry crashed with that error, and after a later
rewrite stalled silently instead; the rebuilt graph restarted its
timestamps at zero.

**Why it is hard.** Inferring duration from the next timestamp means always
holding the latest frame back and flushing it explicitly at the end. Order
within one position carries information that sorting by position destroys.
Lining up a new stream on the end of the previous one requires converting
the previous end into the new time base first. A rebuilt graph is, for
timing purposes, a new stream and must be announced as one.

**How to tell.** Push several frames at the same position through a buffer:
their order is preserved. Switch a raw stream between a 1/44100 and a
1/48000 source: timestamps never decrease. Run `loudnorm` behind a `buffer`
whose input goes unavailable for a few seconds and comes back: no error, the
output resumes, and output duration matches the input to within one frame.

**Checked by.** [tests.md §23](tests.md#23-known-complexity-checks), row 7.

## 8. Interleaving, sparse streams and the muxer's first write

**Situation.** The muxer interleaves packets across its streams. Some streams
are sparse: an ID3 metadata stream in MPEG-TS, a subtitle stream whose first
cue comes after twenty minutes. On the decoding side, a live input may
deliver seconds of audio before the first video packet.

**What went wrong.** Seven fixes. An HLS output's first segment was empty
while the playlist announced audio in it: the interleaving muxer waited for a
packet on the never-written metadata stream, and the rule choosing
interleaving by stream count was off by one. ID3 tags written before the
streams were initialised were lost. Live RTMP input started out of sync
because audio decoding ran ahead of the first video frame. Sparse subtitle
streams needed the time before their first packet counted, or every later
cue landed early.

**Why it is hard.** An interleaving muxer is correct only if every stream
eventually writes; a sparse stream breaks that silently. Non-interleaved
writing avoids the stall but lets one stream run ahead. The empty time at
the head of a sparse stream is not visible in its packets and has to be
carried as length alone. Decoded streams have to be released in timestamp
order across streams, not in arrival order.

**How to tell.** Audio-only MPEG-TS HLS with metadata: the first segment is
non-empty and contains the first ID3 tag. A subtitle stream whose first cue
is at 20 min, copied after another source: the cue is at 20 min after the
join. Repeat with a previous source whose last cue ends ten minutes before
the source does, and with no other copied stream from either source: the
cue is still at 20 min after the join. A live input whose audio starts 2 s
before video, both decoded: the first decoded audio and video positions
match to within one audio frame. A live input whose video appears 8 s after
its audio: no data until the video appears, one warning, then both streams
start together.

**Checked by.** [tests.md §23](tests.md#23-known-complexity-checks), row 8.

## 9. Segment boundaries, keyframes and drift on HLS

**Situation.** A segmented output (HLS) splits on a duration grid, but a
segment can only start on a keyframe of every encoded stream, and encoders
emit packets in units that do not match the internal frame size (2048-sample
HE-AAC against a 960-sample frame).

**What went wrong.** Five fixes. Segments overshot `EXT-X-TARGETDURATION`
and players stalled. Forced splits without a keyframe produced undecodable
segments. Fields not encoded by the output still blocked the keyframe test.
HLS variants mixing HE-AAC and AAC-LC drifted apart by about 10.7 ms per
segment, reaching 1.8 s, and some players stalled on the master playlist.

**Why it is hard.** The split decision is made per internal frame while the
data arrives per codec packet; leftovers carried into the next segment must
count towards its length but not move the grid, or boundaries drift by a
codec-dependent amount. The target duration may not change during a stream,
so waiting for a late keyframe has to be bounded by an announced value.

**How to tell.** Two variants, HE-AAC and AAC-LC, of one source: after
fifty segments, the first PTS of segment _n_ is equal across variants to
within one codec frame. A video with keyframes every 4 s and 1 s segments,
re-encoded: every segment starts with a keyframe and no `EXTINF` exceeds the
target duration. The same video copied: no `EXTINF` exceeds the target
duration, each cut taken without a keyframe logs a warning, and the playlist
does not announce `EXT-X-INDEPENDENT-SEGMENTS`. A video with a keyframe every segment duration: the first packet
of every segment is a keyframe, and no segment ends with a keyframe. A
copied video whose keyframes fall just before each boundary: every cut is at
the keyframe before its boundary, and the boundaries do not drift.

**Checked by.** [tests.md §23](tests.md#23-known-complexity-checks), row 9.

## 10. Data held back at the end of a stream

**Situation.** Almost every stage of the pipeline holds data back: a codec
keeps its delay and lookahead, a frame-rate stage keeps its latest frame, a
re-framer keeps a partial frame, a resampler keeps its filter delay, and any
stage that derives a duration from the next timestamp keeps the latest item
until the next one comes. When the stream ends, the source switches, or a
stage is replaced, what it holds is either drained or lost.

**What went wrong.** Three fixes, and several losses still observed in the
as-built pass. The frame-rate stage needed a flushing interface before
re-encoded video ended on its last frames; decoders had to be drained at end
of input; the last packet's duration had to be counted at joins (§1). The
as-built pass observes: each copied file contributing one audio packet fewer
than it holds; raw decoders and chunkers keeping their last item; a
frame-rate stage replaced on a format or source change without being
drained; the audio re-framer and resampler never drained; an inline
encoder's tail written into a buffer nobody reads; items still waiting for
the cross-stream release order (§8) discarded at end of input; track marks
and metadata of an inline encode arriving ahead of the data they belong to,
by the amount held back.

**Why it is hard.** Each stage has its own end-of-stream signal and its own
idea of what "drained" means, and the events that should trigger a drain
(end of input, source switch, format change, sleep, stop) are not the same
events at every stage. A drain must run before the stage is discarded and
its output must land where the next stage still reads it. The loss is small
(one packet, one frame, a few milliseconds of audio), so it passes every
check that does not count items.

**How to tell.** For each path (copy decode, raw decode, internal decode,
re-encode, inline encode, filter graph), run one file of known packet and
frame count to an output and compare counts: equal, to within the codec's
documented priming. Change the video format mid-stream: no input frame is
lost at the change. Run an inline encoder over a finite input to its end: the
encoded tail reaches the output. Track marks of an inline encode fall on the
first packet of the new track.

**Checked by.** [tests.md §23](tests.md#23-known-complexity-checks), row 10.

## 11. Codec parameters across copied sources

**Situation.** A copy stream takes its codec parameters (codec, extradata,
packetisation such as AVCC against Annex B for H.264) from its first source.
Later sources may carry the same codec with other parameters, or another
codec.

**What went wrong.** An open request: switching a copied stream between a
live RTMP input and an MP4 file needs bitstream filters picked by the user
per source, and any input not in the expected form breaks the output. The
as-built pass observes a switch from an MP4-sourced H.264 to a TS-sourced
one stopping the output with `Invalid data found when processing input`,
and an audio copy decoder that does not rebuild on a codec change under the
same source identity.

**Why it is hard.** A container that stores a stream's configuration in its
header fixes it when it writes the header; a container that carries it in
band follows a change. Whether a difference matters therefore depends on the
property and on the output's container (some are harmless, some need a
bitstream filter, some no container survives), and the switch is decided before the new source's
first packet is seen.

**How to tell.** Copy H.264 from an MP4 file, then from an MPEG-TS file:
the second file is rejected when it is opened, naming the bitstream form and
printing both parameter sets; no packet of it reaches the output. Switch a
copied audio stream from AAC to MP3: rejected the same way, with the relaxed
setting on as well. Copy two H.264 files of the same form whose extradata
differ: both are accepted at open; an MPEG-TS output keeps streaming, an MP4
file output stops at the switch with an error printing both parameter sets,
and an HLS variant starts a new segment. Copy files that differ only in a
colour property or HDR metadata: every file plays, and each switch to a
file whose value differs from the stream header logs a warning naming both
values. The muxer never receives packets in the wrong
form.

**Checked by.** [tests.md §23](tests.md#23-known-complexity-checks), row 11.

## 12. Copied and re-encoded streams in one output

**Situation.** One output copies some streams and re-encodes others
(copied video with re-encoded audio, for instance). Copied timestamps follow
the sources' own timestamps, rebased at each switch; re-encoded ones follow
sample and frame counts.

**What went wrong.** No report yet; the as-built pass shows the two
timelines have no common reference. Time a copied stream does not account
for (trailing gaps before a switch, packets discarded while waiting for a
keyframe, any drift of the source's own timestamps) moves it against the
re-encoded streams, and nothing brings them back together.

**Why it is hard.** Each half is right on its own terms: a re-encoded stream
must be gap-free, a copied stream must keep its source's spacing. Aligning
them needs one of the two to bend at every switch, and the copy side only
learns the new source's timing from its first accepted packet.

**How to tell.** Copy video and re-encode audio over a playlist of fifty
files whose video starts mid-GOP. After the last join, audio and video of the
last file start together to within one video frame, as they do in the file.

**Checked by.** [tests.md §23](tests.md#23-known-complexity-checks), row 12.

## 13. Chunk lengths from timestamp steps

**Situation.** A producer of copy or raw content (decoder, inline encoder,
bitstream filter, filter output) gives each chunk the length of the step to
the next item's timestamp, converted from the item's time base into the
internal time base.

**What went wrong.** An open report: a live RTMP relay copying audio and
video ran for two to three hours, then failed with `Generator max buffered
length exceeded`, its buffered video growing by about 0.4 s per hour while
audio stayed small. The as-built pass measures the mechanism: each step is
converted and truncated on its own, so content is shorter than the media it
carries (a 600.023 s file plays 599.834 s; inline AAC at 48 kHz yields 940
ticks per 940.8-tick packet). The loss differs per stream (it depends on the
step size in each time base), so one stream's content runs short of
another's, and a reader filling all of them buffers the longer one without
bound. With FLV's 1/1000 time base, AAC at 44.1 kHz and 30 fps video, the
per-stream losses differ by about 0.3 s per hour, which matches the report.

**Why it is hard.** A length must be a whole number of internal ticks and
the item's time base rarely divides into it. Rounding each step separately
looks harmless (under a tick each) and accumulates without bound; carrying
the remainder, or converting the absolute timestamps and taking their
difference, does not. Nothing fails until a buffer limit is hit hours later.

**How to tell.** Copy a 10-hour stream (or a looped file) of AAC at 44.1 kHz
in a 1/1000 time base with 30 fps video: the content length of each stream
equals the media duration to within one tick after the whole run, and no
buffer grows. Encode AAC at 48 kHz inline for an hour on a free-running
clock: input and output frame counts stay within a constant offset.

**Checked by.** [tests.md §23](tests.md#23-known-complexity-checks), row 13.

## Reconciliation

Each entry is split into the points it carries. Every point is answered by a
normative rule; the table names the section and how it answers. Where the
normative design removes the mechanism a point was about, the row says what
handles the situation instead.

| Entry | Point                                                                       | Answered by                                                                                                                                                                                                                                                                                                                               | How                                                                                                                                                                                                     |
| ----- | --------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1     | One offset on PTS and DTS keeps the reordering delay                        | [timestamps.md §6.1](timestamps.md#61-order-of-operations)                                                                                                                                                                                                                                                                                | step 2 adds one group offset to both values                                                                                                                                                             |
| 1     | Timestamps are never synthesised from positions                             | [timestamps.md §6.1](timestamps.md#61-order-of-operations), [§1.4](timestamps.md#14-the-output-timeline)                                                                                                                                                                                                                                  | written values are the source's own plus the offset; positions only anchor the offset                                                                                                                   |
| 1     | End of a source = last DTS + that packet's duration                         | [timestamps.md §6.1](timestamps.md#61-order-of-operations)                                                                                                                                                                                                                                                                                | step 4: a track's end is the greatest written DTS plus extent                                                                                                                                           |
| 1     | End of a source when the last packet has no duration                        | [timestamps.md §6.1](timestamps.md#61-order-of-operations), [§6.4](timestamps.md#64-missing-values)                                                                                                                                                                                                                                       | the extent is the span to the next packet or the end of its run, so a join never overlaps                                                                                                               |
| 1     | New source's first DTS from the first accepted packet                       | [encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start), [timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase)                                                                                                                                                                                                  | the offset is fixed at group start from each member's first held packet after the trim                                                                                                                  |
| 1     | One time base for all bookkeeping                                           | [timestamps.md §1.1](timestamps.md#11-time-bases-in-play), [§1.2](timestamps.md#12-encoder-side-conversion)                                                                                                                                                                                                                               | offsets, ends and positions in main ticks, each converted from its absolute value                                                                                                                       |
| 1     | Reconnect is a source switch                                                | [io.md §13](io.md#13-timestamps-across-reconnects), [encoder-copy.md §12.6](encoder-copy.md#126-reconnects-and-new-decoders)                                                                                                                                                                                                              | each connection is a new decoder and a new source id, rebased like any switch                                                                                                                           |
| 1     | Errors surface hours later at a strict ingest server                        | [timestamps.md §6.3](timestamps.md#63-monotonicity-check)                                                                                                                                                                                                                                                                                 | stored DTS are strictly increasing per stream, checked on the stored value; the muxer never sees a non-monotonic DTS                                                                                    |
| 1     | No packet lost at the start or the end of a file                            | [decoder.md §8.5](decoder.md#85-start-and-seek-threshold), [§13.1](decoder.md#131-end-of-input), [draining.md §6](draining.md#6-conformance), [encoder-copy.md §8](encoder-copy.md#8-flush-and-end)                                                                                                                                       | an all-copy mapping releases every packet; the last packet reaches the field at end of input; held packets are written before a stop                                                                    |
| 2     | One offset per source, shared by its streams                                | [timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase), [§7](timestamps.md#7-copy-streams-across-tracks)                                                                                                                                                                                                                        | one offset per group, applied to every packet of every member                                                                                                                                           |
| 2     | The offset accounts for the latest end of the previous source               | [timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase)                                                                                                                                                                                                                                                                          | the offset is the maximum of the members' candidates, each bounded below by that member's end                                                                                                           |
| 2     | Raising the offset after a sibling started moves it                         | [timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase), [encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start)                                                                                                                                                                                                  | no raise: members are held until the group starts, and the offset is fixed once, before any member writes                                                                                               |
| 2     | Streams of the new source start at different moments                        | [encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start), [timestamps.md §6.3](timestamps.md#63-monotonicity-check)                                                                                                                                                                                                         | late members write with the fixed offset; their packets below the track's end are dropped as join overlap                                                                                               |
| 2     | Same result whatever the declaration order                                  | [encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start)                                                                                                                                                                                                                                                                    | membership and start are evaluated at the end of the frame                                                                                                                                              |
| 2     | Streams of one decoder keep their relation across a timestamp discontinuity | [decoder.md §9.2](decoder.md#92-discontinuities-and-invalid-timestamps)                                                                                                                                                                                                                                                                   | one event per mapping: one new source id and the same shift for every stream, each stream crossing at its own jump                                                                                      |
| 3     | Discard after the fact until a keyframe                                     | [encoder-copy.md §12.2](encoder-copy.md#122-accepting-a-packet)                                                                                                                                                                                                                                                                           | a tenure that needs a keyframe drops packets up to its first key packet                                                                                                                                 |
| 3     | Same-source streams start together                                          | [encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start)                                                                                                                                                                                                                                                                    | a group starts once every member is ready; held packets are trimmed to the key packet's PTS                                                                                                             |
| 3     | Different-source streams do not wait for each other                         | [encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start), [content.md §5](content.md#5-stream_idx-and-time_base)                                                                                                                                                                                                            | one group per source id; groups start independently                                                                                                                                                     |
| 3     | Intra-only streams are ready at once                                        | [encoder-copy.md §12.2](encoder-copy.md#122-accepting-a-packet)                                                                                                                                                                                                                                                                           | a tenure needs a keyframe only with `wait_for_keyframe` and a codec that is not intra-only                                                                                                              |
| 3     | A stream leaving before it is ready                                         | [encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start)                                                                                                                                                                                                                                                                    | no counter: a tenure that closes unready leaves the membership, which is recomputed each frame                                                                                                          |
| 3     | Offset from the first accepted packet                                       | [encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start), [timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase)                                                                                                                                                                                                  | the offset uses the first held packet after the trim                                                                                                                                                    |
| 3     | Both keyframe choices are wanted                                            | [encoder-copy.md §12.2](encoder-copy.md#122-accepting-a-packet)                                                                                                                                                                                                                                                                           | each track's choice affects only its own readiness                                                                                                                                                      |
| 3     | A keyframe that never comes                                                 | [encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start), [faults.md §5](faults.md#5-waiting)                                                                                                                                                                                                                               | `settings.ffmpeg.content.copy.max_keyframe_wait` (20 s) starts the group with its ready members, with a warning                                                                                         |
| 3     | A source whose video ends before its first keyframe                         | [encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start)                                                                                                                                                                                                                                                                    | a group that never starts is discarded; the next group starts normally                                                                                                                                  |
| 3     | Time skipped while waiting for a keyframe                                   | [timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase)                                                                                                                                                                                                                                                                          | removed at a whole-output switch, kept as a gap when another stream continues                                                                                                                           |
| 3     | A declared discontinuity whose bitstream continues                          | [encoder-copy.md §12.6](encoder-copy.md#126-reconnects-and-new-decoders), [decoder.md §9.2](decoder.md#92-discontinuities-and-invalid-timestamps)                                                                                                                                                                                         | the new id carries the continuing mark: the group is rebased with no keyframe wait                                                                                                                      |
| 4     | One identity for every track an inline encode produces                      | [inline-codecs.md §5](inline-codecs.md#5-stream-index), [content.md §5](content.md#5-stream_idx-and-time_base)                                                                                                                                                                                                                            | sibling transforms on one input share one source-id record and its id                                                                                                                                   |
| 4     | Identity independent of memory location                                     | [content.md §5](content.md#5-stream_idx-and-time_base)                                                                                                                                                                                                                                                                                    | a source id is a counter value carried in the content; no table is keyed by sources                                                                                                                     |
| 4     | State exists exactly while a stream of the source is muxed                  | [encoder-copy.md §12.5](encoder-copy.md#125-lifetime-of-per-source-state)                                                                                                                                                                                                                                                                 | a group lives from its first tenure to the frame its last member leaves                                                                                                                                 |
| 4     | Readiness bookkeeping cannot go negative                                    | [encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start)                                                                                                                                                                                                                                                                    | readiness is a property of each member's tenure, read at the end of the frame; nothing is counted                                                                                                       |
| 4     | Result independent of memory reclamation                                    | [encoder-copy.md §12.5](encoder-copy.md#125-lifetime-of-per-source-state)                                                                                                                                                                                                                                                                 | behaviour depends only on the sequence of runs and packets                                                                                                                                              |
| 4     | Producer-side state per source                                              | [inline-codecs.md §5](inline-codecs.md#5-stream-index)                                                                                                                                                                                                                                                                                    | a record lives exactly as long as a transform uses it                                                                                                                                                   |
| 4     | Inline encoder put to sleep and woken, copy encoder downstream              | [inline-codecs.md §5](inline-codecs.md#5-stream-index), [§9](inline-codecs.md#9-timestamps-inline-part)                                                                                                                                                                                                                                   | the woken encoder joins a live sibling's timeline, or takes a new id that downstream rebases                                                                                                            |
| 4     | Container restart with copied streams (Ogg metadata, HLS)                   | [encoder-copy.md §12.5](encoder-copy.md#125-lifetime-of-per-source-state), [timestamps.md §6.5](timestamps.md#65-restarts)                                                                                                                                                                                                                | a restart ends every group; an Ogg link starts at 0, an HLS restart carries each stream's end over so DTS keeps increasing                                                                              |
| 5     | Packets without DTS are written                                             | [timestamps.md §6.4](timestamps.md#64-missing-values)                                                                                                                                                                                                                                                                                     | without reordering, DTS is filled from PTS; with reordering, the muxer derives it, else the packet is an item fault; both missing: from the position                                                    |
| 5     | Equal or decreasing DTS dropped with a warning                              | [timestamps.md §6.3](timestamps.md#63-monotonicity-check)                                                                                                                                                                                                                                                                                 | compared as times, so a time-base change within a source is legal; item faults, dropped and counted; an equal DTS is valid input but cannot be written (D5)                                             |
| 5     | The check holds after a packet without DTS                                  | [timestamps.md §6.4](timestamps.md#64-missing-values)                                                                                                                                                                                                                                                                                     | filling never clears the track's previous DTS                                                                                                                                                           |
| 5     | The check uses the offset actually applied                                  | [timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase), [§6.3](timestamps.md#63-monotonicity-check)                                                                                                                                                                                                                             | the offset is fixed before a source's first write; within a tenure the check compares source DTS, and across a switch §6.2 guarantees the increase                                                      |
| 5     | DTS closer than one unit of the container time base                         | [timestamps.md §6.3](timestamps.md#63-monotonicity-check)                                                                                                                                                                                                                                                                                 | stored as previous plus one unit, within `max_rounding_shift`                                                                                                                                           |
| 5     | A source whose DTS keeps failing (timestamp restart under one id)           | [timestamps.md §6.3](timestamps.md#63-monotonicity-check), [encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start), [faults.md §4](faults.md#4-item-faults-escalate)                                                                                                                                                       | each run of `max_consecutive_faults` drops restarts the group, rebased; a data fault never stops the output                                                                                             |
| 5     | A large forward DTS jump within one source                                  | [timestamps.md §6.3](timestamps.md#63-monotonicity-check)                                                                                                                                                                                                                                                                                 | written as it is, with a warning above `max_keyframe_wait`                                                                                                                                              |
| 6     | Audio timestamps from a sample counter                                      | [timestamps.md §3](timestamps.md#3-audio-internal-and-raw)                                                                                                                                                                                                                                                                                | pts = samples given to the codec; input timestamps are not read                                                                                                                                         |
| 6     | A rebuilt frame-rate stage continues the output grid                        | [timestamps.md §4.3](timestamps.md#43-building-and-re-anchoring-a-converter), [encoder-internal.md §4.6](encoder-internal.md#46-frame-rate-conversion-both-video-modes), [filters.md §16.2](filters.md#162-fps)                                                                                                                           | the old converter is drained; the new one gets the next grid slot as start time, with no `setpts` re-anchoring                                                                                          |
| 6     | A restarted decoder's timestamps cannot reset the output                    | [timestamps.md §4.1](timestamps.md#41-frame-timestamps-entering-the-converter), [§2](timestamps.md#2-lifetimes-what-resets-what)                                                                                                                                                                                                          | video enters the encoder timed by output position; a new source id resets nothing                                                                                                                       |
| 6     | Decoder-side stage rebuild and start clamp                                  | [decoder.md §16.1](decoder.md#161-internal-video-converter), [filters.md §16.2](filters.md#162-fps)                                                                                                                                                                                                                                       | the old stage is drained before a rebuild; each user has its own start rule (none, start PTS, start time), with its reason                                                                              |
| 6     | Raw re-encode across a source switch                                        | [timestamps.md §2](timestamps.md#2-lifetimes-what-resets-what), [§3](timestamps.md#3-audio-internal-and-raw), [encoder-internal.md §4.2](encoder-internal.md#42-audio-raw)                                                                                                                                                                | a new id leaves the audio counter running; new raw audio parameters drain and rebuild the resampler                                                                                                     |
| 6     | Frames held at a change reach the output                                    | [draining.md §2](draining.md#2-the-rule), [encoder-internal.md §8](encoder-internal.md#8-flush-and-end)                                                                                                                                                                                                                                   | a replaced stage is drained before it is released                                                                                                                                                       |
| 6     | No frame repeated across a gap in the content                               | [timestamps.md §4.3](timestamps.md#43-building-and-re-anchoring-a-converter), [filters.md §16.2](filters.md#162-fps)                                                                                                                                                                                                                      | the converter is drained before the gap and rebuilt after it; the stream carries a timestamp gap                                                                                                        |
| 6     | Raw audio gaps                                                              | [timestamps.md §3](timestamps.md#3-audio-internal-and-raw), [encoder-internal.md §4.2](encoder-internal.md#42-audio-raw)                                                                                                                                                                                                                  | the counter jumps over the gap, a timestamp gap with no invented samples                                                                                                                                |
| 6     | Frames without PTS fall back to the best-effort timestamp                   | [decoder.md §9](decoder.md#9-timestamps-on-the-decoding-side)                                                                                                                                                                                                                                                                             | the frame timestamp is the PTS, else the best-effort timestamp; a frame with neither is an item fault                                                                                                   |
| 7     | Durations from the next timestamp, sign right                               | [common.md §6.2](common.md#62-push-t-item)                                                                                                                                                                                                                                                                                                | a chunk's length is the later position minus the group's, emitted only when positive                                                                                                                    |
| 7     | The latest item is held and flushed at the end                              | [common.md §6.3](common.md#63-flush-t), [§6.5](common.md#65-draining-and-discarding)                                                                                                                                                                                                                                                      | every event that ends the helper flushes it                                                                                                                                                             |
| 7     | Order within one position preserved                                         | [common.md §6.2](common.md#62-push-t-item), [timestamps.md §4.1](timestamps.md#41-frame-timestamps-entering-the-converter)                                                                                                                                                                                                                | items at one position travel in one group in arrival order; frames at equal positions keep their order                                                                                                  |
| 7     | A time-base switch lines up on the previous end                             | [common.md §6.4](common.md#64-last_ts-t), [filters.md §13.1](filters.md#131-input-side), [§13.2](filters.md#132-output-side)                                                                                                                                                                                                              | a replacement helper starts at the old one's end position; graph inputs take PTS from positions                                                                                                         |
| 7     | Non-monotonic content does not crash the script                             | [content.md §4.3](content.md#43-invalid-content), [faults.md §2](faults.md#2-classes)                                                                                                                                                                                                                                                     | an item fault, dropped with a warning; nothing raises out of a streaming source                                                                                                                         |
| 7     | A graph behind a dry input rebuilds and keeps its tail                      | [filters.md §11.4](filters.md#114-what-ends-a-generation), [§11.7](filters.md#117-draining-a-generation), [§12.2](filters.md#122-filling-the-buffer)                                                                                                                                                                                      | the generation is drained when an input runs dry and rebuilt when data comes back                                                                                                                       |
| 7     | A rebuilt graph is announced as a new stream                                | [filters.md §11.3](filters.md#113-build), [§13.2](filters.md#132-output-side)                                                                                                                                                                                                                                                             | each generation takes a new source id; sink timelines continue across generations                                                                                                                       |
| 7     | Decreasing PTS out of a filter                                              | [filters.md §13.3](filters.md#133-decreasing-video-timestamps-from-a-filter)                                                                                                                                                                                                                                                              | repaired with a warning; the frame is kept                                                                                                                                                              |
| 8     | Interleaving chosen by stream count, off for HLS                            | [encoder.md §3](encoder.md#3-building-an-instance)                                                                                                                                                                                                                                                                                        | step 2 turns it off under HLS; step 3.3 turns it on for more than one non-drop track                                                                                                                    |
| 8     | A sparse stream delays an interleaved output                                | [encoder.md §3](encoder.md#3-building-an-instance)                                                                                                                                                                                                                                                                                        | step 3.3: the delay is bounded by the muxer's `max_interleave_delta`                                                                                                                                    |
| 8     | Timed ID3 before the header is queued                                       | [encoder.md §8.5](encoder.md#85-timed-id3-in-mpegts), [timestamps.md §8](timestamps.md#8-timed-id3-hls-mpegts)                                                                                                                                                                                                                            | queued until the header exists; timed on the media timeline; the ID3 stream is no copy-group member                                                                                                     |
| 8     | Decoded streams released in timestamp order                                 | [decoder.md §8.4](decoder.md#84-waiting-for-all-streams)                                                                                                                                                                                                                                                                                  | the pending queue is released by release key across streams                                                                                                                                             |
| 8     | Live audio well ahead of the first video                                    | [decoder.md §8.4](decoder.md#84-waiting-for-all-streams)                                                                                                                                                                                                                                                                                  | decoded streams start at the latest first timestamp; past `max_interleave_duration` a sliding window drops the oldest items until the late stream appears; copied fields carry the lead as an empty run |
| 8     | Start of a mapping mixing decoded streams and copied video                  | [decoder.md §8.5](decoder.md#85-start-and-seek-threshold)                                                                                                                                                                                                                                                                                 | every stream starts at one copied key packet, the last one within `start_key_tolerance` before the threshold, else the first after it                                                                   |
| 8     | Empty time at the head of a sparse stream counted                           | [encoder-copy.md §12.4](encoder-copy.md#124-sparse-sources), [timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase)                                                                                                                                                                                                             | a source's first packet is anchored at its output position                                                                                                                                              |
| 8     | Empty time at the tail of a sparse stream counted across a join             | [encoder-copy.md §12.4](encoder-copy.md#124-sparse-sources), [timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase)                                                                                                                                                                                                             | the same anchoring keeps the previous source's trailing time                                                                                                                                            |
| 9     | A segment starts on a keyframe of every stream                              | [encoder.md §8.2](encoder.md#82-keyframe-hooks), [§8.4](encoder.md#84-split_encode-frame)                                                                                                                                                                                                                                                 | a split point is accepted only when every participating stream's next packet is key                                                                                                                     |
| 9     | Only encoded streams block the split                                        | [encoder.md §8.2](encoder.md#82-keyframe-hooks)                                                                                                                                                                                                                                                                                           | participating streams are the non-drop tracks; timed ID3, waiting and silent streams do not hold the decision                                                                                           |
| 9     | Leftovers do not move the grid                                              | [encoder.md §8.7](encoder.md#87-segment-grid-hls-output-side), [timestamps.md §1.4](timestamps.md#14-the-output-timeline)                                                                                                                                                                                                                 | items 1 and 4: boundaries are absolute on the media timeline; `EXTINF` is the media time between cuts                                                                                                   |
| 9     | Where a segment is cut                                                      | [encoder.md §8.7](encoder.md#87-segment-grid-hls-output-side)                                                                                                                                                                                                                                                                             | item 3: at the last split point at or before the boundary, else the first after it                                                                                                                      |
| 9     | Cuts for metadata                                                           | [encoder.md §8.7](encoder.md#87-segment-grid-hls-output-side)                                                                                                                                                                                                                                                                             | item 5: extra cuts at split points move no later boundary; `insert_id3` runs at segment open                                                                                                            |
| 9     | Bounded wait for a split point                                              | [encoder.md §8.7](encoder.md#87-segment-grid-hls-output-side), [faults.md §5](faults.md#5-waiting)                                                                                                                                                                                                                                        | item 6: a forced cut only when the segment holds no split point within the RFC 8216 rounded target duration, with a warning; such variants do not announce independent segments                         |
| 10    | Every stage drains before it ends                                           | [draining.md §2](draining.md#2-the-rule), [§3](draining.md#3-order)                                                                                                                                                                                                                                                                       | the rule, its events and its upstream-first order                                                                                                                                                       |
| 10    | A copied file's last packet                                                 | [decoder.md §13.1](decoder.md#131-end-of-input), [decoder-copy.md §6](decoder-copy.md#6-flush)                                                                                                                                                                                                                                            | the helper is flushed at end of input; the track ends at the longest field                                                                                                                              |
| 10    | Raw decoders and chunkers keep their last item                              | [decoder-raw.md §6](decoder-raw.md#6-flush), [common.md §6.5](common.md#65-draining-and-discarding), [inline-codecs.md §8](inline-codecs.md#8-chunker)                                                                                                                                                                                    | every helper user flushes at every end event                                                                                                                                                            |
| 10    | Frame-rate stage replaced without draining                                  | [timestamps.md §4.3](timestamps.md#43-building-and-re-anchoring-a-converter), [encoder-internal.md §4.6](encoder-internal.md#46-frame-rate-conversion-both-video-modes)                                                                                                                                                                   | drained before every rebuild                                                                                                                                                                            |
| 10    | Audio re-framer and resampler                                               | [encoder-internal.md §8](encoder-internal.md#8-flush-and-end), [§4.2](encoder-internal.md#42-audio-raw), [draining.md §4](draining.md#4-partial-final-units)                                                                                                                                                                              | drained at stop, restart and parameter change; the final partial frame is delivered                                                                                                                     |
| 10    | Inline encoder's tail                                                       | [inline-codecs.md §13](inline-codecs.md#13-flush-and-end), [draining.md §5](draining.md#5-discarded-on-purpose)                                                                                                                                                                                                                           | drained at end of input into the buffer the producer delivers; a sleeping producer has no reader and discards                                                                                           |
| 10    | Items waiting for the cross-stream release order                            | [decoder.md §13.1](decoder.md#131-end-of-input)                                                                                                                                                                                                                                                                                           | step 1: end of input starts the mapping and releases the queue                                                                                                                                          |
| 10    | Track marks and metadata of an inline encode                                | [inline-codecs.md §3.3](inline-codecs.md#33-metadata-and-track-marks)                                                                                                                                                                                                                                                                     | placed by input time, before the first output item derived from later input                                                                                                                             |
| 10    | Copy packets held during a switch at stop                                   | [encoder-copy.md §8](encoder-copy.md#8-flush-and-end)                                                                                                                                                                                                                                                                                     | `flush` starts every waiting group, so held packets are written                                                                                                                                         |
| 10    | Filter graph generation                                                     | [filters.md §11.7](filters.md#117-draining-a-generation)                                                                                                                                                                                                                                                                                  | each generation is drained before the next one's output                                                                                                                                                 |
| 11    | Which parameter differences a copied stream survives                        | [content.md §3.1](content.md#31-ffmpegcopy)                                                                                                                                                                                                                                                                                               | split in three: type properties, header details judged per container, descriptive properties                                                                                                            |
| 11    | A type-property mismatch is rejected before its packets                     | [content.md §3.1.1](content.md#311-where-copy-parameters-are-merged)                                                                                                                                                                                                                                                                      | every path merges at open; a failed merge is a source fault of that source                                                                                                                              |
| 11    | Header details at a switch                                                  | [encoder.md §13.3](encoder.md#133-incompatible-copy-source), [encoder-copy.md §12.7](encoder-copy.md#127-parameters-across-sources)                                                                                                                                                                                                       | in-band containers accept the change; header-storing containers restart where they define a clean restart, else the output stops naming both parameter sets                                             |
| 11    | Descriptive differences                                                     | [content.md §3.1](content.md#31-ffmpegcopy), [encoder.md §13.3](encoder.md#133-incompatible-copy-source)                                                                                                                                                                                                                                  | a warning naming both values; the first header is kept; never a restart, stop or rejection                                                                                                              |
| 11    | Live relay whose remote restarts with new extradata                         | [content.md §3.1.1](content.md#311-where-copy-parameters-are-merged), [io.md §2.2](io.md#22-one-connection-attempt)                                                                                                                                                                                                                       | accepted at reconnect; the output's container judges the change                                                                                                                                         |
| 11    | Converting the packetisation per source                                     | [bitstream-filters.md §5](bitstream-filters.md#5-packets-through-the-filter), [content.md §3.1.1](content.md#311-where-copy-parameters-are-merged)                                                                                                                                                                                        | operators that rewrite parameters accept any input; only their output format is merged                                                                                                                  |
| 11    | Relaxed check                                                               | [content.md §3.1](content.md#31-ffmpegcopy), [encoder.md §13.3](encoder.md#133-incompatible-copy-source)                                                                                                                                                                                                                                  | type merge and runtime check reduce to media and codec id; libav judges the rest                                                                                                                        |
| 11    | Copy decoder on a codec change under one source id                          | [inline-codecs.md §10.1](inline-codecs.md#101-copy-trackffmpegdecodeaudio)                                                                                                                                                                                                                                                                | a change of type properties drains and rebuilds; an extradata change alone goes to the decoder in band                                                                                                  |
| 12    | Copied and re-encoded streams share a reference                             | [timestamps.md §1.4](timestamps.md#14-the-output-timeline)                                                                                                                                                                                                                                                                                | every stream follows the output position                                                                                                                                                                |
| 12    | Encoded streams do not start ahead of copied ones                           | [encoder.md §5](encoder.md#5-encoding-a-frame), [encoder-copy.md §2](encoder-copy.md#2-stream-creation)                                                                                                                                                                                                                                   | streams are created at the first frame in which every copy field has known parameters; earlier frames write nothing                                                                                     |
| 12    | Time a copied stream skips stays in the timeline                            | [timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase)                                                                                                                                                                                                                                                                          | `skipped` grows only at a whole-output switch, which an output with a re-encoded stream never has                                                                                                       |
| 12    | The copy side learns timing from its first accepted packet                  | [timestamps.md §6.2](timestamps.md#62-beginning-a-source-rebase)                                                                                                                                                                                                                                                                          | the first held packet after the trim is anchored at its output position                                                                                                                                 |
| 13    | Chunk lengths do not accumulate rounding                                    | [common.md §6.0](common.md#60-absolute-origin), [§6.1](common.md#61-state)                                                                                                                                                                                                                                                                | positions are converted absolute timestamps minus a converted origin                                                                                                                                    |
| 13    | Every producer follows the rule                                             | [decoder.md §8.3](decoder.md#83-positions-and-sparse-advance), [§9.1](decoder.md#91-grouping-by-timestamp-increments), [decoder-copy.md §5](decoder-copy.md#5-timestamps), [inline-codecs.md §8](inline-codecs.md#8-chunker), [filters.md §13.2](filters.md#132-output-side), [content.md §7](content.md#7-timestamps-carried-in-content) | one owner of content positions (copied packets by DTS); each producer derives lengths from the helper or from absolute sample counts                                                                    |
| 13    | Streams of one source stay together                                         | [common.md §6.6](common.md#66-conformance)                                                                                                                                                                                                                                                                                                | two streams' length difference equals their timestamp-span difference within two ticks after 10 hours                                                                                                   |

Totals: 97 points, all answered.

## Trace

| Entry | Commits                                                                                                                                | Issues / PRs                                                                                                                                                                              |
| ----- | -------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1     | 8f26206a7, 104498249, 233a3ef84, 89a4affe9, e4a31719a, 26946cfbb, 65c7161b1, 575faefe8, 0e44c47ab, 4b56f7b80                           | #1979 (YouTube stall), PR #1286, PR #3628; test `test_ffmpeg_copy_concat.liq`                                                                                                             |
| 2     | c12e3d020, 4b56f7b80, 0e44c47ab                                                                                                        | PR #2192, PR #3628                                                                                                                                                                        |
| 3     | 2f3bccefc, 575faefe8, d9a4cba15, 07a5a54d9                                                                                             | PR #2343, PR #2382, #1389, #3840 / PR #3841 (test `GH3840.liq`), PR #5302                                                                                                                 |
| 4     | a71e499b6, 25f118689, 07a5a54d9                                                                                                        | PR #2869, PR #5302 ("Obj.magic" hash, negative count; "mk_stream_store keeps its protocol")                                                                                               |
| 5     | 2f3bccefc, 19f6d3de6, 3d9ee0145, 884609d31 (revert)                                                                                    | —                                                                                                                                                                                         |
| 6     | d8c2321be, 6b362dc4f, 842c6b467, 68fd89f94, f682e8d9b, d42c7ecbf, 31eda00d4; 45f579b8f (removes the best-effort fallback of f682e8d9b) | #1091, #1100, #1609, PR #2978, PR #3032, PR #3109, #3329 (open)                                                                                                                           |
| 7     | 7b7c63220, 406dd323c, 341eb4da1, 07a5a54d9, 676ecbf1e                                                                                  | #3944 / PR #5332 (test `GH3944.liq`), #5071, PR #3764, PR #5302                                                                                                                           |
| 8     | ac3451d62, 229719d8a, 058d170aa, 2aec3874c, 4b56f7b80, 6f08ad6f7, 89b81b2e8                                                            | PR #3638, #4398 / PR #4401, PR #5124, PR #3616, PR #4861, PR #4876, PR #4877                                                                                                              |
| 9     | 205027736, a3e5bdaaa, 44e8f661e (current keyframe hook order), PR #3636, PR #5320, PR #5327 merges                                     | #3630, #5266, #5319 (test `GH5319.liq`), PR #4841                                                                                                                                         |
| 10    | 31eda00d4, 233a3ef84, d9d649c45 (bindings: drain decoders at end of input)                                                             | PR #3109, PR #5442; as-built pass: "Encoder tail is never delivered", "Metadata and track marks are not aligned", "Raw decoders drop the last frame", "Pending interleave items are lost" |
| 11    | —                                                                                                                                      | #3164 (open); as-built pass: "Audio copy decoder ignores codec changes"                                                                                                                   |
| 12    | —                                                                                                                                      | as-built pass                                                                                                                                                                             |
| 13    | —                                                                                                                                      | #4502 (open); as-built pass: "Chunk lengths truncate per packet", "Copy chunk lengths drift", "Duration chunks drift"                                                                     |
