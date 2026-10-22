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
current rules live there; this file only says where they were learnt.
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
the first DTS after a join equals the previous source's last DTS plus that
packet's duration plus the time the new source spent before its first packet;
the packet count equals the sum of the inputs' packet counts (no packet lost
at the end of a file, §10); total duration equals the sum of the inputs to
within one frame. Repeat with inputs whose packets carry no duration: no join
overlaps the previous source's last packet.

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
has to be shared state that every stream of the set can raise to the latest
end seen, not a value fixed once. Raising it after a sibling has already
started rebased packets with the smaller value moves that sibling forward by
the difference.

**How to tell.** Build a sequence of files in which audio is 200 ms shorter
than video, copy both. After ten joins, the first audio PTS and the first
video PTS of the last file differ by the same amount as in the source file,
to within one frame; no stream's first packet after a join precedes the
latest end of any stream of the previous source. Run it twice, with the
streams declared to the encoder in both orders: the two outputs are
identical. Within each source, every stream's DTS steps equal its source's
own steps, including the step between the first and second packet after a
join: no stream jumps forward once a sibling has started.

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
- A stream can leave the set before it ever became ready; whatever counts
  readiness must then not be decremented below what that stream contributed.
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

**How to tell.** Feed a copy stream packets lacking DTS: they are written.
Feed it a packet whose rebased DTS equals the previous one: it is dropped
with a warning and the output keeps running. Feed a packet lacking DTS, then
one whose DTS is not greater than the last DTS written: the second is
dropped. Feed rebased DTS that differ by less than one unit of the output
container's time base: the output keeps running. At no point does the muxer
report a non-monotonic DTS.

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
cue is still at 20 min after the join. A live input whose audio starts 2 s before video: the first decoded
audio and video positions match to within 30 ms.

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
within one codec frame. A video with keyframes every 4 s and 1 s segments:
every segment starts with a keyframe and no `EXTINF` exceeds the target
duration. A video with a keyframe every segment duration: the first packet
of every segment is a keyframe, and no segment ends with a keyframe.

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
lost at the change. Put an inline encoder to sleep after a finite input: the
encoded tail reaches the output. Track marks of an inline encode fall on the
first packet of the new track.

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

**Why it is hard.** A muxer fixes a stream's parameters when it writes the
header; a copy stream cannot change them afterwards. Comparing parameters is
codec-specific (which differences are harmless, which need a bitstream
filter, which are fatal), and the switch is decided before the new source's
first packet is seen.

**How to tell.** Copy H.264 from an MP4 file, then from an MPEG-TS file, to
an MPEG-TS output: the output either converts the packetisation or reports
the mismatch at the switch with the names of both sources; it never feeds
the muxer packets in the wrong form. Switch a copied audio stream between
AAC and MP3 sources: same.

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

## Reconciliation

Each entry is split into the lessons it carries. Status: **Covered** (the
linked rule handles it), **Not covered** (the spec is silent or wrong),
**Contradicts** (the as-built rule is the behaviour the history shows to be
wrong). Findings are in
[findings/known-complexity.md](findings/known-complexity.md) and, where
named, [findings/encoder.md](findings/encoder.md) (E-) or
[findings/inline-codecs.md](findings/inline-codecs.md) (I-).

| Entry | Lesson                                                           | Status      | Rule or finding                                                                                                                                                                                                  |
| ----- | ---------------------------------------------------------------- | ----------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1     | One offset on PTS and DTS keeps the reordering delay             | Covered     | [timestamps.md#61-order-of-operations](timestamps.md#61-order-of-operations) step 4                                                                                                                              |
| 1     | End of a source = last DTS + that packet's duration              | Covered     | [timestamps.md#61-order-of-operations](timestamps.md#61-order-of-operations) step 5                                                                                                                              |
| 1     | End of a source when the last packet has no duration             | Contradicts | +1 tick, so a join overlaps by a packet: [timestamps.md#64-missing-values](timestamps.md#64-missing-values); finding "Missing duration overlaps a join"                                                          |
| 1     | New source's first DTS from the first accepted packet            | Covered     | [timestamps.md#62-beginning-a-source-rebase](timestamps.md#62-beginning-a-source-rebase)                                                                                                                         |
| 1     | One time base for all bookkeeping                                | Covered     | [timestamps.md#6-copy-streams-per-track](timestamps.md#6-copy-streams-per-track) (main ticks)                                                                                                                    |
| 1     | Reconnect is a source switch                                     | Covered     | [encoder-copy.md#126-reconnects-and-new-decoders](encoder-copy.md#126-reconnects-and-new-decoders), [io.md#13-timestamps-across-reconnects](io.md#13-timestamps-across-reconnects); E-V4                         |
| 2     | One offset per source, raised to the latest end                  | Covered     | [timestamps.md#7-copy-streams-across-tracks](timestamps.md#7-copy-streams-across-tracks), [encoder-copy.md#121-model](encoder-copy.md#121-model)                                                                 |
| 2     | Raising the offset after a sibling started moves it              | Contradicts | E-D4 (checked); finding "Shared offset: order-dependent jump and early read"                                                                                                                                     |
| 3     | Discard after the fact until a keyframe                          | Covered     | [encoder-copy.md#122-accepting-a-packet](encoder-copy.md#122-accepting-a-packet)                                                                                                                                 |
| 3     | Same-source streams start together                               | Contradicts | E-D2 (checked), E-D3; finding "Same-source streams do not wait for each other"                                                                                                                                   |
| 3     | Different-source streams do not wait for each other              | Covered     | [encoder-copy.md#123-contributions-and-readiness](encoder-copy.md#123-contributions-and-readiness) (step 1.5 row; GH3840)                                                                                        |
| 3     | Intra-only streams are ready at once                             | Covered     | [encoder-copy.md#122-accepting-a-packet](encoder-copy.md#122-accepting-a-packet) step 1.1                                                                                                                        |
| 3     | A stream leaving before it is ready does not underflow the count | Covered     | [encoder-copy.md#125-release-and-lifetime-of-source-records](encoder-copy.md#125-release-and-lifetime-of-source-records)                                                                                         |
| 4     | One identity for every track an inline encode produces           | Covered     | [inline-codecs.md#5-stream-index](inline-codecs.md#5-stream-index)                                                                                                                                               |
| 4     | Identity stable when memory moves                                | Covered     | [inline-codecs.md#5-stream-index](inline-codecs.md#5-stream-index) (hashed by id, compared physically)                                                                                                           |
| 4     | State exists exactly while a stream of the source is muxed       | Contradicts | E-D5, E-G1, I-"Stream index after sleep depends on the GC"; finding "Per-source state outlives or predeceases its streams"                                                                                       |
| 5     | Packets without DTS are written                                  | Covered     | [timestamps.md#63-monotonicity-check](timestamps.md#63-monotonicity-check), [timestamps.md#64-missing-values](timestamps.md#64-missing-values)                                                                   |
| 5     | Equal or decreasing DTS dropped with a warning                   | Covered     | [timestamps.md#63-monotonicity-check](timestamps.md#63-monotonicity-check)                                                                                                                                       |
| 5     | The check holds after a packet without DTS                       | Not covered | E-D6; finding "Monotonicity check skipped after a DTS-less packet"                                                                                                                                               |
| 5     | The check uses the offset actually applied                       | Covered     | [timestamps.md#63-monotonicity-check](timestamps.md#63-monotonicity-check) (last paragraph: checked value below written value, both above the last DTS)                                                          |
| 6     | Audio timestamps from a sample counter                           | Covered     | [timestamps.md#3-audio-internal-and-raw](timestamps.md#3-audio-internal-and-raw)                                                                                                                                 |
| 6     | A rebuilt frame-rate stage continues the output grid             | Covered     | [timestamps.md#43-building-and-re-anchoring-a-converter](timestamps.md#43-building-and-re-anchoring-a-converter)                                                                                                 |
| 6     | Decoder-side stage restart and start clamp                       | Covered     | [decoder-internal.md#22-internal-video-converter](decoder-internal.md#22-internal-video-converter), [filters.md#162-fps](filters.md#162-fps)                                                                     |
| 6     | Frames without PTS fall back to the best-effort timestamp        | Contradicts | finding "Best-effort timestamp fallback removed"                                                                                                                                                                 |
| 6     | Raw re-encode across a source switch (#3329)                     | Contradicts | video: [timestamps.md#43-building-and-re-anchoring-a-converter](timestamps.md#43-building-and-re-anchoring-a-converter); audio: E-A1; finding "Raw audio keeps the first source's resampler"                     |
| 7     | Durations from the next timestamp, sign right                    | Covered     | [common.md#62-push-t-item](common.md#62-push-t-item)                                                                                                                                                             |
| 7     | Order within one position preserved                              | Not covered | finding "Order at equal positions is not stated"                                                                                                                                                                 |
| 7     | A time-base switch lines up on the previous end                  | Covered     | [filters.md#131-input-side](filters.md#131-input-side) (duration converter)                                                                                                                                      |
| 7     | A graph behind a dry input rebuilds and keeps its tail           | Covered     | [filters.md#114-what-ends-a-generation](filters.md#114-what-ends-a-generation), [filters.md#122-filling-the-buffer](filters.md#122-filling-the-buffer), [filters.md#132-output-side](filters.md#132-output-side) |
| 7     | A rebuilt graph is announced as a new stream                     | Covered     | [filters.md#132-output-side](filters.md#132-output-side), [filters.md#115-reset](filters.md#115-reset)                                                                                                           |
| 8     | Interleaving chosen by stream count; off for HLS                 | Covered     | [encoder.md#3-building-an-instance](encoder.md#3-building-an-instance) steps 2 and 4.3; E-A3 (drop tracks counted)                                                                                               |
| 8     | Timed ID3 before the header is queued                            | Covered     | [encoder.md#85-timed-id3-in-mpegts](encoder.md#85-timed-id3-in-mpegts), [timestamps.md#8-timed-id3-hls-mpegts](timestamps.md#8-timed-id3-hls-mpegts)                                                             |
| 8     | Decoded streams released in timestamp order                      | Covered     | [decoder.md#84-waiting-for-all-streams](decoder.md#84-waiting-for-all-streams)                                                                                                                                   |
| 8     | Empty time at the head of a sparse stream counted                | Covered     | [encoder-copy.md#124-stream-head-of-a-sparse-source](encoder-copy.md#124-stream-head-of-a-sparse-source), [timestamps.md#62-beginning-a-source-rebase](timestamps.md#62-beginning-a-source-rebase)               |
| 8     | Empty time at the tail of a sparse stream counted across a join  | Contradicts | finding "Trailing empty time of a sparse copy stream is not counted"                                                                                                                                             |
| 9     | A segment starts on a keyframe of every encoded stream           | Contradicts | E-D1 (checked); finding "HLS split lands after the keyframe"                                                                                                                                                     |
| 9     | Only encoded streams block the split                             | Covered     | [encoder.md#84-split_encode-frame](encoder.md#84-split_encode-frame) step 1                                                                                                                                      |
| 9     | Bounded wait, grid not moved by leftovers                        | Not covered | finding "HLS segment grid has no owner in this spec"                                                                                                                                                             |
| 10    | Every stage drains before it is discarded                        | Contradicts | E-G2, E-G3, E-G8, I-"Encoder tail is never delivered", I-"Metadata and track marks are not aligned"; finding "Held data is lost at ends and changes"                                                             |
| 11    | Parameters of a new copied source checked                        | Not covered | E-D10 (checked), I-"Audio copy decoder ignores codec changes"; finding "Copy parameters across sources"                                                                                                          |
| 12    | Copied and re-encoded streams share a reference                  | Not covered | E-A2; finding "Copied and re-encoded streams drift apart"                                                                                                                                                        |
| 13    | Chunk lengths do not accumulate rounding                         | Contradicts | I-"Chunk lengths truncate per packet" (checked), findings/decoder.md "Copy chunk lengths drift", findings/common.md "Duration chunks drift"; finding "Chunk-length drift matches #4502"                          |

Totals: 27 covered, 5 not covered, 10 contradicts.

## Trace

| Entry | Commits                                                                                                                                | Issues / PRs                                                                                                                                                                                                                                                       |
| ----- | -------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| 1     | 8f26206a7, 104498249, 233a3ef84, 89a4affe9, e4a31719a, 26946cfbb, 65c7161b1, 575faefe8, 0e44c47ab, 4b56f7b80                           | #1979 (YouTube stall), PR #1286, PR #3628; test `test_ffmpeg_copy_concat.liq`                                                                                                                                                                                      |
| 2     | c12e3d020, 4b56f7b80, 0e44c47ab                                                                                                        | PR #2192, PR #3628                                                                                                                                                                                                                                                 |
| 3     | 2f3bccefc, 575faefe8, d9a4cba15, 07a5a54d9                                                                                             | PR #2343, PR #2382, #1389, #3840 / PR #3841 (test `GH3840.liq`), PR #5302                                                                                                                                                                                          |
| 4     | a71e499b6, 25f118689, 07a5a54d9                                                                                                        | PR #2869, PR #5302 ("Obj.magic" hash, negative count; "mk_stream_store keeps its protocol")                                                                                                                                                                        |
| 5     | 2f3bccefc, 19f6d3de6, 3d9ee0145, 884609d31 (revert)                                                                                    | —                                                                                                                                                                                                                                                                  |
| 6     | d8c2321be, 6b362dc4f, 842c6b467, 68fd89f94, f682e8d9b, d42c7ecbf, 31eda00d4; 45f579b8f (removes the best-effort fallback of f682e8d9b) | #1091, #1100, #1609, PR #2978, PR #3032, PR #3109, #3329 (open)                                                                                                                                                                                                    |
| 7     | 7b7c63220, 406dd323c, 341eb4da1, 07a5a54d9, 676ecbf1e                                                                                  | #3944 / PR #5332 (test `GH3944.liq`), #5071, PR #3764, PR #5302                                                                                                                                                                                                    |
| 8     | ac3451d62, 229719d8a, 058d170aa, 2aec3874c, 4b56f7b80, 6f08ad6f7, 89b81b2e8                                                            | PR #3638, #4398 / PR #4401, PR #5124, PR #3616, PR #4861, PR #4876, PR #4877                                                                                                                                                                                       |
| 9     | 205027736, a3e5bdaaa, 44e8f661e (current keyframe hook order), PR #3636, PR #5320, PR #5327 merges                                     | #3630, #5266, #5319 (test `GH5319.liq`), PR #4841                                                                                                                                                                                                                  |
| 10    | 31eda00d4, 233a3ef84, d9d649c45 (bindings: drain decoders at end of input)                                                             | PR #3109, PR #5442; as-built: findings/encoder.md G2, G3, G8; findings/inline-codecs.md "Encoder tail is never delivered", "Metadata and track marks are not aligned"; findings/decoder.md "Raw decoders drop the last frame", "Pending interleave items are lost" |
| 11    | —                                                                                                                                      | #3164 (open); as-built: findings/encoder.md D10, findings/inline-codecs.md "Audio copy decoder ignores codec changes"                                                                                                                                              |
| 12    | —                                                                                                                                      | as-built: findings/encoder.md A2                                                                                                                                                                                                                                   |
| 13    | —                                                                                                                                      | #4502 (open); as-built: findings/inline-codecs.md "Chunk lengths truncate per packet", findings/decoder.md "Copy chunk lengths drift", findings/common.md "Duration chunks drift"                                                                                  |
