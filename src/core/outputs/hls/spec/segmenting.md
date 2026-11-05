# Segmenting

Part A. This file owns how each variant's byte stream is cut into
segments: the grid and its origin, split points, the cut rule, the
target-duration bound, output-wide cuts and container restarts. It relies on
the encoder pieces of [encoder-contract.md §5](encoder-contract.md#5-split_encode).
What opening and closing a segment does on disk is [files.md](files.md);
how segments are numbered is [playlists.md §2.1](playlists.md#21-media-sequence).

These rules are the only statement of the grid. The `%ffmpeg` encoder spec
states what an encoder provides to it
([encoder.md §8.7](../../../optionals/ffmpeg/spec/encoder.md#87-segment-grid-hls-output-side)).

## 1. Time, the grid and the target duration

Time is counted in **main ticks**, the internal time unit. A **frame** is the
configured frame size in ticks.

- `D`, the segment duration in ticks, is `segment_duration` rounded down to
  whole frames. A `segment_duration` shorter than one frame MUST be rejected
  at construction ([operator.md §2](operator.md#2-construction)).
- `T`, the target duration, is `D` in seconds rounded to the nearest
  integer, halves up, and at least 1. It is fixed for the life of the
  output's playlists, and every variant announces the same `T` (RFC 8216
  §6.2.1, §6.2.4). Reason for rounding rather than ceiling: a nominal
  6.006 s segment (29.97 fps) then announces 6, as Apple's authoring
  specification recommends (§7.5–7.6), and its rounded `EXTINF` is still 6.
- **Run**: the time from a `start`, or from the rebuild after an error
  ([operator.md §6](operator.md#6-errors-and-reopening)), to the next `stop`
  or error. All variants of an output are built together at the start of a
  run and MUST NOT be rebuilt one at a time.
- **Media time**: ticks given to the encoders since the run started. It
  advances by each chunk's length and is the same for every variant.
- **Grid**: boundary _k_ is `k × D` of media time. Its **origin** is media
  time 0 of the run, shared by every variant. A boundary MUST NOT depend on
  where any segment actually ended.

Reason for one shared origin and a strict grid: a player switching variants
mid-stream expects segment boundaries at the same media time in every
variant (Apple §8.22). Reason for absolute boundaries: codec frames do not
line up with internal frames, and a grid placed relative to the previous cut
drifts by a codec-dependent amount, so variants drift apart without bound.

## 2. Segments in memory

An open segment has a start (media time of its first byte), its bytes so
far, and its **split points**: pairs (media time, byte offset), each strictly
after the start, in increasing time. Its **length** is the media time between
its start and its cut, or the current media time while open. Its `EXTINF`
duration is its length in seconds. It also records whether it starts at a
forced split.

A variant is **free-split** when it has a split point within one codec
frame of any media time the output requests (audio-only and intra-only
content, packed audio). The encoder says so
([encoder-contract.md §6](encoder-contract.md#6-properties)).

The **request** passed to the encoder with each chunk is the next media
time at which the output wants a split point: the first grid boundary at or
after the chunk's media time (and after 0), or a pending sync time (§7) when
that comes first. Reason: the encoder must learn of each boundary before it
encodes the image there; an encoder whose delay exceeds the segment duration
would otherwise get the request only after the cut for the previous
boundary, too late to make a key frame on time.

## 3. Applying pieces

The pieces an encoder returns for a chunk are applied in order:

- **Bytes**: appended to the open segment.
- **Split** `{time, forced}`: recorded (§2) if after the segment's start,
  then pending sync cuts (§7) and the cut rule (§4) run. A forced split is
  logged as a warning naming the variant and the time.
- **Restart** `{time, init}`: §8.

## 4. The cut rule

Each variant tracks its **current boundary** _k_, the first boundary after
its open segment's start. When a split point with time `t ≥ k` is recorded:

1. **Candidates.** `b`, the latest split point at or before _k_, if any;
   `a`, the one just recorded.
2. **Choice.** A candidate is **eligible** when cutting there gives the
   segment a length that, in seconds rounded to the nearest integer, is at
   most `T`. The cut point is the eligible candidate nearer to _k_, `b` on a
   tie. If neither is eligible, `a`: the bound of §5 did not hold because
   the encoder declined a forced split.
3. **Served.** When the choice is `a`, `a` is eligible, and the open
   segment's start is nearer to _k_ than `a`, no cut is made: the cut that
   opened the segment has served _k_.
4. **Cut** there (§4.1), unless step 3 applies. The current boundary becomes
   the first boundary after both _k_ and the cut point. If a split point of
   the open segment is at or after that boundary, repeat from step 1.

Reason for step 3 and for "after _k_": with split points just before every
boundary, a cut made at one of them — by this rule or by the bound of §5 —
leaves the next split point past _k_, and cutting there would leave a stub
of one key-frame interval.

A segment MUST NOT be cut anywhere but at a split point.

Reason for "nearest": content often puts every key frame a few tens of
milliseconds after its boundary (B-frame delay, encoder priming). A rule
that only looks at or before _k_ then cuts one key-frame interval early and
leaves a stub, on every segment. Eligibility keeps the tolerance inside RFC
8216 §4.3.3.1 (a rounded `EXTINF` never exceeds the target duration) and
Apple §7.7 (no segment exceeds it by more than 0.5 s). Because `T` is then
a rounded bound, playlists announce version 6
([playlists.md §1](playlists.md#1-common-form)).

What is not promised: when key frames sit close to half a second after the
grid, `a` is not eligible, and segments alternate long and short.

### 4.1 Cutting

Cutting the open segment at (`t`, `o`):

1. The bytes after `o` and the split points after `t` are taken out.
2. The segment's length becomes `t − start`; it is closed with its bytes up
   to `o` ([files.md §4](files.md#4-closing-a-segment)).
3. A new segment opens at `t` (§6).
4. The taken bytes and split points are moved into it.

### 4.2 Worked example

`D` = 6 s, `T` = 6. A copied video variant has key frames every 2 s, each
0.1 s after a multiple of 2 s, so its split points arrive at 2.1, 4.1, 6.1, …

| Split arrives | Boundary | Candidates     | Cut  | Segment   |
| ------------- | -------- | -------------- | ---- | --------- |
| 6.1           | 6        | b 4.1, a 6.1   | 6.1  | 0–6.1     |
| 12.1          | 12       | b 10.1, a 12.1 | 12.1 | 6.1–12.1  |
| 18.1          | 18       | b 16.1, a 18.1 | 18.1 | 12.1–18.1 |

The first segment is 6.1 s (rounded 6); every later one is 6 s. A
free-split audio variant of the same output cuts at 6, 12, 18: segment _n_
starts within 0.1 s in both.

## 5. The target-duration bound

Before each chunk is given to `split_encode`, with `len` its length: if the
open segment's length after the chunk, rounded to the nearest second, would
exceed `T`:

1. If the segment holds a split point, cut at the latest one (§4.1); the
   current boundary becomes the first after it; check again.
2. Otherwise `split_encode` is called with `force` true.

`force` is false otherwise. The encoder decides whether to honour it
([encoder-contract.md §5](encoder-contract.md#5-split_encode)). Each
segment's rounded length stays at or below `T` when it does.

## 6. Opening a segment

A segment opens with a start time. It takes, from its variant, the pending
discontinuity flag, the pending inserted tags
([metadata.md §3](metadata.md#3-inserted-tags)) and the current init
segment. Before any encoder byte, in this order, it receives the ID3 tag
for its start when one is due ([metadata.md §2](metadata.md#2-id3)), then
the encoder's `header()` bytes.

The first segment of a run opens at media time 0, at the first chunk.

A segment with no encoder bytes, or with length 0, MUST NOT be published or
listed: it is dropped. A segment is dropped also when aborted
([operator.md §6](operator.md#6-errors-and-reopening)). A dropped segment
gives its discontinuity flag and inserted tags back to the variant's
pending state, so the next segment carries them.

## 7. Output-wide cuts

A **sync cut** at media time `t` ends the open segment of every variant at
that variant's first split point at or after `t` that is not already in a
closed segment. Until it is made, `t` is a pending sync time of the variant
and is passed as the request (§2), so encoders that make key frames make
one at `t`, and free-split variants have a split point there. It is used
for:

- a container restart in any variant (§8), marked as a discontinuity;
- track metadata and inserted tags, as decided by
  [metadata.md §4](metadata.md#4-cuts-on-metadata-and-tags).

Rules:

- In each variant, a sync cut is one cut. If its point is also the point
  the cut rule (§4) chooses, that one cut serves both.
- A sync cut does not move the grid: the current boundary after it is the
  first boundary after its cut point.
- A sync cut MUST NOT be forced. It waits for a split point; the bound of
  §5 applies meanwhile.
- A discontinuous sync cut marks the segment it opens as discontinuous in
  every variant, so the discontinuity sequence numbers stay equal across
  variants (RFC 8216 §6.2.4, Apple §8.15).

**Alignment.** When every variant is free-split or makes key frames, a sync
cut lands within one codec frame of `t` in every variant, and the grid stays
aligned: segment _n_ covers the same media time everywhere and counts are
equal. What is not promised: in a copied variant that does not itself
restart, the cut waits for the next key frame of the source, so content
between `t` and that key frame carries different discontinuity sequence
numbers in different variants; and a sync cut just before a boundary can be
two cuts in one variant and one in another, so counts can differ by one
after it.

## 8. Container restart

On `Restart {time, init}` in one variant:

1. Its open segment is cut at (`time`, current byte position) (§4.1); the
   new segment is discontinuous and takes `init` as its init segment
   ([files.md §6](files.md#6-init-segments)).
2. A discontinuous sync cut at `time` (§7) is made in every other variant.
   A variant that restarts at the same `time` (copied streams of one
   source) takes its own restart as that cut.

A restart is a split point, a discontinuity and possibly a new init
segment at once; the three land on the same segment. A `Restart` may reach
the output after other variants have passed `time`; they cut at their next
split point, and the mismatch of §7 covers the content in between.
