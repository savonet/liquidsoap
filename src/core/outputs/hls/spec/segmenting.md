# Segmenting

Part A. This file owns how each variant's byte stream is cut into
segments: the grid and its origin, split points, the cut rules, the target
duration, output-wide cuts, container restarts and the warnings on content
that cannot be cut regularly. It relies on
the encoder pieces of [encoder-contract.md §5](encoder-contract.md#5-access-points-and-restarts).
What opening and closing a segment does on disk is [files.md](files.md);
how segments are numbered is [playlists.md §2.1](playlists.md#21-media-sequence).

These rules are the only statement of the grid. The `%ffmpeg` encoder spec
states what an encoder provides to it
([encoder.md §8.7](../../../optionals/ffmpeg/spec/encoder.md#87-segmenting-hls-output-side)).

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
  where any segment actually ended. Only free-split variants (§2) cut on the
  grid (§4.2).

Reason for one shared origin and a strict grid: a player switching variants
mid-stream expects segment boundaries at the same media time in every
variant (Apple §8.22). Reason for absolute boundaries: codec frames do not
line up with internal frames, and a grid placed relative to the previous cut
drifts by a codec-dependent amount, so free-split variants of different
codecs (AAC and MP3, AAC-LC and HE-AAC) drift apart without bound.

Reason for no grid with key frames: the output cannot place key frames, so a
grid only works with key frames whose interval divides `D` exactly. At 29.97
fps, key frames every 2.002 s drift from a 30 s grid by 0.03 s per segment
until the nearest key frame flips to the one before, about every 34
segments. Cut relative to the previous cut (§4.1), the same key frames give
the same length every time.

A variant's segmenting depends only on its own split points; the variants of
an output are aligned when their content is: variants whose split points
have the same times have the same cuts. Content fed to every variant from
one source carries the same discontinuities, sync points and restarts to
each of them in the same tick, so their origins and resets agree. Key frames
at the same times in every key-frame variant are the script's to provide:
the encoders' key-frame settings, or the source's for copied streams.

A free-split variant beside key-frame variants (an audio-only variant that
drops the video track) cuts on the grid while they cut on their key frames.
Its segment _n_ starts within the key frames' offset from the grid, which
stays constant when the key-frame interval divides `D` exactly, and drifts
otherwise (29.97 fps). Choosing such an interval is the script's: the
output does not check it.

## 2. Segments in memory

An open segment has a start (media time of its first byte), its bytes so
far, and its **split points**: pairs (media time, byte offset), each strictly
after the start, in increasing time. Its **length** is the media time between
its start and its cut, or the current media time while open. Its `EXTINF`
duration is its length in seconds.

A variant is **free-split** when it has a split point within one codec
frame of any media time the output requests (audio-only and intra-only
content, packed audio). The encoder says so
([encoder-contract.md §6](encoder-contract.md#6-properties)). Any other
variant is a **key-frame variant**: its split points are where its content
has key frames, and the output has no say in where they are.

The **request** held in the writer's sink for each chunk
([encoder-contract.md §2](encoder-contract.md#2-calls-and-order)) is the next media
time at which the output wants a split point: the first grid boundary at or
after the chunk's media time (and after 0), or a pending sync time (§7) when
that comes first. A free-split encoder makes a split point there; a
key-frame variant's split points do not depend on it.

## 3. Applying pieces

The pieces a writer writes for a chunk
([encoder-contract.md §1](encoder-contract.md#1-role)) are applied in order:

- **Bytes**: appended to the open segment.
- **Split** `{time}`: recorded (§2) if after the segment's start, then
  pending sync cuts (§7) and the variant's cut rule (§4) run.
- **Restart** `{time, init}`: §8.

## 4. The cut rule

A segment MUST NOT be cut anywhere but at a split point.

A candidate split point is **eligible** when cutting there gives the
segment a length that, in seconds rounded to the nearest integer, is at
most `T`. Eligibility keeps the tolerance inside RFC 8216 §4.3.3.1 (a
rounded `EXTINF` never exceeds the target duration) and Apple §7.7 (no
segment exceeds it by more than 0.5 s). Because `T` is then a rounded bound,
playlists announce version 6 ([playlists.md §1](playlists.md#1-common-form)).

The rule a variant applies depends on its kind at the time a split point is
recorded: §4.1 for a key-frame variant, §4.2 for a free-split one. A
variant's kind can change at a restart.

### 4.1 Key-frame variants

The **target** of the open segment is its start plus `D`. When a split point
`a` at or after the target is recorded, `a` being the first such point:

1. **Candidates.** `b`, the latest split point before the target, if any;
   and `a`.
2. **Choice.** `b` when `a` is not eligible, or when `b` is at least as
   near the target as `a`; `a` otherwise. `b` is always eligible: it is
   less than `D` after the start.
3. **Cut** there (§4.3). If a split point of the new segment is at or after
   its own target, repeat from step 1.

The choice depends on split-point times only, never on the media time at
which a split point is recorded. Reason: a key packet reaches the output
some time after its own time, by the source's interleaving for a copied
stream (audio sent ahead of video by an MPEG-TS muxer) or by the encoder's
delay (lookahead, B-frames). A rule that cuts once the media time passes a
limit acts on whatever split points happen to have arrived: it cuts one
key-frame interval early when the delay exceeds the margin between the key
frame and the limit, and variants with the same key frames but different
delays cut differently.

Reason for "nearest": content often puts every key frame a few tens of
milliseconds after or before its target (B-frame delay, encoder priming, a
key-frame interval rounded to whole frames). A rule that only looks after
the target lengthens a segment by one interval when the key frame comes
just before it; one that only looks before shortens it when the key frame
comes just after.

A decision waits for `a`, so a segment is published up to one key-frame
interval, plus the delay with which its key packet is recorded, after its
target. A key-frame interval that divides `D` keeps the wait short.

### 4.2 Free-split variants

Each free-split variant tracks its **current boundary** _k_, the first
grid boundary after its open segment's start. When a split point with time
`t ≥ k` is recorded:

1. **Candidates.** `b`, the latest split point at or before _k_, if any;
   `a`, the one just recorded.
2. **Choice.** The cut point is the eligible candidate nearer to _k_, `b`
   on a tie. If neither is eligible, `a`.
3. **Served.** When the choice is `a`, `a` is eligible, and the open
   segment's start is nearer to _k_ than `a`, no cut is made: the cut that
   opened the segment has served _k_.
4. **Cut** there (§4.3), unless step 3 applies. The current boundary becomes
   the first boundary after both _k_ and the cut point. If a split point of
   the open segment is at or after that boundary, repeat from step 1.

Reason for step 3 and for "after _k_": a sync cut (§7) just before a
boundary leaves the next split point past _k_, and cutting there would
leave a stub of a fraction of a second.

### 4.3 Cutting

Cutting the open segment at (`t`, `o`):

1. The bytes after `o` and the split points after `t` are taken out.
2. The segment's length becomes `t − start`; it is closed with its bytes up
   to `o` ([files.md §4](files.md#4-closing-a-segment)).
3. A new segment opens at `t` (§6).
4. The taken bytes and split points are moved into it.

### 4.4 Worked examples

`D` = 30 s, `T` = 30. A copied video variant has key frames every 2.002 s
(29.97 fps, 60 frames), the first at 0.1 s; its key packets are recorded 0.9
s after their time.

| Start | Target | Decided when recorded | b      | a     | Cut   | Length |
| ----- | ------ | --------------------- | ------ | ----- | ----- | ------ |
| 0     | 30     | 30.13 (at 31.03)      | 28.128 | 30.13 | 30.13 | 30.13  |
| 30.13 | 60.13  | 60.16 (at 61.06)      | 58.158 | 60.16 | 60.16 | 30.03  |
| 60.16 | 90.16  | 90.19 (at 91.09)      | 88.188 | 90.19 | 90.19 | 30.03  |

Every later segment is 30.03 s, 15 key-frame intervals, whatever the delay.

`D` = 6 s, `T` = 6, key frames every 2.5 s: `a` at 7.5 s is not eligible
(it rounds to 8), so each segment ends at `b` and lasts 5 s, steadily.

A free-split audio variant with `D` = 6 s cuts within one codec frame of
6, 12, 18 s.

## 5. The target duration

No rule forces a split, and an encoder is never asked for one. Each
segment's rounded length stays at or below `T` when its content has a split
point within the eligible window: free-split content always has one, and
key-frame content has one when its key-frame interval is at most `D`.

Key frames sparser than that leave no eligible candidate: the segment is
cut at the first split point after its target and its rounded length
exceeds `T`, which RFC 8216 §4.3.3.1 forbids. The output warns (§9).
Content whose key packets never come (a copied stream that stalls) leaves
the segment open until one does or the run stops.

What is not promised: segments of key-frame variants lasting `D` when the
content's key frames are not at intervals that divide `D`; alignment of
key-frame variants whose key frames are not at the same times. Both are
warned about (§9).

## 6. Opening a segment

A segment opens with a start time. It takes, from its variant, the pending
discontinuity flag, the pending inserted tags
([metadata.md §3](metadata.md#3-inserted-tags)) and the current init
segment. Before any encoder byte, it receives the ID3 tag for its start
when one is due ([metadata.md §2](metadata.md#2-id3)).

The first segment of a run opens at media time 0, at the first chunk.

A segment with no encoder bytes, or with length 0, MUST NOT be published or
listed: it is dropped. A segment is dropped also when aborted
([operator.md §6](operator.md#6-errors-and-reopening)), or when publishing
it fails ([files.md §4](files.md#4-closing-a-segment)). A dropped segment
gives its discontinuity flag, its inserted tags and the first ID3 tag of new
metadata it carried back to the variant's pending state, so the next segment
carries them.

## 7. Output-wide cuts

A **sync cut** at media time `t` ends the open segment of every variant at
that variant's first split point at or after `t` that is not already in a
closed segment. When the open segment starts at or after `t`, that point is
its start: no cut is made, and the open segment takes the cut's
discontinuity. Until it is made, `t` is a pending sync time of the variant
and is passed as the request (§2), so encoders that make key frames make
one at `t`, and free-split variants have a split point there. It is used
for:

- a container restart in any variant (§8), marked as a discontinuity;
- track metadata and inserted tags, as decided by
  [metadata.md §4](metadata.md#4-cuts-on-metadata-and-tags).

Rules:

- In each variant, a sync cut is one cut. If its point is also the point
  the cut rule (§4) chooses, that one cut serves both.
- A sync cut does not move the grid: the current boundary of a free-split
  variant after it is the first boundary after its cut point. A key-frame
  variant's next target is its cut point plus `D` (§4.1).
- A sync cut waits for a split point.
- A discontinuous sync cut marks the segment it opens as discontinuous in
  every variant, so the discontinuity sequence numbers stay equal across
  variants (RFC 8216 §6.2.4, Apple §8.15).

**Alignment.** When every variant is free-split, a sync cut lands within
one codec frame of `t` in every variant, and the grid stays aligned:
segment _n_ covers the same media time everywhere and counts are equal. In
key-frame variants, the cut waits for the next key frame: variants whose
key frames are at the same times cut at the same one. What is not promised:
in a copied variant that does not itself restart, content between `t` and
that key frame carries different discontinuity sequence numbers than in a
variant that does; and a sync cut just before a boundary can be two cuts in
one variant and one in another, so counts can differ by one after it.

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

## 9. Warnings

The output checks every closed segment of a key-frame variant, and logs a
warning naming the variant when the content cannot be cut as `D` asks.
Each warning is logged when its condition starts in a variant, not again
while it holds; it is logged again once the condition has cleared and comes
back.

| Condition                                                                               | Holds until                                                  |
| --------------------------------------------------------------------------------------- | ------------------------------------------------------------ |
| a segment's rounded length exceeds `T` (§5)                                             | a segment of the variant closes within `T`                   |
| a segment cut by §4.1 lasts more than 0.5 s less than `D`                               | a segment cut by §4.1 closes within 0.5 s of `D`, or longer  |
| a cut has no cut within 0.1 s in another key-frame variant that has already cut past it | a cut of the variant matches one of the other's within 0.1 s |

The third condition is checked, for each pair of key-frame variants, by the
one that cuts later, against the other's latest 16 cuts. A run's final
segment, closed at its stop, is not checked for length below `T` nor
against other variants.

Reason for comparing with `D` rather than `T`: with a fractional `D` the
rounding goes the other way (2.5 s rounds to 3), and a steady 2.48 s would
count as short. Reason for these conditions: segments of `D` and aligned variants follow
from the content (§1), so these are what the script can fix: the encoder's
key-frame interval, or the source's for copied streams. Reason for 0.1 s:
the same key frame in variants of different frame rates or time bases sits
within a few milliseconds; a key frame at a different time is at least one
frame, tens of milliseconds, away, and usually a whole key-frame interval.
