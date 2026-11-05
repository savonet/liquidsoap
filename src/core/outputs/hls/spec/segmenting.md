# Segmenting

Part A. As built. This file owns how one variant's byte stream is cut into
segments: the grid, split points, cuts, the target-duration bound, extra
cuts and container restarts. It relies on the encoder pieces of
[encoder-contract.md §5](encoder-contract.md#5-split_encode). What closing
and opening a segment does on disk is [files.md](files.md).

The same grid is also stated, from the encoder's side, in the `%ffmpeg`
encoder spec ([encoder.md §8.7](../../../optionals/ffmpeg/spec/encoder.md#87-segment-grid-hls-output-side)).
This file describes the behaviour; the two texts agree today (finding
F-OWN-1).

## 1. Segment duration and the grid

Time is counted in **main ticks**, the internal time unit (with default
settings, one tick per audio sample: 44100 per second). A **frame** is the
configured frame size in ticks.

- `D`, the segment duration in ticks, is `segment_duration` rounded **down**
  to whole frames: `floor(ticks(segment_duration) / frame) × frame`. The
  seconds value used everywhere after is `D` in seconds.
- `T`, the announced target duration, is `ceil(D in seconds)`, an integer.
- **Media time** of a variant: ticks given to its encoder since the variant
  was built ([variants.md §2](variants.md#2-building-a-variant)). It
  advances by each chunk's length ([operator.md §4](operator.md#4-per-frame-loop)).
  Each variant has its own; all advance by the same amounts, so they are
  equal unless a variant was rebuilt.
- **Grid**: boundary _k_ is `k × D`. The next boundary after time `t` is
  `(floor(t / D) + 1) × D`, always strictly after `t`. Boundaries never
  depend on where a segment actually ended.

A `segment_duration` shorter than one frame gives `D = 0` and every frame
raises a division by zero (finding F-SEG-1, checked).

Each variant holds a **current boundary**, initially `D`.

## 2. Segments in memory

The open segment of a variant has a start (media time of its first byte), a
length (ticks), its bytes so far, and its **split points**: pairs
(media time, byte offset in the segment), each strictly after the start.

A segment's `EXTINF` duration is its length. Length is the media time
between the segment's two cuts, or between its start and the current media
time while open.

## 3. Applying pieces

The pieces an encoder returns for a chunk are applied in order
([encoder-contract.md §5](encoder-contract.md#5-split_encode)):

- **Bytes**: appended to the open segment.
- **Split** `{time, forced}`: if `forced`, a level-3 log:
  `Stream <name>: no split point within the target duration, segment cut at
<time>s without a key packet of every stream.` Then the split is handled
  at the segment's current byte position (§5).
- **Restart** `{time, init}`: §7.

## 4. Opening a segment

A segment opens with a start time. It receives the variant's next position
number, takes the pending discontinuity flag and the pending inserted tags,
and records the variant's current init segment. Before any encoder byte,
in this order, it receives: the ID3 tag for its start
([metadata.md §2](metadata.md#2-id3)), then the encoder's `header()` bytes
([encoder-contract.md §8](encoder-contract.md#8-header-and-stop)).

The first segment opens at the first chunk, before the encoder has returned
anything.

## 5. Handling a split

Given a split (time `t`, offset `o`):

1. If `t` is after the open segment's start, (`t`, `o`) is recorded.
2. **Due cut.** If `t ≥` current boundary _k_:
   1. The **target** is the latest recorded split point at or before _k_;
      if there is none, the earliest one after _k_.
   2. With a target (`t'`, `o'`): log level 4 `Terminating current segment
<id> on stream <name> at its split point`; the current boundary
      becomes the next boundary after `t'`; the segment is **cut** at
      (`t'`, `o'`) (§5.1); repeat step 2 on the new open segment.
   3. With none (the split equals the segment's start): the current boundary
      becomes the next boundary after `t`. No cut.
3. **Extra cut.** Else, if the variant has a reason
   ([metadata.md §4](metadata.md#4-extra-cuts)) and `t` is after the
   segment's start: log level 4 `Terminating current segment <id> on stream
<name> <reason>` and cut at `t`, at its offset in the current segment
   (an earlier cut in the same step may have moved it). The boundary is
   unchanged.

A segment is never cut anywhere but at a split point.

### 5.1 Cutting

Cutting the open segment at (`t`, `o`):

1. The bytes after `o` are taken out; the split points after `t` are kept.
2. The segment's length becomes `t − start`; it is truncated at `o` and
   closed ([files.md §4](files.md#4-closing-a-segment)).
3. A new segment opens at `t` (§4: ID3 tag, header).
4. The taken bytes are appended, the kept split points move with them, and
   the new segment's length is media time minus `t`.

### 5.2 Worked example

`D` = 6 s. A copied video variant has key frames every 2 s, each 0.1 s after
a multiple of 2 s (codec delay in the source), so split points arrive at
2.1, 4.1, 6.1, 8.1, …

| Split arrives | Boundary | Recorded in open segment | Action                                                                           |
| ------------- | -------- | ------------------------ | -------------------------------------------------------------------------------- |
| 2.1           | 6        | 2.1                      | none                                                                             |
| 4.1           | 6        | 2.1, 4.1                 | none                                                                             |
| 6.1           | 6        | 2.1, 4.1, 6.1            | target 4.1 (latest ≤ 6): cut, segment 0–4.1; boundary → 6                        |
| (repeat)      | 6        | 6.1                      | target 6.1 (none ≤ 6, earliest after): cut, segment 4.1–6.1 (2 s); boundary → 12 |
| 12.1          | 12       | 8.1, 10.1, 12.1          | target 10.1: segment 6.1–10.1 (4 s); then 12.1: segment 10.1–12.1 (2 s)          |

`EXTINF`s alternate 4, 2, 4, 2, while every key frame is within 0.1 s of a
boundary. This is the behaviour reported in the field and is what the rule
gives (known complexity KC3).

## 6. The target-duration bound

Before each chunk is given to `split_encode`, with `len` the chunk's length:

1. If the open segment's duration after the chunk, `media time + len −
start`, in seconds, rounded to the nearest integer, exceeds `T`:
   1. If the segment has a target (§5 step 2.1, computed against the current
      boundary): log level 4 `Terminating current segment <id> on stream
<name> at its split point before the target duration`; the boundary
      becomes the next one after the target; cut there; check again.
   2. Else: `split_encode` is called with `force` true.
2. Otherwise `force` is false.

A forced `Split` comes back at the chunk's start
([encoder-contract.md §5](encoder-contract.md#5-split_encode)) and is handled
like any split. Each segment's rounded duration therefore stays at or below
`T`, provided the encoder honours `force`.

## 7. Container restart

On `Restart {time, init}`:

1. The current boundary becomes the next boundary after `time`.
2. The next segment is marked discontinuous.
3. The open segment is cut at (`time`, current byte position).
4. The init position number increases by one, and the new segment takes
   `init` as its init segment ([files.md §6](files.md#6-init-segments)).

## 8. Position numbers

Each variant numbers its segments from 1, one per opened segment. A segment
aborted on error ([operator.md §6](operator.md#6-errors-and-reopening))
keeps its number unused. Restoring persisted state skips one number
([persistence.md §4](persistence.md#4-restoring)). A `stop` without
persistence restarts the numbering at 1 ([operator.md §3.2](operator.md#32-stop)).
Position numbers are the media sequence numbers of
[playlists.md §2](playlists.md#2-media-playlist).
