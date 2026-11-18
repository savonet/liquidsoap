# Cross, as built (Part A)

What `cross` does on branch `switch-rewrite`. Descriptive: nothing here is a
requirement. Defects found while reading are in [findings.md](findings.md)
and referred to as F1, F2, …

The seam and the source contract are
[../switch/frame-assembly.md](../switch/frame-assembly.md) and
[../switch/source-contract.md](../switch/source-contract.md). The way the
child is read is [child-clock.md](child-clock.md). The transition function
and its arguments are [transition.md](transition.md).

## 1. Parameters

`cross(~id, ~duration, ~override_duration, ~persist_override, ~width, transition, child)`,
with the method `cross_duration()`.

| Parameter           | Default                | Meaning                                                                              |
| ------------------- | ---------------------- | ------------------------------------------------------------------------------------ |
| `duration`          | `5.`                   | Seconds, a constant or a function. The **cross duration** ([§3](#3-cross-duration)). |
| `override_duration` | `"liq_cross_duration"` | Metadata key that replaces `duration` ([§3](#3-cross-duration)).                     |
| `persist_override`  | `false`                | Keep a replaced duration past the end of the track.                                  |
| `width`             | `2.`                   | Seconds. The window of the level analysis ([§6](#6-level-analysis)).                 |
| `transition`        | none                   | The transition function ([transition.md](transition.md)).                            |
| `child`             | none                   | The source whose tracks are crossed. Audio is required.                              |

`cross_duration()` returns the current cross duration in seconds.

At creation the child's sync answer is checked
([child-clock.md §6](child-clock.md#6-creation-check)).

Script code does not see this operator directly: the standard library wraps
it ([script-layer.md §1](script-layer.md#1-the-cross-wrapper)).

## 2. State

Durations are in **ticks** of the main rate; a duration in seconds is
converted by truncation. `F` is the frame size (0.02 s by default).

- `phase`: `idle`, `before` or `after`. With `before` comes the **ending
  buffer source**; with `after` comes the **compound**. That source is the
  **designated source**. `idle` is the initial phase and is never entered
  again.
- `D`: the cross duration, in ticks.
- The **ending store** and the **starting store**: two FIFO stores of
  stream data (audio, marks, metadata at their positions).
- The **ending metadata** and the **starting metadata**: one metadata set
  each, or none ([§9](#9-metadata-kept)).
- The level analysis state ([§6](#6-level-analysis)).
- The **release handle** of the last transition
  ([transition.md §5](transition.md#5-callbacks)).
- A pending-abort flag ([§10](#10-abort)).
- The **intake** and the child clock ([child-clock.md](child-clock.md)).

A **buffer source** is a source that delivers the content of one store, one
frame at most per cycle, in order. It is ready when its store is not empty.
Its segment ends `full` or, when the store ran out, `late`. It is always
fallible. Its remaining time is the length of its store. Its effective
source is itself. It reports no sync source. It is **fed** or not: a fed
buffer source never declares exhaustion; one that is not fed declares it
when its store is empty. It lives in the clock of `cross`.

## 3. Cross duration

**Resolving** a value `d` in seconds gives `D`:

| `d` in ticks | `D`       | Log (level 3)                                                |
| ------------ | --------- | ------------------------------------------------------------ |
| negative     | `F`       | `Cannot set crossfade duration to negative value <d>!`       |
| zero         | `F`       | none                                                         |
| below `F`    | `F`       | `Cannot set crossfade duration to less than the frame size!` |
| otherwise    | the value | none                                                         |

So a duration of zero does not disable anything: it crosses over one frame.

`D` is resolved:

- at creation, from `duration`;
- at a **reset**: `duration` is read again and resolved;
- at an **override**: a metadata holding the key `override_duration` with a
  value that parses as a float replaces the value for good, until the next
  reset. A value that does not parse is ignored. Each override writes, at
  level 4, `Overriding crossfade duration from metadata <key>`, before the
  value is parsed, and twice (F7).

An override is looked for in:

- every metadata of every piece appended to the ending store or to the
  starting store, in stream order, when the piece is appended;
- every metadata `cross` itself delivers, when it is delivered (F4).

A reset happens only where [§5](#5-probe) says so, and only when
`persist_override` is false. With `persist_override`, `duration` is
therefore read once, at creation (F10).

## 4. Taking from the child

`take(first)` returns the next **piece** of the child's stream, of at most
`F` ticks, and removes it from the intake
([child-clock.md §4](child-clock.md#4-reading)). The piece is the first `F`
ticks of the intake, or all of it when it holds less, cut as follows:

| The piece holds                                 | Returned                                     |
| ----------------------------------------------- | -------------------------------------------- |
| a mark on its first sample, and `first` is true | The whole piece, later marks included (F18). |
| a mark on its first sample, `first` false       | Nothing: an empty piece.                     |
| its first mark after its first sample           | What precedes that mark.                     |
| no mark                                         | The whole piece.                             |

A returned piece shorter than `F` is an **end piece**. `cross` takes it for
the end of the track being buffered. It arises at a mark, and also when the
child stopped being available with less than `F` in the intake and no mark
(F21, F2).

The child is **available** when the intake is not empty or the child is
ready.

## 5. Probe

Everything `cross` does to its state happens when it is first probed in a
cycle. The answer is kept for the cycle
([source-contract.md §1](../switch/source-contract.md#1-streaming-cycle));
the request that follows only reads the designated source (F12).

**Probe**, by phase:

- `idle`: the child is not available: not ready. Otherwise
  [start buffering](#start-buffering).
- `before`: [top up](#top-up) with `first` false. Then, if the phase is
  still `before`: ready when the ending store is not empty. If the phase
  became `after`: ready when the compound is.
- `after`: the compound is ready: ready. Otherwise
  [start buffering](#start-buffering).

### Start buffering

1. Create a fed buffer source over the ending store, named
   `<id>.pre_buffer`, and wake it.
2. Designate it: the phase is `before`. The previous designated source, if
   any, is released, and the release handle is called and cleared.
3. [Top up](#top-up) with `first` true.
4. Ready when the designated source, which may by now be a compound, is
   ready.

The ending store is not emptied here. It is empty after a crossing
([§7](#7-building-the-compound), step 9). It holds what was buffered when
this runs from the phase `before`, which only an abort causes
([§10](#10-abort)).

Log, level 4: `Buffering end of track...`.

### Top up

While the ending store holds less than `D` and the child is available:

1. `take(first)`; `first` is false from the second turn on.
2. Append the piece to the ending store ([§9](#9-metadata-kept), and the
   override of [§3](#3-cross-duration)).
3. Add the piece to the ending level ([§6](#6-level-analysis)).
4. If it is an end piece, [the ending track is over](#the-ending-track-is-over):
   stop.

`D` is read again at every turn. In steady state one piece is taken per
cycle and the store holds between `D − F` and `D` when a cycle starts. The
first top up after `idle` and after a compound takes about `D` in one probe
(F12).

### The ending track is over

1. `E` ← `D`: the **ending duration**.
2. Unless `persist_override`: reset.
3. Fill the starting store. `first` is true for the first piece. While the
   starting store holds less than `max(E, fade-in window)` and the child is
   available:
   1. `take(first)`.
   2. Append the piece to the starting store.
   3. Add the piece to the starting level ([§6](#6-level-analysis)).
   4. If it is an end piece: unless `persist_override`, reset; log at level
      1 the message below; go to step 4.
4. [Build the compound](#7-building-the-compound).

```
End of track reached while buffering next track data, crossfade duration is longer than the track's duration. Make sure to adjust the crossfade duration to avoid issues.
```

The **fade-in window** is `liq_fade_in_delay + liq_fade_in`, read from the
starting metadata as known at that turn, each counting as zero when missing
or not a float. It is read again at every turn, so it applies from the piece
that carries the metadata.

The override carried by the starting track is applied in step 3.2, after
the reset of step 2: it governs the starting track's own end.

## 6. Level analysis

`W` is `width` in audio samples, truncated.

- **Ending level.** A sliding sum of squared samples over the last pieces
  taken into the ending store. It keeps `W` slots, a write position and a
  count. For a piece of `n` samples whose squared samples, all channels
  together, sum to `q`: the sum loses the value of the slot at the write
  position and gains `q`; the slot is set to `q`; the write position moves
  by `n` modulo `W`; the count grows by `n`, up to `W`. With pieces of equal
  size dividing `W` this is the sum over the last `W` samples (F9). `W` of
  zero fails at the first piece (F5).
- **Starting level.** A plain sum and count over the pieces taken into the
  starting store, for as long as the store, the piece included, holds no
  more than `W` ticks (F9).

The level handed to the transition is
`20·log10(sqrt(sum / count / channels))`. With a count of zero it is not a
number.

Both are zeroed when the compound is built and when `cross` is woken.

## 7. Building the compound

With `Lb` and `La` the lengths of the ending and starting stores:

1. `X` ← `min(Lb, La)`: what the two tracks overlap on.
2. `S` ← `min(La, max(X, fade-in window))`: what the transition gets of the
   starting track.
3. Compute both levels.
4. Adjust the ending metadata for a window of `X` and the starting metadata
   for a window of `S` ([transition.md §2](transition.md#2-metadata-adjustments)).
5. Cut the stores:
   - **head**: when `X < Lb`, the first `Lb − X` of the ending store, as a
     buffer source `<id>.head_buffer`;
   - **ending source**: the last `X` of the ending store, as a buffer source
     `<id>.before_buffer`;
   - **starting source**: the first `S` of the starting store, as a buffer
     source `<id>.after_buffer`;
   - **tail**: when `S < La`, the rest of the starting store, as a buffer
     source `<id>.tail_buffer`.

   None of the four is fed. A head and a tail never exist together.

6. Log, level 3: `Analysis: <ending level>dB / <starting level>dB (<Lb>s / <La>s)`.
7. Call the transition function
   ([transition.md §1](transition.md#1-arguments)). Its result is the
   **transition source**.
8. Assemble:

   | Head | Tail | Compound                                                                                                                                         |
   | ---- | ---- | ------------------------------------------------------------------------------------------------------------------------------------------------ |
   | no   | no   | The transition source.                                                                                                                           |
   | yes  | no   | A sequence `<id>.before_head` of the head, then the transition source, presented as one track: the transition source is entered with `continue`. |
   | no   | yes  | A sequence `<id>.after_tail` of the transition source, then the tail ([transition.md §4](transition.md#4-end-of-the-transition)).                |

   When the starting metadata is not empty, the tail is wrapped so that its
   first segment, if it carries no metadata, gets the starting metadata plus
   `liq_cross_replayed_metadata = "true"` on its first sample
   ([script-layer.md §1](script-layer.md#1-the-cross-wrapper)).

9. Wake the compound in the clock of `cross`. Reset the analysis: both
   stores are new and empty, both levels zeroed, the ending metadata becomes
   the starting metadata as adjusted, the starting metadata becomes none.
10. Designate the compound: the phase is `after`. The previous designated
    source is released and the previous release handle called. The handle of
    this transition is kept.

In steady state `Lb` is between `D − F` and `D` and `La` is the first
multiple of `F` reaching `max(E, fade-in window)`: there is a tail of at
most a frame and no head. A head exists when the starting track gave less
than the ending store held: it ended, or the child stopped, inside the
window.

## 8. Delivery

`cross` is a user of the seam. On a request it reads the designated source
found at the probe, and nothing else.

**Entry of the first sample taken from a source it was not reading:**

| Situation                                | Entry   | Effect                                                                         |
| ---------------------------------------- | ------- | ------------------------------------------------------------------------------ |
| First designation, whatever the phase    | as is   | A mark the data carries on that sample is kept and begins a track.             |
| `before` → `after` (the compound)        | `track` | A mark is ensured on the first sample of the compound.                         |
| `after` → `before` (a new buffer source) | as is   | As above. `track` instead when the compound's last segment stated `track end`. |
| `before` → `before` (after an abort)     | as is   | As above.                                                                      |

So a crossing begins a track on the first sample of the compound: that of
the head when there is one (F3), of the transition source otherwise. The
track event carries the last metadata found in that first segment, or none.
What follows the compound continues that track, unless the data carries a
mark there: the next track's own start, when the starting track ended inside
the window.

A stream whose first track is over before `D` is buffered starts in the
compound, entered as is (F6).

**Marks after the first sample** of the designated source end the segment
before them:

- in `after`: `cross` states `track end` and the sample holding the mark is
  entered with `track`. Marks carried by the transition source are tracks
  of their own;
- in `before`: `cross` states `pause` and the mark is kept as is.

**End of a segment:**

| The designated source's segment         | `cross` states                           |
| --------------------------------------- | ---------------------------------------- |
| was cut at a mark, or ended `track end` | `after`: `track end`. `before`: `pause`. |
| ended `pause`                           | `pause`                                  |
| otherwise, and the frame is full        | `full`                                   |
| otherwise, and the frame is short       | `pause`                                  |

`cross` never states `late`, and states nothing where the ending buffer
source gives way to the compound: the segment before a crossing ends
`full` (F14).

## 9. Metadata kept

- When a piece is appended to a store and holds metadata, the last metadata
  of the piece **replaces** the ending metadata, or the starting metadata,
  whole (F19).
- At the end of a build the ending metadata is the adjusted starting
  metadata of that crossing; it is then replaced by any later metadata of
  the same track as it is buffered.
- Both are none when `cross` is first woken.

Metadata stays in the stores at its position and is delivered with the
data. `cross` adds none, except on a tail ([§7](#7-building-the-compound),
step 8).

## 10. Abort

An abort of `cross`:

1. is forwarded to the child, at once;
2. raises the pending-abort flag.

When `cross` is next probed in a new cycle, before anything else, a raised
flag is cleared and, unless the phase is `idle`,
[start buffering](#start-buffering) runs; its answer is dropped and the
probe proceeds as in [§5](#5-probe).

- In `after`: the compound is released, with what it had left of both
  tracks. The stores are empty; the buffer starts with the child's next
  track, whose mark is kept. Nothing of the aborted track plays.
- In `before`: the ending store keeps its content. The new buffer source
  delivers what was buffered of the aborted track, about `D`, then the
  child's next track, whose mark is kept. There is no crossing (F1).
- The intake is not emptied.

The seam is not told of the abort: no end is stated, and the entry is as
is.

## 11. Other properties

- **Remaining.** `idle`: the child's. `before`: the ending store's length
  plus the child's, unknown when the child's is. `after`: the compound's
  (F22).
- **Effective source.** `idle`: the child's. Otherwise that of the
  designated source: a buffer source is its own.
- **Sync source.** `cross` reports what its child reports, kind and sync
  source (F16). Nothing is reported for the compound.
- **Fallibility.** Always fallible.
- **Exhaustion.** Never declared (F20).
- **Composition.** `cross` has no tag. It resolves to the child's profile in
  `idle` and to `live` in `before` and `after` (F15).
- **Wake.** The child is woken, through the child clock and directly; the
  analysis is reset as in [§7](#7-building-the-compound), step 9.
- **Sleep.** The child is released. The designated source is kept, awake,
  with the phase (F11).

## 12. Seam answers

In the order of
[frame-assembly.md §9](../switch/frame-assembly.md#9-contract-clauses).

- **C1.** [§5](#5-probe). The decision and what it commits are one act, at
  the probe.
- **C2.** In `before`, each cycle: the same act.
- **C3.** [§8](#8-delivery): `track end` in `after`, `pause` in `before`.
  The sequences inside the compound relay their own statements.
- **C4.** As is.
- **C5.** `cross` wakes each buffer source and each compound when it creates
  it, in its own clock, and releases the previous one before designating the
  new one. The four buffer sources of a crossing are woken by whatever reads
  them. Nothing is released at sleep.
- **C6.** [§10](#10-abort), [§11](#11-other-properties).
- **C7.** [§11](#11-other-properties).

## 13. Worked example

`D` = 1 s, `F` = 0.02 s, `width` = 1 s, a child playing three tones of
exactly 3 s, `a`, `b`, `c`, each with a title; the transition adds its two
sources. Times are positions in the output of `cross`. Taken from a run.

| Time | Event                                                                                                                                                                                                                                                     |
| ---- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 0.00 | Probe in `idle`. 50 pieces of `a` taken. Track `a` begins.                                                                                                                                                                                                |
| …    | One piece per cycle. The store holds 0.98 s at each probe, 1.00 s after it.                                                                                                                                                                               |
| 2.02 | Probe: the piece starts with the mark of `b`: empty end piece. `Lb` = 0.98 s. 50 pieces of `b` taken: `La` = 1.00 s. `X` = `S` = 0.98 s. Tail: 0.02 s. Transition called; both sources report 0.98 s remaining. A track begins, with the metadata of `b`. |
| 3.00 | The transition source is exhausted; the tail plays.                                                                                                                                                                                                       |
| 3.02 | Probe in `after`, compound not ready: start buffering. 50 pieces of `b`. No mark.                                                                                                                                                                         |
| 4.04 | Second crossing, same figures, `b` to `c`.                                                                                                                                                                                                                |
| 5.06 | Buffering `c`. Its 100 remaining pieces are taken by 6.06.                                                                                                                                                                                                |
| 7.04 | The store is empty and the child is not available: `cross` is not ready, for good. No transition was called for the end of `c`.                                                                                                                           |

The output is 7.04 s long: 9 s less two overlaps of 0.98 s.

With tones of 3.01 s the same script calls the transition a third time, at
6.06, with an empty starting source and a starting level that is not a
number, and a track begins there with no metadata (F2).
