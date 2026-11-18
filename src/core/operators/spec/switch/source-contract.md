# Source contract (Part A)

What every source MUST provide for the frame-assembly seam and its users to
hold their guarantees. It applies to all sources, not only to the seam's
users: one operator that breaks it between two selectors breaks them both.

## 1. Delivery

A source delivers at most one **frame** per streaming cycle, of at most the
frame size. The frame grows by **segments**. A segment is produced when a
reader **requests** it: the first request of the cycle, then one request per
continuation.

Each segment ends for one stated reason:

| Reason      | Meaning                                                                                      |
| ----------- | -------------------------------------------------------------------------------------------- |
| `full`      | The frame size is reached.                                                                   |
| `track end` | The source's current track ends here. What follows, now or later, begins a new track.        |
| `pause`     | The track goes on, but the source must be asked again before it produces more in this cycle. |
| `late`      | The source has nothing more for now. Its track has not ended.                                |

When a track ends exactly where the frame gets full, the reason is
`track end`.

- A segment MUST hold at least one sample, except a segment ending with `late`
  or `track end`, which MAY be empty.
- A source MUST end its segment wherever going on would make it decide
  something: choose a source, run script code, count a play, fire a track
  event. It MUST NOT make that decision before a reader requests the next
  segment.
- A segment is produced once. Every reader of the source sees the same data at
  the same positions; a later reader gets segments already produced without
  anything running again.
- **Requesting is taking.** A reader MUST NOT request a segment to find out
  whether data exists; that is what probes are for ([§2](#2-probes)). A source
  MAY treat a request as the statement that its data is being played.
- **No reading ahead.** An operator MUST NOT request from a child a segment
  that its own reader has not requested from it. An operator's segment ends
  wherever a segment of the child it relays ends, with the same reason, unless
  the operator's own contract says otherwise.
- An operator that buffers its child, or reads it through a child clock, is
  that child's consumer: the child's commitments are made relative to what the
  operator takes, not to what the output plays.

Rationale: a frame generated whole crosses the source's own track boundary
before anyone took it. Every choice made past the boundary is then a
commitment to a track nobody heard, and the reader that leaves at the boundary
cannot undo it.

## 2. Probes

A **probe** asks a source what it could deliver beyond a position in its
frame for this cycle. It answers one of:

| Answer      | Meaning                                 |
| ----------- | --------------------------------------- |
| `nothing`   | No data now.                            |
| `more`      | Data that continues the current track.  |
| `new track` | Data whose first sample begins a track. |

- A probe MUST have no effect that a reader, a script callback, a policy or
  the output can observe. Any number of probes in a cycle MUST leave the
  source as zero probes would. The one exception is stated in
  [dynamic.md §3](dynamic.md#3-designation).
- The answer for one position MUST NOT change within a cycle.
- A source that answered `more` or `new track` MUST deliver at least one
  sample to the next request at that position, of the kind it announced.
- A source MUST answer `nothing` before its clock has started and while it is
  not awake.
- A source is **ready** when a probe at the position it has delivered so far
  does not answer `nothing`.

A reader therefore learns of every boundary before it takes data past it:
from the reason of the previous segment, or from the probe.

## 3. Tracks

- A **track mark** states the logical start of a unit of content. It sits on
  a delivered sample, never beyond the data.
- A source SHOULD mark the first sample of every track it produces, the first
  of its stream included. Data that no mark precedes is **lead-in**: it
  belongs to no announced track.
- A mark MUST only sit on the first sample of a segment, and only where a
  boundary was announced: after a `track end`, after a `new track` probe
  answer, or at the start of the stream.
- A frame MAY hold any number of track boundaries. No reader MAY assume a
  bound.
- An operator MUST pass the marks and the metadata of the content it relays
  unchanged, at their positions, unless its own contract states a change. It
  MUST NOT add a mark to filler it inserts (silence, padding).

## 4. Metadata

- Metadata belongs to a position. It travels with the sample it was produced
  on.
- A source's **last metadata** is the metadata it delivered since the start
  of its current track. It MUST be cleared when that track ends and when it is
  aborted ([§6](#6-abort)). An operator's last metadata is that of what it
  relays.
- A source can be asked to **insert** metadata on the next sample it
  delivers. Metadata already present on that sample wins, key by key.
- A source that produces whether or not it is read MUST NOT lose, when it is
  first read, the track start and the metadata of what it then delivers.

## 5. Consumption

- A reader reports how much of each segment it used. The largest amount
  reported for a source in a cycle wins.
- What was produced and not used stays with the source that produced it,
  belongs to its current track, and is delivered first at the next request. A
  source holding such data is ready.
- For every source and every cycle: samples produced equal samples used plus
  samples held. Nothing is dropped at a segment end and nothing is delivered
  twice. A reader that uses the shortest of several sources loses nothing of
  the others.
- Held data is discarded only by an abort ([§6](#6-abort)) or when the source
  goes down.
- A source that is not read MUST hold a bounded amount of data.

## 6. Abort

**Abort track** tells a source that its current track is over.

1. The track ends at what was already used. Data of that track produced and
   not used, held or not, MUST be discarded.
2. The last metadata is cleared.
3. The source is at a boundary: the next data it delivers begins a new track,
   and its probe answers `new track` or `nothing`.

- An operator MUST apply the abort to its own state and forward it to the
  sources it currently reads.
- An abort issued while a cycle of the source's clock runs, from any thread
  and from a callback of that cycle alike, takes effect when the cycle is
  over. It MUST be complete before the first probe of the next cycle.
- A source that was aborted MUST NOT play, on its return, anything of the
  aborted track.

## 7. Lifecycle

- A consumer **wakes** a source and gets an activation; it **releases** the
  source with that activation, exactly once. A source is up while it has at
  least one activation.
- A source MUST be woken before it is first probed.
- **Exhausted.** A source MAY declare that it will never deliver again: a
  finite source at its end, a source that never produces. Exhaustion is
  declared by the source, never deduced by a reader from `nothing` or `late`.
  An operator is exhausted when it can never deliver again whatever its
  children do. An exhausted source answers `nothing` to every probe.

## 8. Other calls

- **Remaining**: time left in the current track, or unknown.
- **Effective source**: the single source an operator currently reads, or
  itself.
- **Sync source**: by the rule of the clock specification
  ([pacing §3](../../../clock/spec/pacing.md#3-finding-the-sync-source)): that
  of the data its reader is about to pull.

## 9. No inference

A source MUST NOT derive any state from whether, when or how often it was
probed, requested or ticked: not from a gap in ticks, not from a segment that
was produced and not used, not from a probe that no request followed. Every
change of state has a named cause: a request, a reported use, an abort, a
track end, a wake or a release.

A clock that ticks a source without anything reading it changes nothing in
that source's selection, counters or track state.

## 10. Callbacks

- Track and metadata callbacks of a source fire when the segment holding the
  mark or the metadata is produced, which only a request causes.
- For one boundary, the end-of-track notifications of every level fire before
  the new-track notifications of any level; within each kind, the wrapped
  source's fire before those of the operator wrapping it. The order is the
  same for a natural end and for an abort.
- A callback that, while a segment is being produced, reads the source being
  produced gets what was produced so far and returns. It MUST NOT start
  another production.

## 11. Concurrency

- Script code can group writes in an atomic section. A reader running inside
  one never sees such a group part way through.
- Nothing a script can write MUST be able to reach an assertion or end the
  process from the streaming cycle. A condition the seam cannot serve ends the
  frame and is logged ([frame-assembly.md §7](frame-assembly.md#7-bounds)).
