# Frame assembly (Part A)

The seam of every operator that delivers one stream out of successive
sources, changing source inside a frame when one ends: `switch`, `sequence`,
`source.dynamic` and `cross`. Sources are as in
[source-contract.md](source-contract.md). The seam is a contract on
behaviour: `cross` answers its clauses with an assembly of its own, since it
reads its child through a [child clock](child-clock.md) and never hands the
child itself to the seam.

## 1. Roles

- The **user** is the operator built on the seam.
- Its **reader** is whatever requests its segments.
- The **designated source** is the one the user currently reads.
- A **join** is a change of designated source.
- A **decision point** is a place in the user's stream where it chooses what
  to read:
  - **boundary**: nothing is designated; or the designated source's track
    ended, was aborted, or the source answers `nothing`;
  - **mid-track**: the start of a cycle while the designated source's track
    is still playing.

## 2. Decisions

A **decision** is the user's answer, at a decision point, to "which source
would be read from here": a source or none.

- A decision MUST NOT change anything a reader, a script callback, a policy
  or the output can observe. It MAY probe sources.
- A decision made at a point is kept until it is committed
  ([§3](#3-commit)), until an abort, or until the cycle ends, whichever comes
  first. Every probe and the following request see the same one.
- A user that never re-decides while a track plays makes no mid-track
  decision: it keeps its designated source.
- A decision is made from stated facts only: the reason the last segment
  ended, probe answers, an abort received, the user's own state. See
  [source-contract.md §9](source-contract.md#9-no-inference).

## 3. Commit

A decision is **committed** when the user's reader requests a segment past
the decision point. Only then MAY the user:

- change its designated source, wake and release sources;
- run script code attached to the change;
- fire track and metadata events;
- change anything a policy counts.

A decision that keeps the designated source commits nothing.

Consequence: a user left at a boundary by its reader has committed nothing
past that boundary. When it is read again it is at a boundary and decides,
with nothing to undo. A user left in the middle of a track resumes where it
was, unless it was aborted
([source-contract.md §6](source-contract.md#6-abort)).

## 4. Readiness

The user's probe answer ([source-contract.md §2](source-contract.md#2-probes))
at a position is:

- mid-segment data it already produced: `more`;
- at a decision point: `nothing` when the decision is none; else the probe
  answer of the decided source, turned into `new track` when the entry will
  ensure a mark ([§6](#6-joins)) and into `more` when it will remove one.

The request that follows MUST deliver from the decided source.

## 5. Segments

On a request, the user:

1. At a decision point: takes the decision and commits it.
2. Requests the designated source's next segment, limited to the room left in
   its own frame.
3. Applies the entry rule to the first sample when it follows a stated end
   ([§6](#6-joins)).
4. Ends its own segment where the designated source's ended, with:

| Designated source ended with | The user's segment ends with                          |
| ---------------------------- | ----------------------------------------------------- |
| `full`                       | `full`.                                               |
| `pause`                      | `pause`.                                              |
| `late`                       | `late`.                                               |
| `track end`                  | The user's **end statement**: `track end` or `pause`. |

The **end statement** says whether the user's own track ends where the
designated source's does. The user MUST give it at that moment, from what it
knows then, and it binds what follows: after `track end` the next data begins
a track; after `pause` it continues the track. Each user states its rule
under clause C3 ([§9](#9-contract-clauses)).

Rationale: whether a join is merged belongs to the join. The user states it
when the source being left ends, before it has moved on, and the same answer
is used to assemble the join wherever it falls in a frame.

## 6. Joins

The first sample taken after a stated end, or at the user's first
designation, is **entered** in one of two ways:

- **`track`**: the sample begins a track. A mark is ensured on it: kept if
  the source has one, added if not. The user's track events fire with the
  metadata found on that sample, or none.
- **`continue`**: the sample continues the current track. A mark on it is
  removed. Its metadata stays where it is and is announced as metadata. No
  track event fires.

- After a `track end` statement the entry is `track`. After a `pause`
  statement it is `continue`. It makes no difference whether the join falls
  inside a frame, on a frame edge or in the first frame of the stream.
- At the first designation, and after a period with nothing designated, the
  entry is the one the user states under clause C4.
- After an abort the entry is `track`.
- A source change committed by a mid-track decision is entered with `track`
  unless the user states otherwise.
- A mark is never put on an empty segment.
- Marks the designated source carries anywhere else are its own track
  starts. They are never inside a segment
  ([source-contract.md §3](source-contract.md#3-tracks)), so each one reaches
  the user as a `track end` and gets an end statement.

## 7. Bounds

- A source contributes at most one contiguous span to one frame of the user.
  A decision that designates a source which already contributed to this frame
  and is not the designated one ends the frame with `late`. The decision
  stands for the next cycle.
- A request to the designated source that delivers no sample ends the
  user's frame, with `late` or with that source's `track end`.
- So every request either delivers a sample or ends the frame: no decision
  loops inside a cycle.
- A condition the seam cannot serve (a decided source that then delivers
  nothing, a source that answers differently from its probe) ends the frame
  and is logged. The seam itself does not raise; when the stream was
  declared infallible, the output that gets the short frame fails the cycle
  ([source-contract.md §11](source-contract.md#11-concurrency)).

## 8. Lifetime

- The user holds no data of its own. What its reader reports as used is
  forwarded, span by span, to the sources that produced it; what is not used
  stays with them ([source-contract.md §5](source-contract.md#5-consumption)).
- A source MUST be awake before it is first probed for a decision.
- At a join the entered source is awake before the one left is released.
- A source is released when its last sample was used or it was abandoned,
  exactly once. A source present on both sides of a join is neither released
  nor woken again.
- Reachability is not use: no release depends on a collector.
- Each user states who wakes and who releases under clause C5.

## 9. Contract clauses

Every user of the seam answers these, in this order, under the heading "Seam
answers" of its own file. A clause one user answers and another cannot is a
defect of the specification.

| Clause | Question                                                                            |
| ------ | ----------------------------------------------------------------------------------- |
| C1     | Decision at a boundary: which source, from what facts; and what its commit does.    |
| C2     | Mid-track decisions: whether the user makes them, when, and what their commit does. |
| C3     | End statement: when the designated source's track ends, does the user's.            |
| C4     | Entry at the first designation and after a period with nothing designated.          |
| C5     | Who wakes the designated sources and who releases them.                             |
| C6     | Remaining, abort track, effective source, sync source.                              |
| C7     | Fallibility and exhaustion.                                                         |

| User             | File                                                            |
| ---------------- | --------------------------------------------------------------- |
| `switch`         | [switch.md](../../operators/spec/switch/switch.md)              |
| `sequence`       | [sequence.md](../../operators/spec/switch/sequence.md)          |
| `source.dynamic` | [dynamic.md](../../operators/spec/switch/dynamic.md)            |
| `cross`          | [cross.md](../../operators/spec/cross/cross.md#14-seam-answers) |

**Sync source.** For every user: its sync source is that of what it reads
after its last committed decision, transition sources included. A commit
happens at a read, after the sync sources of the cycle were settled
([pacing §3](../../clock/spec/pacing.md#3-finding-the-sync-source)). So a
user that reads another source than before MUST say so to its clock, and the
answers settled in that cycle are computed again before the clock's next
pacing point: the clock follows the change at its next tick, as it does for
a change found while preparing a cycle. A cycle in which no user changed
source costs each source one check. Nothing registered for this outlives
the user.

**Abort.** For every user: an abort ends its own track
([source-contract.md §6](source-contract.md#6-abort)). The user is then at a
boundary and decides at its next request. This is how a reader that cuts into
a user tells it that its selection is over.
