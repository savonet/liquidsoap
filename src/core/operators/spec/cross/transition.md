# The transition function (Part A)

What `cross` hands the script function it was given, and what it does with
the source that comes back. When it is called is
[cross.md §6](cross.md#6-crossing).

## 1. Arguments

`transition(ending, starting)` returns a source. Each argument has three
fields:

| Field      | `ending`                                             | `starting`                                            |
| ---------- | ---------------------------------------------------- | ----------------------------------------------------- |
| `source`   | The ending source: the last `X` of the ending track. | The starting source: the first `S` of the next track. |
| `db_level` | The ending level, in dB.                             | The starting level, in dB.                            |
| `metadata` | The ending track's metadata.                         | The next track's metadata.                            |

`X`, `S` and the levels are [cross.md §6](cross.md#6-crossing) and
[§7](cross.md#7-level-analysis); a track's metadata is
[cross.md §9](cross.md#9-metadata).

- Both sources are finite, not yet woken, in the clock of `cross`, fallible.
  Each MUST report its exact length as its remaining time, deliver its data
  in order with marks and metadata at their positions, and declare
  exhaustion when it is empty. Each reports no sync source.
- Either MAY be empty. An empty source is exhausted from the start.
- The starting source begins with the mark and the metadata of its track.
  The ending source carries those of its track only when the whole track
  is in it.
- A side with no metadata gets an empty one. A level over nothing is minus
  infinity.
- The function is called once per crossing, at its commit, while `cross`
  serves a request. It is never called without a next track
  ([cross.md §5](cross.md#5-the-end-of-a-track)).

## 2. Metadata

Each side's `metadata` is the track's metadata as the track carried it
([cross.md §9](cross.md#9-metadata)): `cross` changes none of its keys and
adds none. A side with no metadata gets an empty one.

Where each fade sits in the crossing is not the operator's business. It is
computed once, by the plan of
[script-layer.md §2](script-layer.md#2-the-plan-of-a-crossing), from this
metadata and from the exact lengths the two sources report.

The operator reads two of these keys for itself, and only to know how much
of the next track to buffer: the fade-in window of
[cross.md §4](cross.md#4-reading-the-child).

## 3. The result

- The source returned MUST produce what `cross` produces: its type is
  unified with that of `cross` when the script is typed, so that a function
  written without annotations fits a `cross` of any content.
- It is placed in the clock of `cross` and woken by `cross`
  ([cross.md §13](cross.md#13-lifetime)).
- It need not read either argument. Data of an argument it does not read is
  never played.
- Its sync source, if it has one, is reported by `cross` while it plays
  ([cross.md §12](cross.md#12-other-properties)).
- A result that delivers nothing at once is replaced
  ([cross.md §6](cross.md#6-crossing), step 7).
- A function that raises a script error is treated the same way: the error
  is logged, what the function registered is released, and the crossing
  plays its two sources in order. The stream does not stop for it.

## 4. End of the transition

`cross` has no notion of the transition's length. The transition is **over**
when either holds:

- the transition source **declares exhaustion**
  ([source-contract.md §7](../switch/source-contract.md#7-lifecycle));
- at the start of a cycle it answers `nothing` while what follows it can
  deliver ([cross.md §11](cross.md#11-readiness)). It is then passed over
  for good, like an item of a
  [sequence](../switch/sequence.md#3-items).

Nothing else ends it:

- a track end inside the transition source is a track of its own
  ([cross.md §8](cross.md#8-delivery));
- a segment that is short, or ends `late` or `pause`, says nothing: a
  transition made of several items, with gaps between them, plays them all.

So a transition function MUST return a source that ends: finite sources,
and operators over them, which declare exhaustion when their inputs have.
**Nothing bounds a transition that never ends**: it is played for as long as
it delivers, the child's later tracks do not start, and the child is not
read past one cross duration meanwhile. No length is natural to a
transition: one that inserts items between its two sources is rightly
longer than both windows.

## 5. Callbacks

Callbacks the function registers, while it runs, on the ending source, on
the starting source or on the child of `cross` are collected. They MUST be
released when `cross` leaves the transition source: over, replaced,
aborted, or at sleep. Over any number of crossings the callbacks registered
this way on the child stay bounded by those of the crossings in progress.

## 6. Shapes in use

Consequences of [cross.md §8](cross.md#8-delivery) and
[§9](cross.md#9-metadata), for the shapes scripts return. `a` and `b` are
the two sources.

| Returned                          | Tracks in a crossing                                                              | The next track is announced                         |
| --------------------------------- | --------------------------------------------------------------------------------- | --------------------------------------------------- |
| `add([a, b])`, faded or not       | One, on the first sample of the transition. The sum carries no mark of its own.   | Where `b`'s first sample plays.                     |
| `sequence([a, b])`                | Two: the first sample of the transition, then the join, which the sequence marks. | At the join.                                        |
| `sequence([a, x, y, b])`          | One per item.                                                                     | At the last join.                                   |
| `a` alone                         | One.                                                                              | When the transition is over, by the insertion rule. |
| a source unrelated to `a` and `b` | One, with whatever that source carries.                                           | When the transition is over, by the insertion rule. |
| empty sources, nothing delivered  | One, on the first sample of the track that follows.                               | There, by the track's own metadata.                 |
