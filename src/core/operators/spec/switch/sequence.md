# Sequence (Part A)

Plays its items in order: each one until it is finished, the last one for as
long as it delivers. Segments, decisions, commits and joins are
[frame-assembly.md](frame-assembly.md).

## 1. Parameters

`sequence(~id, ~merge, ~single_track, sources)`, with the method `queue()`.

| Parameter      | Default | Meaning                                                                                |
| -------------- | ------- | -------------------------------------------------------------------------------------- |
| `merge`        | false   | Present the items as one track ([§4](#4-merging)).                                     |
| `single_track` | true    | An item is finished at its first track end. False: an item is finished when exhausted. |

Each item has an **entry**, `track` or `continue`
([frame-assembly.md §6](frame-assembly.md#6-joins)). Scripts set it through
`merge` only. An operator that builds a sequence internally MAY give each
item its own entry.

## 2. State

- `queue`: the items left, head first. Starts as the full list. `queue()`
  returns it.
- The **current item** is the head.

The queue changes only in a commit, in an abort and when the sequence is
released. A sequence that is left and read again resumes where it was.

## 3. Items

**When an item is finished.** Only the item says so:

- with `single_track`: when its track ends (`track end`);
- without: when it declares itself exhausted
  ([source-contract.md §7](source-contract.md#7-lifecycle)).

The last item is never finished: it stays the current item.

`nothing`, `late` and a segment shorter than asked do not say that an item is
finished. What the sequence does with an item that has nothing to deliver
and is not exhausted depends on its last item:

- the last item can deliver: the item is **passed over**. It is dropped like
  a finished one and the sequence moves on, so that a sequence whose last
  item is available always has something to deliver;
- the last item cannot: the item is **kept**. The sequence is not ready in
  the cycles where the item has nothing; it plays the item when it has.

Nothing blocks inside a cycle: a sequence that keeps an item answers
`nothing` to probes until the item is available.

**Skipped items.** An item that is exhausted when its turn comes, without
having delivered anything, is skipped. The skip is logged.

**Decision at a boundary.** The first item of the queue, after the finished
one, that is not exhausted; the last item if all others are. The decision is
none when that item's probe answers `nothing`.

**Commit.** The finished item and the skipped ones are dropped from the
queue and released, each exactly once, after the item entered is awake. Each
drop is logged.

**Empty items.** An item that delivers no sample takes no audio from its
neighbours: the next item's first sample follows the previous item's last.

## 4. Merging

With `merge`, the sequence is one track:

- the first item that delivers data is entered with `track`;
- every later item is entered with `continue`: no boundary at the join, the
  item's metadata announced at the join as metadata;
- once the last item is the current one, its own later tracks keep their
  boundaries: they are not joins.

Without `merge`, every item is entered with `track`.

**End statement.** When the current item's track ends:

- the item is finished: `pause` if the next item to be entered has the entry
  `continue`, `track end` otherwise. The next item to be entered is known at
  that moment: exhaustion is declared, not probed;
- the item is not finished (the last item, or an item played until
  exhausted): `track end`. The same item's next track is its own.

The result is the same wherever the join falls: inside a frame, on a frame
edge, in the first frame of the stream.

## 5. Abort

- Without `merge`: forwarded to the current item. Its track has ended; with
  `single_track` the item is finished.
- With `merge`: the whole unit is aborted. Every item before the last is
  dropped and released. If the current item was the last, its track is
  aborted; otherwise the last item is entered at its start.

In both cases the sequence is at a boundary: what it delivers next begins a
track.

## 6. Other properties

- **Remaining**: with `merge`, the sum over the queue, unknown if any item's
  is unknown; otherwise the current item's.
- **Effective source**: the current item's, or the sequence itself with an
  empty queue.
- **Sync source**: by [frame-assembly.md §9](frame-assembly.md#9-contract-clauses).

## 7. Seam answers

- **C1.** [§3](#3-items). The commit drops and releases items.
- **C2.** None: a sequence never re-decides while a track plays.
- **C3.** [§4](#4-merging).
- **C4.** The entry of the first item that delivers: `track`, unless an
  internal builder gave it `continue`. After a wait, the stream continues
  where it stopped: no entry is applied.
- **C5.** Items are woken with the sequence. Each is released when it is
  dropped; what is left is released with the sequence.
- **C6.** [§5](#5-abort), [§6](#6-other-properties).
- **C7.** Infallible when its last item is infallible. Exhausted when the last
  item is exhausted and every other item is finished, skipped or exhausted.
  It then stops being ready, and not before the end of what it delivered was
  taken.
