# Sequence (Part A, as-built)

Plays its items in order: one track from each, then the last one for as long
as it has data. Frame building is [frame-assembly.md](frame-assembly.md).

## 1. Parameters

`sequence(~id, ~merge, ~single_track, sources)`, with the method `queue()`.

| Parameter      | Default | Meaning                                                                                |
| -------------- | ------- | -------------------------------------------------------------------------------------- |
| `merge`        | false   | Merge tracks when moving from one item to the next. Not on the last item's own tracks. |
| `single_track` | true    | Move on at an item's track boundary. False: play each item until it is unavailable.    |

One more parameter exists for operators built inside the core and is not
visible to scripts: whether a move to the next item begins a track (default
yes). `cross` sets it to no.

## 2. State

- `queue`: the items left, head first. Starts as the full list. `queue()`
  returns it.
- `started`: false until, at some call, any item of the queue is ready; then
  true for good.

## 3. Designation

`get_source(mode)`:

1. If not `started`, or the queue is empty: none.
2. **One item left**: return it if it passes `can_reselect` with `force`
   turned into `ok`; else none. The last item is never dropped.
3. **Several items left**, head `s`: test `can_reselect(m, s)` where `m` is
   `force` when the mode is `after_position`, `single_track` holds, and this
   call has not already dropped an item; else the mode as given.
   - Passes: return `s`.
   - Fails: log "Finished with"; drop `s` from the queue; release it; start
     again at step 1 with mode `ok` and "already dropped" set.

Consequences:

- With `single_track`, an item's track boundary always moves to the next
  item: `force` never passes.
- An item that is not ready when its turn comes is dropped for good, whatever
  the reason.
- The item moved to is tested with `ok`, not with the position computed on the
  item just finished. Rationale recorded: carrying the position over rejected
  an item with no more samples than the one before, losing its first chunk and
  its metadata.
- One call can drop several items.

## 4. Merging

`merge()` is: merging was asked, and the queue holds more than one item. It is
read at the join, after §3 has dropped the finished item
([frame-assembly.md §8](frame-assembly.md#8-when-merge-is-read)).

The parameter's help points to a `merge_tracks()` for merging the last item's
own tracks. A script function of that name exists and is deprecated; it
removes the track marks of a source. A core wrapper doing the same sits next
to `sequence` and is reachable from nowhere.

## 5. Other properties

- **Remaining**: with `merge` asked, the sum over the queue, unknown if any
  item's is unknown; otherwise the head's.
- **Abort track**: with `merge` asked, the queue becomes its last item only
  (the items dropped this way are not released); then abort the head's track.
- **Effective source**: the head's, or the sequence itself with an empty
  queue.
- **Self-sync**: the head's.

## 6. Seam answers

- **C1.** §3. `force` is treated as `ok`. A call can drop and release items.
- **C2.** Always.
- **C3.** §4. Changes when the queue shrinks to one item.
- **C4.** Yes, unless built with "no" (§1).
- **C5.** Items are the sequence's sources: woken with it. A dropped item is
  released at once.
- **C6.** §5.
- **C7.** Fallible exactly when the last item is.

## 7. Built on it

- `once(s)`: `sequence([s, <a source that is never ready>])`. One track of
  `s`, then nothing.
- A delayed fade-in: `sequence(merge=true, [silence of the delay, s])`.
- `prepend(merge)`: per track, `sequence(merge=merge, [prefix, s])` handed out
  by a track-sensitive `source.dynamic`.
- The merged looping rotation ([rotate-merge.md](rotate-merge.md)).
- `cross`, around its transitions ([cross-seam.md](cross-seam.md)).
