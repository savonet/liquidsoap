# Frame assembly (Part A, as-built)

The seam shared by `switch`, `cross` and `sequence`: build one frame per cycle
out of whichever source the operator designates, changing source inside the
frame when the current one ends.

## 1. What the operator provides

- **`get_source(reselect)`**: the source to read now, or none.
- **`track_sensitive()`**: whether the operator re-decides only at track
  boundaries.
- **`merge()`**: whether to drop track marks where sources or tracks join.
- **`new_track_on_source_switch()`**: whether a change of source begins a
  track. Default: yes.

## 2. Reselect modes

`get_source` is told why it is asked:

| Mode                | Meaning                                                                 |
| ------------------- | ----------------------------------------------------------------------- |
| `ok`                | Keep the current source whenever possible.                              |
| `force`             | At least try to find another source.                                    |
| `after_position(p)` | Keep the current source only if its frame for this cycle goes past `p`. |

**`can_reselect(mode, s)`** is the shared test: `s` is ready, and: `ok` → yes;
`force` → no; `after_position(p)` → `p <` position of `s`'s frame.

The `after_position` test reads `s`'s frame and marks it wholly consumed. It is
a read-ahead: `s` generates its frame for the cycle, with every side effect
that has, whether or not the caller ends up using the data past `p`.

## 3. Readiness

The operator can generate a frame when
`get_source(track_sensitive() ? ok : force)` returns a ready source. That
source is remembered as the one to start the frame with.

This runs once per cycle, when the operator's readiness is first asked.

## 4. Building the frame

State kept across cycles: `current`, the last source used.

1. **First chunk.** Read the remembered source `s`. Split its frame at its
   first track mark.
   - Mark at 0, and `s` is `current`: `s` is at the start of a new track of its
     own. Clear the operator's last metadata (if it resets on track). Take
     nothing.
   - Mark at 0, and `s` is not `current`: clear last metadata likewise, take
     the whole frame and **begin a track** (§5).
   - Otherwise take the part before the mark (the whole frame if no mark). If
     `s` is not `current` and the part is not empty, apply **source switch**
     (§5).

   Only what is taken is reported as consumed.

2. **Loop** while the frame built so far is shorter than the frame size. Let
   `p` be the position of the last chunk taken from its source, and `rem` the
   room left. Record the last chunk's metadata as the operator's last
   metadata. Ask `get_source(after_position(p))`.
   - **Same source as the last chunk**: its frame MUST go past `p`; otherwise
     the operator logs a critical error and aborts. Take the data between `p`
     and `p + rem`. If `merge()`, drop its track marks. Append it.
   - **Another source**: it MUST be ready; otherwise critical error and abort.
     Split its frame: with a mark at 0 take the frame, else the part before
     the mark; in both cases at most `rem`. Apply **source switch** (§5).
     Append it.
   - **None**: stop. The frame is short.
3. Set `current` to the source of the last chunk.

## 5. Track marks at joins

- **Begin a track**: if `merge()`, drop the chunk's track marks. Otherwise put
  a track mark at the chunk's position 0 (when the chunk is not empty) and run
  the operator's track callbacks with it.
- **Source switch**: if `new_track_on_source_switch()`, begin a track. Else if
  `merge()`, drop the chunk's track marks. Else leave the chunk as is.

## 6. Consequences

- A track ending mid-frame makes the operator ask `get_source` in the same
  cycle, with `after_position`. A frame can hold the end of one track and the
  start of the next.
- A source whose frame has a mark at `p` is consumed up to `p` first. The rest
  stays with the source ([source-contract.md §2](source-contract.md#2-frames))
  and is what `after_position(p)` later finds.
- A frame is generated whole, up to the frame size, the first time it is read
  in a cycle. An operator built on this seam therefore crosses its own track
  boundary while generating: it selects its next source, begins the next track
  and runs its track callbacks, before its consumer has taken anything.
- A consumer that stops at the mark and then reads something else leaves that
  first chunk of the next track cached in the operator. The operator has
  started a track that nobody has heard. Checked on a nested `rotate`: the
  chunk is served when the operator is next read, however much later.

## 7. Users

| Operator   | `track_sensitive` | `merge`                            | New track on switch                   |
| ---------- | ----------------- | ---------------------------------- | ------------------------------------- |
| `switch`   | all children are  | never                              | always                                |
| `cross`    | never             | never                              | only when moving to the "after" phase |
| `sequence` | always            | configurable, off on the last item | configurable                          |

`cross` and `sequence` map `force` to `ok` in their `get_source`. `sequence`
turns `after_position` into `force` to play one track per item.
