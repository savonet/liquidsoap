# Cross, as a user of the seam (Part A, as-built)

Only what `cross` asks of [frame-assembly.md](frame-assembly.md). Buffering,
level analysis, durations, the child clock and the transition function's
arguments are outside this spec.

## 1. Phases

`cross` reads its child through a clock of its own and never hands the child
itself to the seam. It is in one of three phases:

| Phase    | What the seam reads                                                                           |
| -------- | --------------------------------------------------------------------------------------------- |
| `idle`   | Nothing yet.                                                                                  |
| `before` | A buffer holding the current track, filled from the child, kept one crossfade duration ahead. |
| `after`  | The **compound**: the result of the transition function, built when the current track ended.  |

The compound is the transition's source, possibly wrapped:

- when the ending track had more buffered than the transition takes: a
  sequence of [that head, transition], asked to merge, not beginning a track
  at its join;
- when the starting track had more buffered than the transition takes: a
  sequence of [transition, that tail], not beginning a track at its join, not
  single-track; the tail replays the starting track's metadata, marked as a
  replay.

Both are [sequence.md](sequence.md) with its core-only parameter.

## 2. Designation

`get_source(mode)`, with `force` treated as `ok`:

- `idle`: if the child is ready, start a `before` buffer and return it if
  ready; else none.
- `before`: buffer more from the child. This can end the track and build the
  compound, moving to `after`. Then: still `before` → the buffer if it passes
  `can_reselect`, else none; now `after` → the compound.
- `after`: the compound if it passes `can_reselect`; otherwise start a new
  `before` buffer, as from `idle`.

## 3. Seam answers

- **C1.** §2. A call can read the child, run the transition function, and
  create, wake and put sources to sleep.
- **C2.** Never: readiness asks with `force`, which is then treated as `ok`.
- **C3.** Never.
- **C4.** Only when the phase is `after` at the moment of the join. Read after
  `get_source` has moved the phase: entering the compound begins a track;
  returning to a `before` buffer does not. Rationale recorded: the transition
  is where the new track begins, and going back to buffering continues it.
- **C5.** `cross` wakes each buffer and each compound when it creates it and
  puts the previous one to sleep. Callbacks the transition function registers
  on the two sources it is given, or on the child, are released when the next
  compound replaces this one.
- **C6.** Remaining: the child's in `idle`; buffer plus child in `before`,
  unknown if either is; the compound's in `after`. Effective source: the
  child's in `idle`, else that of the buffer or compound. Abort track: forward
  to the child and set a flag acted on at the start of the next cycle.
  Self-sync: the child's.
- **C7.** Always fallible.
