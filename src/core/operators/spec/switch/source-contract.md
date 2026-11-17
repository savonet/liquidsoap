# Source contract (Part A, as-built)

What the switch and the frame-assembly seam rely on. Stated as it holds today.

## 1. Readiness

- A source is **ready** in a cycle when it is awake, its clock is started, and
  it either holds cached data or reports that it can generate a frame.
- Readiness is settled the first time it is asked in a cycle and is the same
  value until the cycle ends.
- Asking readiness opens the source's cycle: its before-cycle hooks run, then
  its own "can generate" rule runs. **For a source that selects among others,
  that rule performs a selection.** Asking a switch whether it is ready can
  therefore evaluate predicates, call `on_select`, wake and put sources to
  sleep. See [frame-assembly.md §3](frame-assembly.md#3-readiness).

## 2. Frames

- A ready source gives one frame per cycle. The frame is generated the first
  time it is read and is the same value until the cycle ends.
- A frame holds at most the frame size. It holds less only when the source has
  nothing more: a short frame means the source ended.
- By convention a frame carries at most one track mark. A source that
  generates several keeps the first position and drops the others, with a log
  line.
- A consumer reports how much of the frame it used. The largest reported amount
  wins. **What was not used is kept by the source and served first on the next
  cycle in which it is read**, however long that takes. A source holding such
  data is ready even if it could not generate anything new.
- Reading a frame without reporting any use ("peek") consumes nothing.
- A frame is **split** at its first track mark into the part before it and the
  part from it on. A mark at position 0 gives an empty first part. A mark past
  the frame's position is ignored, with a log line.

## 3. Track and metadata events

- Each source remembers its **last metadata**: the last metadata of the last
  chunk it produced.
- On a track mark, a source clears its last metadata first, unless it was told
  not to reset on track.
- Track callbacks run at most once per cycle per source. They receive the
  metadata found at the mark, or the last metadata, or nothing.
- A source can be asked to **insert metadata** into the next frame it
  generates, at position 0, optionally with a track mark. With override off,
  metadata the frame already has at position 0 wins over the inserted one, key
  by key.

## 4. Lifecycle

- A source is **woken** by a consumer, which gets an activation, and is put to
  **sleep** with that activation. A source is **up** while it has at least one
  activation.
- A source runs its sleep hooks when it goes down.

## 5. Other calls

- **Abort track**: tells a source to end its current track. Operators forward
  it to what they wrap.
- **Remaining**: time left in the current track, or unknown.
- **Effective source**: the single source an operator currently reads, or
  itself.
- **Self-sync**: whether the source paces itself; read once per cycle.

## 6. Clock

- The clock counts ticks. A source can read the count.
- A source's before-cycle hooks run at most once per tick, and only in ticks
  where something asks the source for its readiness. A source nobody asks is
  not animated in that tick.
- A clock controlled by an operator can tick without that operator reading its
  child: the child is animated, not read.

## 7. Per-cycle values

A value can be declared settled per cycle: computed on first read, held until
the cycle ends, dropped at the end of the cycle and again when the next one
opens.

## 8. Atomic sections

Script code can group writes to several references in an atomic section. A
reader that runs inside an atomic section never observes such a group part way
through.
