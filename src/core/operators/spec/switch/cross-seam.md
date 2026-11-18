# Cross, as a user of the seam (Part A)

Only what [frame-assembly.md](frame-assembly.md) requires of `cross`.
Buffering, level analysis, durations, the child clock and the transition
function's arguments are outside this specification ([§4](#4-open)).

## 1. Phases

`cross` reads its child through a clock of its own and never hands the child
itself to the seam. It is the child's consumer
([source-contract.md §1](source-contract.md#1-delivery)): the child commits
relative to what `cross` buffers.

| Phase    | What the seam reads                                                                           |
| -------- | --------------------------------------------------------------------------------------------- |
| `idle`   | Nothing.                                                                                      |
| `before` | A buffer holding the current track, filled from the child, kept one crossfade duration ahead. |
| `after`  | The **compound**, built when the buffered track ended.                                        |

The compound delivers, in this order:

1. the **head**, when the ending track had more buffered than the transition
   takes: the rest of that track;
2. the **transition**: the source the transition function returned, played
   until it is exhausted;
3. the **tail**, when the starting track had more buffered than the
   transition takes.

## 2. Decisions

- `idle`, boundary: a new buffer when the child's probe does not answer
  `nothing`; else none. Commit: start the buffer, enter `before`.
- `before`: no decision while the buffered track plays. When it ends: the
  compound. Commit: run the transition function, build the compound, enter
  `after`.
- `after`: no decision while the compound delivers. When it is exhausted: as
  from `idle`, but the new buffer continues the track.

`cross` reads its child and runs the transition function only while serving
a request. A probe does neither.

## 3. Seam answers

- **C1.** [§2](#2-decisions).
- **C2.** None.
- **C3.** The compound is a [sequence](sequence.md) whose items have their
  own entries; `cross` relays its statements. When the buffered track ends: `pause` if the compound starts with
  a head, `track end` otherwise. When the head ends: `track end`. A track end
  inside the transition: `track end`. When the transition is exhausted and a
  tail follows: `pause`. When the compound is exhausted: `pause`.

  So a crossing puts one mark, on the first sample of the transition, and
  none where the transition ends. Marks the transition carries after its
  first sample are its own tracks and pass: a transition that wants a
  crossing to be one track carries none.

- **C4.** `track` for a buffer started from `idle`.
- **C5.** `cross` wakes each buffer and each compound when it creates it and
  releases the previous one after that. Callbacks the transition function
  registers on the sources it is given, or on the child, are released with
  the compound.
- **C6.** Remaining: the child's in `idle`; buffer plus child in `before`,
  unknown if either is; the compound's in `after`. Effective source: the
  child's in `idle`, else that of the buffer or compound. Abort: forwarded to
  the child; the phase ends when the cycle is over and `cross` is at a
  boundary. Sync source: the child's is tracked by the child clock; `cross`
  reports none on its behalf.
- **C7.** Always fallible. Exhausted when the child is and nothing is
  buffered.

## 4. Open

The rest of `cross` gets its own specification before its implementation is
changed. That specification has to settle, against the clauses above:

- how the buffer stays one crossfade duration ahead when the child is read
  only while a request is served;
- a track shorter than the crossfade duration, and a starting track that
  ends inside the compound;
- which metadata the mark of a crossing carries, and how the tail announces
  the starting track;
- the effect of an abort during a transition.
