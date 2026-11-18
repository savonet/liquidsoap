# Cross specification: as-built snapshot

What `cross` does today, written to be reasoned from without the code in
view. **Pass 1 of 3**: descriptive, not normative. The normative pass
rewrites these files; the diff against this snapshot is the record of what
changed.

**Snapshot of**: branch `switch-rewrite`, commit 75acc5e67.

**Scope.** The operator `cross`; the way it reads its child through a clock
of its own; the transition function and what it is handed; the standard
library around it (`cross` wrapper, `cross.simple`, `crossfade`). Out:
`fade.in` and `fade.out` themselves, autocue's analysis, the other users of
a child clock, the clock. The seam and the source contract are specified in
[../switch/](../switch/README.md) and not repeated.

**Reading order.**

| File                                       | Content                                                                              |
| ------------------------------------------ | ------------------------------------------------------------------------------------ |
| [brief.md](brief.md)                       | The framing, the principles that bind, what pass 2 has to settle.                    |
| [child-clock.md](child-clock.md)           | Part A. The generic seam: a child read through its own clock.                        |
| [cross.md](cross.md)                       | Part A. The operator: phases, buffering, the compound, delivery, abort.              |
| [transition.md](transition.md)             | Part A. The transition function: arguments, adjustments, result, its end.            |
| [script-layer.md](script-layer.md)         | Part A. The wrapper, `cross.simple`, `crossfade`, the metadata keys.                 |
| [tests.md](tests.md)                       | The suite by principle: what is binding, what is not, what is not covered.           |
| [known-complexity.md](known-complexity.md) | What the history shows to be hard, by situation, each reconciled with this snapshot. |
| [findings.md](findings.md)                 | Defects, asymmetries, gaps, each checked or read only. Agenda for pass 2.            |
| [ocaml-notes.md](ocaml-notes.md)           | Part B. Where things are in the code. Not for pass 3 unless asked.                   |
| [pass3-handoff.md](pass3-handoff.md)       | For the rebuild session only.                                                        |

No document is given to crash safety, durability or security: `cross` holds
no durable state and crosses no trust boundary. The script function it calls
runs inside the streaming cycle. Concurrency gets none either: the one
cross-thread entry is the abort ([cross.md §10](cross.md#10-abort)).

## Vocabulary

The vocabulary of [../switch/README.md](../switch/README.md#vocabulary),
plus:

- **Child**: the source given to `cross`. **Child clock**, **intake**:
  [child-clock.md §1](child-clock.md#1-parts).
- **Tick** (unit): one step of the main rate. **`F`**: the frame size.
- **Cross duration**, `D`: how much of the end of a track `cross` keeps
  buffered ahead.
- **Ending track**, **starting track**: the two sides of a crossing.
- **Crossing**: the passage from one track of the child to the next,
  through a transition.
- **Piece**: what one take from the intake returns, at most `F`.
  **End piece**: one shorter than `F`.
- **Ending store**, **starting store**: where the pieces of each side wait.
- **Buffer source**: a source delivering a store. **Fed**: still being
  filled.
- **Ending duration**, `E`: `D` at the moment the ending track ended.
- **Fade-in window**: what the starting track asks to have buffered.
- **`Lb`, `La`**: lengths of the two stores at a crossing. **`X`**: their
  overlap. **`S`**: what the transition gets of the starting track.
- **Ending source**, **starting source**: the two sources handed to the
  transition function. **Transition source**: what it returns.
- **Head**: the part of the ending store before the overlap. **Tail**: the
  part of the starting store past `S`.
- **Compound**: head, transition source and tail as one source.
- **Phase**: `idle`, `before` (a buffer source is read) or `after` (a
  compound is read). **Designated source**: that buffer source or compound.
