# Cross specification

What `cross` and the library around it MUST, SHOULD and MAY do. It is
written to be implemented from, without the code in view.

**Scope.** The operator `cross`; the way it reads its child through a clock
of its own; the transition function and what it is handed; the standard
library around it (`cross` wrapper, `cross.plan`, `cross.apply`,
`cross.simple`, `crossfade`). Out: `fade.in` and `fade.out` themselves,
autocue's analysis, the clock. The source contract, the frame-assembly seam
and the child-clock seam are the
[streaming model](../../../source/spec/README.md)'s, and not repeated.

**Reading order**, which is also the order to build in:

| File                                       | Content                                                                   |
| ------------------------------------------ | ------------------------------------------------------------------------- |
| [cross.md](cross.md)                       | Part A. The operator: buffer, durations, crossings, delivery, abort.      |
| [transition.md](transition.md)             | Part A. The transition function: arguments, adjustments, result, its end. |
| [script-layer.md](script-layer.md)         | Part A. The wrapper, `cross.simple`, `crossfade`, the metadata keys.      |
| [conformance.md](conformance.md)           | The checks an implementation is run against, by principle.                |
| [known-complexity.md](known-complexity.md) | What the history shows to be hard, by situation, each with its rule.      |
| [ocaml-notes.md](ocaml-notes.md)           | Part B. Notes on the OCaml implementation. Not normative.                 |

No document is given to crash safety, durability or security: `cross` holds
no durable state and crosses no trust boundary. The script function it calls
runs inside the streaming cycle. Concurrency gets none either: the one
cross-thread entry is the abort ([cross.md §10](cross.md#10-abort)).

**Four rules hold everywhere.**

1. Everything is counted in samples: the project's frame independence
   ([principles.md](../../../../spec/principles.md#time)) applied to buffers
   and windows ([cross.md §2](cross.md#2-state)).
2. A track ends where the child stated it, and nowhere else
   ([child-clock.md §2](../../../source/spec/child-clock.md#2-a-tick-of-the-child-clock)).
3. A probe reads nothing and runs nothing. The child is read, and the
   transition function run, while a request is served
   ([cross.md §11](cross.md#11-readiness)).
4. A crossing puts one mark, on the first sample delivered after the
   crossing point ([cross.md §8](cross.md#8-delivery)).

**Script-visible surface.**

- `cross(~id, ~duration, ~override_duration, ~persist_override, ~width, transition, s)`,
  with the method `cross_duration()`; the script wrapper adds
  `~deduplicate` ([script-layer.md §1](script-layer.md#1-the-cross-wrapper)).
- The transition function's arguments: `ending` and `starting`, each with
  `source`, `db_level`, `metadata`.
- `cross.plan`, `cross.apply`, `cross.simple`, `crossfade`.
- The metadata keys of
  [script-layer.md §4](script-layer.md#4-metadata-keys).

**Compatibility surface.** The names above may change only when the change
is motivated, explained and confirmed. The `liq_*` metadata keys are an
interface with partners through autocue: they change only for a major
discrepancy or a design gap. `liq_cross_replayed_metadata` is gone: `cross`
announces the next track itself ([cross.md §9](cross.md#9-metadata)).

## Vocabulary

The vocabulary of the [streaming model](../../../source/spec/README.md#vocabulary),
plus:

- **Child**: the source given to `cross`. **Child clock**, **child
  output**, **intake**: [child-clock.md §1](../../../source/spec/child-clock.md#1-parts).
- **Buffer**: what the child delivered and `cross` has neither delivered
  nor handed to a transition. **Current part**, of length `r`: the rest of
  the current track. **Next part**, of length `La`: the start of the next
  one.
- **Found** (the end of a track): the child stated it and `cross` read up
  to it. **Begun** (the next track): the buffer holds a sample of it.
- **Cross duration**, `D`: how far ahead a track is read, and how much of
  its end is crossed. **`E`**: that of the current track, frozen when its
  end is found.
- **Fade-in window**: what the next track asks to have buffered.
  **Starting window**, `W`: what `cross` reads of the next track before a
  crossing. **Complete** (the next part): nothing more is waited for.
- **Head**: what of the current track plays before the crossing point.
  **Crossing point**: where the head is over.
- **Crossing**: the passage from one track of the child to the next,
  through a transition. **Overlap**, `X`: what the transition gets of the
  ending track. **`S`**: what it gets of the next one.
- **Ending source**, **starting source**: the two sources handed to the
  transition function. **Transition source**: what it returns.
- **Phase**: `idle`, `track` or `transition`.
- **Top up**: reading the child, while a request is served, up to what the
  rules ask.
