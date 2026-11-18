# Streaming model specification

What every source MUST, SHOULD and MAY do, and the seams operators are built
on. It is written to be implemented from, without the code in view, under the
[conventions](../../../spec/conventions.md) and
[principles](../../../spec/principles.md) of the project.

**Scope.** The streaming cycle and what a source owes its readers; the
frame-assembly seam of operators that read several sources in turn; source
composition; the child-clock seam of operators that read a child through a
clock of their own; draining. Out: clocks themselves
([clock spec](../../clock/spec/README.md)), faults
([faults.md](../../../spec/faults.md)), each operator (its own tree).

**Reading order**, which is also the order to build in:

| File                                     | Content                                                                         |
| ---------------------------------------- | ------------------------------------------------------------------------------- |
| [source-contract.md](source-contract.md) | Part A. Cycles, segments, probes, tracks, consumption, abort, callbacks.        |
| [frame-assembly.md](frame-assembly.md)   | Part A. The seam: decisions, commits, joins, and the clauses each user answers. |
| [composition.md](composition.md)         | Part A. Tags, profiles, per-source methods, the default handoff.                |
| [child-clock.md](child-clock.md)         | Part A. Reading a child through a clock of its own.                             |
| [draining.md](draining.md)               | Part A. Every stage delivers what it holds before it ends.                      |

The checks of these documents live with their users: the source contract and
the seam in [switch/conformance.md](../../operators/spec/switch/conformance.md),
the child clock in [cross/conformance.md](../../operators/spec/cross/conformance.md).
What the history shows to be hard about them is in the
[switch](../../operators/spec/switch/known-complexity.md) and
[cross](../../operators/spec/cross/known-complexity.md) known-complexity files.

**Compatibility surface.** `composition_type`, the composition methods and
profiles of [composition.md](composition.md), and
`settings.source.composition.max_fade`. Internal interfaces change freely.

## Vocabulary

- **Tick**: one turn of a clock, in which each output gathers one frame of
  at most the **frame size** ([clock spec](../../clock/spec/README.md#vocabulary)).
- **Streaming cycle**: the unit in which sources produce. A tick holds one
  cycle, and one more each time an output's frame stopped short at a
  `track end` or a `pause`. Within a cycle every property of a source is
  settled once ([source-contract.md §1](source-contract.md#1-streaming-cycle)).
- **Streaming thread**: the thread running a clock's tick.
- **Segment**: what a source delivers in one cycle, ending for a stated
  reason: `full`, `track end`, `pause` or `late`. The segments of a tick,
  end to end, are the frame.
- **Request**: asking a source for its next segment. **Probe**: asking what
  it could deliver, with no effect. **Reader** or **consumer**: what
  requests a source's segments.
- **Ready**: a source whose probe does not answer `nothing`
  ([source-contract.md §2](source-contract.md#2-probes)).
- **Track mark**: a sample where a track begins. **Boundary**: a point where
  a source's track ended. **Lead-in**: data no mark precedes.
- **Exhausted**: a source's own declaration that it will never deliver
  again.
- **Wake**, **release**, **activation**: how a consumer brings a source up
  and lets it go ([source-contract.md §7](source-contract.md#7-lifecycle)).
- **Effective source**: the single source an operator currently reads, or
  itself.
- **User** (of the frame-assembly seam): an operator built on it.
  **Designated source**: the one it reads. **Join**: a change of designated
  source.
- **Decision**: which source would be read from a point; changes nothing.
  **Commit**: acting on a decision, when the reader requests past the point.
- **End statement**: the user saying whether its own track ends where the
  designated source's does. **Entry**: `track` or `continue`, how the first
  sample after it is delivered.
- **Composition tag**: `file`, `live` or inherited. **Profile**: the
  defaults attached to a tag.
- **Child clock**, **child output**, **intake**: the parts of the child-clock
  seam ([child-clock.md §1](child-clock.md#1-parts)).
- **Stage**: anything that can hold data it accepted and has not produced
  yet ([draining.md §1](draining.md#1-stages)).
