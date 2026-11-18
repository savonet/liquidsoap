# Switch specification

What the switch operator family and the seam it is built on MUST, SHOULD and
MAY do. It is written to be implemented from, without the code in view.

**Scope.** What every source owes its readers; the frame-assembly seam; each
operator built on it, in its own file: `switch`, `sequence`,
`source.dynamic`, and `cross` for its use of the seam only. Source
composition (tags, profiles, the default handoff). The policies built on
`switch`: `fallback`, `rotate`, `random`, and the merged rotation. The
script helpers built on `sequence` and `source.dynamic`, for where their
tracks start. The rest of `cross` is specified in
[../cross/](../cross/README.md). The clock is specified in
[its own directory](../../../clock/spec/README.md).

**Reading order**, which is also the order to build in:

| File                                       | Content                                                                         |
| ------------------------------------------ | ------------------------------------------------------------------------------- |
| [source-contract.md](source-contract.md)   | Part A. Segments, probes, tracks, consumption, abort: what every source owes.   |
| [frame-assembly.md](frame-assembly.md)     | Part A. The seam: decisions, commits, joins, and the clauses each user answers. |
| [switch.md](switch.md)                     | Part A. Seam user: children, decisions, handoff, leaving.                       |
| [composition.md](composition.md)           | Part A. Tags, profiles, per-source methods, the default handoff.                |
| [policies.md](policies.md)                 | Part A. The predicate contract.                                                 |
| [fallback.md](fallback.md)                 | Part A. Policy: first available child.                                          |
| [rotate.md](rotate.md)                     | Part A. Policy: weighted round-robin.                                           |
| [random.md](random.md)                     | Part A. Policy: weighted random.                                                |
| [sequence.md](sequence.md)                 | Part A. Seam user: items played in order.                                       |
| [dynamic.md](dynamic.md)                   | Part A. Seam user: the source a script function designates.                     |
| [rotate-merge.md](rotate-merge.md)         | Part A. Rounds presented as single tracks.                                      |
| [helpers.md](helpers.md)                   | Part A. `once`, delayed fade-in, `prepend`, `append`: where tracks start.       |
| [cross-seam.md](cross-seam.md)             | Part A. Seam user: `cross`, for its use of the seam only.                       |
| [conformance.md](conformance.md)           | The checks an implementation is run against, by principle.                      |
| [known-complexity.md](known-complexity.md) | What the history shows to be hard, by situation, each with its rule.            |
| [brief.md](brief.md)                       | The maintainer's framing, the binding scenarios, the rulings.                   |
| [ocaml-notes.md](ocaml-notes.md)           | Part B. Notes on the previous OCaml implementation. Not normative.              |

No document is given to crash safety, durability or security: these
operators hold no durable state and cross no trust boundary. Script
callbacks run inside the streaming cycle; their failure is
[switch.md §10](switch.md#10-failure).

**Three rules hold everywhere.**

1. A source stops where going on would decide something, and decides only
   when its reader asks for more
   ([source-contract.md §1](source-contract.md#1b-delivery),
   [frame-assembly.md §3](frame-assembly.md#3-commit)).
2. Nothing is deduced from being probed, read or ticked; every change of
   state has a named cause
   ([source-contract.md §9](source-contract.md#9-no-inference)).
3. A track mark sits on the logical start of content, with its metadata
   ([source-contract.md §3](source-contract.md#3-tracks),
   [helpers.md](helpers.md)).

**Script-visible surface.**

- `switch(~id, [(predicate, source)])`, with the method `selected()`.
- Per-source methods read by a switch: `track_sensitive`, `replay_metadata`,
  `single`, `on_select`, `on_leave`. `weight` is read by `rotate` and
  `random`, which do not read `track_sensitive`.
- `composition_type` on every non-output source.
- `source.composition.file(profile)`, `source.composition.live(profile)`,
  `source.composition.default_on_select`,
  `settings.source.composition.max_fade`.
- `fallback(~id, sources)`, `rotate(~id, sources)`, `random(~id, sources)`.
- `rotate.merge(~id, sources)`.
- `sequence(~id, ~merge, ~single_track, sources)`, with the method `queue()`.
- `source.dynamic(~id, ~init, ~track_sensitive, ~infallible, ~self_sync, ~merge, next)`,
  with the methods `current_source()` and `prepare(s)`.

For logs and errors the content is specified and the wording is free.

## Vocabulary

- **Tick**: one turn of the clock, in which each output gathers one frame of
  at most the **frame size**.
- **Streaming cycle**: the unit in which sources produce. A tick holds one
  cycle, and one more each time an output's frame stopped short at a
  `track end` or a `pause`. Within a cycle every property of a source is
  settled once ([source-contract.md §1](source-contract.md#1-streaming-cycle)).
- **Segment**: what a source delivers in one cycle, ending for a stated
  reason: `full`, `track end`, `pause` or `late`. The segments of a tick,
  end to end, are the frame.
- **Request**: asking a source for its next segment. **Probe**: asking what
  it could deliver, with no effect.
- **Available**: a probe does not answer `nothing`. **Ready**: available at
  the position delivered so far.
- **Track mark**: a sample where a track begins. **Boundary**: a point where
  a source's track ended. **Lead-in**: data no mark precedes.
- **Exhausted**: a source's own declaration that it will never deliver
  again.
- **User** (of the seam): an operator built on it. **Designated source**:
  the one it reads. **Join**: a change of designated source.
- **Decision**: which source would be read from a point; changes nothing.
  **Commit**: acting on a decision, when the reader requests past the point.
- **End statement**: the user saying whether its own track ends where the
  designated source's does. **Entry**: `track` or `continue`, how the first
  sample after it is delivered.
- **Child**: a source given to a switch, with its predicate and its
  composition methods. **Predicate**: a script function saying whether its
  child may be chosen.
- **Selection**: the child a switch streams. **Presented source**: the child
  as handed to script code, one per selection. **Effective source**: what
  the switch reads for a selection, the result of `on_select`.
- **Handoff**: a change of selection. **Preemption**: a handoff that cuts
  into a track. **Ending**: the previous presented source, given to
  `on_select` on a preemption.
- **Leaving**: a previous selection still read by a transition.
- **Track-sensitive** (a child): left, and started, at track boundaries.
- **Item**: a source in a sequence's queue.
- **Policy**: predicates and their state, turning `switch` into `fallback`,
  `rotate` or `random`.
- **Composition tag**: `file`, `live` or inherited. **Profile**: the
  defaults attached to a tag.
