# Switch specification

What the switch operator family MUST, SHOULD and MAY do. It is written to be
implemented from, without the code in view, on the
[streaming model](../../../source/spec/README.md): the source contract, the
frame-assembly seam and composition are specified there, under the
[principles](../../../../spec/principles.md) of the project.

**Scope.** Each operator built on the frame-assembly seam, in its own file:
`switch`, `sequence`, `source.dynamic`. The policies built on `switch`:
`fallback`, `rotate`, `random`, and the merged rotation. The script helpers
built on `sequence` and `source.dynamic`, for where their tracks start.
`cross` is specified in [../cross/](../cross/README.md).

**Reading order**, which is also the order to build in, after the streaming
model:

| File                                       | Content                                                                   |
| ------------------------------------------ | ------------------------------------------------------------------------- |
| [switch.md](switch.md)                     | Part A. Seam user: children, decisions, handoff, leaving.                 |
| [policies.md](policies.md)                 | Part A. The predicate contract.                                           |
| [fallback.md](fallback.md)                 | Part A. Policy: first available child.                                    |
| [rotate.md](rotate.md)                     | Part A. Policy: weighted round-robin.                                     |
| [random.md](random.md)                     | Part A. Policy: weighted random.                                          |
| [sequence.md](sequence.md)                 | Part A. Seam user: items played in order.                                 |
| [dynamic.md](dynamic.md)                   | Part A. Seam user: the source a script function designates.               |
| [rotate-merge.md](rotate-merge.md)         | Part A. Rounds presented as single tracks.                                |
| [helpers.md](helpers.md)                   | Part A. `once`, delayed fade-in, `prepend`, `append`: where tracks start. |
| [conformance.md](conformance.md)           | The checks, for this tree and for the source contract and the seam.       |
| [known-complexity.md](known-complexity.md) | What the history shows to be hard, by situation, each with its rule.      |
| [ocaml-notes.md](ocaml-notes.md)           | Part B. Notes on the OCaml implementation. Not normative.                 |

No document is given to crash safety, durability or security: these
operators hold no durable state and cross no trust boundary. Script
callbacks run inside the streaming cycle; their failure is
[switch.md §10](switch.md#10-failure).

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

**Compatibility surface.** The names above. `switch` lost `all_predicates`
and the argument of `on_leave` is `{source, preempted}`; any further
removal or rename is confirmed with the maintainer first. `rotate` and
`random` stay script-level, on the predicate contract of
[policies.md](policies.md); the merged rotation is specified by behaviour,
its construction is free.

## Vocabulary

The vocabulary of the [streaming model](../../../source/spec/README.md#vocabulary),
plus:

- **Child**: a source given to a switch, with its predicate and its
  composition methods. **Predicate**: a script function saying whether its
  child may be chosen. **Available** (a child): what would be read for it is
  ready ([switch.md §4](switch.md#4-decisions)).
- **Selection**: the child a switch streams. **Presented source**: the child
  as handed to script code, one per selection. **Selected source**: what
  the switch reads for a selection, the result of `on_select`; it is the
  switch's effective source.
- **Handoff**: a change of selection. **Preemption**: a handoff that cuts
  into a track. **Ending**: the previous presented source, given to
  `on_select` on a preemption.
- **Leaving**: a previous selection still read by a transition.
- **Track-sensitive** (a child): left, and started, at track boundaries.
- **Item**: a source in a sequence's queue.
- **Policy**: predicates and their state, turning `switch` into `fallback`,
  `rotate` or `random`.
