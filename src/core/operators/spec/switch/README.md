# Switch specification (as-built)

What the switch operator family does today, read from the code. It describes;
it does not judge. Defects and open questions are in
[findings.md](findings.md).

**Status.** Pass 1 of a spec extraction: an as-built snapshot. The normative
pass rewrites it from these documents alone. [brief.md](brief.md) carries the
framing the later passes must apply.

**Scope.** The `switch` operator; the frame-assembly seam it shares with
`cross` and `sequence`; source composition (tags, profiles, the default
handoff); the three selection policies built on `switch`: `fallback`,
`rotate`, `random`. Sources, frames and the clock appear only as the contracts
the switch relies on. `cross` and `sequence` appear only as other users of the
seam.

**Reading order.**

| File                                       | Content                                                                          |
| ------------------------------------------ | -------------------------------------------------------------------------------- |
| [source-contract.md](source-contract.md)   | Part A. What the switch requires of a source, a frame and a clock.               |
| [frame-assembly.md](frame-assembly.md)     | Part A. The seam: building one frame out of successive sources.                  |
| [switch.md](switch.md)                     | Part A. Children, state, the selection algorithm, handoff, leaving.              |
| [composition.md](composition.md)           | Part A. Composition tags, profiles, per-source methods, the default handoff.     |
| [policies.md](policies.md)                 | Part A. The policy contract: what a predicate may assume.                        |
| [fallback.md](fallback.md)                 | Part A. Policy: first ready child.                                               |
| [rotate.md](rotate.md)                     | Part A. Policy: weighted round-robin.                                            |
| [random.md](random.md)                     | Part A. Policy: weighted random.                                                 |
| [conformance.md](conformance.md)           | The test suite, by principle.                                                    |
| [known-complexity.md](known-complexity.md) | What the history shows to be hard, by situation.                                 |
| [findings.md](findings.md)                 | Defects, asymmetries, gaps, things to verify. The agenda for the normative pass. |
| [brief.md](brief.md)                       | Framing rules and the concrete scenarios for the normative pass and the rebuild. |
| [ocaml-notes.md](ocaml-notes.md)           | Part B. Notes on the OCaml implementation. Not normative.                        |

No document is given to crash safety, durability or security: a switch holds no
durable state and crosses no trust boundary. Script callbacks run inside the
streaming cycle; their failure behaviour is in
[switch.md §12](switch.md#12-failure).

**Script-visible surface** (what a script can name today):

- `switch(~id, ~all_predicates, [(predicate, source)])`, with the method
  `selected()`.
- Per-source methods read by a switch: `track_sensitive`, `replay_metadata`,
  `single`, `on_select`, `on_leave`. `weight` is read by `rotate` and `random`.
- `composition_type` on every non-output source.
- `source.composition.file(profile)`, `source.composition.live(profile)`,
  `source.composition.default_on_select`,
  `settings.source.composition.max_fade`.
- `fallback(~id, sources)`, `rotate(~id, sources)`, `random(~id, sources)`.

## Vocabulary

- **Frame**: the data a source produces in one streaming cycle. Its
  **position** is its length. **Frame size** is the configured maximum.
- **Streaming cycle**: one tick of the clock. A source's readiness and frame
  are settled once per cycle.
- **Track mark**: a position in a frame where a new track begins. **Track
  boundary**: the point where a source has no more data for its current track.
- **Child**: a source given to a switch, with its predicate and its
  composition methods.
- **Predicate**: a script function returning whether its child may be chosen.
- **Selection**: the child a switch currently streams, with what it streams
  for it.
- **Proxy**: the child wrapped so that the first frame it gives carries a track
  mark. One proxy is made per selection.
- **Effective source**: what the switch actually reads for a selection: the
  result of the child's `on_select`.
- **Handoff**: a change of selection. **Ending**: the previous proxy, handed to
  `on_select` only when its track was cut.
- **Leaving**: a previous selection whose `on_leave` has not run yet.
- **Transition in progress**: a leaving proxy is still awake, because the
  current effective source still reads it.
- **Track-sensitive** (a child): insists on track boundaries, both for being
  left and for being started.
- **Preempted**: left while its track was still playing.
- **Read-ahead**: asking a source for its frame to learn how far it can go,
  without necessarily consuming it.
- **Resuming**: the first cycle a switch is animated after one or more ticks in
  which it was not.
- **Policy**: a set of predicates that turns `switch` into `fallback`,
  `rotate` or `random`.
- **Composition tag**: `file`, `live` or inherited. **Profile**: the defaults
  attached to a tag.
