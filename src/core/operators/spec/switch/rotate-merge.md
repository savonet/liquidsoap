# Merged rotation (Part A, as-built)

`rotate.merge`: play one track from each child in turn and present each round
as a single track. Sections follow [policies.md](policies.md).

## 1. Children

`rotate.merge(~id, sources)`. Children as for [rotate.md](rotate.md).

## 2. State

`ready`: a boolean, true at start.

## 3. Predicates

The result is a `switch` over one child, listed twice: first with the
predicate "`ready` is true", then with "`ready` is false". Exactly one holds
at any time.

## 4. Events

Each time the rotation selects its **first** child, that child's `on_select`
is replaced by one that:

1. flips `ready`;
2. returns `sequence(merge=true, [one frame of silence, starting])`.

It ignores `ending` and does not replay metadata.

## 5. Shape

From the inside out:

1. `rotate` over the children, the first one carrying the `on_select` above;
2. that rotation with its track marks removed, and `replay_metadata` false;
3. the two-predicate `switch` of §3 over it, which carries `id`.

How rounds are meant to become tracks, as far as the code and its comments
say:

- Inside a round nothing marks a track: step 2 removed the marks.
- At the start of each round `ready` flips, so the outer switch's other
  predicate becomes the true one, and the outer switch is expected to hand off
  to the same source through its other entry, which would begin a track
  ([switch.md §8](switch.md#8-handoff), [§9](switch.md#9-proxy)).
- The frame of silence exists, by the comment recorded with it, "so merged
  tracks stay separable". It relies on the join into the last item of a
  merged sequence keeping its mark
  ([frame-assembly.md §8](frame-assembly.md#8-when-merge-is-read)).

Nothing states what makes the outer switch search when `ready` flips: its
selection has no track marks, and both entries hold the same source, which
the search treats as the selected child
([switch.md §7](switch.md#7-search)).

**Observed** (three children of equal short tracks, each titled): one track is
reported for the whole stream, at its start; the first child's title appears
once and never again while the other two alternate; the inner rotation logs a
handoff to each of the three children on every frame. See
[findings.md](findings.md) D10.
