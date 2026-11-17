# Rotate (Part A, as-built)

Policy: play `weight` tracks from each child in turn. Sections follow
[policies.md](policies.md).

## 1. Children

`rotate(~id, sources)`. `weight` of a child: how many tracks to take from it
per round.

## 2. State

- `picked`: index of the child whose turn it is. Starts at -1 (none).
- `played`: per child, a count of tracks.

## 3. Predicates

The predicate of child `i`:

1. If `picked` is -1: **pick**.
2. If `picked` is not -1 and the picked child's weight is at most its
   `played`: **pick**.
3. Return `picked == i`.

**Pick**: set every `played` to 0. If no child is ready, `picked` := -1. Else
`picked` := the first ready child found from `picked + 1`, wrapping around.

The weight getter is read at step 2, on every predicate call.

A predicate call can therefore advance the rotation. Predicates are called in
list order, each one time per search, so one search can pick at most once per
child tested.

## 4. Events

On each track of the inner switch: add 1 to `played` of the child the inner
switch has selected. Synchronous: it runs while the frame is generated.

The count goes up when the track's first chunk is generated, not when the
track ends and not when it is heard.

## 5. Shape

Two nested switches:

- **inner**: `switch` over the children with the predicates above, named
  `<id>.rotate`;
- **outer**: `fallback([inner, child1, …, childN])`, which is what `rotate`
  returns and what carries `id`.

The reason for the outer fallback is not recorded. Effects: `rotate` is
infallible when a child is; when no predicate holds for a ready child the
outer plays the first ready child directly; every `rotate` is a nested
switch.
