# Random (Part A, as-built)

Policy: at each track, play a ready child chosen at random. Sections follow
[policies.md](policies.md).

## 1. Children

`random(~id, sources)`. `weight` of a child: its relative probability.

## 2. State

`next`: index of the chosen child. Starts at -1 (none).

## 3. Predicates

The predicate of child `i`:

1. If `next` is -1: **pick**.
2. Return `next == i`.

**Pick**: lay the ready children end to end, each as an interval as wide as
its weight, in list order. Draw an integer uniformly in `[0, total)`. `next`
:= the child whose interval holds it; -1 when no child is ready.

Weights and readiness are read at pick time.

## 4. Events

On each track of the inner switch: `next` := -1. Synchronous.

A choice is thus made at the first search after a track began, and holds
until the next track begins.

## 5. Shape

As [rotate.md §5](rotate.md#5-shape): an inner `switch` named `<id>.random`
inside `fallback([inner, child1, …, childN])`.
