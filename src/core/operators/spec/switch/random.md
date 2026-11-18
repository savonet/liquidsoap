# Random (Part A)

Policy: at each track, play an available child chosen at random. Sections
follow [policies.md](policies.md).

## 1. Children

`random(~id, sources)`. `weight` of a child: its relative probability. A
weight below 1 is 0.

## 2. State

`draw`: a number drawn uniformly in `[0, 1)`. Drawn at creation.

## 3. Predicates

The **target** is computed from `draw`, the weights and probes, and changes
nothing: lay the available children of weight 1 or more end to end, in list
order, each as an interval as wide as its weight; the target is the child
whose interval holds `draw × total`. With no such child: the first available
child in list order; else none.

The predicate of a child holds when that child is the target.

For one `draw`, the target changes only when weights or availability do.

## 4. Events

On each track event of the switch
([policies.md §6](policies.md#6-predicate-contract)): draw again.

So a choice is made once per track played, whatever the weights.

## 5. Shape

`random` behaves as one switch over the children with the predicates above.

- A child is chosen with a probability proportional to its weight among the
  available children.
- A child of weight 0 is never chosen while a child of positive weight is
  available.
- Infallible when a child is.
