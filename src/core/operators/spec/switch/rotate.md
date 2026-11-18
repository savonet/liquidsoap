# Rotate (Part A)

Policy: play `weight` tracks from each child in turn. Sections follow
[policies.md](policies.md).

## 1. Children

`rotate(~id, sources)`. `weight` of a child: how many tracks to take from it
per turn. A weight below 1 is 0: the child has no turn.

## 2. State

- `cursor`: the child whose turn it is. None at start.
- `played`: how many tracks of the current turn were played.

## 3. Predicates

The **target** is computed from the state, the weights and probes, and
changes nothing:

1. the `cursor` child, if there is one, `played` is below its weight, and it
   is available;
2. else the first available child with a weight of 1 or more, searched in
   list order from the child after `cursor`, wrapping around and ending with
   `cursor` itself; from the first child when there is no `cursor`;
3. else the first available child in list order;
4. else none.

The predicate of a child holds when that child is the target. Weights are
read at each computation.

Rule 3 is what keeps `rotate` infallible when a child is
([policies.md §7](policies.md#7-fallibility)).

## 4. Events

On each track event of the switch
([policies.md §6](policies.md#6-predicate-contract)), with `c` the child it
comes from:

- `c` is `cursor`: `played` := `played` + 1;
- otherwise: `cursor` := `c`, `played` := 1.

So a track counts when it is played, once, and never when it was only
decided. The rotation is the same whether `rotate` is read by an output, by
an operator that reads it on some cycles only, or by a switch that leaves it
and returns.

## 5. Shape

`rotate` behaves as one switch over the children with the predicates above.

- Each available child is played for `weight` consecutive tracks, then the
  next available child in order, from the first round on.
- A child that is not available at its turn is passed over; it is picked at
  its next turn if available then.
- Infallible when a child is.
