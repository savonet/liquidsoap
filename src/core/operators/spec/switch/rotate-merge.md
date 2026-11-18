# Merged rotation (Part A)

`rotate.merge`: play one track from each child in turn and present each
round as a single track. It is specified by behaviour; construction is free.

## 1. Children

`rotate.merge(~id, sources)`. Children as for [rotate.md](rotate.md);
`weight` is not read.

## 2. Rounds

A **round** is one pass over the children in list order.

- In a round each available child is played for exactly one full track.
- A child that has nothing to deliver at its turn is passed over for that
  round. It is not waited for.
- A round in which no child delivered is not a round: the operator has
  nothing to deliver.

## 3. Tracks

- The first sample of each round begins a track: one mark, on that sample.
- Inside a round there is no boundary. Each child's metadata is announced at
  its join as metadata.
- Nothing is inserted to separate rounds: the first sample of a round
  follows the last sample of the previous one.
- The end of the last child's track of a round is a `track end` of the
  operator; every other join is a `pause`
  ([frame-assembly.md §5](frame-assembly.md#5-segments)).

## 4. Commitment

The operator is a user of the seam through what it is built from: it
advances to the next child, and to the next round, only when its reader
requests data past the join
([frame-assembly.md §3](frame-assembly.md#3-commit)). Left at the end of a
round, it starts the next round when it is read again.

## 5. Abort

An abort ends the round: the track of the child being played is aborted,
the children not yet played in that round are passed over, and the next data
starts a new round.

## 6. Fallibility

Infallible when a child is.
