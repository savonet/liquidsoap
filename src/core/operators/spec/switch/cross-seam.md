# Cross, as a user of the seam (Part A)

`cross` is specified in [its own directory](../cross/README.md). This file
only places it among the users of [frame-assembly.md](frame-assembly.md).

## 1. Phases

`cross` reads its child through a clock of its own and never hands the child
itself to the seam. It is the child's consumer
([source-contract.md §1b](source-contract.md#1b-delivery)): the child
commits relative to what `cross` buffers. What the seam reads in each phase
is [../cross/cross.md §2](../cross/cross.md#2-state).

## 2. Decisions

`cross` reads its child and runs the transition function only while serving
a request. A probe does neither
([../cross/cross.md §11](../cross/cross.md#11-readiness)).

## 3. Seam answers

Clauses C1 to C7 are answered in
[../cross/cross.md §14](../cross/cross.md#14-seam-answers), which owns them.

In short, for the readers of this directory: a crossing puts one mark, on
the first sample of the transition, and none where the transition ends;
marks the transition carries after its first sample are its own tracks.
