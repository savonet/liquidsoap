# Tests — as built

Seven test programs cover the clock library. Each drives the clock through its
public interface, using passive clocks ticked by hand so that no animator and
no real time are involved.

Doubles a new harness needs: a source that never produces, and an output over
it that never produces either. Both must go through real attachment and
wake-up.

## What is covered

### Lifecycle and ticking

- A new passive clock is stopped: reported mode `stopped`, not started, 0
  ticks, stream time −1.
- After a forced start it is started with mode `passive` and 0 ticks; after
  activating pending sources its output is listed.
- Each tick adds exactly one to the tick count.
- An on-tick callback runs before an after-tick callback of the same tick, and
  each runs once only.
- Stream time is ticks × frame duration, exactly.
- Stopping a passive clock is immediate and returns it to the stopped figures.

Binding: the counts, the order, the exact time value. Incidental: the ids.

### Sub-clocks

- Ticking a clock ticks its sub-clocks at every depth, once each.
- A sub-clock already ticked during its parent's tick is not ticked again.
- A deregistered sub-clock is no longer ticked; re-registering resumes it.
- Two registered sub-clocks that are unified count as one, and are ticked once.
- Stopping a clock stops its sub-clocks even when putting one of its outputs
  to sleep deregisters the sub-clock during the stop.

Binding: tick counts per clock, the sub-clock count, the sub-clock ending
stopped.

### Unification

- A handle unifies with itself.
- Two stopped automatic clocks unify; the surviving clock takes the other's id
  when it has none, and keeps its own when both have one.
- An automatic clock unifies with a CPU clock and with an unsynced one, in
  either order. Two stopped CPU clocks unify.
- A stopped CPU clock and a stopped unsynced clock conflict.
- Sources pending on either clock are pending on both handles afterwards.
- Unifying a clock with its sub-clock is a loop error.
- Two different external controllers are a controller conflict; a clock with
  no controller unifies with a controlled one.

Binding: which error, the ids, the pending counts.

### Operator sync type

Two threads computing an operator's memoised sync type at the same moment both
get the right value and none fails. The test forces the overlap with a
rendezvous inside the computation.

Binding: both results, no error, and that the two computations did overlap.
Incidental: how many times the computation ran.

### Error text

The conflict, loop and controller-conflict errors go through the application's
error reporter and each produces its own sentence; an error nothing claims
still produces the generic one.

Binding: a distinctive fragment of each sentence. Incidental: the rest of the
layout.

### Reports

Each report format is checked against exact expected text: a simple clock, a
clock with a sub-clock, line wrapping, empty lists, activations, several
clocks, a shared source, several activations, singletons, external activators
of outputs and of singletons, and a mix.

Binding: the full text, character for character.

## What is not covered

No test in this set exercises: the animator in either form, latency control,
resting, catching up, the latency reset, sync source tracking and switching,
the registry and start pass, the collecting scope, global stop and shutdown,
the source error rule, detaching, the leak warning, or requiring a thread.
