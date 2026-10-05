# Conformance checks

The checks an implementation is run against, by principle. Each names what is
**binding**. Unless said otherwise, ids, wording and timing are incidental.
Entries marked K are the situations of
[known-complexity.md](known-complexity.md).

## Doubles a harness needs

- A source that never produces, and an output over it that never produces
  either. Both go through real attachment and wake-up.
- A source that fails on demand, and one whose animation takes a chosen time.
- A time source whose now is set by the test, with a `timer` wait; one with a
  `blocking` wait that the test completes, and can end.
- A sync source of each kind ([pacing.md §2](pacing.md#2-sync-source)), and a
  device double that blocks for one frame of real time per tick.
- A scheduler with a chosen number of workers, which records each task it
  picks and when.
- A clock failure policy that records its calls.

Passive clocks ticked by hand, with an external entity as owner, keep
most checks free of animators and of real time.

## Lifecycle and ticking

- A new clock is stopped with reason `never started`; ticks, stream time and
  lateness are "no value"; reading them logs nothing and does not fail, also
  after the global stop.
- After a forced start a passive clock is started with 0 ticks; after
  activation its output is listed.
- Each tick adds exactly one to the tick count. Stream time is ticks × frame
  duration, exactly.
- An on-tick callback runs before an after-tick callback of the same tick,
  and each runs once.
- Stopping a passive clock that nothing is ticking is immediate and returns
  it to "no value". Stopping it from inside its own tick winds it down at the
  end of that tick.
- Ticking a stopped clock fails with `not running` and logs nothing.
- A start that cannot proceed leaves every field of the clock as it was.
  Binding: every field, and the registry.

## Registry and start

- A top-level clock is in `waiting` or `running`, never both, never
  neither; a clock with a parent is in neither, before and after it stops.
- A clock started explicitly is in `running`, and is stopped and waited for
  at shutdown.
- A start scope whose function fails starts nothing. Binding: the state
  of the clocks it created.
- K5: a start pass over N clocks that cannot start costs proportionally to N;
  with nothing waiting, a constant. Binding: the count of clocks examined.
- Clocks created and discarded without being started do not accumulate.
  Binding: registry size.

## Sources

- K2: on a running clock, attach and discard a source many times. Binding:
  subscriptions and sources kept alive return to their starting value.
- K17: an output attached to a running clock stays awake for as long as the
  clock runs; winding down puts it to sleep once per activation. Binding: the
  awake state.
- A detach asked during a tick takes the detached source, and nothing else,
  out of what that tick animates. Binding: the sources animated in that tick.
- Crossing a multiple of the leak threshold logs the leak warning once.

## Sub-clocks

- Ticking a clock ticks its started sub-clocks at every depth, once each. One
  already ticked during the parent's tick is not ticked again.
- A stopped sub-clock is skipped; the parent's tick succeeds.
- Registering on a started parent starts the sub-clock; the last
  deregistration stops it; with two registrants, the first deregistration
  changes nothing.
- Registering a non-passive clock, or a passive clock on a clock that is not
  its parent, fails with `not a sub-clock`.
- K3: register and deregister many times: the entry count returns to its
  starting value. Stop a clock whose output deregisters a sub-clock while
  going to sleep: the sub-clock ends stopped. Unify two sub-clocks of one
  parent: one entry, ticked once per tick.
- K16: an operator with a child clock created while its parent runs produces
  data on its first cycle. An operator created and never woken adds no entry
  to its parent.
- K14: two readers on one child clock: a pull by one fills both buffers; a
  tick that is not a pull buffers nothing; at diverging rates the error names
  the slower one, at the limit; a remainder held when the child ends is still
  delivered.
- K15: a passive clock cannot be created without a controller. Two exclusive
  child clocks stay two; two readers of a shared child end with one.

## Unification

- A handle unifies with itself.
- Two stopped automatic clocks unify; the survivor takes the other's id when
  it has none, and keeps its own when both have one.
- An automatic clock unifies with a CPU clock and with an unsynced one, in
  either order. Two stopped CPU clocks unify. A stopped CPU clock and a
  stopped unsynced clock conflict.
- Sources pending on either clock are pending on both handles afterwards.
- Unifying a clock with a clock nested in it, at any depth, is a loop error.
- Two passive clocks with different owners, or of which only one has an
  owner, are a controller conflict. Two passive clocks without owner unify
  exactly when their parents do, and then those are unified too.
- K11: a failed unification leaves both clocks, their controllers and their
  parents as they were, the case where the controllers could unify and the
  clocks could not included. Binding: every field of every clock involved.
- K11: after any merge, no set holds the clock merged away and none holds a
  clock twice. Merging into a started clock leaves it ticking, the other's
  sources joining at its next tick.
- K11: many threads unifying and reading at once: no failure, every handle
  designates a clock. A long run that creates, unifies and discards clocks
  uses flat memory.
- K18: no sequence of creation, registration and unification yields a clock
  that is its own ancestor.
- K4: deduplicating distinct clocks never fails, whatever they hold.

## Sync sources

- K1: with a deep graph and nothing changing, the count of sync source
  queries made to sources over many ticks is zero.
- K6: switch a selecting operator between a child that paces and one that
  does not; connect and disconnect a pacing input. Binding: the clock's
  pacing after one tick.
- K8: with a pacing source that is not ready, the clock paces itself.
- An operator that reads a passive pacing source reports it from the first
  tick, stops reporting it on the tick where the source is not ready, and
  reports it again on the tick where it is.
- A change reported from another thread during a tick is applied at the next
  tick and not during this one. Two changes of one source before a tick: the
  later one applies. Two sources moving together from one sync source to
  another: no sync error.
- A source detached during a tick, by a caller or by the error rule, is not
  animated again, that tick included.
- Two distinct sync sources on one clock, in each sync mode: a sync error
  charged to the source that brought the second one. With an error handler
  the clock carries on without that source; without one the clock fails.
- K4: a change check on an unchanged sync source costs a constant. Two sync
  sources are the same exactly when their identities are.
- Two sources with the same id and different sync sources are two entries.
- A switch keeps stream time continuous and leaves the clock neither late nor
  ahead.
- K7: two threads computing an operator's `static`/`dynamic` declaration in a
  forced overlap both get the right value.

## Pacing

- K9: with a device-paced stream, over a measured span of real time, the
  stream produced equals the real time elapsed, with clocks as tasks on and
  off. Binding: the ratio, within the device's tolerance.
- A clock following a `self-paced` sync source never rests, never warns and
  never resets.
- A paced clock produces at most the latency plus one frame ahead of its time
  source. Over a long run on a time source with a `timer` wait, it does not
  drift: binding, ticks against now at the end.
- Lateness at or above the log threshold logs a warning, at most one per log
  period; at or above the maximum, the reset sets ticks to
  `floor(now / frame duration)` and resets every animated source.
- An unsynced clock and a passive clock do none of this.

## Time box and animator

- K12: N clocks that rest, on fewer than N workers, all stay in real time.
  Binding: no clock late by more than its latency.
- K12: more late clocks than workers: each is given a worker again within one
  time box plus one tick per clock ahead of it. Binding: the scheduler's
  record of picks.
- A late clock alone on its worker, with lower-ranked work ready: the clock
  is picked again first at every release. Binding: the order of picks.
- K12: as many unsynced clocks as workers: a task due at a known time runs on
  time, because those clocks hold no worker. Binding: its delay.
- A clock that blocks, by its own sync source or a sub-clock's, an unsynced
  clock, and any clock
  with clocks as tasks off, are animated by a thread; every other by a task.
- A blocking sync source that joins a running task clock: the clock moves to
  a thread between two ticks, with ticks continuous and no error. The same
  when the sync source is in a sub-clock.
- A blocking sync source that leaves a clock that has seen no flap: the
  clock moves back to a task at that tick, however many times it happens.
- A blocking sync source that returns within the thread lease: the flap is
  logged, with the time the source stayed away, and the thread the clock
  moves to is leased.
- A leased clock whose sync source leaves and returns within the lease, any
  number of times: the clock stays on its thread. Binding: the count of
  animator changes. The start of each lease is logged.
- A clock made to move between a task and a thread at every tick, two
  thousand times, animates its sources once per tick throughout, with no
  reset.
- During a lease the clock rests and stays in real time. Binding: the stream
  produced against the real time elapsed.
- A leased clock whose lease has elapsed moves back to a task. Its next
  thread starts without a lease: a sync source that then joins and leaves
  moves the clock back at once.
- With a thread lease of 0, a clock moves back at the tick where its sync
  source leaves, a leased clock included.
- A tick that waits, on one worker and with no other clock ready, for work
  only a worker can do completes as soon as that work is done. Binding: the
  delay, against the work's own duration.

## Server-driven clocks

K10, with the blocking time source double as the server:

- one tick per server period and no drift over a long run;
- ending the server stops the clock with reason `sync source ended` within
  one period, and it is not reported as a failure;
- two clocks on one server each keep their own pace;
- such a source can join a clock that is already running.

## Failure and shutdown

- A source that fails on a clock with no handler, with each animator and each
  kind of error: the clock ends stopped with reason `failed` and the error,
  in `waiting`, its outputs asleep, its sub-clocks stopped, and the policy is
  called once. Binding: all of these, the same in every combination.
- With a handler: the source is detached, the handler called, the tick
  completes, the policy is not called.
- A sub-clock failing under its parent's tick: the parent carries on. Failing
  under a pull: the reader gets the error.
- K13: after a global stop every clock is stopped within one tick plus one
  rest, resting clocks included; nothing a tick reads is torn down before
  that or before the shutdown wait. A failed clock is not waited for.
  Binding: the order of the events and the bound.
- The shutdown wait does not change with the maximum latency.

## Observability

- Each event of [observability.md §2](observability.md#2-log-events) is
  logged with its facts. Binding: the presence and value of each fact.
- A tick longer than the time box is reported from the second tick of a
  start, and by a clock that follows a self-paced sync source it is not.
- A latency warning's breakdown adds up: time producing, waiting, released
  and without a worker account for the span since the last one.
- The reports are checked on their structure: which clocks, in which order
  and nesting, which fields, which sources under which. Binding: content,
  order and nesting. Incidental: glyphs, widths, wrapping.
- A stopped clock in a report shows its stop reason and no ticks or time.
