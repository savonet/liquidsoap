# Clocks (Part A)

Pacing, the time box and the animator are in [pacing.md](pacing.md).
Unification is in [unification.md](unification.md). Logs and reports are in
[observability.md](observability.md).

## 1. Entities

**Clock.** Fields that exist for the clock's whole life:

| Field           | Meaning                                                                                                  |
| --------------- | -------------------------------------------------------------------------------------------------------- |
| identity        | A number unique in the process, increasing with creation order. Clocks are compared by identity only.    |
| id              | Optional name. See [§2](#2-names).                                                                       |
| sync mode       | One of `auto`, `cpu`, `none`, `passive`. Fixed at creation.                                              |
| parent          | Passive clocks only: the clock inside whose ticks this clock is ticked. Optional when there is an owner. |
| owner           | Passive clocks only, optional: a named external entity (a kind and an object id) that alone may tick it. |
| stack           | Script positions, for error reports. Set once: a second set is ignored.                                  |
| state           | `stopped`, `started` or `stopping`.                                                                      |
| stop reason     | Why the clock is stopped. See [§4](#4-states).                                                           |
| pending sources | Sources attached but not yet activated. No duplicates.                                                   |
| sub-clocks      | The clocks this clock prepares and cleans up, each with its registrants. See [§10](#10-sub-clocks).      |
| error handlers  | Callbacks `(error, backtrace)`.                                                                          |

The **controller** of a passive clock is what ticks it: its owner if it has
one, else its parent. It is derived, never stored. A clock MUST have a
controller if and only if it is passive: a passive clock has a parent, an
owner, or both. A clock registered as a sub-clock MUST have a parent.

Rationale: "no controller" on a passive clock would mean "unify with
anything", and losing a controller would raise no error where it is lost.
Making it mandatory turns that loss into a refused creation.

**Streaming state.** Exists only while the clock is `started` or `stopping`,
and is created anew at every start:

| Field                | Meaning                                                                                  |
| -------------------- | ---------------------------------------------------------------------------------------- |
| ticks                | Number of ticks completed. Starts at 0.                                                  |
| frame duration       | Seconds of stream per tick. Read once at start from the frame settings.                  |
| pacing state         | Time source, anchor, tracked and followed sync source. See [pacing.md](pacing.md).       |
| outputs              | Ordered list of (activation, source). Keeps each source alive.                           |
| active sources       | Set. MUST NOT be what keeps a source alive.                                              |
| passive sources      | Set. MUST NOT be what keeps a source alive.                                              |
| on-tick callbacks    | One-shot.                                                                                |
| after-tick callbacks | One-shot.                                                                                |
| animator             | `task` or `thread`; absent on a passive clock. See [pacing.md §8](pacing.md#8-animator). |
| statistics           | See [observability.md §1](observability.md#1-status-record).                             |

**Stream time** is `ticks × frame duration`.

**No value.** A clock without streaming state has no ticks, no stream time, no
sources other than pending ones, and is not self-sync. Reading any of these
MUST return "no value", the same one for all of them. It MUST NOT fail, MUST
NOT log, and MUST behave the same during shutdown.

## 2. Names

A clock's name is, in order: its id if set; else the id of its most significant
pending source (outputs first, then active, then passive; first in attachment
order within a type); else `generic`.

Setting an id MUST make the wanted name unique among clock ids. Setting the id
a clock already has does nothing. At start, the clock's name at that moment is
frozen as its id.

## 3. Sync modes

| Mode      | Text      | Meaning                                                                                 |
| --------- | --------- | --------------------------------------------------------------------------------------- |
| automatic | `auto`    | Follows its sync source when it has one, the configured time source otherwise. Default. |
| CPU       | `cpu`     | Always paced by the configured time source.                                             |
| unsynced  | `none`    | Not paced: ticks as fast as possible.                                                   |
| passive   | `passive` | Has no animator. Ticked by its controller.                                              |

Parsing any other text than the four above MUST fail. What each mode does
after a tick is in [pacing.md §5](pacing.md#5-after-a-tick).

## 4. States

```
            start                        stop
 stopped ───────────▶ started ─────────────────────▶ stopping
    ▲                                                    │
    └────────── whoever ticks it winds it down ◀─────────┘
```

- `stop` on a stopped or stopping clock does nothing.
- `stop` on a started clock moves it to `stopping`. It is then wound down
  ([§9](#9-winding-down)):
  - a clock with an animator, by its animator, before its next tick;
  - a passive clock with a tick in progress, by that tick, as its last step
    ([§8](#8-tick));
  - a passive clock with no tick in progress, by the caller of `stop`, at
    once.
- A tick and the winding down of one clock are mutually exclusive
  ([§16](#16-concurrency)).
- A clock MAY be started again after it stopped. It gets a new streaming state,
  and activates the sources it held when it stopped ([§9](#9-winding-down)).

A stopped clock carries a **stop reason**, one of:

| Reason              | When                                                                                         |
| ------------------- | -------------------------------------------------------------------------------------------- |
| `never started`     | From creation until the first start.                                                         |
| `requested`         | `stop` was called.                                                                           |
| `no sources`        | The animator found nothing to process ([pacing.md §8](pacing.md#8-animator)).                |
| `global stop`       | [§12](#12-global-stop-and-shutdown).                                                         |
| `sync source ended` | The followed sync source ended the clock ([pacing.md §9](pacing.md#9-server-driven-clocks)). |
| `parent stopped`    | A sub-clock stopped with its parent, or at its last deregistration.                          |
| `failed`            | With the error. See [§11](#11-failure).                                                      |

When several hold, `failed` wins, then `global stop`, then the others in the
order above.

## 5. Registry

Two process-wide sets of **top-level** clocks: the clocks that have no
parent. A clock with a parent is never in either: it is reached through its
parent.

| Set     | Holds                                                   |
| ------- | ------------------------------------------------------- |
| waiting | Stopped top-level clocks.                               |
| running | Started and stopping top-level clocks, however started. |

- A top-level clock MUST be in exactly one of the two for as long as
  something other than the registry refers to it.
- The registry MUST NOT be what keeps a stopped clock alive: memory MUST NOT
  grow with the number of clocks created and discarded without being started.
- No set holds one clock twice, and no set holds a clock merged away by a
  unification ([unification.md §5](unification.md#5-guarantees)).

"The clocks of the application" is waiting + running.

## 6. Creation and start

**Creation.** A new clock is `stopped` with reason `never started`, with the
given sync mode (default automatic), id, stack and error handler. Creating a
passive clock without a controller, or a non-passive clock with one, MUST be
refused. A top-level clock joins `waiting`.

**Start scope.** Runs a function that may create clocks. When the function
returns, a start pass runs. When the function fails, no start pass runs: the
clocks it created stay in `waiting`, and the error is passed on. An error
MUST NOT have the side effect of starting clocks.

**Start pass.** For each non-passive clock in `waiting` that can start, and
whose stop reason is `never started` or `no sources`: start it. A start pass
also runs once when the application starts. A clock stopped for any other
reason starts again by an explicit start only, so that a stop holds.

- A pass MUST cost at most a time proportional to the number of waiting
  clocks.
- With nothing waiting it MUST cost a constant.

**Can start.** All of:

- the clock is stopped;
- the global stop is not set;
- the application has started, or the start is forced;
- the clock is passive, or has a pending output, or the start is forced.

**Explicit start.** `start` on a handle starts the clock if it can start and
fails with `cannot start` otherwise. It follows the same steps as a start
pass, registry included.

**Starting.** Every check comes before any effect: a start that fails MUST
leave the clock exactly as it was. In order:

1. Check that the clock can start.
2. Freeze the name ([§2](#2-names)).
3. Create the streaming state: 0 ticks, the frame duration, the pacing state
   anchored so that now is 0 ([pacing.md §4](pacing.md#4-switch)).
4. Set the state to `started`. A top-level clock moves from `waiting` to
   `running`.
5. Log the start ([observability.md §2](observability.md#2-log-events)).
6. Start each registered sub-clock that can start, with the same force flag.
7. Unless passive: choose and start the animator
   ([pacing.md §8](pacing.md#8-animator)). No source is tracked yet: the
   choice is revised at the first tick.

Starting, stopping and unifying one clock are mutually exclusive
([§16](#16-concurrency)).

## 7. Sources on a clock

**Attach** adds the source to the pending sources, unless already there.

**Activation** happens at the start of every tick, and on request by the
controller of a passive clock. Each pending source is removed and, under the
source error rule ([§11](#11-failure)):

- **active**: added to the active set; its sync source is tracked
  ([pacing.md §3](pacing.md#3-finding-the-sync-source));
- **output**: woken up with itself as the requester, which yields an
  activation; (activation, source) is appended to the outputs; its sync source
  is tracked;
- **passive**: added to the passive set.

The clock is the one holder of an output's activation that MUST NOT let go
while it runs: an output attached to a running clock stays awake for as long
as the clock runs.

After a batch, if the total number of activated sources crossed a multiple of
the leak threshold because of this batch, a warning MUST be logged
([observability.md §2](observability.md#2-log-events)).

**Detach** removes the source from the pending sources at once. If the clock
has streaming state, the **removal** below is queued and applied by whoever
ticks the clock, at the end of the tick in progress or at the start of the
next ([§8](#8-tick)), so that it never changes the lists a tick is reading.
Winding down applies it to everything. A source whose removal is queued MUST
NOT be animated again, the tick in progress included.

The removal: take the source's entries out of the outputs and put the source to
sleep once per activation taken; remove it from the active and passive sets;
drop its sync source entry. The lists MUST be changed
before the source is put to sleep, because putting a source to sleep detaches
its children, which re-enters the removal. Errors from putting a source to
sleep are logged and ignored.

The clock holds nothing of a source that left it, whatever the way: detach,
failure or wind-down. On a clock that keeps running, attaching and discarding
a source any number of times MUST leave the number of sources kept alive
unchanged.

## 8. Tick

A tick produces one frame, in three phases. A clock's ticks MUST run one at
a time.

**Prepare**, in order:

1. Apply the queued removals ([§7](#7-sources-on-a-clock)).
2. Activate pending sources ([§7](#7-sources-on-a-clock)).
3. The pacing point: apply the sync source changes, switch and change
   animator if due ([pacing.md §3](pacing.md#3-finding-the-sync-source)).
4. Prepare each started sub-clock ([§10](#10-sub-clocks)).

**Produce**, in order:

1. Animate each output in list order, then each active source, each under the
   source error rule ([§11](#11-failure)). The active set is read as a
   snapshot.
2. Run and drop the on-tick callbacks, with a stop check before each.
3. Stop check. Add one to ticks. Stop check.

**Cleanup**, in order:

1. Run and drop the after-tick callbacks, with a stop check before each.
2. Apply the queued removals.
3. Clean up each started sub-clock.

The tick then updates the statistics
([observability.md §1](observability.md#1-status-record)). A passive clock
that is `stopping` is wound down ([§9](#9-winding-down)) at the end of its
tick, also when the tick was abandoned.

A sub-clock is thus prepared and cleaned up once by each tick of its parent,
around the parent's produce phase, whether or not it produces. Its own
prepare or cleanup takes the place of a tick under the one-at-a-time rule,
fails like one and ends with the same wind-down. A sub-clock that is not
started is skipped.

Prepare and cleanup consume what they process, so running either again with
nothing new changes nothing. A sub-clock that its readers tick is prepared
and cleaned up by each of those ticks as well as by its parent.

What the clock does between this tick and the next (rest, lateness, release)
is not part of the tick: [pacing.md §5](pacing.md#5-after-a-tick).

**Stop check.** If the global stop is set, the tick is abandoned with the
**stop signal**. The stop signal is never reported as an error.

Ticking a handle whose clock is not started fails: with the stop signal if
the global stop is set, otherwise with `not running`. Registering an on-tick
or after-tick callback fails the same way on a stopped clock. On a stopping
clock the callback is accepted and dropped by the wind-down, so that a source
registering its next callback from inside a tick is unaffected by a stop.
Neither logs.

A tick asked of a clock while one of its ticks is in progress MUST be
refused.

A passive clock with a parent MUST only be ticked from inside a tick of its
parent, by a reader. When it has an owner, the owner
is its only reader. A passive clock without a parent is ticked by its owner.

## 9. Winding down

Run by whoever ticks the clock, between ticks, or by the caller of `stop` when
nothing ticks it ([§4](#4-states)). In order:

1. Take a snapshot of the registered sub-clocks.
2. Put each output to sleep with its activation. Errors are logged and
   ignored.
3. Move the sources still held to the pending sources: the outputs, the active
   set and the passive set, less the sources whose removal is queued. Drop the
   callbacks not yet run.
4. Stop each sub-clock of the snapshot that is started, with reason
   `parent stopped`, and wind it down.
5. Set the state to `stopped` with its reason. The streaming state is gone.
6. A top-level clock moves from `running` to `waiting`.
7. Log the stop.
8. If the reason is `failed`, report it ([§11](#11-failure)).

The snapshot in step 1 exists because putting an output to sleep can
deregister sub-clocks, and those MUST still be stopped.

The clock does not wake active or passive sources, so it does not put them to
sleep. A source the clock held is pending again, and a later start activates
it like a newly attached one. A source that was detached, or that detached
itself when put to sleep, stays out until it is attached again.

Winding down MUST complete whatever fails inside it.

## 10. Sub-clocks

A sub-clock is a passive clock prepared and cleaned up by each tick of its
parent, and ticked by its readers in between.

**Registration** is made by a registrant, on the sub-clock's parent, and is
counted: a sub-clock stays registered while at least one registrant holds it.
It MUST be refused with `not a sub-clock` unless the clock is passive and its
parent is the clock it is registered on. So a clock has at most one parent,
fixed at its creation, and no chain of parents can be changed into a cycle by
registration.

- A registration on a started parent starts the sub-clock if it is stopped
  and can start.
- A parent that starts starts its registered sub-clocks
  ([§6](#6-creation-and-start)).
- The last deregistration stops the sub-clock with reason `parent stopped`.
- A parent that winds down stops its registered sub-clocks
  ([§9](#9-winding-down)). Their registrations are kept.

Each tick of the parent prepares its started sub-clocks before it produces
and cleans them up after, at every depth ([§8](#8-tick)). Sources that joined
or left, sync source changes, the streaming cycles their sources opened to
answer a readiness check and a pending stop are thus settled within each
tick of the parent, whether or not a frame is produced. A sub-clock that is
not started is skipped, never an error.

A sub-clock is ticked only by its readers: the sources of the parent that
take what it produces. While the parent produces, a reader may tick it any
number of times, including none. Each of those is a full tick. A sub-clock
that no reader ticks produces nothing: its outputs, active sources and
on-tick callbacks wait, and its tick count stands.

A tick produces one frame and carries no time of its own: time enters only
through what paces a clock ([pacing.md](pacing.md)). A sub-clock is paced
through its readers alone, by the number of its ticks they ask for each tick
of its parent. That number is the reader's choice: one for one over time for
a reader that only buffers, more or fewer for a reader that skips or
stretches.

A source that follows a measure of time of its own, such as a live input,
produces one frame per tick whatever asked for the tick. Placing it under
readers whose ticks follow that measure is the script's responsibility.

After two registered sub-clocks of one parent are unified, the parent holds
one entry, with the registrants of both, prepared and cleaned up once per
tick.

On a parent where sub-clocks are registered and deregistered any number of
times, the number of entries MUST return to its starting value, and the cost
of a tick MUST depend only on the sub-clocks currently registered.

## 11. Failure

Clocks can fail. A failure has one outcome, whatever the animator and
whatever the kind of error.

**Source error rule.** Used for the activation and the animation of one
source, and for a sync error charged to it
([pacing.md §3](pacing.md#3-finding-the-sync-source)):

1. Stop check.
2. Run. On any error other than the stop signal:
   - log the failure with the source, the error and the backtrace;
   - detach the source ([§7](#7-sources-on-a-clock));
   - if the clock has error handlers, call each with the error and backtrace
     and carry on with the tick;
   - otherwise the clock fails with that error.

**Clock failure.** A clock fails when an error other than the stop signal
leaves its tick or what follows it: a source error with no handler, an error
from an error handler, from an on-tick or after-tick callback, from a rest,
or from the clock itself. Then:

1. The tick is abandoned.
2. The clock is wound down ([§9](#9-winding-down)) with reason `failed`
   and the error. It ends `stopped`, in `waiting` if it is top-level, its
   outputs asleep and its sub-clocks stopped.
3. The failure is logged once, with the clock, the error and the backtrace.
4. The failure is reported, once per failed clock, to the application's
   **clock failure policy**.

**Clock failure policy.** One per application, given the clock and the error.
The application installs it. Its policy MUST start an orderly shutdown of the
application ([§12](#12-global-stop-and-shutdown)). A script MAY replace it,
for instance to keep running without the failed clock. The policy is called
after the wind-down, possibly from inside a tick of another clock: it MUST NOT
wait for clocks to stop. The application's policy only asks for the shutdown,
which runs elsewhere.

The clock reports a failure to the policy until the global stop is set
([§12](#12-global-stop-and-shutdown)). A tick in progress when the global stop
is set may fail on a source that the shutdown puts out of service: that
failure is logged, and the exit status already asked for stands.

**Sub-clocks.** A failing sub-clock fails like any clock: it is wound down and
reported. Then:

- if it was being prepared or cleaned up by its parent ([§8](#8-tick)), the
  parent's tick carries on;
- if it was being ticked by a reader, the error is passed to the reader. It is
  then an error of that reader, a source of the parent, under the source
  error rule.

Later ticks of the parent skip it, and a later tick meets `not running`.

**Errors.** The content is binding, the wording is not:

| Error               | Number | Content                                                              |
| ------------------- | ------ | -------------------------------------------------------------------- |
| conflict            | 10     | The two clocks, and that a source cannot belong to two clocks.       |
| loop                | 11     | The two clocks, and that they are nested.                            |
| controller conflict | 16     | The two clocks and the controller of each.                           |
| sync error          | 17     | [pacing.md §3](pacing.md#3-finding-the-sync-source)                  |
| not running         | —      | The clock.                                                           |
| cannot start        | —      | The clock and which condition of [§6](#6-creation-and-start) failed. |
| not a sub-clock     | —      | The clock and the parent it was registered on.                       |
| stop signal         | —      | Never reported.                                                      |

## 12. Global stop and shutdown

**Global stop** is a process-wide flag. Once set, nothing starts, every tick
is abandoned at its next stop check, every rest and wait ends, and every
animator winds its clock down.

**Shutdown.** The global stop is the clocks' record that the shutdown is under
way. It MUST be set once the application has left its main loop and before
anything a tick reads from is torn down. A failure before that point is a
failure of the script and keeps its effect on the exit status.

1. Set the global stop.
2. Stop every clock in `running`.
3. Wait until `running` is empty or the shutdown wait
   ([§13](#13-parameters)) has elapsed.
4. If any remain, log each of them with what it was doing
   ([observability.md §2](observability.md#2-log-events)).

A clock MUST act on a stop within one tick: a rest and a release MUST
end early on a stop, whatever the animator. A call blocked in a device is
bounded by the device.

A passive clock with no parent is stopped at step 2 like any top-level clock,
and wound down per [§4](#4-states).

## 13. Parameters

Every limit names what it protects. The values of the first seven are fixed
with their names; the others are recommendations.

| Parameter             | Setting                     | Value   | Protects                                                                                                                                                                         |
| --------------------- | --------------------------- | ------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| latency               | `clock.latency`             | 0.1 s   | Wake-ups: how far ahead of its time source a paced clock produces before it rests.                                                                                               |
| maximum latency       | `clock.max_latency`         | 60 s    | Listeners: beyond it a late stream is reset instead of caught up.                                                                                                                |
| latency log period    | `clock.log_delay`           | 1 s     | The log: minimum time between two latency warnings of one clock.                                                                                                                 |
| latency log threshold | `clock.log_delay_threshold` | 0.2 s   | The log: lateness below it is ordinary jitter.                                                                                                                                   |
| time source           | `clock.preferred`           | `posix` | —. The default is part of some builds only: a build without it uses the built-in time source and says so. Any other unknown name falls back to the built-in one, with a warning. |
| clocks as tasks       | `clock.task`                | true    | —. When off, every clock is animated by a thread.                                                                                                                                |
| leak threshold        | `clock.leak_warning`        | 50      | Memory: source count multiple at which the leak warning is logged.                                                                                                               |
| time box              | `clock.time_box`            | 0.1 s   | Other clocks: the longest a clock keeps a worker while another clock waits for one, a tick's overrun aside.                                                                      |
| shutdown wait         | `clock.shutdown_wait`       | 10 s    | Shutdown: the longest the application waits for clocks that do not stop.                                                                                                         |
| thread lease          | `clock.thread_lease`        | 5 s     | Threads: a blocking source that returns within it is flapping, and its clock then keeps its thread this long after each drop ([pacing.md §8](pacing.md#8-animator)).             |

The first seven names and values are compatibility surface. The shutdown wait
MUST NOT be derived from the maximum latency.

A change to the latency, the maximum latency or the thread lease MUST take
effect by the clock's next tick. A change to any other MUST take effect by the
clock's next start.

## 14. Contracts the clock relies on

**Source.** The clock uses, and requires, only:

- an identity, an id and a stack;
- a type: passive, active or output;
- wake-up by a requester, yielding an activation, and sleep of an activation.
  An activation dropped without being handed back puts the source to sleep on
  its own: every holder but the clock may let go;
- for an active or output source: animate, and reset;
- its current sync source, read at a constant cost between cycles
  ([pacing.md §3](pacing.md#3-finding-the-sync-source));
- the ids of its current activations, for the reports
  ([observability.md §1](observability.md#1-status-record)).

**Time source.** [pacing.md §2](pacing.md#2-sync-source).

**Scheduler.** [pacing.md §10](pacing.md#10-what-the-clock-asks-of-the-scheduler).

**Application.** A started flag, a shutdown sequence the clock joins
([§12](#12-global-stop-and-shutdown)), and the clock failure policy
([§11](#11-failure)).

## 15. Child clocks

How operators use sub-clocks. This is the other half of the contract of
[§10](#10-sub-clocks); it binds the operators, not the clock.

An operator that reads from its child at a pace of its own (a crossfade, a
time stretch, an inline encoder, a resampler) gives the child a clock of its
own.

Everything below SHOULD be held by one reader component that such operators
use. An operator then declares only what is its own: whether it is the sole
reader, and its buffer limit.

**Set-up, when the operator is created:**

- it wraps its child in an output that lives in the child clock;
- it creates a passive clock whose parent is the operator's own clock, with
  no owner;
- it unifies that clock with the wrapped child's clock.

It MUST NOT register the sub-clock yet.

**While running:**

- on waking up it registers the child clock as a sub-clock, which starts it
  when the parent runs, creates its buffer and wakes the wrapped child;
- on going to sleep it deregisters it and flushes its buffer.

An operator created and never woken therefore costs its parent nothing, and
an operator created inside a running script produces data on its first cycle.

**Reading.** The operator reads from its buffer. While the buffer holds less
than a frame and the child is ready, it ticks the child clock, and the
wrapped child appends its frame to the buffer. A cycle served from the buffer
ticks nothing, so the child clock produces exactly the frames its readers
asked for.

**Shared child clocks.** Several operators may read from one child clock;
each is a registrant. A tick by any of them fills the buffer of all. A buffer
holding more than the child buffer limit is an error naming the operator that
fell behind, raised at the limit and not before. A reader holding a remainder
when its child ends still delivers it.

**Exclusive child clocks.** An operator that must be the only reader of its
child, such as a crossfade, makes itself the owner of its child clock, with
its own clock as the parent. Two such child clocks never unify
([unification.md §3](unification.md#3-plan)).

**Pacing.** A sync source inside a child clock paces that child clock's
ticks, not the operator's clock, which goes on pacing by its own means
([pacing.md §3](pacing.md#3-finding-the-sync-source)). An operator that
cannot work that way SHOULD refuse, at creation, a child that declares a
sync source or whose sync source can change at run time: "This source may
control its own latency and cannot be used with this operator" is the
content of the error. For this a
source declares whether its sync source is `static` or `dynamic`; an operator
is `dynamic` if any of its children is. The clock does not read this
declaration. Where it is computed once and remembered, two threads computing
it at the same moment MUST both get the right value.

## 16. Concurrency

- A clock's ticks run one at a time, on whatever ticks it.
- Everything that changes the streaming state is applied by whatever ticks the
  clock, or by the caller of `stop` when nothing does, between two steps that
  read it: sync source changes
  ([pacing.md §3](pacing.md#3-finding-the-sync-source)), removals
  ([§7](#7-sources-on-a-clock)), the animator change
  ([pacing.md §8](pacing.md#8-animator)), winding down.
- These MAY be called from any thread at any time, a tick in progress
  included: attach, detach, stop, register and deregister a sub-clock,
  register a callback, report a sync source change, read any figure or
  report, unify.
- Every state transition of a clock, a tick of a passive clock included, and
  every unification that names it are mutually exclusive. The exclusion is
  re-entrant: winding down, registering and creating operators start, stop
  and unify clocks from inside it. Reads are never blocked by it.
- A read returns a value the field held at some moment; several fields read
  together need not be from the same moment.
