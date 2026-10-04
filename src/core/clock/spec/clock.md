# Clocks — as built (Part A)

## 1. Entities

**Clock.** Fields that exist for the clock's whole life:

| Field           | Meaning                                                                                                 |
| --------------- | ------------------------------------------------------------------------------------------------------- |
| identity        | A number unique in the process, increasing with creation order. Used to compare and deduplicate clocks. |
| id              | Optional name. See [§2](#2-names).                                                                      |
| sync mode       | One of `auto`, `cpu`, `none`, `passive`. Fixed at creation.                                             |
| controller      | `none`, another clock, or a named external entity (a kind string plus an object with an id).            |
| stack           | Script positions, for error reports. Set once: a second set is ignored.                                 |
| state           | `stopped`, `started` or `stopping`.                                                                     |
| pending sources | Sources attached but not yet activated. No duplicates.                                                  |
| sub-clocks      | Handles of the clocks this clock ticks. No two designate the same clock.                                |
| needs thread    | Whether a source required a thread animator.                                                            |
| error handlers  | Callbacks `(error, backtrace)`.                                                                         |

**Streaming state.** Exists only while the clock is `started` or `stopping`,
and is created anew at every start:

| Field                | Meaning                                                                                      |
| -------------------- | -------------------------------------------------------------------------------------------- |
| ticks                | Number of ticks completed. Starts at 0.                                                      |
| frame duration       | Seconds of stream per tick. Read once at start from the frame settings.                      |
| time source          | Where "now" comes from. Anchored so that now = 0 at tick 0.                                  |
| current sync source  | The sync source that paces the clock, with its latency and maximum latency.                  |
| sync source entries  | One entry per source that currently declares a sync source: source name, sync source, stack. |
| outputs              | Ordered list of (activation, source).                                                        |
| active sources       | Set, weakly held.                                                                            |
| passive sources      | Set, weakly held.                                                                            |
| sync deregistrations | One callback per animated source, undoing its change subscription.                           |
| on-tick callbacks    | One-shot.                                                                                    |
| after-tick callbacks | One-shot.                                                                                    |
| pulled               | Whether the tick in progress was requested by a reader.                                      |
| animator             | `none`, `thread` or `task`.                                                                  |
| catch-up ticks       | Ticks taken in a row without resting.                                                        |
| last latency log     | Time of the last latency warning. Starts at the clock's start.                               |

"Weakly held" means the set does not keep a source alive: a source nothing
else references disappears from it.

## 2. Names

A clock's name is, in order: its id if set; else the id of its most significant
pending source (outputs first, then active, then passive; first in attachment
order within a type); else `generic`.

Setting an id passes the wanted name through a generator that makes it unique
among clock ids. Setting the id a clock already has does nothing. At start, the
clock's name at that moment is frozen as its id.

The description string is `clock(id=<name>,sync=<mode>)`. For a stopped clock
`<mode>` is `stopped` and the string ends `,pending=<sync mode>)` instead.

## 3. Sync modes

| Mode      | Text      | Meaning                                                                          |
| --------- | --------- | -------------------------------------------------------------------------------- |
| automatic | `auto`    | Paced by its sync source when it has one, by the time source otherwise. Default. |
| CPU       | `cpu`     | Always paced by the time source.                                                 |
| unsynced  | `none`    | Not paced: ticks as fast as possible.                                            |
| passive   | `passive` | Has no animator. Ticked by whoever controls it.                                  |

The reported mode of a clock also takes the values `stopped` and `stopping`,
which replace the mode while the clock is in those states. Parsing any other
text than the four above fails.

## 4. States

```
            start                       stop (non-passive)
 stopped ───────────▶ started ─────────────────────────▶ stopping
    ▲                    │                                   │
    │   stop (passive)   │                                   │
    └────────────────────┘                                   │
    └──────────── animator sees it, winds down ◀─────────────┘
```

- `stop` on a stopped or stopping clock does nothing.
- `stop` on a started passive clock winds it down at once, in the caller
  ([§11](#11-winding-down)), keeping its controller.
- `stop` on any other started clock only moves it to `stopping`. Its animator
  notices before its next tick and winds it down, clearing the controller.

A stopped clock reports 0 ticks, no sources other than pending ones, not
self-sync, and stream time −1.

## 5. Registry

Process-wide sets of handles:

| Set      | Holds                                                                                               | Strength |
| -------- | --------------------------------------------------------------------------------------------------- | -------- |
| all      | Every clock created.                                                                                | weak     |
| retained | Clocks created outside a collecting scope ([§6](#6-creation-and-start)), until the next start pass. | strong   |
| pending  | Clocks that could not start, and clocks that stopped.                                               | weak     |
| started  | Clocks started by a start pass. Removed when they stop.                                             | strong   |

"The clocks of the application" is retained + pending + started, deduplicated
by clock identity.

## 6. Creation and start

**Creation.** A new clock is `stopped`, with the given sync mode (default
automatic), controller (default none), id, stack and error handler. It joins
the `all` set. Unless passive, it is announced: inside a collecting scope it is
added to that scope's list; outside any, it goes to `retained`.

**Collecting scope.** Runs a function, collecting the clocks announced during
it. When the function ends, normally or by an error, a start pass runs over the
collected clocks in creation order, unless the global stop is set or there is
nothing collected and nothing retained or pending.

**Start pass.** Takes the given clocks plus everything in `retained` and
`pending` (both emptied), deduplicated by identity. For each one that is
stopped:

- if it can start and is not passive, it is started and added to `started`;
- if it can start and is passive, nothing happens;
- otherwise it goes back to `pending`.

A start pass also runs once when the application starts, after setting the
"application started" flag.

**Can start.** All of:

- the clock is stopped;
- the global stop is not set;
- the application has started, or the start is forced;
- the clock is passive, or has a pending output, or the start is forced.

**Explicit start.** `start` on a handle starts the clock if it can start. It
does not add it to `started`.

**Starting.** In order:

1. Freeze the name ([§2](#2-names)).
2. Log `Starting <top-level|passive> clock <id>[ controlled by <controller>] with sources: <list>[ and sync: <mode>]`.
   The source list is `id (type)` per pending source. `and sync` is omitted for
   a passive clock.
3. Anchor the time source at now.
4. Create the streaming state.
5. Start each sub-clock through explicit start, with the same force flag.
6. Set the state to `started`.
7. Unless passive: start the animator ([§10](#10-animator)). If the controller
   was `none`, set it to an external entity of kind `task` or `thread`.
   Otherwise fail with "invalid state".

## 7. Attaching sources

**Attach** adds the source to the pending sources, unless already there.

**Activation** happens at the start of every tick, and on request (used for a
passive clock before its controller ticks it). If there are pending sources,
each is removed and, under the error rule of [§13](#13-failure-behaviour):

- **active**: added to the active set; sync tracking is set up ([§8](#8-sync-sources));
- **output**: woken up with itself as the requester, which yields an
  activation; (activation, source) is appended to the outputs; sync tracking is
  set up;
- **passive**: added to the passive set.

After a batch, if the total number of activated sources crossed a multiple of
the leak threshold because of this batch, a warning is logged with the total
and the source report ([reports.md §1](reports.md#1-source-list)).

**Detach** removes the source from the pending sources at once. Then:

- stopped clock: nothing more;
- stopping clock: the removal below runs at once;
- started clock: the removal is queued as an after-tick callback, so that it
  never changes the lists a tick is reading.

The removal: take the source's entries out of the outputs and put the source to
sleep once per activation taken; remove it from the active and passive sets;
run and drop its sync deregistrations, ignoring their errors. The lists are
changed first and the callbacks run after, because putting a source to sleep
detaches its children, which re-enters the removal.

## 8. Sync sources

A sync source carries a printable name, a time source, a latency and a maximum
latency. Two sync sources are the same if they compare equal by value.

**A source's sync declaration** is a pair: `static` or `dynamic`, and an
optional sync source.

**For an operator over children:**

- its declaration is `dynamic` if any child's is, else `static`; this is
  computed once and remembered;
- its sync source is the single distinct sync source among the children that
  are ready, if any. Two distinct ones among ready children is a sync error
  naming the operator.

**Tracking on a clock.** When an animated source is activated, the clock
subscribes to its sync source changes, keeps the unsubscribe callback, and
records its current sync source. On every change, and at activation:

1. Drop the entry with that source's name; add a new one if a sync source is
   given.
2. Deduplicate the entries by sync source. More than one distinct sync source
   is a sync error naming the clock; the entries are left as they were.
3. The clock's sync source is the single remaining one, or none.
4. If it differs from the current one and the clock is automatic, switch.

**Switch.** Log one of `Switching to self-sync mode (<name>)`,
`Switching to non-self-sync mode`, `Switching self-sync source to <name>`. Set
the current sync source with its latency and maximum latency (the configured
values where it gives none). Take the new time source (the sync source's, or
the configured one when there is none) and anchor it so that now equals the
current stream time. Whatever advance or latency had accumulated is dropped.

In the other sync modes, steps 1–3 still apply and step 4 does not.

**Stated intent.** The interface documents this as: a clock with a current sync
source "delegates latency control to it". The clock has no separate code path
for a self-sync clock. The delegation is carried entirely by the time source
the sync source supplies: latency control ([§9.1](#91-latency-control)) runs
unchanged, on that time source. A sync source that paces the stream by itself
supplies the unconstrained time source ([§17](#17-contracts-the-clock-relies-on)),
on which the clock is never behind and sleeping returns at once. A sync source
may instead supply a real time source and so keep the clock's own pacing.

**The three kinds of sync source in use** ([§18](#18-how-the-clock-is-used)):

| Kind                                                   | Time source        | Latency / maximum | Effect on the clock                                                                        |
| ------------------------------------------------------ | ------------------ | ----------------- | ------------------------------------------------------------------------------------------ |
| device-paced (sound cards, network input)              | unconstrained      | configured values | The clock does no pacing. The device's blocking call paces.                                |
| server-driven (an audio server with its own callback)  | the server's       | 0 s / 0.5 s       | The clock's own pacing runs, on the server's time: the server's callback drives the clock. |
| generic (a source that only declares itself self-sync) | the configured one | configured values | Pacing is unchanged. The source only takes the clock's single sync source slot.            |

**Sync error text** (error number 17):

```
<name> has multiple synchronization sources. Do you need to set self_sync=false?

Sync sources:
 <sync source> from source <source name>
 ...

Stack traces:
<name>:
<up to 3 positions, or " Unknown position">

<source name>:
...
```

## 9. Tick

A tick may be requested as a **pull** (a reader wants data) or not (the clock
is merely animated). One tick, in order:

1. Set `pulled` to whether this is a pull.
2. Record, for each sub-clock, its tick count.
3. Activate pending sources ([§7](#7-attaching-sources)).
4. Animate each output in list order, then each active source, each under the
   error rule ([§13](#13-failure-behaviour)). The active set is read as a
   snapshot.
5. Run and drop the on-tick callbacks, checking the global stop before each.
6. Set `pulled` to false. Check the global stop.
7. For each sub-clock recorded in step 2 whose tick count has not changed
   since, tick it (not a pull). One whose count changed was already ticked
   during step 4 or 5 and is left alone.
8. Add one to ticks. Check the global stop.
9. Run and drop the after-tick callbacks, checking the global stop before each.
10. Latency control ([§9.1](#91-latency-control)).
11. Check the global stop.

"Check the global stop" means: if it is set, the tick is abandoned with the
"has stopped" signal.

Ticking a handle whose clock is stopped fails: with "has stopped" if the global
stop is set, otherwise with "invalid state" after a critical log line
`Clock <id> has invalid state: stopped`. Registering an on-tick or after-tick
callback on a stopped clock fails the same way.

### 9.1 Latency control

Let `end` be now on the clock's time source, and `target` = ticks × frame
duration. Let `L` be the latency of the current sync source, or the configured
latency without one.

| Mode           | `end < target` (ahead)     | otherwise (behind)    |
| -------------- | -------------------------- | --------------------- |
| passive        | nothing                    | nothing               |
| unsynced       | yield                      | yield                 |
| automatic, CPU | reset catch-up ticks; rest | handle latency; yield |

**Rest.** Only if `target − end ≥ L`. Compute `delay = target − now`. If the
delay is positive and the clock can park ([§10](#10-animator)), park for
`delay`. Otherwise sleep on the time source until `target`.

So a paced clock produces up to `L` of stream ahead of real time, then rests
until real time catches up.

**Handle latency.** Let `late = end − target` and `M` the maximum latency of
the current sync source, or the configured one.

- If `late ≥ M`: log `Too much latency! Resetting active sources...`, set ticks
  to `floor(end / frame duration)`, and reset every output and active source.
- Else if `late ≥` the log threshold and at least the log period has passed
  since the last warning: log
  `Latency is too high: we must catchup <late, 2 decimals> seconds! ...` and
  record the time.

**Yield.** Add one to catch-up ticks. If it has reached
`max(1, floor(L / frame duration))`, reset it to 0 and park for no delay if the
clock can park.

## 10. Animator

A started, non-passive clock has one animator, chosen at start and never
changed:

- **task** if the "clocks as tasks" setting is on and no source required a
  thread;
- **thread** otherwise.

**Loop.** Both run the same loop:

```
while state is started and global stop is not set and there is something to process:
    tick
wind down, clearing the controller
```

"Something to process" is: a pending source, an output, or an active source.
The "has stopped" signal raised inside a tick ends the loop and winds down the
same way. Any other error leaves the loop without winding down.

Before winding down it logs `Clock has stopped: <reasons>.`, the reasons being
those that hold among `clock stopped`, `global stop`,
`no more sources to process`.

**Task.** One scheduler task of the clock priority, ready at once. Its handler
runs the whole loop and returns no follow-up task. It logs
`Clock task is starting`. Its controller id is `task-<identity>`.

**Thread.** A thread named `Clock <id>`. It logs `Clock thread is starting`.
Its controller id is the thread's number.

**Parking.** Only a task animator can park, and only from inside its handler.
Parking suspends the loop where it is, gives the scheduler worker back, and
queues the continuation as a task of the clock priority, ready after the given
delay. A thread animator, a passive clock, and a tick driven from outside the
task cannot park.

**Requiring a thread.** A source may require it before the clock starts. On a
clock that is not stopped this fails with the animator conflict (error number
18), text: `Clock <description> has already started as a scheduler task. A
source that rests by waiting on an external server, such as JACK input or
output, needs a clock animated by a thread of its own and cannot join a clock
that is already running.`

### 10.1 What the clock relies on from the scheduler

- Tasks have a priority; a worker picks, among ready tasks, by priority order.
  The order is: server-like tasks, clocks, tasks that keep a worker busy, tasks
  that may wait.
- A clock task occupies its worker from the moment it is picked until it parks
  or ends.
- A parked clock is picked again like any ready clock task, possibly by another
  worker.

## 11. Winding down

1. Take a snapshot of the sub-clocks.
2. Put each output to sleep with its activation, ignoring errors.
3. Run and drop every sync deregistration, ignoring errors.
4. Stop each sub-clock from the snapshot.
5. If asked, set the controller to `none`.
6. Set the state to `stopped`. The streaming state is gone.
7. Remove the clock from `started` and add it to `pending`.
8. Log `Clock stopped`.

The snapshot in step 1 exists because putting an output to sleep can remove
sub-clocks from the clock, and those must still be stopped.

Active and passive sources are not notified.

## 12. Global stop and shutdown

**Global stop** is a process-wide flag. Once set, nothing starts, every tick
is abandoned at its next check, and every animator winds down.

**Shutdown**, before the rest of the application shuts down:

1. Set the global stop.
2. Stop every non-passive clock in `started`.
3. Poll every 10 ms until `started` is empty or the maximum-latency setting
   has elapsed.
4. If any remain, log `Clocks still running at shutdown: <ids>`.

## 13. Failure behaviour

**Source error rule.** Used for activation and animation of one source:

1. Check the global stop.
2. Run. On any error other than "has stopped":
   - log `Source <id> failed while streaming: <error>!` with the backtrace;
   - detach the source ([§7](#7-attaching-sources));
   - if the clock has error handlers, call each with the error and backtrace
     and carry on with the tick;
   - otherwise let the error out of the tick.

**Errors and their text:**

| Error               | Number | Text                                                                                                            |
| ------------------- | ------ | --------------------------------------------------------------------------------------------------------------- |
| conflict            | 10     | `A source cannot belong to two clocks (<a>, <b>).`                                                              |
| loop                | 11     | `Cannot unify two nested clocks (<a>, <b>).`                                                                    |
| controller conflict | 16     | `Cannot unify clocks <l> and <r>: clock <l> is controlled by clock <lc> while clock <r> is controlled by <rc>.` |
| sync error          | 17     | [§8](#8-sync-sources)                                                                                           |
| animator conflict   | 18     | [§10](#10-animator)                                                                                             |
| invalid state       | —      | none                                                                                                            |
| has stopped         | —      | none; a signal, never reported                                                                                  |

## 14. Unification

`unify(a, b)` makes two handles designate one clock.

1. **Controllers must be compatible**, else controller conflict:
   - either is `none`: compatible;
   - both external: same kind and the same object;
   - both clocks: compatible if those two clocks unify. They are unified as
     part of the test;
   - one clock, one external: not compatible.
2. If both handles already designate the same clock: done.
3. Pick the direction. "Pending mode" of a clock is its sync mode, or
   `stopping` while it is stopping.
   - both stopped with the same sync mode: merge `a` into `b`;
   - `a` stopped, and `a` is automatic or its mode equals `b`'s pending mode:
     merge `a` into `b`;
   - `b` stopped, and `b` is automatic or its mode equals `a`'s pending mode:
     merge `b` into `a`;
   - otherwise: conflict.

   The clock merged away is always a stopped one.

**Merge `x` into `y`:**

1. Fail with loop if `y` is a sub-clock of `x` at any depth, or `x` of `y`.
2. If `y` has no controller, it takes `x`'s.
3. Move `x`'s pending sources to `y`.
4. Move `x`'s sub-clocks to `y`.
5. If `x` needs a thread, require it of `y` ([§10](#10-animator)).
6. Move `x`'s error handlers to `y`.
7. Id: if `y` has none it takes `x`'s; if both have one `y` keeps its own and
   the event is logged.
8. Point `x`'s handle at `y`.
9. Deduplicate the sub-clock list of `y` and of every clock in the `all` set.
10. Remove `x`'s handle from every registry set.

`y` keeps its sync mode, state and streaming state.

## 15. Sub-clocks

Registering a sub-clock adds its handle unless one already designates the same
clock. Deregistering removes every handle designating it. Neither starts nor
stops anything.

See [§18.3](#183-child-clocks) for who registers sub-clocks.

A sub-clock is started when its parent starts ([§6](#6-creation-and-start)),
ticked by its parent's tick unless already ticked during it
([§9](#9-tick)), and stopped when its parent winds down
([§11](#11-winding-down)).

## 16. Constants

| Name                        | Value   | Use                                                               |
| --------------------------- | ------- | ----------------------------------------------------------------- |
| `clock.latency`             | 0.1 s   | Advance at which a paced clock rests; size of a catch-up turn.    |
| `clock.max_latency`         | 60 s    | Latency at which sources are reset. Also the shutdown wait.       |
| `clock.log_delay`           | 1 s     | Minimum time between latency warnings.                            |
| `clock.log_delay_threshold` | 0.2 s   | Latency below which no warning is logged.                         |
| `clock.preferred`           | `posix` | Time source name. An unknown name falls back to the built-in one. |
| `clock.task`                | true    | Animate clocks as scheduler tasks.                                |
| `clock.leak_warning`        | 50      | Source count multiple at which the leak warning is logged.        |
| shutdown poll               | 10 ms   | [§12](#12-global-stop-and-shutdown)                               |

The log and latency settings are read when a clock starts, except the latency
and maximum latency, which are read each time they are used.

## 17. Contracts the clock relies on

**Source.** Has an id and a stack; a type (passive, active, output); a sync
declaration; whether it is ready; its current sync source; a subscription to
sync source changes returning an unsubscribe callback; wake-up (by a requester,
yielding an activation) and sleep (of an activation); its list of activations.
An active or output source can be reset and animated ("output").

**Time source.** Gives now; sleeps until a time; converts to and from seconds;
adds, subtracts and compares times. It can be wrapped with an offset: now is
then `now − offset` and sleeping until `t` sleeps until `t + offset`.

One time source is provided here, the **unconstrained** one: now is always 0
and sleeping returns at once. It is for sync sources that pace the stream
entirely by themselves.

A time source's sleep may end the clock: if it raises the "has stopped" signal,
the animator loop winds the clock down ([§10](#10-animator)).

## 18. How the clock is used

The clock's rules above only make sense with what its users do. These are not
clock code; they are the other half of each contract.

### 18.1 Sync source reporting by sources

A source reports its sync source only while it is really pacing: a sound card
source while its stream is open, a network input while it is connected, each
only if its own "self-sync" option is on. Otherwise it reports none.

The report travels up the source graph by notification, not by the clock
asking:

- every source keeps its last reported sync source and a list of subscribers;
- a source notifies its subscribers when the value changes, compared by
  identity;
- an operator subscribes to its children when it wakes up and unsubscribes when
  it sleeps; on a child's change it recomputes its own value from its ready
  children ([§8](#8-sync-sources));
- going to sleep notifies "none";
- an operator that selects among children at run time notifies explicitly when
  its selection changes, since the selected child is not among its fixed
  children;
- a source whose pacing depends on a connection notifies on connect and on
  disconnect;
- before each streaming cycle a source also recomputes its value and notifies
  if it changed.

The clock subscribes only to its outputs and active sources
([§7](#7-attaching-sources)).

### 18.2 Server-driven clocks

An audio server that calls back at its own rhythm is given one sync source per
server. Its time source is backed by the server:

- **now** is the time the server's callback has counted so far;
- **sleep until `t`** blocks until the server's callback has reached `t`, and
  raises the "has stopped" signal if the server stopped before or during the
  wait.

Its latency is 0 and its maximum latency 0.5 s. With these, the ordinary
latency control makes the clock produce one frame and then wait for the
server's callback: the whole clock follows the server. Several clocks may wait
on one server.

Because that sleep blocks in a foreign call, sources of this kind require a
thread animator ([§10](#10-animator)). If the server stops, the clock stops.

### 18.3 Child clocks

An operator that reads from its child at a pace of its own (a crossfade, a
time stretch, an inline encoder, a resampler) gives the child a clock of its
own.

**Set-up, when the operator is created:**

- it wraps its child in an output that lives in the child clock;
- it creates a passive clock whose controller is, by default, the operator's
  own clock;
- it unifies that clock with the wrapped child's clock;
- it registers the child clock as a sub-clock of its own clock, at once.

**While running:**

- on waking up it registers the sub-clock again (a no-op the first time),
  creates its buffer and wakes the wrapped child;
- before each streaming cycle it starts the child clock if it is not started
  and activates its pending sources;
- on going to sleep it deregisters the sub-clock, puts the wrapped child to
  sleep and flushes it.

**Reading.** The operator reads from its buffer. While the buffer holds less
than a frame and the child is ready, it ticks the child clock **as a pull**.
On a pull, the wrapped child appends its frame to the buffer; on a tick that
is not a pull, it does nothing. The parent's tick still reaches the child
clock when no reader pulled during it ([§9](#9-tick) step 7), which keeps real
outputs and active sources inside the child clock running without buffering
data nobody asked for.

**Shared child clocks.** Several operators may read from one child clock. A
pull by any of them fills the buffer of all. A buffer holding more than the
child buffer limit (10 s by default) is an error naming the operator that fell
behind.

**Exclusive child clocks.** A crossfade's child clock is controlled by the
crossfade itself, as an external entity, not by the crossfade's clock. Two
such child clocks therefore never unify
([§14](#14-unification)): the crossfade needs to be the only reader of its
child.

**Pacing.** Such an operator may ask for a check at creation, which refuses a
child that declares a sync source or a dynamic sync type: "This source may control its own latency
and cannot be used with this operator."

So two child clocks with the default controller can only be unified if the
clocks controlling them can ([§14](#14-unification)).
