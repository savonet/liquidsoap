# Pacing (Part A)

Who sets the pace of a clock, what the clock does between two ticks, when it
gives its worker back, and what animates it.

## 1. The rule

**A clock MUST NOT pace a stream that the sync source it follows paces.**

A source connected to a sound card controls its own latency. The clock learns
this from a declaration made by the sync source ([§2](#2-sync-source)), never
by inference from a time value. Every way a clock rests is derived from that
declaration, so no way of resting can bypass it.

Only an automatic clock follows a sync source ([§3](#3-finding-the-sync-source)).
A CPU clock paces whatever its sources do: that is what the script asked for.

## 2. Sync source

A **sync source** is what sets the pace of a stream by its own means. It is a
declaration with:

| Field           | Meaning                                                                                              |
| --------------- | ---------------------------------------------------------------------------------------------------- |
| identity        | Two sync sources are the same if and only if they have the same identity. Compared in constant time. |
| name            | Printable.                                                                                           |
| pacing          | `self-paced` or `timed`.                                                                             |
| time source     | `timed` only, optional. Default: the configured time source.                                         |
| latency         | `timed` only, optional. Default: the latency parameter.                                              |
| maximum latency | `timed` only, optional. Default: the maximum latency parameter.                                      |

Whoever creates a sync source decides what "the same pacer" is, by handing out
one identity per pacing entity: one per device stream, one per audio server.

**Pacing.**

- `self-paced`: the source paces the stream inside the tick, by blocking in
  its device until the device is ready. There is no time source.
- `timed`: the pace is read from a time source. The clock rests on it.

A sync source MUST NOT restate a default: it gives a time source, a latency or
a maximum latency only where its own differs.

**Time source.** Gives:

- **now**, in seconds. It MUST never go backwards and MUST advance.
- its **wait**, one of:
  - `timer`: for a time `t`, the real-time delay after which now will have
    reached `t`. Now advances with real time.
  - `blocking`: a call that returns when the stream for time `t` is due: when
    now has reached `t`, or earlier by a lead the time source states
    ([§9](#9-server-driven-clocks)). It MUST return early when asked to, and MAY end with the **ended** signal when what it
    waits on has gone.

A time source can be anchored: given an offset, now is `now − offset`.

**Blocking.** A sync source **blocks** if it is `self-paced` or if its time
source's wait is `blocking`.

**The kinds in use:**

| Kind                                          | Pacing       | Time source  | Wait     | Latency / maximum |
| --------------------------------------------- | ------------ | ------------ | -------- | ----------------- |
| device-paced (sound card, network input)      | `self-paced` | —            | —        | —                 |
| server-driven ([§9](#9-server-driven-clocks)) | `timed`      | the server's | blocking | its own           |
| generic (a source declared self-sync)         | `timed`      | default      | timer    | default           |

A generic sync source changes nothing to the pacing. It takes the clock's
single sync source slot.

## 3. Finding the sync source

**Reporting, by sources.** A source reports a sync source only while it is
really pacing: a device while its stream is open, a network input while it is
connected, each only if its own self-sync option is on. A sync source that is
not ready MUST NOT be reported: a clock whose only pacer is not producing
paces itself.

An operator's sync source is the single distinct one among its children that
are ready and that it currently reads, if any. Two distinct ones is a sync
error charged to the operator.

A report does not cross a child clock: an operator that reads its child
through a child clock ([clock.md §16](clock.md#16-child-clocks)) reports
nothing on its behalf. The child's sync source is tracked by the child
clock.

The report travels by notification. A source MUST notify its subscribers
whenever its answer changes, after its own state has changed, so that a
subscriber reading back sees the new state. The clock MUST NOT ask.

- With nothing changing, the work done per tick to know the sync source MUST
  be zero on the source side and constant on the clock side, whatever the
  size of the source graph. A source MUST NOT recompute its answer on every
  cycle.
- An answer depends on a source's own state, on which children it currently
  reads, on their readiness and on their answers. The rule "notify when any
  of these changes" SHOULD be held once, by what all sources share, and fed
  with those inputs, not written again by each operator. An operator that
  selects a child at run time then only declares its selection.
- A change check MUST cost a constant: sync sources are compared by identity
  ([§2](#2-sync-source)), by sources and by the clock alike.

**Tracking, by the clock.** In every sync mode, passive included, the clock
subscribes to each animated source when it activates it
([clock.md §7](clock.md#7-sources-on-a-clock)) and reads its current sync
source.

A change may be reported from any thread. It MUST be queued and applied by
whatever ticks the clock, at the **pacing point** of its next tick: after
activation, before any source is animated
([clock.md §8](clock.md#8-tick) step 4). The latest change of a source
replaces an earlier one not yet applied. No change may be lost.

At the pacing point, with every queued change and every source just
activated taken together:

1. For each source concerned, replace its entry, keyed by the source's
   identity, by its new sync source, or drop it if it has none.
2. While the entries hold more than one distinct sync source: this is a
   **sync error** charged, under the source error rule
   ([clock.md §12](clock.md#12-failure)), to the source whose change came
   last among those in conflict. It is detached and its entry dropped.
3. The **tracked** sync source is the single remaining one, or none.
4. If the clock's answer to "blocks" changed, tell the parent and change the
   animator if one is due ([§8](#8-animator)).
5. An automatic clock **follows** the tracked sync source: when it differs
   from the one followed, the clock switches ([§4](#4-switch)).

The whole batch is applied before the check, so that several sources moving
together from one sync source to another are not seen half-way.

The tracked sync source decides the animator in every mode. A clock that
follows a sync source is **self-sync**.

Rationale for one sync source per clock in every mode: two devices in one
clock each pace the same stream at their own rate, and one of them starves or
overflows whatever the clock's mode. The mode only says whether the clock
itself also paces.

A clock changes pacing within one tick of the change being reported.

**Sync error** (number 17) MUST carry: the clock's name, a hint that a
source's self-sync option may need turning off, each sync source with the source that
reports it, and for each of these sources up to three stack positions.

## 4. Switch

A switch makes the clock follow another sync source, or none. It happens at
the pacing point ([§3](#3-finding-the-sync-source)): the sources of that tick
are animated under the new pacing.

1. Log it ([observability.md §2](observability.md#2-log-events)).
2. Take the new time source, if the new pacing is `timed` or there is no sync
   source, and anchor it so that now equals the current stream time.

The latency and maximum latency **in effect** are those the followed sync
source gives; where it gives none, the current value of the parameter
([clock.md §14](clock.md#14-parameters)).

Stream time is continuous across a switch. Whatever advance or lateness had
accumulated is dropped, on purpose: lateness measured against one pacer means
nothing against another, and carrying it would start the new pacer with a
burst or a reset.

At start, the clock is in the state a switch to "none" leaves, with now
anchored at 0.

## 5. After a tick

What a clock does between two ticks is decided by its mode and by what it
follows:

| Clock                                           | Rest | Measures lateness | Warns, resets |
| ----------------------------------------------- | ---- | ----------------- | ------------- |
| passive                                         | no   | no                | no            |
| unsynced                                        | no   | no                | no            |
| CPU                                             | yes  | yes               | yes           |
| automatic, no sync source                       | yes  | yes               | yes           |
| automatic, following a `timed` sync source      | yes  | yes               | yes           |
| automatic, following a `self-paced` sync source | no   | no                | no            |

A clock following a `self-paced` sync source, and an unsynced clock, MUST go
from one tick straight to the next. They have no target and no lateness:
their lateness is "no value" ([clock.md §1](clock.md#1-entities)).

The time box ([§7](#7-time-box-and-release)) applies to every clock that holds
a scheduler worker, whatever its row above. A clock animated by a thread
holds none and has nothing to release; a passive clock is inside the time box
of the clock whose animator ticks it.

Order, after each tick: stop check; rest or lateness
([§6](#6-rest-and-lateness)); if the clock did not rest, the time box
([§7](#7-time-box-and-release)).

## 6. Rest and lateness

For a clock that rests. Let `now` be read on the clock's time source,
`target = ticks × frame duration`, `L` the latency and `M` the maximum latency
in effect ([§4](#4-switch)).

**Ahead** (`now < target`). If `target − now ≥ L`, the clock **rests until
`target`**. Otherwise it goes on to the next tick.

So a paced clock produces up to `L` of stream ahead of its time source, then
rests until the time source catches up.

**Rest until `target`.** The deadline is the absolute time `target` on the
time source. It MUST NOT be accumulated from relative delays: every rest
re-reads now and aims at the true target.

| Wait of the time source | Animator | The rest                                                                |
| ----------------------- | -------- | ----------------------------------------------------------------------- |
| `timer`                 | task     | Give the worker back until the delay the time source gives has elapsed. |
| `timer`                 | thread   | Sleep for that delay.                                                   |
| `blocking`              | thread   | Call the wait.                                                          |

A `blocking` wait never runs on a task ([§8](#8-animator)). A rest ends early
on a stop, on a thread as on a task. A rest ending with the **ended** signal stops the clock with reason
`sync source ended`; this is not a failure.

**Behind** (`now ≥ target`). Let `late = now − target`.

- If `late ≥ M`: **reset**. Log it, set ticks to
  `floor(now / frame duration)`, and reset every output and active source.
- Else if `late ≥` the latency log threshold and at least the latency log
  period has passed since the last warning: log a latency warning
  ([observability.md §2](observability.md#2-log-events)).

Then the clock goes on to the next tick, subject to the time box.

## 7. Time box and release

**A clock that holds a scheduler worker is always time-boxed.** Whatever its
sync mode, and whether or not it is late, it MUST give the worker back once it
has held it for the time box.

- The time box is one value for the pool, not a property of a clock.
- The time held is measured on real, monotonic time, never on the clock's time
  source, from the moment the clock was last given a worker.
- It is checked between ticks. A tick cannot be interrupted, so a clock holds
  a worker for at most the time box plus one tick.
- A rest ([§6](#6-rest-and-lateness)) and a wait
  ([clock.md §9](clock.md#9-waiting-inside-a-tick)) give the worker back too,
  and start a new time box.

**Release.** Giving the worker back at the end of a time box is a release.
**A released clock yields to other clocks, and to nothing ranked after
clocks.** It MUST resume once every clock that was ready at the instant of
the release has been given a turn (picked by a worker), and at once on a
stop. Consequences, each of which MUST hold:

- With no other clock ready, the clock resumes at once: a release costs
  nothing.
- A clock that becomes ready after the release does not delay the released
  clock: it is behind it in line.
- More late clocks than workers take turns. Each is given a worker again
  after at most one time box plus one tick per clock ahead of it.
- Work ranked after clocks runs only when no clock is ready, that is, while
  clocks rest.

Rationale: clocks are the application's reason to exist and run first. The
time box is there so that one clock catching up does not keep the others
from their worker. When every worker holds a clock that is catching up, the
application is already failing to keep real time, and running other work
would make it later.

The clocks that never rest by design do not hold a worker
([§8](#8-animator)), so a clock task that does not rest is a late one.

A tick that lasted longer than the time box is logged
([observability.md §2](observability.md#2-log-events)).

## 8. Animator

The animator is what runs a started, non-passive clock. There is exactly one
at any time.

**Choice.** A clock **blocks** if its tracked sync source blocks
([§2](#2-sync-source)), or if a started sub-clock blocks. Each clock holds
that one answer and tells its parent when it changes; a parent MUST NOT walk
its sub-clocks to find out, so the cost per tick is a constant when nothing
changes. The animator is:

- a **thread** of the clock's own, if the clock blocks, if it is unsynced, or
  if clocks as tasks is off ([clock.md §14](clock.md#14-parameters));
- a **scheduler task** otherwise.

Rationale: a task is right for a clock that rests most of the time. A call
that blocks in a device or in a library holds whatever runs it, and the
scheduler cannot take a worker back from it. An unsynced clock never rests,
and since a release yields to clocks only ([§7](#7-time-box-and-release)),
as a task it would keep all other work off its worker for as long as it
runs.

**Placement.** The threads of clocks that never rest SHOULD be spread over the
cores as evenly as the platform allows, so that several unsynced clocks run
in parallel instead of sharing one core. Where the runtime ties a thread to
an execution unit that runs one thread at a time, this means spreading them
over those units.

**Change.** The choice is made at start, from what is known then, and again
whenever the clock's answer to "blocks" changes. A change of animator happens
at the pacing point ([§3](#3-finding-the-sync-source)), before any source is
animated: the old animator ends, the new one continues the same tick and the
same loop, on the same streaming state. Ticks and stream time are continuous. It
is logged.

So no source can be refused by a clock for the way that clock is animated,
and nothing has to be declared before a clock starts.

**Thread lease.** A clock moves to a thread as soon as it blocks, and moves
back to a task when it stops blocking. A clock whose blocking source flaps
keeps its thread for a lease instead.

- A clock that stops blocking records the time, on real, monotonic time like
  the time box.
- A clock that blocks again within the thread lease
  ([clock.md §14](clock.md#14-parameters)) of that time has seen a **flap**.
  The thread it moves to is **leased**. The flap is logged, with the time the
  source stayed away
  ([observability.md §2](observability.md#2-log-events)).
- A leased clock that stops blocking keeps its thread for the thread lease
  and rests on it ([§6](#6-rest-and-lateness)). Pacing follows the sync
  source as usual: the lease only holds the animator. The start of each lease
  is logged.
- A leased clock that blocks again during its lease keeps its thread. The
  animator is unchanged.
- A leased clock whose lease has elapsed MUST move to a task at the pacing
  point of its next tick. The check costs a constant per tick.
- The lease ends with the thread. A clock that is back on a task starts over:
  its next thread is leased only if it sees a new flap.
- A clock starts without a lease.
- A thread lease of 0 turns the mechanism off: every clock moves back at the
  pacing point where it stops blocking.
- The lease applies to a clock that is on a thread because it blocked. An
  unsynced clock stays on its thread for its whole run, and so does every
  clock while clocks as tasks is off.

Consequences, each of which MUST hold:

- A clock whose blocking source leaves moves back to a task at once, for as
  long as it has seen no flap. A script that alternates between a playlist
  and a live input every few minutes sees each move happen at the switch.
- A clock whose blocking source leaves and returns within the thread lease
  moves twice for that first return, then keeps its thread through every
  drop shorter than the lease.
- A clock whose source stays away longer than the lease moves back to a task,
  and treats the next drop like a first one.

Rationale: a source that connects and drops every few seconds moves its clock
twice per drop, and each move to a thread creates one. The lease is a
mechanism for that degraded case: a thread earns it by a flap and keeps it
while the flapping lasts, and every other move happens at once. A resting thread uses no processing.

**Loop.**

```
while state is started and global stop is not set and there is something to process:
    tick
    what follows a tick (§5)
wind down
```

"Something to process" is: a pending source, an output, or an active source.
The stop signal ends the loop and winds down the same way. Any other error is
a clock failure ([clock.md §12](clock.md#12-failure)), which winds down too.
The loop never ends without winding down.

A clock task MUST NOT animate a source while the clock blocks. A blocking
sync source first reported in the middle of a tick may block a worker for the
rest of that tick, and no longer.

## 9. Server-driven clocks

An audio server that calls back at its own rhythm drives the clocks that use
it. This is a way to drive a clock in its own right: the server's callback,
not the clock, decides when each tick happens.

**Declaration.** One sync source per server, `timed`, with:

- a time source of its own, whose **now** is the stream time the server's
  callback has consumed so far, and whose wait is `blocking`;
- a latency and a maximum latency of its own. Recommended: 0 s and 0.5 s.
  The maximum protects the server from a clock that fell behind: beyond it
  the clock resets instead of catching up.

**Effect.** With a latency of 0, the rules of [§6](#6-rest-and-lateness) make
the clock produce one frame and then wait for the server: one tick per frame
the server consumes, no drift, the clock's stream time locked to the
server's.

**What the wait MUST guarantee:**

- It returns early enough for the frame that follows to be produced before
  the server's callback asks for it.
- Several clocks may wait on one server. Each waits on its own target; one
  clock's wait MUST NOT end or delay another's.
- Its lead is one server period.
- If the server stops before or during a wait, the wait ends with the
  **ended** signal and the clock stops with reason `sync source ended`,
  within one server period.
- It returns early on a stop.

**Animator.** The wait blocks in the server's library, so the clock is
animated by a thread for as long as it tracks this sync source
([§8](#8-animator)). A source of this kind MAY join a clock that is already
running.

**In a CPU or unsynced clock.** The clock does not follow the server. It still
tracks the sync source, so the single sync source rule and the animator rule
hold.

## 10. What the clock asks of the scheduler

Relied on as given:

- A pool of workers. A worker picks one task among those ready, by priority
  rank: server-like tasks, then clocks, then tasks that keep a worker busy,
  then tasks that may wait. Server-like tasks get a turn between any two
  other tasks.
- A clock task holds its worker from the moment it is picked until it gives
  it back. Nothing takes it back.
- A clock that gave its worker back resumes as a ready clock task, possibly on
  another worker.

Asked for:

| Operation                 | Meaning                                                                                                                              |
| ------------------------- | ------------------------------------------------------------------------------------------------------------------------------------ |
| rest                      | Give the worker back; ready again after a delay. Used by [§6](#6-rest-and-lateness).                                                 |
| release                   | Give the worker back; ready again per [§7](#7-time-box-and-release).                                                                 |
| wait                      | Give the worker back; ready again when a condition holds ([clock.md §9](clock.md#9-waiting-inside-a-tick)).                          |
| interrupt                 | Make a resting, released or waiting clock ready at once. Used by a stop.                                                             |
| time since given a worker | Real, monotonic. Used by [§7](#7-time-box-and-release).                                                                              |
| clock thread              | A thread for a clock that cannot be a task. Its placement ([§8](#8-animator)) is the scheduler's: it is the one that sees the cores. |

The scheduler is the one that knows which other clocks wait on the pool. The
clock only says that its time box has elapsed; whether the release costs the
clock anything is the scheduler's answer.

How long a clock then waited for a worker is measured by the clock, from the
instant it became ready: the deadline of a rest, the moment the condition of
a wait was signalled. After a release the clock is ready at once, so the whole
span until it is given a worker counts as time released
([observability.md §1](observability.md#1-status-record)).
