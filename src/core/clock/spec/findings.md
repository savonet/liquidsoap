# Findings

Read from the clock code at `ab89262b0`. Each finding is marked **checked**
(observed in a run) or **read only** (from reading; a suspect until someone
tries to refute it). Section numbers refer to [clock.md](clock.md).

## Defects

### D1. A clock paced by a sound card crawls when animated as a task — checked

The code contradicts its own stated rule: a self-sync clock delegates latency
control to its sync source ([clock.md §8](clock.md#8-sync-sources)). The rule
is not written as a rule anywhere in the clock; it holds only as a side effect
of the time source, and the task animator's rest does not go through the time
source.

Sync sources that pace the stream themselves use the unconstrained time
source: now never advances (§17). After a switch to such a sync source, the
stream time is therefore always ahead of now, by the amount produced since the
switch. Once that reaches the latency, the clock rests (§9.1): it computes
`delay = target − now`, which is positive and grows by one frame at every
tick, and a task animator parks for it. The rule assumed the rest would be a
sleep on the time source, which for this time source returns at once.

Observed with a PulseAudio output and a silent source, 8 s of real time:

| Animator       | Stream produced |
| -------------- | --------------- |
| task (default) | 0.6 s           |
| thread         | 8.0 s           |

0.6 s is what the mechanism predicts: parks of 0.10, 0.12, 0.14 … s add up to
8 s after about 28 ticks.

Seven sync sources use this time source (ALSA, AO, NDI, OSS, PortAudio,
PulseAudio, SRT). Only JACK requires a thread.

### D2. A clock task that never rests keeps lower-priority tasks from running — checked

An unsynced clock, and a paced clock that stays late, never rest. They yield
every `latency / frame duration` ticks (§9.1), but the yield queues the clock
again at the clock priority, ready at once, and the scheduler picks by
priority (§10.1). Tasks ranked after clocks are never picked while there are
as many such clocks as workers.

Observed: a task due after 1 s had not run after 16 s, with one unsynced clock
on one core, and with two unsynced clocks on two cores. It ran when the clocks
stopped. One unsynced clock on two cores is fine. Yielding after every tick
instead changes nothing.

### D3. The yield is counted in ticks, not in time — read only

The catch-up turn is a number of ticks (§9.1). A tick's real duration is not
bounded, so the time a clock holds its worker between two yields is not
bounded either.

### D4. An error that leaves a tick leaves the clock started with no animator — read only

When a source fails and the clock has no error handler, the error leaves the
tick (§13) and the animator loop without winding down (§10). The clock stays
`started`, in the `started` set, with its controller set and its outputs
awake. Nothing ticks it again, and shutdown waits the full 60 s for it (§12).

The same holds for "invalid state" raised from inside a tick (D5).

### D5. A stopped sub-clock makes its parent's tick fail — read only

In the tick (§9 step 7) a stopped sub-clock reports 0 ticks before and after,
so it is ticked; ticking a stopped clock fails with "invalid state". Sub-clocks
are started when their parent starts, but registering one on a started parent
starts nothing (§15), and a sub-clock can be stopped on its own.

### D6. Unification can fail half-way — read only

- Two clock controllers are unified as part of the compatibility test (§14
  step 1), before the direction is chosen. If the clocks themselves then
  conflict, their controllers stay unified.
- In a merge, requiring a thread of a clock that is not stopped fails (step 5)
  after pending sources and sub-clocks were already moved (steps 3–4).

### D7. Starting a non-passive clock that has a controller starts it, then fails — read only

The controller check comes last (§6 step 7): by then the state is `started`
and the animator is running. The caller gets "invalid state" for a clock that
is ticking.

### D8. Reading the time of a stopped clock logs a critical error — read only

Stream time of a stopped clock is −1 (§4), obtained by catching "invalid
state", which is logged as critical before it is raised (§9). The clock dump
reads the time of every clock, stopped ones included. During shutdown the same
read raises "has stopped" instead, which is not caught.

## Asymmetries

### A1. Stopped clocks report 0 ticks but time −1 — read only

Two "no value" conventions for two figures of the same clock (§4).

### A2. Only automatic clocks follow a sync source, but every clock rejects two — read only

A CPU or unsynced clock ignores sync sources for pacing, yet raises the sync
error when two are present (§8).

### A3. Passive clocks are "outside the registry" but are put in it when they stop — read only

Winding down adds every clock to `pending` (§11 step 7), passive ones too. The
next start pass takes it out and neither starts nor keeps it (§6). Until then
it is listed among the clocks of the application.

### A4. Explicit start does not register the clock — read only

Only the start pass adds to `started` (§6). A clock started explicitly is not
stopped by the shutdown sequence and is not waited for (§12); it ends through
the global stop alone. Four callers start clocks this way.

### A5. Outputs are put to sleep at wind-down, active sources are not told — read only

§11. Active sources are not woken by the clock either, so this may be
consistent; nothing states it.

### A6. Sources compare sync sources by identity, the clock by value — read only

A source decides whether its sync source changed by identity
([clock.md §18.1](clock.md#181-sync-source-reporting-by-sources)); the clock
decides whether two sync sources are the same by value (§8). Two equal but
distinct values would be a change for the source and the same pacer for the
clock.

### A7. Registering a sub-clock checks nothing — read only

History says registration validated that the sub-clock is passive and not
already owned by another clock
([known-complexity.md K3](known-complexity.md#k3-sub-clocks-that-pile-up)). The
as-built registration only skips duplicates (§15).

### A8. A child clock is registered as a sub-clock before its operator wakes — read only

[clock.md §18.3](clock.md#183-child-clocks). Registration is undone when the
operator goes to sleep. An operator that is created and never woken leaves its
child clock registered, and ticked, for as long as the parent lives. This is
the accumulation of
[known-complexity.md K3](known-complexity.md#k3-sub-clocks-that-pile-up),
still open for that case.

## Gaps

### G1. The shutdown wait borrows the maximum-latency setting — read only

60 s by default, and it changes when a user tunes latency resets (§12, §16).

### G2. Sync source entries are keyed by source name — read only

Two sources with the same id would overwrite each other's entry (§8). Nothing
in the clock requires ids to be unique.

### G3. Sync sources are compared by value — read only

Equality walks the sync source's content (§8). Nothing says the content is
comparable or small.

### G4. Fields shared between threads without protection — read only

The current sync source, the time source and the entries are written by
whichever thread reports a sync source change, and read by the animator
([ocaml-notes.md](ocaml-notes.md#concurrency)).

### G5. A collecting scope starts clocks even when its function failed — read only

§6. Clocks created by a script evaluation that raised are started.

### G6. A switch of sync source drops accumulated latency — read only

§8. Stated as behaviour; nothing says whether it is intended.

### G7. Nothing bounds how long a blocked tick holds a worker — checked

A tick that waits on work only scheduler tasks can do waits on its own worker
when there is one worker. Observed: a process started from inside a tick on one
core is not seen to exit until its 29 s timeout. With two workers it completes
at once. The cause is outside the clock; the clock's part is that a tick runs
on a scheduler worker (§10.1).

### G8. Most of the clock is untested — checked

See [tests.md](tests.md#what-is-not-covered): nothing covers the animator,
latency control or sync source tracking. D1 and D2 are in that area.

### G9. Sync source changes are applied on whichever thread reports them — read only

Sharpens G4. When push notification was introduced, a change coming from
another thread was handed to the clock and applied at the end of its tick
([known-complexity.md K1](known-complexity.md#k1-finding-the-pacer-on-every-tick)).
As built, the change is applied at once on the notifying thread, which also
replaces the time source the animator is reading (§8).

### G10. Sources still recompute their sync source before every cycle — read only

[clock.md §18.1](clock.md#181-sync-source-reporting-by-sources), last item. An
operator's recomputation asks each of its children, which ask theirs. This is
the per-tick graph walk that push notification was meant to remove, moved from
the clock to the sources. Not measured.

### G11. The reports are hard to read and the logs say little — owner's statement

The three tree reports ([reports.md](reports.md)) are described by the project
owner as clunky. The clock logs its start, its stop, a sync source switch and
latency; it logs nothing about who animates it, how long ticks take, when it
rests or releases its worker, or why it is late.

## Ownership

From an ownership review of the clock's seams: is each rule held by one place,
and by the place that has the knowledge to decide. Counts are from a search of
the sources; none of this was run.

### O1. How a clock rests is decided by the clock, which has to guess — read only

A sync source hands the clock a time source, and the contract is that the
clock rests by sleeping on it (§8). The time source cannot say what that sleep
is: free (device-paced), a timer (real time), or a blocking wait in a library
(server-driven). The clock therefore guesses from "now" and parks itself
(§9.1), which is D1. The knowledge belongs to the sync source, as a
declaration.

### O2. A source enacts the choice of animator instead of declaring it — read only

"This clock needs a thread" is called on the clock by the source (§10), by one
caller. It must happen before the clock starts, or it fails with error 18. The
fact belongs to the sync source: it is the same declaration as O1.

### O3. The clock decides when to give its worker back — read only

The yield is computed from the clock's own latency (§9.1). The clock knows its
deadline; it cannot see what else waits on the pool. The scheduler can. This
is D2 and D3 seen from the side of ownership.

### O4. The shutdown wait belongs to no one — read only

Same as G1: it reuses the maximum latency.

### O5. Eight of nine sync sources restate the clock's defaults — read only

Eight declare "my latency is the clock's configured latency" and the same for
the maximum; only the server-driven one differs. Seven declare the
unconstrained time source. A default that almost every implementation spells
out by hand is a default the contract should own.

### O6. "Tell the clock when your sync source changes" is written by hand — read only

Three operators notify explicitly when their selection changes. A network
input, which was given explicit notifications when push notification was
introduced, has none today and relies on the per-cycle recomputation (G10). A
fourth place that forgets fails silently.

### O7. Sync source equality has two owners that disagree — read only

Same as A6. A divergence, not a repetition.

### O8. The source contract is wider than its use — read only

The clock requires of a source two members it never calls: whether it is
active, and its frame (§17). Two exported readers of a sync source's latency
have no caller outside the clock. One exported accessor, the pending sources,
has callers only in tests.

### Left alone, on purpose

- A crossfade controlling its own child clock: only the crossfade knows it must
  be the sole reader.
- Each device deciding when it is pacing (stream open, socket connected): that
  knowledge is local.
- The child buffer limit living with child clocks: it protects that buffer only.

## To verify

- Whether every sync source using the unconstrained time source is affected by
  D1 the way PulseAudio is. Only PulseAudio was run.
- Whether D2 predates the scheduler's move to a C core. Not checked on an older
  build.

## Carried into the normative pass

Decisions the project owner has already stated, to be turned into rules:

1. **A clock is always time-boxed**: it releases its worker after a deadline,
   whatever its sync mode and whether or not it is late. This replaces the
   tick-count yield (D3) and must make the release effective for
   lower-priority work (D2).
2. **Whether an unsynced clock needs a thread of its own** is to be decided
   after rule 1, not before: with an effective release it may not.
3. **D1 is a logic gap in the specification**, to be closed by the normative
   pass and not by a code fix ahead of it: the contract between a sync source
   and the clock cannot say what kind of rest the sync source offers (none, a
   timer, a blocking wait), so the clock guesses.
4. **Better logging and observability of clocks is a goal** (G11): the tree
   reports are to be improved and the logs extended.

Suggested, not decided: **at least two workers** for the single-worker wait
(G7). It is a last resort, to be adopted only if no other solution is found.
