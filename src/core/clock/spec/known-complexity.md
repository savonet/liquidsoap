# Known complexity

What the project's history shows to be hard about clocks, stated without
reference to the code. Each entry ends with the rule that answers it, and has
its check in [conformance.md](conformance.md).

**Coverage.** This is a partial mining. Six areas were read: finding the
pacer, sub-clocks, child clocks, unification, server-driven clocks, and clocks
as scheduler tasks. The clock has 113 commits, 13 of them fixes and 2 reverts; the rest of
the history was not read.

## K1. Finding the pacer on every tick

**Situation.** A clock must know whether something else is pacing its stream.
The answer depends on run-time state spread over the whole source graph: which
sources are ready, which child an operator currently selects, whether a
network input is connected.

**What went wrong.** The clock asked on every tick. Each animated source asked
its children, which asked theirs. The cost per tick grew with the number of
animated sources times the depth of the graph, for an answer that changes only
when a source starts, stops or switches. CPU use was high with nothing
happening.

**Why it is hard.** The answer cannot be computed once, and the obvious way to
keep it current is to recompute it. Making it cheap means inverting the flow:
whoever changes tells its readers. That only works if every place that can
change the answer remembers to tell, and a place that forgets fails silently:
the clock keeps the old answer.

**How to tell.** With a deep graph and nothing changing, the work a tick does
to know its pacer does not depend on the size of the graph. Binding: the count
of pacing queries made to sources over many idle ticks is zero, or a constant
per tick. Incidental: the mechanism.

**Rule.** The clock never asks: sources notify, the clock applies changes at its
next tick, and the per-tick cost is fixed on both sides
([pacing.md §3](pacing.md#3-finding-the-sync-source)). Changes reported from
another thread are handed to the clock, never applied in place.

Trace: #5133.

## K2. Notifications that are never unsubscribed

**Situation.** A long-lived clock on which sources come and go: transitions
between tracks create and discard sources continuously. Each animated source
that joins the clock is subscribed to.

**What went wrong.** Subscriptions were kept after their source had gone. Each
kept its source alive. Memory grew without bound and CPU rose gradually from
collection pressure. A stopped clock was also kept alive through the same
chain. It took three fixes: unsubscribing when a source sleeps, then when the
clock stops, then when the source is detached.

**Why it is hard.** A subscription has two owners, and each of the three
moments is "the end" for only some sources: sleeping is not reached by every
kind, and the clock stopping never comes for a clock that runs for weeks. Only
the moment a source leaves the clock is common to all.

**How to tell.** On a clock that keeps running, attach and discard a source
many times. Binding: the number of live subscriptions and the number of
sources kept alive return to their starting value; memory does not grow with
the count of cycles.

**Rule.** Every subscription ends when its source leaves the clock, whatever the
way ([clock.md §7](clock.md#7-sources-on-a-clock),
[§10](clock.md#10-winding-down)).

Trace: #5153, #5163 and one direct commit. Three fixes.

## K3. Sub-clocks that pile up

**Situation.** Operators with a child clock are created and discarded at run
time, inside transitions and dynamic sources. A clock ticks every sub-clock it
knows.

**What went wrong.** Three separate failures.

- Sub-clocks were registered when their operator was created and never
  removed. The parent ticked every one ever created: CPU grew with uptime.
- Stopping a clock put its outputs to sleep first; that made an operator
  remove its sub-clock, which was then never stopped and its resources never
  freed: a leak per track.
- After two clocks were unified, a parent held two entries for what was now
  one clock.

**Why it is hard.** The set of sub-clocks is changed by the very callbacks the
clock runs while it walks or stops them, and "the same clock" is not a stable
notion: unification makes two entries one after they were added.

**How to tell.**

- Create and discard an operator with a child clock many times: the parent's
  sub-clock count returns to its starting value. Binding: the count.
- Stop a clock whose output removes a sub-clock while going to sleep: the
  sub-clock ends stopped. Binding: its state.
- Unify two sub-clocks of one parent: the parent has one entry and ticks it
  once per tick. Binding: the count and the tick count.

**Rule.** Registration is counted, made when an operator wakes and undone when it
sleeps, so the list holds only what is in use
([clock.md §11](clock.md#11-sub-clocks), [§16](clock.md#16-child-clocks)).
Winding down stops the sub-clocks it saw before putting outputs to sleep
([clock.md §10](clock.md#10-winding-down)). A merge leaves one entry
([unification.md §4](unification.md#4-commit)). Nesting is carried
by the parent, fixed at creation, not by the list
([clock.md §1](clock.md#1-entities)).

Trace: #5029, #5103, #5114. Three fixes.

## K4. Comparing things that cannot be compared cheaply

**Situation.** Clocks and sync sources are deduplicated and checked for
change.

**What went wrong.**

- Comparing two distinct clocks by content failed outright, because a clock
  holds callbacks, which cannot be compared.
- Comparing sync sources by content before every streaming cycle walked their
  content each time: a cost on every cycle for a check that almost always says
  "unchanged".

**Why it is hard.** Comparison by content looks correct and works until the
content holds something that cannot be compared or is large. Identity is cheap
and total, but then two values that mean the same thing are different.

**How to tell.** Deduplicating a set of distinct clocks never fails, whatever
they hold. A change check on an unchanged sync source costs a constant.

**Rule.** Clocks are compared by identity ([clock.md §1](clock.md#1-entities)).
Sync sources are compared by an identity they declare, in constant time, by
sources and clock alike ([pacing.md §2](pacing.md#2-sync-source)).

Trace: #5114, #5153.

## K5. A start pass that slows down with clocks that cannot start

**Situation.** Many clocks exist that have nothing to animate yet. A start pass
runs often: after every script evaluation and around callbacks.

**What went wrong.** Each clock that could not start was put back with a
duplicate check against all the others. The pass was quadratic in the number
of waiting clocks.

**Why it is hard.** The pass is on a path taken very often, so anything
proportional to the number of idle clocks is paid constantly.

**How to tell.** A start pass over N clocks that cannot start costs
proportionally to N. With nothing waiting and nothing new it costs a constant.

**Rule.** [clock.md §6](clock.md#6-creation-and-start): the pass is linear in the
waiting clocks and constant with none.

Trace: #5153.

## K6. A choice made at run time is invisible to a subscription made at start

**Situation.** An operator plays one of several children, chosen while
running. A network input paces the stream only while connected.

**What went wrong.** Subscribing to "the children" missed the child actually
playing, because it was not in the fixed list, or was one of several. The
clock kept pacing, or kept deferring, after the pacer had changed. Notifying
explicitly at each switch fixed it, in three rounds: the dynamic operator, the
network input, then the selecting and sequencing operators. One case also
needed the connection to be marked established before its listeners were
told, so that they read the new state.

**Why it is hard.** The rule "notify when your answer changes" has to be
applied by hand at every place an answer can change, and the order between
changing the state and announcing it matters.

**How to tell.** Switch a selecting operator between a child that paces and
one that does not: the clock changes pacing within one tick. Connect and
disconnect a pacing input: same. Binding: the clock's pacing after one tick.

**Rule.** A contract on sources, with the rule held once by what all sources
share and the order between changing state and announcing it stated
([pacing.md §3](pacing.md#3-finding-the-sync-source)). A source settles its
answer once per streaming cycle, from what it reads in that cycle.

Trace: #5133 and two direct commits. Three rounds.

## K7. A remembered value computed by two threads at once

**Situation.** An operator's sync type is computed once and remembered. It is
read from the clock, from notifications arriving on other threads, and from
finalisation.

**What went wrong.** Two threads computing it at the same time crashed the
clock at start-up.

**Why it is hard.** "Computed once" suggests a mechanism that forbids a second
computation, which fails when two start together. The value is deterministic,
so letting both compute is safe.

**How to tell.** Two threads compute the value in a forced overlap: both get
the right value and none fails.

**Rule.** [clock.md §16](clock.md#16-child-clocks), last paragraph. The clock
itself does not read this value.

Trace: one direct commit.

## K8. A pacer that is not ready

**Situation.** A source that paces the stream is attached but not producing.

**What went wrong.** The clock stopped pacing because a pacer existed,
although nothing was pacing.

**How to tell.** With a pacing source that is not ready, the clock paces
itself.

**Rule.** Only a sync source that is really pacing is reported, and only ready
children count ([pacing.md §3](pacing.md#3-finding-the-sync-source)).

Trace: two direct commits.

## K9. Two pacers for one stream

**Situation.** A device paces a stream by blocking, and the clock also knows
how to pace.

**What went wrong.** Originally the clock carried a flag meaning "do not
pace". That flag was replaced by a design where each pacer declares its own
timing: its time source, the advance at which to rest, the latency at which to
reset. A device that paces by itself declares a time source on which the clock
is never late and resting is free. The clock then needs no special case.

Later, clocks became scheduler tasks, and resting gained a second form that
does not go through the time source. The clock paced on top of the device
again, and the stream ran at a small fraction of real time.

**Why it is hard.** The rule "the clock does not pace a stream that something
else paces" was never a statement in the clock. It was a property of one
function of the time source. Any new way to rest that bypasses that function
breaks the rule without touching it.

**How to tell.** With a device-paced stream, over a measured span of real
time, the stream produced equals the real time elapsed, whichever way the
clock is animated. Binding: the ratio, within the device's own tolerance.

**Rule.** The rule is a statement of the clock
([pacing.md §1](pacing.md#1-the-rule)). A sync source declares its pacing;
a clock following a `self-paced` one has no rest at all
([pacing.md §5](pacing.md#5-after-a-tick)), and every form of rest is derived
from the declaration ([pacing.md §6](pacing.md#6-rest-and-lateness)).
Every time source's now advances ([pacing.md §2](pacing.md#2-sync-source)).

Trace: #5021, #5397.

## K10. A clock driven by a server's callback

**Situation.** An audio server calls back at its own rhythm and must find its
data ready. Several clocks may use one server. The server can stop.

**What went wrong, and what the design answers.**

- The clock's own time fighting the server's: answered by taking the server's
  time as the clock's time ([pacing.md §9](pacing.md#9-server-driven-clocks)).
- Several clocks waiting on one server stepping on each other: each waits on
  its own target.
- Waking too late to prepare the data: the wait ends a full server period
  before the target.
- A server that stops while a clock waits: the wait ends and the clock stops.
- Resting blocks in the server's library: such a clock cannot share a
  scheduler worker.

**Why it is hard.** The clock is no longer the one that decides when a tick
happens. Everything that assumes it is — resting, lateness, stopping, sharing
a worker — has to be re-read for this case.

**How to tell.** With a server-driven clock: one tick per server period, no
drift over a long run; stopping the server stops the clock within one period;
two clocks on one server each keep their own pace.

**Rule.** A first-class way to drive a clock
([pacing.md §9](pacing.md#9-server-driven-clocks)), each point above stated
as a guarantee of the wait. The clock moves to a thread when such a source
joins it, so it can join a running clock
([pacing.md §8](pacing.md#8-animator)).

Trace: #5021, #5028, #5397.

## K11. Unification

**Situation.** Clocks are created freely while a script is evaluated and are
merged as the script connects sources. Merging goes on at run time, on several
threads, for as long as the script creates sources.

**What went wrong, over time.**

- **Child clocks.** Two child clocks were unified although the clocks
  controlling them were different, or refused although they were the same.
  Unifying two controlled clocks now requires, and performs, the unification
  of their controllers. The notion of controller was then refined twice: a
  controller is another clock, or a named external entity, and a clock's own
  thread or task is one such entity.
- **Leftovers.** A clock merged away stayed in the set of clocks waiting to
  start.
- **Started clocks.** A started clock is referred to by what is ticking it, so
  it must be the one that survives a merge.
- **Starting a controlled clock.** A clock that has a controller must not be
  started on its own.
- **Memory.** Handles formed chains. A long-running script creating operators
  per track grew by a fixed amount per track, because a chain kept every
  handle created after its head. Balancing the chains made the growth flat.
- **Threads.** Two threads unifying at once could corrupt the chains; merging
  is now exclusive while reading stays free.

**Why it is hard.** A merge changes what "the same clock" means for every set
that holds clocks, while those sets are being read by running clocks. It has a
direction that matters, it recurses through controllers, and it can fail
after it has started changing things.

**How to tell.**

- Two child clocks unify exactly when their controllers do. Binding: which
  error, or none.
- After any merge, no set of clocks holds the clock merged away, and no set
  holds one clock twice. Binding: the sets' contents.
- Merging into a started clock leaves it ticking, with the other's sources
  joining at its next tick. Binding: tick counts keep increasing.
- A script that creates and discards an operator per track for a long time:
  memory is flat. Binding: live memory against track count.
- Many threads unifying and reading at once: no failure, and every handle
  designates a clock. Binding: no error.
- A failed unification leaves both clocks as they were. Binding: every field
  of both.

**Rule.** [unification.md](unification.md): a plan that checks and a commit that
cannot fail, so a failed unification leaves nothing behind. A clock has a
controller exactly when it is passive, and a passive clock never gets an
animator, so a controlled clock never runs on its own
([clock.md §1](clock.md#1-entities), [§6](clock.md#6-creation-and-start)). Leftovers, memory and threads are
guarantees of [unification.md §5](unification.md#5-guarantees).

Trace: #3905, #4638, #4643, #4645, #4647, #5114, #5336, #5447 and one direct
commit. Nine changes.

## K12. One thread per clock does not scale; one task per clock holds a worker

**Situation.** Many small streams, a clock each, on a machine with fewer cores
than clocks.

**What went wrong.** With a thread per clock, the machine held about half as
many streams in real time as with clocks sharing a pool of workers: the
measure that led to clocks as tasks was 51 streams against 27. As tasks,
clocks then met the opposite problem: a tick cannot be interrupted, so a clock
that does not rest keeps its worker, and other work waits.

**Why it is hard.** Both forms are right for some clocks. A clock that rests
most of the time is cheap as a task. One that never rests, or that rests by
blocking in a library, is not a good task. Which one a clock is can change
while it runs.

**How to tell.**

- N clocks that rest, on fewer than N workers, all stay in real time.
  Binding: no clock late by more than its latency.
- With as many unsynced clocks as workers, other work still runs on time.
  Binding: the delay of a task due at a known time.
- More late clocks than workers take turns within a stated bound. Binding:
  the longest a ready clock waits for a worker.

**Rule.** The animator follows what the clock is at the moment, and changes with
it ([pacing.md §8](pacing.md#8-animator)): clocks that block or never rest
get a thread, the others share workers. A clock task is time-boxed and
yields to other clocks ([pacing.md §7](pacing.md#7-time-box-and-release)).
Work ranked after clocks waits for clocks by design: it is delayed only while
every worker holds a late clock.

Trace: #5397.

## K13. Stopping must reach a tick that is under way

**Situation.** The application shuts down while clocks are in the middle of a
tick.

**What went wrong.** Ticks ran to their end, or blocked, after the stop was
asked.

**How to tell.** After a global stop, every clock is stopped within a stated
bound, and nothing a tick reads from is torn down before the clock has
stopped. Binding: the order of the two events and the bound.

**Rule.** The stop is checked between the steps of a tick
([clock.md §8](clock.md#8-tick)), ends rests and waits, and shutdown waits
for every running clock under a parameter of its own
([clock.md §13](clock.md#13-global-stop-and-shutdown)). A failed clock is
wound down like any other ([clock.md §12](clock.md#12-failure)).

Trace: one direct commit.

## K14. Several readers of one child clock

**Situation.** Several operators read the same child source, each at its own
moment: encoders sharing one input, a filter graph with several outputs.

**What went wrong.** Two mechanisms existed for reading a child. In one, a
reader switched the child's recording on only around its own read. When
another reader ticked the child clock, the first recorded nothing: readers
silently skipped each other's frames.

Then the opposite problem: a child clock is also ticked by its parent, to keep
real outputs and active sources inside it running. Recording on those ticks
buffers data nobody asked for.

**Why it is hard.** A child clock has two kinds of tick that must be told
apart: one that means "a reader wants data" and one that means "keep things
alive". And a tick is shared: whoever asks, everyone must receive, so each
reader accumulates what the others asked for. Readers at different rates then
grow without bound, but rates differ legitimately for a while, so comparing
readers is wrong; only a bound on each buffer works.

**How to tell.**

- Two readers on one child clock: a tick asked by one fills both buffers.
  Binding: both buffer lengths.
- A tick that is not a pull buffers nothing. Binding: buffer length unchanged.
- Two readers at diverging rates: an error naming the slower one, at the
  limit. Binding: the error and that it is raised at the limit, not before.
- A reader holding a remainder when its child ends still delivers it.
  Binding: the data delivered.

**Rule.** [clock.md §16](clock.md#16-child-clocks) and the pull flag of
[clock.md §8](clock.md#8-tick). Each reader is a registrant, so one going to
sleep does not take the child clock from the others
([clock.md §11](clock.md#11-sub-clocks)).

Trace: #5267 and the abandoned attempt it replaced.

## K15. Who controls a child clock

**Situation.** A child clock is created with a controller. Some operators want
the default (their own clock); a crossfade wants to be the controller itself.

**What went wrong.** The default was lost through a mix-up of arguments, so
child clocks had no controller. Clocks that should have been distinct, or
should have been driven by their parent, were then unified or started on their
own. Shared encoders crashed on a released version.

**Why it is hard.** "No controller" is a valid value that means "unify with
anything", so losing the controller raises no error where it is lost. It
shows up far away, as a wrong merge.

**How to tell.** A child clock always has a controller from the moment it
exists. Two crossfades over two sources end with two child clocks. Two
readers of one shared child end with one. Binding: the number of distinct
child clocks and each one's controller.

**Rule.** A passive clock without a controller cannot be created
([clock.md §1](clock.md#1-entities), [§6](clock.md#6-creation-and-start)).
Exclusive child clocks: [unification.md §3](unification.md#3-plan).

Trace: #4645, #5113. One regression on a release.

## K16. A child clock that is not started when its operator needs it

**Situation.** Operators with child clocks are created at run time, after the
application's clocks were started.

**What went wrong.** The operator acted on a child clock that had not been
started. The fix made the operator start it before each cycle. The note left
with the fix asks for "a more robust mechanism to account for dynamically
created clocks".

**Why it is hard.** A passive clock is started by nobody but its controller,
and the controller is an operator that may itself be created, woken and put to
sleep at any time.

**How to tell.** An operator with a child clock created inside a running
script produces data on its first cycle. Binding: no failure, data produced.

**Rule.** Registering a sub-clock on a running parent starts it, and a sub-clock
that is not started is skipped, not an error
([clock.md §11](clock.md#11-sub-clocks)). The operator does not have to start
its child clock.

Trace: #4598.

## K17. Activations that keep sources alive, or let them go

**Situation.** A source runs while something holds an activation of it. An
output at the top of a graph has no reader; its clock holds its activation.

**What went wrong.** Two attempts. Tracking activations for cleanup made the
tracked thing keep its source alive forever: a leak. Without it, sources
nobody put to sleep stayed awake. The outcome: waking a source returns an
activation that must be handed back to put it to sleep; an activation dropped
without that puts the source to sleep on its own, with a warning.

**Why it is hard.** The clock is the one holder that must not let go: if it
does, the output stops. Every other holder must let go, or the source never
stops.

**How to tell.** An output attached to a running clock stays awake for as long
as the clock runs. A source whose reader is discarded without putting it to
sleep ends up asleep. Binding: the awake state of each.

**Rule.** [clock.md §7](clock.md#7-sources-on-a-clock) and
[§10](clock.md#10-winding-down).

Trace: #4804, #4808 and one direct commit.

## K18. Loops between clocks

**Situation.** A clock ticks its sub-clocks, which tick theirs.

**What went wrong.** In an earlier design clocks could form cycles, and a
dedicated error existed. The redesign made sub-clocks always passive and
created from their main clock, intending cycles to be impossible by
construction.

**How to tell.** No sequence of creation, registration and unification yields
a clock that is its own sub-clock at any depth. Binding: the loop error, or
the absence of a cycle.

**Rule.** By construction and by check: a parent is fixed at creation,
registration is refused on any other clock
([clock.md §11](clock.md#11-sub-clocks)), and unification refuses a merge
that would nest a clock in itself ([unification.md §3](unification.md#3-plan)).

Trace: #3781.
