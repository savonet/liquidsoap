# Reading a child through its own clock (Part A)

The seam of an operator that reads its child through a clock of its own,
faster or slower than it delivers: `cross`, `stretch`, resampling, inline
encoders, piped processes, blank stripping. Each user's file states what it
reads and when ([cross.md §4](../../operators/spec/cross/cross.md#4-reading-the-child)
for `cross`). What the clock does with a child clock is
[clock.md §15](../../clock/spec/clock.md#15-child-clocks); this file owns
the operator's side.

## 1. Parts

- The **child clock**: a clock nested in the operator's clock, passive: it
  turns only when the operator ticks it. It names the operator as its
  controller.
- The **child output**: an output in the child clock whose only source is
  the child. It MUST be created inside the child clock: it is never, even
  for an instant, in a clock that something else could start.
- The **intake**: a FIFO store of stream data owned by the operator.

The child belongs to the child clock. A child that something also reads
from the operator's clock would be in both: this MUST be refused when the
clocks are unified, as a clock nested in itself.

**Set-up, when the operator is created:** it creates the child clock,
passive, with the operator's clock as its parent, then the child output in
it, which unifies the child's clock with the child clock. It does not
register the child clock yet.

**Shared and exclusive.** Several operators MAY read from one child clock;
each is a registrant, and a tick by any of them fills the intake of all. An
operator that must be the only reader of its child, such as `cross`, makes
itself the owner of its child clock. Two exclusive child clocks never unify
([unification.md §3](../../clock/spec/unification.md#3-plan)).

## 2. A tick of the child clock

On each tick the child output, when the child is ready, requests one
segment from the child and appends it to the intake:

- the data, with marks and metadata at their positions;
- **the place where the child stated `track end`**, when the segment ended
  with it, also when the segment holds no sample.

A segment that ends `full`, `pause` or `late` records nothing but its data.
The intake therefore tells its reader where each track of the child ended,
as the child stated it, and never by the length of what a tick brought.

The child is read by a full request: its track events, its metadata events
and every commitment below it happen when the data enters the intake. The
operator is the child's consumer
([source-contract.md §1b](source-contract.md#1b-delivery)).

## 3. Bound

The intake MUST NOT grow without limit: past `max_buffer` (setting
`settings.clock.child.max_buffer`, recommended 10 s) an append raises a
clock error naming the operator and the child. It protects memory when
operators sharing one child clock consume at different rates, and is raised
at the limit, not before. An operator
that takes what each tick brings stays far below it.

## 4. Reading

The operator reads only while it serves a request of its own reader. To
read, it:

1. ticks the child clock while the child is ready and the intake holds less
   than it wants, which is at least what its reader asked for; a request
   served from the intake ticks nothing;
2. takes from the front of the intake as much as it wants, **never across a
   recorded track end**: a take stops there and says so.

- A probe of the operator MUST NOT tick the child clock.
- The child is **available** when the intake is not empty or the child's
  probe does not answer `nothing`. An operator holding data when its child
  ends still delivers it.
- How much the operator may read in one request is its own rule
  ([cross.md §4](../../operators/spec/cross/cross.md#4-reading-the-child)).

## 5. Lifetime

- When the operator is woken: the child clock is registered with the
  operator's clock as a sub-clock, which starts it when the parent runs; the
  intake is new and empty; the child output is woken. The child is woken
  through the child output only. An operator created and never woken costs
  its parent nothing.
- When it is released: the child output is released, the child clock
  deregistered, the intake discarded.
- The child output ignores a latency reset of either clock: what the
  operator holds is ahead of what is heard by design, and a reset of the
  parent re-anchors its own time only
  ([pacing.md §6](../../clock/spec/pacing.md#6-rest-and-lateness)).

## 6. Creation check

An operator MAY ask for it; `cross` does. At creation the child's sync
answer MUST be "never a sync source". Any other answer, a child that is
one, or that may become one depending on what it reads, fails the creation
with an invalid-value error on the child argument, saying that the source
may control its own latency and cannot be used with this operator.

For this a source declares whether its sync source is `static` or
`dynamic`; an operator is `dynamic` if any of its children is. Where it is
computed once and remembered, two threads computing it at the same moment
MUST both get the right value.

Rationale: a child that paces itself delivers at its own rate and cannot be
read ahead. Read ahead anyway, it drifts and glitches without any error.

## 7. Sync source

The child's sync source paces the child clock, not the operator's. The
operator reports none on the child's behalf. What it reports for the other
sources it reads is its own rule
([cross.md §12](../../operators/spec/cross/cross.md#12-other-properties)).
