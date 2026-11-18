# Reading a child through its own clock (Part A)

The seam `cross` uses to read its child faster than it delivers. Other
operators use it (`stretch`, resampling, inline encoders, piped processes,
blank stripping); only what `cross` relies on is specified here, and
[cross.md §4](cross.md#4-reading-the-child) is its user's file. The clock
is specified in [its own directory](../../../clock/spec/README.md).

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
([source-contract.md §1b](../switch/source-contract.md#1b-delivery)).

## 3. Bound

The intake MUST NOT grow without limit: past `max_buffer` (setting
`settings.clock.child.max_buffer`, recommended 10 s) an append raises a
clock error naming the operator and the child. It protects memory when
operators sharing one child clock consume at different rates. An operator
that takes what each tick brings stays far below it.

## 4. Reading

The operator reads only while it serves a request of its own reader. To
read, it:

1. ticks the child clock while the child is ready and the intake holds less
   than it wants;
2. takes from the front of the intake as much as it wants, **never across a
   recorded track end**: a take stops there and says so.

- A probe of the operator MUST NOT tick the child clock.
- The child is **available** when the intake is not empty or the child's
  probe does not answer `nothing`.
- How much the operator may read in one request is its own rule
  ([cross.md §4](cross.md#4-reading-the-child)).

## 5. Lifetime

- When the operator is woken: the child clock is registered with the
  operator's clock, the intake is new and empty, the child output is woken.
  The child is woken through the child output only.
- When it is released: the child output is released, the child clock
  deregistered, the intake discarded.
- The child output ignores a latency reset.

## 6. Creation check

An operator MAY ask for it; `cross` does. At creation the child's sync
answer MUST be "never a sync source". Any other answer, a child that is
one, or that may become one depending on what it reads, fails the creation
with an invalid-value error on the child argument, saying that the source
may control its own latency and cannot be used with this operator.

Rationale: a child that paces itself delivers at its own rate and cannot be
read ahead. Read ahead anyway, it drifts and glitches without any error.

## 7. Sync source

The child's sync source paces the child clock, not the operator's. The
operator reports none on the child's behalf. What it reports for the other
sources it reads is its own rule
([cross.md §12](cross.md#12-other-properties)).
