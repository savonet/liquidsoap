# Reading a child through its own clock, as built (Part A)

The seam `cross` uses to read its child faster than it delivers. Other
operators use it (`stretch`, resampling, inline encoders, piped processes,
blank stripping); only what `cross` relies on is described, and
[cross.md §4](cross.md#4-taking-from-the-child) is its implementation file.
The clock is specified in
[its own directory](../../../clock/spec/README.md).

## 1. Parts

- The **child clock**: a clock nested in the operator's clock, passive: it
  turns only when the operator ticks it. It carries the name of the
  operator's clock and names the operator as its controller.
- The **child output**: an output in the child clock, named
  `<child id>.child`, whose only source is the child.
- The **intake**: a FIFO store of stream data owned by the operator.

## 2. A tick of the child clock

On each tick the child output, when the child is ready, requests one
segment from the child and appends it to the intake, marks and metadata at
their positions. A segment of no sample appends nothing. It asks for one
segment per tick, whatever the reason the segment ended.

The child is thus read by a full request: its track events, its metadata
events, and every commitment below it happen when the data enters the
intake.

## 3. Bound

After an append, an intake longer than `settings.clock.child.max_buffer`
(10 s) raises a clock error naming the operator and the child. It exists
for operators sharing one child clock and consuming at different rates.
`cross` takes each frame as it arrives and stays far below it.

## 4. Reading

To read, the operator:

1. ticks the child clock while the child is ready and the intake holds less
   than a frame;
2. looks at the first frame of the intake, or at all of it when it holds
   less;
3. decides how much of it to take and removes that much.

The child is **available** when the intake is not empty or the child is
ready.

## 5. Lifetime

- When the operator is woken: the child clock is registered with the
  operator's clock, a new empty intake is created, the child output is
  woken.
- When it is released: the child clock is deregistered and the child output
  released.
- The child output ignores a latency reset.

## 6. Creation check

`cross` asks for it; other users may not. At creation the child's sync
answer must be "never a sync source". Any other answer, a child that is
one, or that may become one depending on what it reads, fails the creation
with an invalid-value error on the child argument: `This source may control
its own latency and cannot be used with this operator.`

## 7. Sync source

The child's sync source paces the child clock, not the operator's. What the
operator reports for itself is its own choice: see
[cross.md §11](cross.md#11-other-properties).
