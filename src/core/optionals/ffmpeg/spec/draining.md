# Draining

Every stage of the layer that holds data between its input and its output
delivers that data before the stage ends. This document owns the rule. Each
stage's document states how the stage meets it, in its own section, and links
here.

## 1. Stages

A **stage** is anything that can hold data it has accepted but not yet
produced: a decoder, an encoder, a resampler, a sample-count re-framer, a
filter graph (frame-rate conversion included), a bitstream filter, an
interleaving queue, and the muxer. A stage that produces its output for an
input before it accepts the next input holds nothing and has nothing to drain.

## 2. The rule

A stage MUST be drained before it ends. Draining means:

1. The stage is told that no more input follows.
2. Everything the stage then produces is passed downstream, in order, as
   ordinary output.
3. Only then is the stage released.

A stage ends at any of these events:

| Event                  | Examples                                                                                                                          |
| ---------------------- | --------------------------------------------------------------------------------------------------------------------------------- |
| End of input           | a file or a network stream reaches its end, a producer finishes                                                                   |
| Replacement mid-stream | a resampler rebuilt for a new rate, a frame-rate graph rebuilt, a filter graph starts a new generation, an inline encoder rebuilt |
| End of its stream      | a track whose pipeline is torn down at a track boundary                                                                           |
| Output stop or restart | an output is stopped, a container is closed or restarted                                                                          |

Drained output is ordinary output. It obeys the timestamp rules
([timestamps.md](timestamps.md)), carries its metadata and track marks at the
positions of the data they belong to, and is counted in every length and
position derived downstream.

## 3. Order

1. Stages drain upstream first. Each stage's drained output enters the next
   stage before the next stage is itself drained.
2. The drained output of a replaced stage precedes the first output of its
   replacement.
3. A container is finalised (trailer written) after every stage feeding it
   has drained.

## 4. Partial final units

A stage that works in units of a fixed size (an audio encoder with a fixed
frame size, a re-framer feeding one) delivers its final partial unit. When the
consumer accepts a short final unit, the stage sends it short. Otherwise the
stage pads it to full size with silence and signals the padding wherever the
container can carry it, so that a reader can trim it.

## 5. Discarded on purpose

These events discard held data instead of draining it:

- **A seek.** A decoder's held data belongs to the position the caller left.
  The decoder discards it. Every stage downstream sees the seek as a source
  switch ([content.md §5](content.md#5-stream_idx-and-time_base)). A
  downstream stage that ends at that switch drains as usual: its held data
  was produced before the switch and already counted in positions.
- **A producer put to sleep.** Its buffer has no reader left. The producer
  and its stages are released without draining.
- **An input stopped by its reader** (the source is stopped or the process
  shuts down). Nothing downstream consumes the held data. An input that
  reaches its own end of input drains (§2).
- **A stage in error.** A stage whose last call failed is released without
  draining. The failure is reported where it happens.

A drain that fails part-way is logged at error level and teardown continues
with the next stage. A failed drain is not escalated: the stage was ending
anyway.

Operators also discard data on purpose as part of their own rules: a copied
stream waiting for a keyframe, packets trimmed at a group start, an overflow
drop at a buffer bound, an `abort_track`. Each owner states those discards,
their reason, and whether one also counts as an item fault
([faults.md §4](faults.md#4-item-faults-escalate)). They are not drains.

## 6. Conformance

- The number of audio samples out of a chain of stages equals the number in,
  up to the codec delay and padding that the container signals.
- Frames out of a frame-rate stage cover the input's full duration.
- Rebuilding a stage mid-stream loses no samples and no frames at the seam.
- The last packet of every copied stream of a file reaches the output.
