# Faults (Part A)

Liquidsoap runs long-lived streams. Content keeps flowing: that has priority
over every other outcome. This document owns that rule for every part of
Liquidsoap. Each document states, for each fault it detects, which class
below the fault belongs to and links here.

## 0. The rule

**Keep streaming, unless the error is major.** An error that leaves enough
context to go on with what the user would expect MUST NOT stop the stream:
it is logged, made visible to the script where the script can act on it,
and the part concerned carries on with that. A fault is contained to the
smallest thing it concerns: one item, one peer, one attempt, one source, one
output.

An error is **major** when carrying on would break a promise the rest of the
system was built on. Only a major error stops anything for good. The
reference cases:

- an infallible source with nothing to deliver
  ([§3](#3-source-faults-reach-the-script), item 3);
- a configuration that cannot be honoured
  ([§2](#2-classes));
- a copied FFmpeg stream its output cannot carry
  ([FFmpeg principle 6](../core/optionals/ffmpeg/spec/README.md#principles)).

**Script code that raises is not major.** A predicate, getter, transition
function, hook or callback that raises while an operator runs it is logged
at `severe` with its backtrace, and the operator goes on with the most
plausible meaning of the script: a predicate counts as false, a hook as
doing nothing, a transition as playing its two sources in order. Other
handlers registered on the same event still run. Each operator's document
states what it goes on with.

## 1. Why

An error raised out of a source while it streams detaches the source and,
with no clock error handler, fails the whole clock
([clock.md §11](../core/clock/spec/clock.md#11-failure)). One bad packet
would then take a station off the air. A fault an operator hides instead
(silence, black, a frozen picture) leaves the script blind: the source looks
healthy, so `fallback` never switches away. Both outcomes take the decision
away from the script author.

## 2. Classes

| Class               | Examples                                                                                                       | Outcome                                                                                                                                                                            |
| ------------------- | -------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Configuration error | unknown option, codec or format; invalid argument; type error; a device or hardware method that cannot be met  | Error at script evaluation or at start-up, before streaming, attached to the argument it concerns. The script never starts on a configuration it cannot honour.                    |
| Item fault          | one corrupt packet, one non-increasing timestamp, one frame a converter rejects, one packet a muxer rejects    | The item is dropped with a warning (§4). Streaming continues.                                                                                                                      |
| Source fault        | a run of item faults past its bound, media that cannot be decoded further, a lost connection, a failed rebuild | The source ends its track, or becomes not ready. The script's `fallback`, `switch` or playlist decides what plays next (§3).                                                       |
| Output fault        | the byte sink fails, the header cannot be written, a peer goes away                                            | The output, or the one peer, stops or restarts under its own restart policy, and reports through its callbacks. The clock and other outputs go on.                                 |
| Unrecoverable       | an infallible source with nothing to deliver; a copied source the output cannot carry and cannot restart for   | The clock fails, with one message naming the source and the cause, without a backtrace for an error the user must act on ([clock.md §11](../core/clock/spec/clock.md#11-failure)). |

A fault is placed in the smallest class that contains it. A fault is raised
out of a source while it streams only in the one case of §3 item 3.

Some configuration errors show only when an output or operator first builds
its stages: a device that cannot be opened, a stream the codec cannot create
with the configured format, a time base the muxer chooses at header write.
That can happen hours into a run (an output started late, a file reopened, a
container restarted). A failure known not to recover, because it follows
from the configuration and the host alone, is a hard error whenever it
shows: a stream that will never start is worse than a stopped script. A
failure that depends on the data or on a remote peer (a source's
parameters, a server's answer) is an output fault or a source fault, because
a later attempt can succeed. A failure waiting cannot fix is not retried; a
failure that may clear up later is retried after a bounded delay.

## 3. Source faults reach the script

A source fault MUST become these signals, in this order, so that content
keeps flowing and the script keeps the decision. The source ends its track at
the fault (1). While the fault persists, a fallible source is not ready (2).
An infallible source that cannot produce content breaks its type's promise:
that is a hard error (3).

1. **End of track.** The source ends its current track at the fault. Data
   already produced is delivered and the stages drain
   ([draining.md](../core/source/spec/draining.md)). A playlist or request
   source moves to its next item.
2. **Not ready.** A fallible source reports that it has no data, so
   `fallback` and `switch` take another source. The source becomes ready
   again when it can produce data again (a reconnect, a new request, a
   wake-up). A producer whose own input stops being read while it is not
   ready (an input in a child clock that only the producer ticks) retries
   after `fault_retry_delay`: it reads its input again and becomes ready on
   the first content it can convert.
3. **Hard error.** A source typed infallible cannot become not ready. When it
   knows it cannot produce content, it raises, and the script fails with an
   error naming the source and the cause
   ([clock.md §11](../core/clock/spec/clock.md#11-failure)). An output given
   less than a full frame by an infallible source asserts: that backstop
   catches a broken promise no source reported
   ([source-contract.md §11](../core/source/spec/source-contract.md#11-concurrency)).

| Parameter           | Recommended | Protects                                                                     |
| ------------------- | ----------- | ---------------------------------------------------------------------------- |
| `fault_retry_delay` | 5 s         | The CPU and log against a retry loop on every frame while the fault persists |

A source MUST NOT invent content: no silence, black or repeated frame stands
in for data it cannot produce. Filler is the script's choice, through
`mksafe`, `fallback` or `blank`. A fault never changes a source's type. An
operator whose output can run dry on a data fault SHOULD be typed fallible,
so that the script handles the fault with `fallback` instead of failing.

Each source fault is logged at `severe` with the source and the cause, and is
reported through the source's callbacks where the operator has them
(`on_error`, `on_disconnect`, `on_stop`).

## 4. Item faults escalate

Item faults are counted per source. A bound on consecutive item faults turns
a stream of them into a source fault, so that a source producing only bad
data is seen by the script instead of being thinned out to nothing. A
delivered item resets the count.

| Parameter                | Recommended | Protects                                                  |
| ------------------------ | ----------- | --------------------------------------------------------- |
| `max_consecutive_faults` | 100         | The script's view of a source that delivers only bad data |

Every document that drops items counts against this one bound. On the output
side an encoder cannot end its upstream source's track, and a data fault
never stops an output: what a run past the bound does there is the output's
document's (for FFmpeg, [encoder-copy.md §12.3](../core/optionals/ffmpeg/spec/encoder-copy.md#123-readiness-and-group-start)
and [encoder.md §5.2](../core/optionals/ffmpeg/spec/encoder.md#52-data-faults-and-output-faults)).

A fault whose item carries no media content does not count toward the bound.
An item that carries media (a packet, a frame, a subtitle) always counts.
Items without media content are: a decision that falls back to its default
(a raising handler, a rotation edit rejected), a metadata tag that does not
parse, a seek that is not honoured. The media keeps flowing, so there is
nothing for the script to switch away from. These are logged like any item
fault.

The first drop of each kind is logged as a warning. Repeats are counted and
logged with their count at most once per `fault_log_interval`, so the log is
neither flooded nor silent.

| Parameter            | Recommended | Protects                          |
| -------------------- | ----------- | --------------------------------- |
| `fault_log_interval` | 10 s        | The log, against a flood of drops |

## 5. Waiting

An operator that waits for a condition in the data (a keyframe, a group's
members, a split point, a peer) bounds the wait by a named parameter. No wait
is unbounded. When the bound is reached, the operator takes the action its
document states and logs a warning. An input-side wait becomes a source fault
(§3). An output-side wait proceeds without the missing condition (for
example, a group starts without its late member) and reports through the
output's callbacks.

A wait inside a streaming cycle is governed by
[source-contract.md §1](../core/source/spec/source-contract.md#1-streaming-cycle):
nothing waits there except a blocking sync source, whose wait is its pacing.

## 6. Conformance

- One corrupt packet in a long input costs that packet only.
- A source whose data stays undecodable ends its track or becomes not ready
  within its bound, and a `fallback` above it switches away.
- No fault in input data raises out of a fallible source while it streams.
- No source produces content it did not receive or compute (no filler).
- A configuration error fails the script before the first frame of the
  stream it concerns is written, at start-up or later.
- A run of data faults on the output side never stops the output.
- Script code that raises inside an operator is logged and the stream goes
  on; the operator does what its document says it goes on with.
- An infallible source with nothing to deliver fails the clock loudly.
