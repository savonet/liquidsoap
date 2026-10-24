# Faults

Liquidsoap runs long-lived streams. Content keeps flowing: that has priority
over every other outcome. Hard errors (§2, configuration errors) fail the
whole script before it streams. Every other fault keeps the stream running
and is made visible through warning and error logs, and through a signal the
script can act on (`fallback`, `switch`, callbacks). This document owns that
rule. Each document states, for each fault it detects, which class below the
fault belongs to and links here.

## 1. Why

An error raised out of a source while it streams detaches the source and,
with no clock error handler, fails the whole clock
([clock.md §11](../../../clock/spec/clock.md#11-failure)). One bad packet
would then take a station off the air. A fault the layer hides instead
(silence, black, a frozen picture) leaves the script blind: the source looks
healthy, so `fallback` never switches away. Both outcomes take the decision
away from the script author.

## 2. Classes

| Class               | Examples                                                                                                       | Outcome                                                                                                                          |
| ------------------- | -------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------- |
| Configuration error | unknown option, codec or format; invalid argument; type error; a hardware method that cannot be met            | Error at script evaluation or at start-up, before streaming. The script never starts on a configuration it cannot honour.        |
| Item fault          | one corrupt packet, one non-increasing timestamp, one frame a converter rejects, one packet a muxer rejects    | The item is dropped with a warning (§4). Streaming continues.                                                                    |
| Source fault        | a run of item faults past its bound, media that cannot be decoded further, a lost connection, a failed rebuild | The source ends its track, or becomes not ready. The script's `fallback`, `switch` or playlist decides what plays next (§3).     |
| Output fault        | the byte sink fails, the header cannot be written, an incompatible copy source with no clean container restart | The output stops or restarts under its own restart policy, and reports through its callbacks. The clock and other outputs go on. |

A fault is placed in the smallest class that contains it. A fault is raised
out of a source while it streams only in the one case of §3 item 3.

Some configuration errors show only when an output or operator first builds
its stages: a hardware device that cannot be opened, a stream the codec
cannot create with the configured format, a time base the muxer chooses at
header write. That can happen hours into a run (an output started late, a
file reopened, a container restarted). A failure the layer knows cannot
recover, because it follows from the configuration and the host alone, is a
hard error whenever it shows: the script fails before any data of that
stream is written. A stream that will never start is worse than a stopped
script. A failure that depends on the data or on a remote peer (a source's
parameters, a server's answer) is an output fault or a source fault, because
a later attempt can succeed.

## 3. Source faults reach the script

A source fault MUST become these signals, in this order, so that content
keeps flowing and the script keeps the decision. The source ends its track at
the fault (1). While the fault persists, a fallible source is not ready (2).
An infallible source that cannot produce content breaks its type's promise:
that is a hard error (3).

1. **End of track.** The source ends its current track at the fault. Data
   already produced is delivered and the stages drain
   ([draining.md](draining.md)). A playlist or request source moves to its
   next item.
2. **Not ready.** A fallible source reports that it has no data, so
   `fallback` and `switch` take another source. The source becomes ready
   again when it can produce data again (a reconnect, a new request, a
   wake-up). A producer whose own input stops being read while it is not
   ready (an input in a child clock that only the producer ticks) retries
   after `fault_retry_delay`: it reads its input again and becomes ready on
   the first content it can convert.
3. **Hard error.** A source typed infallible cannot become not ready. When it
   cannot produce content, it raises, and the script fails with an error
   naming the source and the cause
   ([clock.md §11](../../../clock/spec/clock.md#11-failure)).

| Parameter           | Recommended | Protects                                                                     |
| ------------------- | ----------- | ---------------------------------------------------------------------------- |
| `fault_retry_delay` | 5 s         | The CPU and log against a retry loop on every frame while the fault persists |

A source MUST NOT invent content: no silence, black or repeated frame stands
in for data it cannot produce. Filler is the script's choice, through
`mksafe`, `fallback` or `blank`. A fault never changes a source's type. An
operator whose output can run dry on a data fault SHOULD be typed fallible,
so that the script handles the fault with `fallback` instead of failing.

Each source fault is logged at error level with the source and the cause,
and is reported through the source's callbacks where the operator has them
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
side, an encoder cannot end its upstream source's track, and a data fault
never stops an output. There, a run of item faults past the bound restarts
the copied source's group on its next keyframe
([encoder-copy.md §12.3](encoder-copy.md#123-readiness-and-group-start)).
Further faults stay item faults: dropped and logged at the bounded rate, with
another group restart at each further run past the bound. On an encoded
field, a run past the bound is an output fault: the output restarts under its
policy ([encoder.md §5.2](encoder.md#52-data-faults-and-output-faults)).

A fault whose item carries no media content does not count toward the bound.
An item that carries media (a packet, a frame, a subtitle) always counts.
Items without media content are:
a decision that falls back to its default (a raising `on_undecided` handler, a
rotation edit rejected), a metadata tag that does not parse, a seek that is
not honoured. The media keeps flowing, so there is nothing for the script to
switch away from. These are logged like any item fault.

The first drop of each kind is logged as a warning. Repeats are counted and
logged with their count at most once per `fault_log_interval`, so the log is
neither flooded nor silent.

| Parameter            | Recommended | Protects                          |
| -------------------- | ----------- | --------------------------------- |
| `fault_log_interval` | 10 s        | The log, against a flood of drops |

## 5. Waiting

An operator that waits for a condition in the data (a keyframe, a group's
members, a split point) bounds the wait by a named parameter. No wait is
unbounded. When the bound is reached, the operator takes the action its
document states and logs a warning. An input-side wait becomes a source fault
(§3). An output-side wait proceeds without the missing condition (for
example, a group starts without its late member) and reports through the
output's callbacks.

## 6. Conformance

- One corrupt packet in a long input costs that packet only.
- A source whose data stays undecodable ends its track or becomes not ready
  within its bound, and a `fallback` above it switches away.
- No fault in input data raises out of a fallible source while it streams.
- No source produces content it did not receive or compute (no filler).
- A configuration error fails the script before the first frame of the stream it concerns is written, at start-up or later.
- A run of data faults on the output side never stops the output.
