# SRT input and output — specification

**Normative.** What `input.srt`, `output.srt`, `srt.socket` and the
`settings.srt.*` settings MUST, SHOULD and MAY do. MUST, SHOULD and MAY are
used as in RFC 2119.

**Reader.** Someone rebuilding the two operators inside Liquidsoap. Sources,
clocks, encoders, decoders, the scheduler and the script runtime exist and are
used through the contracts stated here. libsrt is used through the binding
contract of [library.md §6](library.md#6-what-is-relied-upon-from-the-binding),
and the libsrt behaviour this spec relies upon is listed in
[library.md §7](library.md#7-libsrt-behaviour-relied-upon).

**Scope.** In: both operators in both connection roles, the shared readiness
loop, the library lifecycle and settings, the script socket value. Out: libsrt
itself, the encoders and the stream decoders
([FFmpeg layer spec](../../ffmpeg/spec/README.md)), clocks and sync sources
([clock spec](../../../clock/spec/README.md)), the generic output and
start/stop machinery, described here only as far as SRT relies on it.

**Compatibility.** The script interface (parameters, defaults, methods,
callbacks and their arguments) and the setting names are kept; a behaviour
behind them changes only where this spec gives the reason next to the rule.
The bytes on the wire may change, as long as SRT compliance is kept or
improved.

## Seams

Two seams, each with a contract file and one file per implementation.

- **Connection role**: [connection.md](connection.md) is the contract;
  [caller.md](caller.md) and [listener.md](listener.md) answer every section
  under the same number, so "§7" is peer failure in all three.
- **Operator**: [common.md](common.md) is what both operators share;
  [input.md](input.md) and [output.md](output.md) are the operators.

An operator is one role plus one operator kind, chosen by the `mode`
parameter at construction: four combinations, all exist.

## Principles

- **One predicate drives connections**: a role connects while the operator
  wants a connection and releases everything when it stops wanting one
  ([common.md §5](common.md#5-connection-wanted)).
- **A failure affects the smallest thing it concerns**: one peer, one
  attempt, one operator ([failures.md](failures.md)). Only a cause the
  process cannot fix stops anything for good
  ([connection.md §4](connection.md#4-connect)).
- **One connection carries one stream**, from its start or its header to
  its end ([output.md §4](output.md#4-encoder)).
- **The streaming thread never waits for a peer**, and no user code runs
  under an operator lock ([concurrency.md](concurrency.md)).

## Reading order

| File                                       | Content                                                                   |
| ------------------------------------------ | ------------------------------------------------------------------------- |
| [library.md](library.md)                   | libsrt startup, cleanup, logging, settings, shutdown, address resolution. |
| [poll.md](poll.md)                         | The process-wide readiness loop.                                          |
| [common.md](common.md)                     | Shared parameters and validation, callbacks, methods, connection wanted.  |
| [connection.md](connection.md)             | The connection-role contract: setup, failure classes, peers, events.      |
| [caller.md](caller.md)                     | The caller role.                                                          |
| [listener.md](listener.md)                 | The listener role.                                                        |
| [output.md](output.md)                     | `output.srt`: lifecycle, encoder, per-peer streams, sending, wire bytes.  |
| [input.md](input.md)                       | `input.srt`: lifecycle, readiness, receiving and decoding, pacing.        |
| [socket-value.md](socket-value.md)         | `srt.socket`: options, statistics, status.                                |
| [concurrency.md](concurrency.md)           | Threads, callbacks and locks, shared state.                               |
| [failures.md](failures.md)                 | Every failure and what an observer sees.                                  |
| [conformance.md](conformance.md)           | What a test suite must check; what is binding, what is incidental.        |
| [known-complexity.md](known-complexity.md) | What the history shows to be hard, each entry linked to the rule for it.  |

Every file is Part A: behaviour any
runtime could meet.

## Cross-cutting concerns

- **Concurrency** earns a document: several kinds of thread touch one
  operator, and user callbacks may call back into it
  ([concurrency.md](concurrency.md)).
- **Failure classification** earns a document: the same kind of error must
  mean the same thing in every role and kind ([failures.md](failures.md)).
- **Security** does not: encryption is a set of socket options
  ([common.md §2](common.md#2-encryption)), admission is a predicate
  ([listener.md §3](listener.md#3-socket-setup)).
- **Crash safety and persistence** do not: nothing is written to disk except
  the optional input dump, and no state survives a restart.
- **Data layout** does not: the only format is the byte stream carried in SRT
  messages ([output.md §10](output.md#10-wire-bytes)).
