# SRT input and output — specification

**Normative.** What `input.srt`, `output.srt`, `srt.socket` and the
`settings.srt.*` settings MUST, SHOULD and MAY do, written under the
[project conventions](../../../../spec/conventions.md). This tree sits on the
levels above it, indexed in [src/spec](../../../../spec/README.md): the
project rules ([principles](../../../../spec/principles.md),
[faults](../../../../spec/faults.md),
[concurrency](../../../../spec/concurrency.md),
[testing](../../../../spec/testing.md)); the streaming model
([source contract](../../../source/spec/source-contract.md)) for the input;
[clocks and pacing](../../../clock/spec/pacing.md); and the generic seams the
output is built on, the [output base](../../../outputs/spec/output.md) and the
[encoder interface](../../../encoder/spec/encoder.md). A rule of those levels
is linked here, not restated.

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
start/stop machinery ([output.md](../../../outputs/spec/output.md)), linked
here where SRT relies on it.

**Compatibility.** The script interface (parameters, defaults, methods,
callbacks and their arguments) and the setting names are kept; a behaviour
behind them changes only where this spec gives the reason next to the rule.
The bytes on the wire may change, as long as SRT compliance is kept or
improved.

**Logs and messages.** Log levels are named as in
[conventions.md §4](../../../../spec/conventions.md#4-logs-and-errors). The
log lines and error messages quoted in this tree give example wording: what
each one says (the cause, the parameter, the address) is binding, its wording
is not. Binding as written are only the values a script reads or matches:
the error kinds `srt` and `json`, the `status()` values and the `on_socket`
modes.

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

The [project principles](../../../../spec/principles.md) hold here. Two of
them shape this tree most: a fault is contained to the smallest thing it
concerns ([faults.md §0](../../../../spec/faults.md#0-the-rule)), for SRT one
peer, one attempt or one operator ([failures.md](failures.md)); and the
streaming thread never waits for a peer, with no user code under an operator
lock ([concurrency.md §2](../../../../spec/concurrency.md#2-the-streaming-thread)),
mapped onto SRT's threads in [concurrency.md](concurrency.md). SRT's own:

- **One predicate drives connections**: a role connects while the operator
  wants a connection and releases everything when it stops wanting one
  ([common.md §5](common.md#5-connection-wanted)).
- **One connection carries one stream**, from its start or its header to
  its end ([output.md §4](output.md#4-encoder)).

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
