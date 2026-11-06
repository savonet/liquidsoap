# SRT input and output — as-built specification

**As built.** What `input.srt`, `output.srt`, `srt.socket` and the
`settings.srt.*` settings do today, read from the code at `main`
`c89c6beea`. It describes and does not judge: defects, asymmetries and gaps
found while reading are in [findings.md](findings.md), not in the text.

**Reader.** Someone rebuilding the two operators inside Liquidsoap. Sources,
clocks, encoders, decoders, the scheduler and the script runtime exist and are
used through the contracts stated here. libsrt is used through the binding
contract of [library.md §6](library.md#6-what-is-relied-upon-from-the-binding).

**Scope.** In: both operators in both connection roles, the shared readiness
loop, the library lifecycle and settings, the script socket value. Out: libsrt
itself, the encoders and the stream decoders
([FFmpeg layer spec](../../ffmpeg/spec/README.md)), clocks and sync sources
([clock spec](../../../clock/spec/README.md)), the generic output and
start/stop machinery, described here only as far as SRT relies on it.

## Seams

Two seams, each with a contract file and one file per implementation.

- **Connection role**: [connection.md](connection.md) is the contract;
  [caller.md](caller.md) and [listener.md](listener.md) implement it with the
  same section numbers, so "§7" is peer failure in all three.
- **Operator**: [common.md](common.md) is what both operators share;
  [input.md](input.md) and [output.md](output.md) are the operators.

An operator is one role plus one operator kind, chosen by the `mode`
parameter at construction: four combinations, all exist.

## Reading order

| File                                       | Content                                                                   |
| ------------------------------------------ | ------------------------------------------------------------------------- |
| [library.md](library.md)                   | libsrt startup, cleanup, logging, settings, shutdown, address resolution. |
| [poll.md](poll.md)                         | The process-wide readiness loop.                                          |
| [common.md](common.md)                     | Shared parameters and validation, callbacks, methods, the stop flag.      |
| [connection.md](connection.md)             | The connection-role contract.                                             |
| [caller.md](caller.md)                     | The caller role.                                                          |
| [listener.md](listener.md)                 | The listener role.                                                        |
| [output.md](output.md)                     | `output.srt`: lifecycle, encoder, chunking, sending, wire bytes.          |
| [input.md](input.md)                       | `input.srt`: lifecycle, decoder, reading, track boundaries, pacing.       |
| [socket-value.md](socket-value.md)         | `srt.socket`: options, statistics, status.                                |
| [concurrency.md](concurrency.md)           | Threads, the source lock, which callback runs where.                      |
| [failures.md](failures.md)                 | Every failure and what an observer sees.                                  |
| [conformance.md](conformance.md)           | The test suite by principle; what is binding, what is incidental.         |
| [known-complexity.md](known-complexity.md) | What the history shows to be hard, each entry reconciled with this spec.  |
| [ocaml-notes.md](ocaml-notes.md)           | Part B: what is specific to the OCaml implementation.                     |
| [findings.md](findings.md)                 | Defects, asymmetries, gaps, claims to verify.                             |

Every file but [ocaml-notes.md](ocaml-notes.md) is Part A: behaviour any
runtime could meet.

## Cross-cutting concerns

- **Concurrency** earns a document: five kinds of thread touch one operator,
  and user callbacks run under a lock that is not reentrant
  ([concurrency.md](concurrency.md)).
- **Failure classification** earns a document: the same kind of error stops
  the process in one place, retries forever in another and ends a track in a
  third ([failures.md](failures.md)).
- **Security** does not: encryption is a set of socket options
  ([common.md §2](common.md#2-encryption)), admission is a predicate
  ([listener.md §3](listener.md#3-socket-setup)).
- **Crash safety and persistence** do not: nothing is written to disk except
  the optional input dump, and no state survives a restart.
- **Data layout** does not: the only format is the byte stream carried in SRT
  messages ([output.md §10](output.md#10-wire-bytes)).
