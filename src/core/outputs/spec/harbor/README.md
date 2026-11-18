# `output.harbor` — specification

What `output.harbor` MUST, SHOULD and MAY do, stated closely enough to build
it without any other source. It is written for someone implementing the
operator in OCaml inside Liquidsoap, on the same scheduler, clock, encoder and
harbor server. It sits under the [project specifications](../../../../spec/README.md)
and the [outputs specification](../README.md): what every output does is the
output base's ([output.md](../output.md)), the encoder interface is
[encoder.md](../../../encoder/spec/encoder.md), and this tree states what is
specific to harbor.

**Scope.** The `output.harbor` operator: its script interface, how it accepts
listeners on the harbor server, how it encodes and delivers the stream to each
listener, ICY metadata, and its lifecycle.

**Out of scope, stated only as contracts** ([contracts.md](contracts.md)):
the harbor HTTP server shared with `input.harbor` and `harbor.http.*`; the
MIME derivation shared with `output.icecast`; and what harbor needs of the
output base, the encoders and the scheduler beyond their own specifications. The spec states what it
requires of them; a component that falls short is where the fix belongs. The
`%ffmpeg` specifics of the MIME and mp3 options are owned by the FFmpeg spec,
[io.md §12](../../../optionals/ffmpeg/spec/io.md#12-other-outputs-special-casing-ffmpeg).
`output.harbor.hls` is a script-level user and is not covered.

**`icecast.server`** is a primary user: it serves every listener of every
mount through `output.harbor`. What it relies on, and what emulating Icecast
asks of it, is in [icecast-server.md](icecast-server.md); its tests are
acceptance checks.

## Design goals

- **Blends into the scheduler**, under the project's work classes
  ([concurrency.md](../../../../spec/concurrency.md#1-work)); the operator's
  work is listed in [concurrency.md §1](concurrency.md#1-work).
- **Fast I/O.** Delivery is non-blocking, short tasks that never park a
  domain ([delivery.md §1](delivery.md#1-delivery-groups)).
- **Minimal memory and CPU, low latency.** Per-listener memory independent of
  uptime and stream length, idle cost independent of the machine, bounded
  time to first byte ([delivery.md](delivery.md)).

## Reading order

| #   | Document                                     | Owns                                                                                         |
| --- | -------------------------------------------- | -------------------------------------------------------------------------------------------- |
| 1   | [operator.md](operator.md)                   | Parameters, validation, methods, callbacks, the listener record, settings, lifecycle         |
| 2   | [connection.md](connection.md)               | Routing, authentication, the admission predicate, the response head, admission, refusals     |
| 3   | [icy.md](icy.md)                             | ICY negotiation, block format, title composition, stamping and change detection              |
| 4   | [delivery.md](delivery.md)                   | Delivery groups, wake-ups, the delivery pass, timeout, disconnection, stopping, socket close |
| 5   | [modes.md](modes.md)                         | The encoder-mode contract, §1–§11, and the overflow rule                                     |
| 6   | [shared-encoder.md](shared-encoder.md)       | The shared mode, numbered as modes.md: one encoder, a stream store, bursts                   |
| 7   | [dedicated-encoder.md](dedicated-encoder.md) | The dedicated mode, numbered as modes.md: one encoder per listener                           |
| 8   | [ring.md](ring.md)                           | The shared stream store: no blocking, no torn bytes, retention, bounded memory               |
| 9   | [concurrency.md](concurrency.md)             | Work and its classes, ownership of a listener, ordering and exactly-once guarantees          |
| 10  | [contracts.md](contracts.md)                 | What the operator requires of the harbor server, encoders, output base, scheduler            |
| 11  | [icecast-server.md](icecast-server.md)       | What `icecast.server` does with the operator and relies on                                   |
| 12  | [tests.md](tests.md)                         | Conformance checks by principle: binding and incidental parts, doubles, fault injection      |
| 13  | [known-complexity.md](known-complexity.md)   | Pitfalls from the history, implementation-free, each answered by a rule above                |

[modes.md](modes.md) is a seam: [shared-encoder.md](shared-encoder.md) and
[dedicated-encoder.md](dedicated-encoder.md) answer every clause of it under
the same section number, as [conventions.md §3](../../../../spec/conventions.md#3-documents-of-a-tree)
asks of every seam.

## Cross-cutting concerns

- **Concurrency and ownership**: [concurrency.md](concurrency.md). Several
  kinds of work touch a listener, and the exactly-once callback and close
  guarantees depend on who may do what.
- **Failure classification** follows [faults.md](../../../../spec/faults.md)
  (a listener fault is an output fault of one peer) and lives with the rules
  it governs: listener faults
  in [delivery.md §6](delivery.md#6-disconnection), request faults in
  [connection.md §7](connection.md#7-failure-paths), construction errors in
  [operator.md §2](operator.md#2-validation-at-construction).
- **Crash safety, persistence, replication**: none. The operator holds no
  state that outlives the process.
- **Security model**: authentication and the admission predicate in
  [connection.md §3–§4](connection.md#3-authentication), header validation in
  [operator.md §2](operator.md#2-validation-at-construction), address
  exposure in [operator.md §5](operator.md#5-the-listener-record), and the
  server's handling of unsupported versions in
  [contracts.md §1](contracts.md#1-harbor-server).

## Two parts

**Part A** is the documents above. **Part B** is
[language-notes/ocaml.md](language-notes/ocaml.md)
([conventions.md §1](../../../../spec/conventions.md#1-normative-language)).

## Compatibility surface

Frozen; a rebuild keeps it unchanged:

- every parameter's name, type and default ([operator.md §1](operator.md#1-parameters));
- the `listeners` method, the methods of every output, and the `on_connect` /
  `on_disconnect` callbacks ([operator.md §3–§4](operator.md#3-methods));
- the listener record: every field keeps its name, type and meaning; fields
  MAY be added ([operator.md §5](operator.md#5-the-listener-record)).

Not frozen: the bytes on the wire, which stay valid for Icecast- and
Shoutcast-style players ([connection.md §5](connection.md#5-the-response-head),
[icy.md §2](icy.md#2-block-format)). There is no persisted state.
