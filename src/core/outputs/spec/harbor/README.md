# `output.harbor` — as-built specification

What `output.harbor` does today, described closely enough to rebuild it
without reading the code. It is written for someone rewriting the operator in
OCaml inside Liquidsoap, on the same scheduler, clock, encoder and harbor
server.

This is the **as-built** pass: it describes and does not judge. Defects,
asymmetries and gaps found while reading are in [findings.md](findings.md),
never in the descriptive text.

**Scope.** The `output.harbor` operator: its script interface, how it accepts
listeners on the harbor server, how it encodes and delivers the stream to each
listener, ICY metadata, and its lifecycle.

**Out of scope, described only as contracts** ([contracts.md](contracts.md)):
the harbor HTTP server shared with `input.harbor` and `harbor.http.*`; the
MIME derivation shared with `output.icecast`; encoders; the generic output
base and its start/stop machinery; the scheduler. The `%ffmpeg` specifics of
the MIME and mp3 options are owned by the FFmpeg spec,
[io.md §12](../../../optionals/ffmpeg/spec/io.md#12-other-outputs-special-casing-ffmpeg).
`output.harbor.hls` is a script-level user and is not covered.

**`icecast.server`** is a primary user: it serves every listener of every
mount through `output.harbor`. What it relies on is stated in
[icecast-server.md](icecast-server.md), and a rebuild must keep it working.

## Reading order

| #   | Document                                     | Owns                                                                                             |
| --- | -------------------------------------------- | ------------------------------------------------------------------------------------------------ |
| 1   | [operator.md](operator.md)                   | Parameters, validation, methods, callbacks, the listener record, settings, lifecycle             |
| 2   | [connection.md](connection.md)               | Routing a request to the output, authentication, rejections, the response head, admission        |
| 3   | [icy.md](icy.md)                             | ICY negotiation, block format, title composition, cadence and change detection                   |
| 4   | [delivery.md](delivery.md)                   | Writer shards, wake-ups, the flush pass, backpressure, the no-progress timeout, teardown         |
| 5   | [modes.md](modes.md)                         | The encoder-mode contract: what each mode must provide, §1–§11                                   |
| 6   | [shared-encoder.md](shared-encoder.md)       | The shared mode, numbered as modes.md: one encoder, a ring, bursts                               |
| 7   | [dedicated-encoder.md](dedicated-encoder.md) | The dedicated mode, numbered as modes.md: one encoder per listener                               |
| 8   | [ring.md](ring.md)                           | The single-writer, lock-free-reader byte ring the shared mode reads from                         |
| 9   | [concurrency.md](concurrency.md)             | Actors, what each owns, the listener lifecycle, ordering and exactly-once guarantees             |
| 10  | [contracts.md](contracts.md)                 | What the operator relies on from the harbor server, encoders, output base, scheduler             |
| 11  | [tests.md](tests.md)                         | The suite by principle: invariants, binding and incidental parts, doubles                        |
| 12  | [icecast-server.md](icecast-server.md)       | What `icecast.server` does with the operator and relies on: parameters, lifecycle, record fields |
| 13  | [findings.md](findings.md)                   | Defects, asymmetries, gaps and open questions, each checked or read only                         |
| 14  | [known-complexity.md](known-complexity.md)   | Pitfalls from the history, implementation-free, each reconciled with the rules above             |

[modes.md](modes.md) is a seam: [shared-encoder.md](shared-encoder.md) and
[dedicated-encoder.md](dedicated-encoder.md) answer every clause of it under
the same section number.

## Cross-cutting concerns

Each candidate was asked whether a rebuild would get it wrong without one
stated rule.

- **Concurrency and ownership** earns a document,
  [concurrency.md](concurrency.md): four actors touch a listener, and the
  exactly-once callback and close guarantees depend on who may do what.
- **Failure classification** is small enough to live with the rules it
  governs: listener faults in [delivery.md §6](delivery.md#6-disconnection),
  request faults in [connection.md §6](connection.md#6-failure-paths),
  construction errors in [operator.md §2](operator.md#2-validation-at-construction).
- **Crash safety, persistence, replication**: none. The operator holds no
  state that outlives the process.
- **Security model**: authentication and address exposure are in
  [connection.md §3](connection.md#3-authentication) and
  [operator.md §5](operator.md#5-the-listener-record); there is no other
  trust boundary inside the operator.

## Two parts

**Part A** is the documents above. **Part B**,
[language-notes/ocaml.md](language-notes/ocaml.md), maps them onto the OCaml
implementation: the class structure, the atomics and the scheduler idioms.
Part A is complete without it.

## Compatibility surface

What a script or a listener can observe today:

- the operator name, every parameter name, type and default
  ([operator.md §1](operator.md#1-parameters));
- the `listeners` method and the `on_connect` / `on_disconnect` callbacks with
  the listener record fields ([operator.md §3–§5](operator.md#3-methods));
- `settings.harbor.output.writers`;
- the bytes on the wire: the response head
  ([connection.md §4](connection.md#4-the-response-head)) and the ICY block
  format ([icy.md §2](icy.md#2-block-format)).

There is no persistent state: nothing on disk is read back.
