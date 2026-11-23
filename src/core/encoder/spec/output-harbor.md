# `output.harbor` at the encoder seam

Part A. Seam view: how `output.harbor` answers the caller numbering of
[encoder.md §11](encoder.md#11-caller-files). The operator's rules are its
tree's ([outputs/spec/harbor/](../../outputs/spec/harbor/README.md)); the
shared mode's sink is the shared stream
([shared-stream.md](../../outputs/spec/shared-stream.md)). This file states
only what binds the seam, and links the rest.

## 1. Formats and declarations

- The factory is looked up at construction; none is an invalid-value error
  on the format argument
  ([harbor operator.md §2](../../outputs/spec/harbor/operator.md#2-validation-at-construction)).
- The content type is the `format` parameter when non-empty, else the
  format's `mime` helper ([formats.md §9](formats.md#9-helpers)); with
  neither, an invalid-value error on `format`, as for the source client
  ([output-icecast.md §1](output-icecast.md#1-formats-and-declarations),
  item 4).
- **Declarations** ([encoder.md §5.1](encoder.md#51-what-an-output-declares)):

  | Mode                                 | Capabilities       | Readers mid-stream |
  | ------------------------------------ | ------------------ | ------------------ |
  | shared (default)                     | `fresh`, `live(L)` | yes                |
  | dedicated (`dedicated_encoder=true`) | `fresh`, `live(L)` | no                 |

  `L` is the output's `live_latency_bound`
  ([sink.md §1](sink.md#1-capabilities)).

- **Checks** ([encoder.md §5.3](encoder.md#53-the-check)): in shared mode a
  configuration that is not streamable is refused, and the message names
  `dedicated_encoder=true` and, for `%ffmpeg` with a muxer the writer does
  not know and for `%external`, an explicit `streamable=true` as the
  alternatives. In both modes a
  configuration that needs a seekable destination or no byte sink is
  refused, and an explicit preamble is refused. The output never rewrites
  the format.

## 2. The sink

- **Shared**: the shared stream, one per output, from construction to end
  ([shared-stream.md §1](../../outputs/spec/shared-stream.md#1-model)).
  Harbor keeps its store, its window (`[N − buffer, T)`), its listeners and
  their backlog bound `buffer`
  ([harbor shared-encoder.md](../../outputs/spec/harbor/shared-encoder.md)).
- **Dedicated**: one sink per listener, serving it from byte 0: the
  listener's queue, bounded by `buffer`
  ([harbor dedicated-encoder.md](../../outputs/spec/harbor/dedicated-encoder.md)).
- A listener past its bound is that listener's fault; it never reaches the
  writer ([sink.md §7](sink.md#7-faults)). Delivery never runs on the
  streaming thread ([harbor delivery.md](../../outputs/spec/harbor/delivery.md)).

## 3. Writer lifetime

- **Shared**: one writer per run, built at the output's start, ended at its
  stop ([harbor modes.md §7–§8](../../outputs/spec/harbor/modes.md#7-start)).
  It writes whether or not anyone listens.
- **Dedicated**: one writer per listener, built at its admission and ended
  at the output's stop or the listener's teardown. No listener, no writer.
- **Initial metadata**: [output-encoded.md §3.2](output-encoded.md#32-initial-metadata).
  The sink is live: it becomes tags only in Ogg comment headers, when
  `in_band_metadata` is true
  ([encoder.md §3.5](encoder.md#35-header-forms-and-preambles), item 7).

## 4. Pieces and metadata

- Pieces are handed metadata first
  ([output.md §3](../../outputs/spec/output.md#3-a-streaming-cycle)).
- **Out of band**: ICY blocks, negotiated and stamped per listener
  ([harbor icy.md](../../outputs/spec/harbor/icy.md)). The writer is never
  told about them.
- **In band**: each metadata is offered to every running writer that offers
  in-band metadata ([encoder.md §9](encoder.md#9-in-band-metadata)), unless
  `in_band_metadata` (bool, default `true`) is false. Reason: listeners of
  a native Ogg stream get titles in band, which players that cannot follow
  a chained stream handle badly; `false` gives them one logical stream and
  leaves titles to ICY.
- The values offered are the converted ones, cover art removed
  ([harbor modes.md §6](../../outputs/spec/harbor/modes.md#6-metadata)).

## 5. Markers and readers

- **Shared**: the shared stream records the writer's stream starts, access
  points and end; a listener starts at a candidate and is sent the
  recorded entry header
  ([shared-stream.md §3](../../outputs/spec/shared-stream.md#3-starting-a-reader)).
  Harbor never asks a writer for a header or a restart position.
- **Dedicated**: the sink ignores every marker but `end`; a listener's
  stream starts at its writer's first byte, which begins with the header.

## 6. End

A clean end appends the writer's final bytes behind what every listener
already owes, and the last delivery pass sends them within its bound, this
output's `final_delivery_bound` ([sink.md §4](sink.md#4-markers-and-the-end))
([harbor modes.md §11](../../outputs/spec/harbor/modes.md#11-final-bytes),
[delivery.md §7](../../outputs/spec/harbor/delivery.md#7-stopping)). A
dedicated listener's teardown ends its writer `abandoned`.

## 7. Reopen, reconnect, append

No reconnection: listeners come and go. A new run gets a new writer;
listeners kept across runs (`keep_listeners`) continue with the new
writer's stream start and header
([shared-stream.md §4](../../outputs/spec/shared-stream.md#4-writer-changes)).
Append: nothing.

## 8. Faults and restart policy

- Item faults are the writer's ([encoder.md §8](encoder.md#8-faults)).
- An output fault of the shared writer ends it `abandoned`, logged at
  `severe`; the output keeps its listeners and builds a new writer from the
  first piece received `fault_retry_delay` after the fault
  ([faults.md §3](../../../spec/faults.md#3-source-faults-reach-the-script)
  names the parameter); the pieces in between are dropped. Listeners
  continue with the new writer's stream start.
- An output fault of a dedicated listener's writer is that listener's
  fault: the listener is closed, the others go on.
- Listener and request faults are the tree's
  ([harbor delivery.md §6](../../outputs/spec/harbor/delivery.md#6-disconnection),
  [connection.md §7](../../outputs/spec/harbor/connection.md#7-failure-paths)).

## 9. Concurrency

Only the output's streaming work calls a writer, in both modes
([encoder.md §7](encoder.md#7-concurrency)). An admission never calls a
writer: in shared mode it reads the shared stream's records; in dedicated
mode it posts the new listener, and the streaming work builds its writer
before the listener's first piece. The per-listener encoding of dedicated
mode MAY be spread over parallel work, each writer still called by one task
at a time and in piece order
([harbor concurrency.md](../../outputs/spec/harbor/concurrency.md)).

## 10. Tests

[harbor tests.md](../../outputs/spec/harbor/tests.md) and
[shared-stream.md §7](../../outputs/spec/shared-stream.md#7-conformance).
Seam-binding:

- a shared mount of a non-streamable configuration (plain MP4) is refused
  at evaluation, naming `dedicated_encoder=true`; `%external` with
  `streamable=true` emitting ADTS is accepted, and a late listener
  decodes;
- a late listener of `%ffmpeg` Matroska with AAC, no metadata change,
  decodes from its first byte;
- an Ogg listener with `in_band_metadata=false` receives one logical
  stream across three metadata changes, and ICY titles when it asked for
  them;
- an `%mp3` listener receives no ID3v2 tag and no encoder-information frame;
- a clean stop delivers the end-of-stream pages to every listener;
- a dedicated listener's writer receives its first piece from the
  streaming work, never from the admission.
