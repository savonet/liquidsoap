# `output.srt` at the encoder seam

Part A. Seam view: how `output.srt` answers the caller numbering of
[encoder.md §11](encoder.md#11-caller-files). The operator's rules are its
tree's ([optionals/srt/spec/](../../optionals/srt/spec/README.md), in
particular [output.md](../../optionals/srt/spec/output.md)); the listener
role's sink is the shared stream
([shared-stream.md](../../outputs/spec/shared-stream.md)). This file states
only what binds the seam, and links the rest.

## 1. Formats and declarations

- The factory is looked up at construction; none is an invalid-value error
  on the format argument
  ([srt output.md §2](../../optionals/srt/spec/output.md#2-construction)).
- **Declarations** ([encoder.md §5.1](encoder.md#51-what-an-output-declares)):

  | Role     | Capabilities       | Readers mid-stream |
  | -------- | ------------------ | ------------------ |
  | listener | `fresh`, `live(L)` | yes                |
  | caller   | `fresh`, `live(L)` | no                 |

  `L` is the output's `live_latency_bound`, 1 s as recommended
  ([sink.md §1](sink.md#1-capabilities)). SRT's own latency and
  `max_payload_delay` bound what the transport and the sink hold after the
  writer has written it; neither is `L`.

- **Checks** ([encoder.md §5.3](encoder.md#53-the-check)): in the listener
  role a configuration that is not streamable is refused, the message
  naming an explicit `streamable=true` where the configuration takes it
  (`%ffmpeg` with a muxer the writer does not know, `%external`); in both roles a
  configuration that needs a seekable destination or no byte sink is
  refused, and an explicit preamble is refused. Left at its default, the
  writer drops its preambles by itself, so no peer receives an ID3v2 tag
  or an encoder-information frame
  ([encoder.md §3.5](encoder.md#35-header-forms-and-preambles)).

## 2. The sink

- **Listener**: the shared stream, with no history (`B = 0`): a peer starts
  at the first candidate written after it joined
  ([shared-stream.md §3](../../outputs/spec/shared-stream.md#3-starting-a-reader)).
  SRT keeps its peers, payload cutting, delay bound and per-peer queues,
  each bounded by `peer_backlog_limit`
  ([srt output.md §5](../../optionals/srt/spec/output.md#5-sending)).
- **Caller**: one sink per peer connection, serving it from byte 0, with
  the same payload cutting and bounds.
- A peer past its bound, or whose send fails, is that peer's fault; it never
  reaches the writer ([sink.md §7](sink.md#7-faults)).

## 3. Writer lifetime

- **Listener**: a writer runs while the output has at least one peer. The
  first piece that finds a peer and no writer builds one; when the last
  peer ends by failure, the writer is ended `abandoned`; when the output
  ends its peers itself, it is ended `clean` (§6). Peers that join a
  running writer start in its stream (§5).
- **Caller**: one writer per peer connection
  ([encoder.md §2.1](encoder.md#21-one-writer-per-destination-stream)).
- With no writer, pieces are not encoded and metadata becomes the current
  metadata ([output-encoded.md §3.1](output-encoded.md#31-current-metadata)).
- **Initial metadata**: [output-encoded.md §3.2](output-encoded.md#32-initial-metadata).
  The sink is live: it becomes tags only in Ogg comment headers, the
  in-band channel ([encoder.md §3.5](encoder.md#35-header-forms-and-preambles),
  item 7).

## 4. Pieces and metadata

Pieces are handed metadata first
([output.md §3](../../outputs/spec/output.md#3-a-streaming-cycle)). SRT has
no out-of-band channel: each metadata is offered in band to a writer that
offers it ([encoder.md §9](encoder.md#9-in-band-metadata)); there is no
choice to give the script.

## 5. Markers and readers

- **Listener**: the shared stream records stream starts, access points and
  `end`; a joining peer is sent the recorded entry header, then the stream
  from its start. The output never asks a writer for a header or a restart
  position ([shared-stream.md](../../outputs/spec/shared-stream.md)).
- **Caller**: the sink ignores every marker but `end`.
- Payloads are counted from each peer's first byte, the entry header
  included ([srt output.md §10](../../optionals/srt/spec/output.md#10-wire-bytes)).

## 6. End

When the output ends its peers itself, the writer is ended `clean` by the
output's streaming work, as part of the output's stop
([output.md §2](../../outputs/spec/output.md#2-states)); its final bytes
are appended to every peer's stream and delivered within `write_timeout`,
or `shutdown_drain` when that is null
([srt output.md §6](../../optionals/srt/spec/output.md#6-stopping)): that
is this output's `final_delivery_bound`
([sink.md §4](sink.md#4-markers-and-the-end)). The
drain runs on the peers' work and never holds the streaming thread.

## 7. Reopen, reconnect, append

A caller that reconnects gets a new writer: a fresh stream from its first
byte. A listener peer that joins a running writer starts mid-stream (§5).
Append: nothing.

## 8. Faults and restart policy

- Item faults are the writer's ([encoder.md §8](encoder.md#8-faults)).
- An output fault of the writer ends it `abandoned` and ends every peer it
  serves as a peer failure; the role then behaves as after peer failures
  ([srt connection.md §7](../../optionals/srt/spec/connection.md#7-peer-failure)):
  the caller makes a new attempt, the listener keeps accepting. A writer is
  built again no sooner than `fault_retry_delay` after the fault
  ([faults.md §3](../../../spec/faults.md#3-source-faults-reach-the-script)).
- Configuration errors the writer reports at creation are hard errors
  ([encoder.md §8](encoder.md#8-faults)).

## 9. Concurrency

Only the output's streaming work calls a writer
([encoder.md §7](encoder.md#7-concurrency)), its clean end included. A
joining peer is posted to the streaming work; what it is sent first comes
from the shared stream's records. Sends run on the peers' work
([srt concurrency.md](../../optionals/srt/spec/concurrency.md)).

## 10. Tests

[srt conformance.md](../../optionals/srt/spec/conformance.md) and
[shared-stream.md §7](../../outputs/spec/shared-stream.md#7-conformance).
Seam-binding:

- a listener of a non-streamable configuration is refused at evaluation;
- a `%ffmpeg` mp3 stream: neither the first peer nor a late one receives an
  ID3v2 tag;
- a late peer of `%ffmpeg` Matroska with AAC, no metadata change, decodes
  from its first byte;
- a clean stop: each peer's last bytes are the writer's final bytes, and
  the stop returns while a peer that stopped reading drains;
- the writer's `end` is called from the streaming work.
