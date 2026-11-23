# `output.srt`

Encodes a source and sends the bytes in SRT messages, as a caller to one
listener or as a listener to every accepted caller.

## 1. Interface

`output.srt(<common>, max_clients, max_payload_delay, start, fallible, register_telnet, id,
format, source)`.

| Parameter                              | Type    | Default | Meaning                                                                             |
| -------------------------------------- | ------- | ------- | ----------------------------------------------------------------------------------- |
| [common.md §1](common.md#1-parameters) |         |         | `mode` defaults to `"caller"`.                                                      |
| `max_clients`                          | int?    | null    | Client limit of the listener role, at least 1; ignored by the caller.               |
| `max_payload_delay`                    | float?  | `0.1`   | Longest a byte waits for a full payload, in seconds of stream (§5); null: no limit. |
| `start`                                | bool    | `true`  | Start as soon as the source is available.                                           |
| `fallible`                             | bool    | `false` | Accept a fallible source.                                                           |
| `register_telnet`                      | bool    | `true`  | Register the generic server commands.                                               |
| `id`                                   | string? | null    | Operator id.                                                                        |
| (unlabeled)                            | format  | —       | The encoder.                                                                        |
| (unlabeled)                            | source  | —       | The source.                                                                         |

The value returned carries the methods and callbacks of
[common.md §3–§4](common.md#3-callbacks) and the generic output ones.

## 2. Construction

1. Validate the common parameters ([common.md §1](common.md#1-parameters))
   and `max_clients`. `max_payload_delay` MUST be a non-negative number (NaN is
   refused): invalid-value error, example wording
   `Maximum payload delay must be a non-negative number!`.
2. Look up a writer factory for the format
   ([encoder.md §1](../../../encoder/spec/encoder.md#1-registry)). None found:
   invalid-value error on the format, example wording
   `Cannot get a stream encoder for that format`.
3. Declare the role's sink capabilities and whether readers start
   mid-stream, and check the format against the configuration's
   declarations, without building a writer
   ([encoder.md §5](../../../encoder/spec/encoder.md#5-capabilities-and-the-checks-at-evaluation)).
   The declarations and the checks are
   [output-srt.md §1](../../../encoder/spec/output-srt.md#1-formats-and-declarations).
   In the listener role, a configuration that is not streamable is refused:
   an invalid-value error on the format whose message names an explicit
   `streamable=true` where the configuration takes it. Reason: a listener's
   peers start mid-stream, at an access point.
4. Build the operator with the role `mode` names. The generic output checks
   that a non-fallible output has an infallible source
   ([output.md §1](../../../outputs/spec/output.md#1-common-parameters-methods-and-callbacks)).

## 3. Lifecycle

The generic output drives two operations, each called on the streaming
thread:

- **start**, when the output enters the started state: clear the hold
  ([common.md §5](common.md#5-connection-wanted)); the role connects.
- **stop**, when it leaves it, towards stopped or idle: the role releases
  ([connection.md §8](connection.md#8-disconnect)), ending every peer
  cleanly (§6).

A **reset** is the generic one
([output.md §4](../../../outputs/spec/output.md#4-reset)): stop then start.
Afterwards the output is connecting, exactly as after a first start.

The generic output's states
([output.md §2](../../../outputs/spec/output.md#2-states)) make a fallible
output fall back to idle when its source ends: it
therefore ends its peers each time its source becomes unavailable, and
connects again when it comes back: one peer connection carries one stream,
from its start to its end.

## 4. Encoder

The output drives a
[container writer](../../../encoder/spec/encoder.md#3-the-stream-face). Its
lifetime per role, its initial metadata and its markers are
[output-srt.md §3–§5](../../../encoder/spec/output-srt.md#3-writer-lifetime);
this section states what SRT adds.

- **Listener.** The sink is the
  [shared stream](../../../outputs/spec/shared-stream.md). A new peer
  starts with no history (`B = 0`,
  [shared-stream.md §3](../../../outputs/spec/shared-stream.md#3-starting-a-reader)):
  its stream begins at the first stream start or access point the writer
  marks after the peer was admitted, preceded by the entry header the shared
  stream recorded for it, none at a stream start. The output never asks the
  writer for a header or a position. A peer admitted with no writer running
  starts at the next writer's first stream start.
- **Caller.** One writer per peer connection, its sink serving the peer
  from byte 0.
- **Ending.** When the last peer ends by failure, the writer is ended
  `abandoned`. When the output ends its peers itself, §6 applies. Every
  writer is ended exactly once
  ([encoder.md §2.3](../../../encoder/spec/encoder.md#23-states)).

Writing runs on the output's streaming work, and only while a writer runs
or a peer waits for its stream to start. Otherwise a frame is not encoded
and its bytes do not exist; metadata becomes the current metadata
([output-encoded.md §3.1](../../../encoder/spec/output-encoded.md#31-current-metadata)).
Initial metadata is
[encoder.md §2.2](../../../encoder/spec/encoder.md#22-creation)'s. Each
frame is handed to the writer piece by piece, metadata first
([output.md §3](../../../outputs/spec/output.md#3-a-streaming-cycle)), and
the resulting bytes go to every peer's stream (§5).

The writer reports typed faults
([encoder.md §8](../../../encoder/spec/encoder.md#8-faults)). An output
fault of the writer ends it `abandoned` and ends every peer it serves as a
peer failure ([output-srt.md §8](../../../encoder/spec/output-srt.md#8-faults-and-restart-policy)).

## 5. Sending

Each peer has its own **stream**: the bytes it is owed, from its first byte
(§4), the entry header included. The stream is cut into **payloads** of exactly `payload_size` bytes,
counted from the peer's first byte, and again from the byte after each
shorter payload. Each payload is sent as one message;
the library refuses a message larger than `payload_size`. Fewer than `payload_size`
bytes wait for more, until one of:

- a clean end (§6);
- **the delay bound**: the oldest waiting byte was encoded `max_payload_delay`
  seconds of stream ago or more. Everything waiting is then sent as one
  payload shorter than `payload_size`, and counting starts again from the
  next byte.

Stream time is the duration the writer has been given since it was built,
measured at the end of each piece of a frame it encodes. The bound is
checked for every peer each time a piece is encoded, whether or not the
writer produced bytes for it, so a stretch where the writer produces
little or nothing still releases what it produced. Recommended value: 0.1
second.

The bound exists because a low-bitrate stream fills a payload slowly: an
encoder that compresses silence to a few hundred bytes per second takes
seconds to fill 1316 bytes, longer than a receiver waits for data. It is
measured in stream time because the receiver consumes the stream at the
stream's pace. A stream that steadily produces more than
`payload_size / max_payload_delay` bytes per second of stream (about 105
kbit/s with the defaults) fills each payload before the bound.

**The streaming thread never waits for a peer**
([concurrency.md §2](../../../../spec/concurrency.md#2-the-streaming-thread)).
It hands each peer's
payloads to that peer's queue and goes on; payloads leave the queue as the
library accepts them, in order. For a live peer, no payload is skipped,
reordered or split across messages.

A peer **fails** ([connection.md §7](connection.md#7-peer-failure)):

- when a send to it fails;
- when it has queued bytes and the library accepts none of them for
  `write_timeout`;
- when its queue exceeds `peer_backlog_limit` bytes.

`peer_backlog_limit` protects the process's memory against a peer that
accepts data too slowly. Recommended value: 8 MiB. A failing peer affects no
other peer: their streams continue unchanged.

## 6. Stopping

When the output ends its peers itself (stop, `disconnect()`, reset,
shutdown):

1. The output's streaming work ends the writer, if one runs, `clean`
   ([encoder.md §3.6](../../../encoder/spec/encoder.md#36-draining-and-final-bytes)),
   and appends its final bytes to every peer's stream. `disconnect()`, a
   server command or shutdown posts a request that the streaming work
   carries out between pieces
   ([encoder.md §7](../../../encoder/spec/encoder.md#7-concurrency)); no
   other thread calls the writer. A failure of the clean end is logged at
   `severe` and the end goes on
   ([encoder.md §3.6](../../../encoder/spec/encoder.md#36-draining-and-final-bytes));
   the peers are ended anyway.
2. Send each peer's remaining bytes, the last payload shorter than
   `payload_size` if need be.
3. Close each peer once the library has sent everything queued for it and
   the peer's latency has passed since, or after `write_timeout`, whichever
   comes first; then `on_disconnect` fires for it. The receiver holds data
   back for timestamp-based delivery during its latency, and a close that
   arrives first makes it drop what it has not delivered yet. At shutdown,
   with `write_timeout` null, the wait is bounded by `shutdown_drain`, so
   that a peer that stops reading cannot hold the process
   ([connection.md §9](connection.md#9-stopping)). Recommended value: 2
   seconds. This bound is the output's `final_delivery_bound`
   ([sink.md §4](../../../encoder/spec/sink.md#4-markers-and-the-end)).

Steps 2 and 3 run on the peers' work and do not hold the streaming thread:
a start that follows at once (a reset) proceeds while the old peers drain.

## 7. Reactions to peers

The output's own reactions ([connection.md §5](connection.md#5-connected),
[§8](connection.md#8-disconnect)):

- on a new peer: it waits for its stream to start at the next candidate
  (§4);
- on a peer ending: its stream and queue are dropped; if it was the last
  peer and §6 has not already ended the writer, the writer is ended (§4).

## 8. Pacing

As every output ([output.md §5](../../../outputs/spec/output.md#5-sync-and-pacing)),
it is not a sync source and reports its source's self-sync as its own: its
sends never block on a timer.

## 9. Methods and callbacks

[common.md §3–§4](common.md#3-callbacks), plus the generic output's
([output.md §1](../../../outputs/spec/output.md#1-common-parameters-methods-and-callbacks)):
`on_start`, `on_stop`, `on_output`, the start/stop methods, and, with
`register_telnet`, the server commands `start`, `stop`, `status`, `skip`,
`seek`, `metadata`, `remaining`.

## 10. Wire bytes

A peer receives the writer's bytes, from its start (§4), preceded by its
entry header, concatenated and cut into SRT payloads of exactly `payload_size` bytes. A
cut falls where the count says, whatever the writer's own packet
boundaries. A payload is shorter only when the delay bound releases it (§5)
or at a clean end. Receivers
SHOULD accept payloads of any size up to the library's largest live payload.

Example, `payload_size = 1316`, one peer from the writer's first byte:

| Writer output              | Pending after | Sent                                     |
| -------------------------- | ------------- | ---------------------------------------- |
| 500 bytes                  | 500           | nothing                                  |
| 1000 bytes                 | 184           | one payload: stream bytes 0–1315         |
| 2448 bytes                 | 0             | two payloads: bytes 1316–2631, 2632–3947 |
| clean end, 40-byte trailer | 0             | one payload of 40 bytes                  |

A second peer whose start is an access point written after the third row,
with a 58-byte entry header, receives the entry header followed by the
bytes from that access point: its payloads are counted from the entry
header's first byte, independently of the first peer's.

With an MPEG-TS writer every payload is seven whole TS packets, since each
peer's stream starts at an access point, MPEG-TS has an empty entry header,
access points lie on a packet boundary, and 1316 is a multiple of 188. A payload the delay bound releases is a whole
number of TS packets too: the bytes waiting are what follows the last full
payload, and the writer writes whole TS packets.

Example, `max_payload_delay = 0.1`, pieces of 0.04 seconds, a writer that
outputs 25 bytes in the pieces ending at 0.08, 0.16 and 0.24 seconds and
nothing in the others:

| Piece ends at | Writer output | Oldest waiting byte | Sent                |
| ------------- | ------------- | ------------------- | ------------------- |
| 0.08          | 25 bytes      | 0.08                | nothing             |
| 0.12          | none          | 0.08 (0.04 old)     | nothing             |
| 0.16          | 25 bytes      | 0.08 (0.08 old)     | nothing             |
| 0.20          | none          | 0.08 (0.12 old)     | one 50-byte payload |
| 0.24          | 25 bytes      | 0.24                | nothing             |

A byte waits at most the bound plus one piece.
