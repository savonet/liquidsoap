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
   and `max_clients`. `max_payload_delay` MUST NOT be negative: invalid-value
   error `Maximum payload delay must not be negative!`.
2. Look up an encoder factory for the format. None found: invalid-value error
   on the format, `Cannot get a stream encoder for that format`.
3. Build the operator with the role `mode` names. The generic output checks
   that a non-fallible output has an infallible source.

## 3. Lifecycle

The generic output drives two operations, each called on the streaming
thread:

- **start**, when the output enters the started state: clear the hold
  ([common.md §5](common.md#5-connection-wanted)); the role connects.
- **stop**, when it leaves it, towards stopped or idle: the role releases
  ([connection.md §8](connection.md#8-disconnect)), ending every peer
  cleanly (§6).

A **reset**, which the clock asks of its outputs after too much latency, is
stop then start, and only while started. Afterwards the output is started and
connecting, exactly as after a first start.

The generic output enters started when its source is ready and it is idle,
and falls back to idle when the source ends a frame early. A fallible output
therefore ends its peers each time its source becomes unavailable, and
connects again when it comes back: one peer connection carries one stream,
from its start to its end.

## 4. Encoder

The output holds at most one encoder, and runs it only while it has at least
one peer to send to.

- **Creating.** The first encode after a peer joins, with no encoder
  running, creates one from the factory with the operator's id and empty
  metadata. Every joining peer's stream starts at the encoder's first byte.
- **Joining.** A peer joins at the next encode while an encoder runs. When
  the bytes that encode returns contain a **restart point** (where the
  encoder starts a new stream link carrying its own headers, as an Ogg
  encoder does after a metadata change), the peer's stream starts there.
  Otherwise it starts with the encoder's separately available header, read
  at that moment so that it belongs to the link in progress, followed by
  those bytes from their start. An encoder with no separate header
  (MPEG-TS) gives the joiner those bytes alone.
- **Ending.** When the last peer ends by failure, the encoder is stopped and
  its final bytes discarded. When the output ends its peers itself, §6
  applies. Every encoder created is stopped exactly once.

Encoding runs on the streaming thread, and only while an encoder runs or a
peer waits to join. Otherwise a frame is not encoded and its bytes do not
exist; metadata is dropped the same way. Encoder errors are the generic
output's to handle.

Each frame is cut at its metadata positions. For each piece, in order: the
metadata starting in it is encoded first (cover art not exported), then the
piece's data, and the resulting bytes go to every peer's stream (§5). The
generic output adds `on_air` and `on_air_timestamp` to each metadata.

## 5. Sending

Each peer has its own **stream**: the bytes it is owed, from its first byte
(§4). The stream is cut into **payloads** of exactly `payload_size` bytes,
counted from the peer's first byte, and again from the byte after each
shorter payload. Each payload is sent as one message;
the library refuses a message larger than `payload_size`. Fewer than `payload_size`
bytes wait for more, until one of:

- a clean end (§6);
- **the delay bound**: the oldest waiting byte was encoded `max_payload_delay`
  seconds of stream ago or more. Everything waiting is then sent as one
  payload shorter than `payload_size`, and counting starts again from the
  next byte.

Stream time is the duration the encoder has been given since it started,
measured at the end of each piece of a frame it encodes. The bound is
checked for every peer each time a piece is encoded, whether or not the
encoder produced bytes for it, so a stretch where the encoder produces
little or nothing still releases what it produced. Recommended value: 0.1
second.

The bound exists because a low-bitrate stream fills a payload slowly: an
encoder that compresses silence to a few hundred bytes per second takes
seconds to fill 1316 bytes, longer than a receiver waits for data. It is
measured in stream time because the receiver consumes the stream at the
stream's pace. A stream that steadily produces more than
`payload_size / max_payload_delay` bytes per second of stream (about 105
kbit/s with the defaults) fills each payload before the bound.

**The streaming thread never waits for a peer.** It hands each peer's
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

1. Stop the encoder, if one runs, and append its final bytes to every
   peer's stream. An error is logged at level 2 with its backtrace as
   `Error while stopping encoder: <exn>`; the peers are ended anyway.
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
   seconds.

Step 1 runs on the thread that ends the peers, under the encoder's lock,
so the trailer is delivered when the streaming thread no longer runs. The
rest does not hold the streaming thread: a start that follows at once (a
reset) proceeds while the old peers drain.

## 7. Reactions to peers

The output's own reactions ([connection.md §5](connection.md#5-connected),
[§8](connection.md#8-disconnect)):

- on a new peer: it waits to join the stream at the next encode (§4);
- on a peer ending: its stream and queue are dropped; if it was the last
  peer and §6 has not already stopped the encoder, the encoder ends (§4).

## 8. Pacing

The output is not a sync source. It reports its source's self-sync as its
own.

## 9. Methods and callbacks

[common.md §3–§4](common.md#3-callbacks), plus the generic output's:
`on_start`, `on_stop`, `on_output`, the start/stop methods, and, with
`register_telnet`, the server commands `start`, `stop`, `status`, `skip`,
`seek`, `metadata`, `remaining`.

## 10. Wire bytes

A peer receives the encoder's byte output, from its join point (§4),
concatenated and cut into SRT payloads of exactly `payload_size` bytes. A
cut falls where the count says, whatever the encoder's own packet
boundaries. A payload is shorter only when the delay bound releases it (§5)
or at a clean end. Receivers
SHOULD accept payloads of any size up to the library's largest live payload.

Example, `payload_size = 1316`, one peer from the encoder's start:

| Encoder output             | Pending after | Sent                                     |
| -------------------------- | ------------- | ---------------------------------------- |
| 500 bytes                  | 500           | nothing                                  |
| 1000 bytes                 | 184           | one payload: stream bytes 0–1315         |
| 2448 bytes                 | 0             | two payloads: bytes 1316–2631, 2632–3947 |
| clean end, 40-byte trailer | 0             | one payload of 40 bytes                  |

A second peer joining after the third row, with a 58-byte header, receives
the header followed by the next block: its payloads are counted from the
header's first byte, independently of the first peer's.

With an MPEG-TS encoder every payload is seven whole TS packets, since each
peer's stream starts on a block, the TS encoder's blocks start on a packet
boundary, and 1316 is a multiple of 188. A payload the delay bound releases is a whole
number of TS packets too: the bytes waiting are what follows the last full
payload of whole blocks, and blocks end on a packet boundary.

Example, `max_payload_delay = 0.1`, pieces of 0.04 seconds, an encoder that
outputs 25 bytes in the pieces ending at 0.08, 0.16 and 0.24 seconds and
nothing in the others:

| Piece ends at | Encoder output | Oldest waiting byte | Sent                |
| ------------- | -------------- | ------------------- | ------------------- |
| 0.08          | 25 bytes       | 0.08                | nothing             |
| 0.12          | none           | 0.08 (0.04 old)     | nothing             |
| 0.16          | 25 bytes       | 0.08 (0.08 old)     | nothing             |
| 0.20          | none           | 0.08 (0.12 old)     | one 50-byte payload |
| 0.24          | 25 bytes       | 0.24                | nothing             |

A byte waits at most the bound plus one piece.
