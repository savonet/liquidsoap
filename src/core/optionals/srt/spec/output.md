# `output.srt`

Encodes a source and sends the bytes in SRT messages, as a caller to one
listener or as a listener to every accepted caller.

## 1. Interface

`output.srt(<common>, max_clients, start, fallible, register_telnet, id,
format, source)`.

| Parameter                              | Type    | Default | Meaning                                                               |
| -------------------------------------- | ------- | ------- | --------------------------------------------------------------------- |
| [common.md §1](common.md#1-parameters) |         |         | `mode` defaults to `"caller"`.                                        |
| `max_clients`                          | int?    | null    | Client limit of the listener role, at least 1; ignored by the caller. |
| `start`                                | bool    | `true`  | Start as soon as the source is available.                             |
| `fallible`                             | bool    | `false` | Accept a fallible source.                                             |
| `register_telnet`                      | bool    | `true`  | Register the generic server commands.                                 |
| `id`                                   | string? | null    | Operator id.                                                          |
| (unlabeled)                            | format  | —       | The encoder.                                                          |
| (unlabeled)                            | source  | —       | The source.                                                           |

The value returned carries the methods and callbacks of
[common.md §3–§4](common.md#3-callbacks) and the generic output ones.

## 2. Construction

1. Validate the common parameters ([common.md §1](common.md#1-parameters))
   and `max_clients`.
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
- **Joining.** A peer that joins while an encoder runs receives the
  encoder's separately available header, when the encoder has one, then the
  encoder's output from the start of its next output block. An encoder with
  no separate header (MPEG-TS) gives the joiner its output from the next
  block alone.
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
counted from the peer's first byte. A payload is sent as one message when
`messageapi` is set, otherwise as stream data. Fewer than `payload_size`
bytes wait for more, except at a clean end (§6).

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
3. Close each peer once the library has sent everything queued for it, or
   after `write_timeout`, whichever comes first; then `on_disconnect` fires
   for it.

This does not hold the streaming thread: a start that follows at once (a
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
boundaries. Only the last payload of a clean end may be shorter. Receivers
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
boundary, and 1316 is a multiple of 188.
