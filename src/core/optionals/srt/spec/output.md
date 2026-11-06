# `output.srt`

Encodes a source and sends the bytes in SRT messages, as a caller to one
listener or as a listener to every accepted caller.

## 1. Interface

`output.srt(<common>, max_clients, start, fallible, register_telnet, id,
format, source)`.

| Parameter                              | Type    | Default | Meaning                                                   |
| -------------------------------------- | ------- | ------- | --------------------------------------------------------- |
| [common.md §1](common.md#1-parameters) |         |         | `mode` defaults to `"caller"`.                            |
| `max_clients`                          | int?    | null    | Client limit of the listener role; ignored by the caller. |
| `start`                                | bool    | `true`  | Start as soon as the source is available.                 |
| `fallible`                             | bool    | `false` | Accept a fallible source.                                 |
| `register_telnet`                      | bool    | `true`  | Register the generic server commands.                     |
| `id`                                   | string? | null    | Operator id.                                              |
| (unlabeled)                            | format  | —       | The encoder.                                              |
| (unlabeled)                            | source  | —       | The source.                                               |

The value returned carries the methods and callbacks of
[common.md §3–§4](common.md#3-callbacks) and the generic output ones.

## 2. Construction

1. Validate the common parameters ([common.md §1](common.md#1-parameters)).
2. Look up an encoder factory for the format. None found: invalid-value error
   on the format, `Cannot get a stream encoder for that format`.
3. Build the operator with the role `mode` names. The generic output checks
   that a non-fallible output has an infallible source.

## 3. Lifecycle

The generic output drives two operations, each called on the streaming
thread:

- **start**, when the output enters the started state: clear the stop flag
  (under the source lock), then connect.
- **stop**, when it leaves it, towards stopped or idle: set the stop flag
  (under the source lock), stop the encoder (§6), send the full chunks still
  queued (§5), disconnect.

A **reset**, which the clock asks of its outputs after too much latency, is
stop then start, and only while started.

The generic output enters started when its source is ready and it is idle, and
falls back to idle when the source ends a frame early: a fallible output goes
through stop and start each time its source becomes unavailable and comes
back. A listener's stop is a stopping disconnect: it closes every client and
the listening socket, and the next start listens again.

## 4. Encoder

At most one encoder exists at a time.

- It is created on first need while connected: by an encode, a metadata
  encode, or a stop of the encoder. It is created under the source lock, from
  the factory, with the operator's id and empty metadata.
- It is forgotten by §6 and §7.

Encoding happens only while connected. Otherwise a frame is not encoded and
its bytes do not exist; metadata is dropped the same way.

Each frame is cut at its metadata positions. For each piece, in order: the
metadata starting in it is encoded first (cover art not exported), then the
piece's data, and the data's bytes are sent (§5). The generic output adds
`on_air` and `on_air_timestamp` to each metadata.

The encoder's separately available header is never read. A peer receives the
header only when it is connected at the moment a new encoder produces its
first bytes.

## 5. Chunking and sending

One byte **queue** is shared by all peers. Sending encoded bytes, while
connected:

1. Append them to the queue.
2. While the queue holds at least `payload_size` bytes:
   1. take the first `payload_size` bytes off the queue as the **chunk**;
   2. for each socket of the role's list ([connection.md §6](connection.md#6-sockets)),
      in list order, send the whole chunk: with `messageapi`, send it as a
      message with no time-to-live and no ordering requirement; otherwise as
      stream data. Repeat with the unsent remainder until the whole chunk is
      accepted.
   3. If sending to a socket fails: stop the encoder (§6), then hand the
      socket to the role's peer failure ([connection.md §7](connection.md#7-peer-failure)),
      and go on to the next socket with the same chunk.

Fewer than `payload_size` bytes stay queued until more arrive. Because §6
empties the queue, the loop ends after a failure: one peer's failure restarts
the stream for every peer.

Sockets are blocking with the send timeout of `write_timeout`, and sending
runs on the streaming thread: each send can hold the streaming cycle for up
to `write_timeout`, once per socket and per chunk.

## 6. Stopping the encoder

1. If connected: append the encoder's final bytes to the queue (getting an
   encoder first if there was none). An error is logged with its backtrace as
   `Error while stopping encoder: <exn>`.
2. Empty the queue, discarding its contents.
3. Forget the encoder.

The final bytes appended in step 1 are discarded by step 2, with any partial
chunk: no encoder trailer is ever sent.

## 7. Disconnect handler

Registered at construction, before any script callback: on `on_disconnect`,
empty the queue and forget the encoder, without stopping it.

## 8. Pacing

The output is not a sync source. It reports its source's self-sync as its
own.

## 9. Methods and callbacks

[common.md §3–§4](common.md#3-callbacks), plus the generic output's:
`on_start`, `on_stop`, `on_output`, the start/stop methods, and, with
`register_telnet`, the server commands `start`, `stop`, `status`, `skip`,
`seek`, `metadata`, `remaining`.

## 10. Wire bytes

The stream is the encoder's byte output, concatenated, cut into SRT payloads
of exactly `payload_size` bytes. A cut falls where the count says, whatever
the encoder's own packet boundaries. The code produces this one form only.

Example, `payload_size = 1316`:

| Encoder output | Queue after | Sent                                     |
| -------------- | ----------- | ---------------------------------------- |
| 500 bytes      | 500         | nothing                                  |
| 1000 bytes     | 184         | one payload: stream bytes 0–1315         |
| 2448 bytes     | 0           | two payloads: bytes 1316–2631, 2632–3947 |

With an MPEG-TS encoder every payload is seven whole TS packets, since the
stream starts on a packet boundary and 1316 is a multiple of 188. After a
restart (§5, §6) the new stream starts on a payload boundary, so alignment
holds across restarts too.

A listener client accepted while a stream runs receives from the next payload
on: mid-stream, and without the header.
