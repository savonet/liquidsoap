# `input.srt`

Receives a byte stream over SRT, as a listener to one caller or as a caller
to one listener, and decodes it into a source.

## 1. Interface

`input.srt(<common>, start, max, self_sync, dump, content_type, id)`.

| Parameter                              | Type    | Default                | Meaning                                                        |
| -------------------------------------- | ------- | ---------------------- | -------------------------------------------------------------- |
| [common.md §1](common.md#1-parameters) |         |                        | `mode` defaults to `"listener"`.                               |
| `start`                                | bool    | `true`                 | Start as soon as the input wakes up.                           |
| `max`                                  | float   | `10.`                  | Maximum buffered duration, in seconds; must be positive.       |
| `self_sync`                            | bool    | `true`                 | Act as a sync source while the stream is time-controlled (§7). |
| `dump`                                 | string  | `""`                   | File receiving a copy of the received bytes; `""` for none.    |
| `content_type`                         | string  | `"application/ffmpeg"` | MIME type used to find a stream decoder.                       |
| `id`                                   | string? | null                   | Operator id.                                                   |

The input is an active source and always fallible: it has no `fallible`
parameter, and its `stop` method is always allowed. It carries the methods
and callbacks of [common.md §3–§4](common.md#3-callbacks) and the generic
source and start/stop ones.

In listener mode the client limit is 1 ([listener.md §1](listener.md#1-parameters)).

## 2. Lifecycle

The generic start/stop source drives it:

- on wake-up, if `start`, a transition to started is queued; it runs before
  the next streaming cycle;
- on sleep, the transition to stopped runs at once.

Start clears the hold ([common.md §5](common.md#5-connection-wanted)); from
then on the role connects whenever the connection is wanted.

When it wakes up, the buffer's maximum length is set to `max` seconds: the
limit needs the content type, which is known by then. The buffer drops its
excess when it grows past it, logged at level 2.

State lifetimes:

| State                      | Lives as long as           |
| -------------------------- | -------------------------- |
| buffer content and layout  | the operator               |
| buffer limit               | the operator, from wake-up |
| socket, decoder, dump file | one peer                   |
| sync-source declaration    | one peer (§7)              |

## 3. Readiness

The input is ready while the connection is wanted
([common.md §5](common.md#5-connection-wanted)) and its buffer holds at
least one frame of decoded data. A connection with no data yet, or a peer
that stays connected but silent, leaves the input not ready; the connection
is kept. Readiness never depends on a socket the streaming thread would
have to look up: a peer ending between the readiness check and the frame
only shortens the frame (§5).

## 4. Receiving and decoding

For each peer, a **reader** receives and decodes off the streaming thread:

1. Find the stream decoder for `content_type` and the input's content type.
   None: the peer is ended with a level 3 log naming the MIME type, and the
   role goes on as after a peer failure. Parameters in the MIME type, as in
   `application/ffmpeg;format=s16le,ch_layout=mono,sample_rate=48000`, are the
   decoder's business ([FFmpeg layer spec](../../ffmpeg/spec/README.md)).
2. Add a track mark to the buffer: each peer starts a new track.
3. Feed the decoder from the read function (below), with no seek, position
   or length, and append what it decodes to the buffer.

The decoder is created by the reader, so header probing never blocks the
streaming thread.

**Read function**, asked for up to `len` bytes:

1. If the received bytes are used up:
   1. if the end of the stream was seen, signal end of file;
   2. receive one message into a buffer of the library's largest live
      payload, whatever `payload_size` says, so that a sender with a larger
      payload is never truncated;
   3. a closed or broken connection marks the end of the stream; keep the
      bytes received; if a dump file is open, append them to it.
2. Return up to `len` of the kept bytes.

The end of the stream is signalled only once every received byte has been
handed to the decoder. End of file, a receive error or a decoding error is
a peer failure ([connection.md §7](connection.md#7-peer-failure)).

A receive that waits for data MUST be interrupted by the peer being ended:
stop, `disconnect()` and shutdown never wait for the peer to send.

## 5. Producing frames

The streaming thread takes one frame from the buffer. When the buffer holds
less than one frame:

- while the input paces its clock, wait for the reader up to
  `read_timeout` (§7);
- otherwise, or when the wait times out, return what the buffer holds
  followed by an end of track. The input is then not ready until a frame is
  buffered again.

A peer that ends leaves its decoded data in the buffer; it is produced, then
the track ends.

## 6. Reactions to peers

The input's own reactions ([connection.md §5](connection.md#5-connected),
[§8](connection.md#8-disconnect)):

- on a new peer: if `dump` is set, open it for writing in binary, truncating
  it; start the reader;
- on a peer ending: stop the reader, close the decoder, close the dump file.

They complete before the script callbacks of the same event, so a
synchronous `on_connect` sees the dump file open.

## 7. Pacing

The input paces its clock only when the stream drives its latency: with
`self_sync`, while a peer is connected **and** the connection delivers by
sender timestamps (libsrt's timestamp-based packet delivery, negotiated in
the handshake). A connection without time-controlled delivery arrives as
fast as the sender pushes it, and the input does not pace.

While it paces, the input declares a sync source whose identity is the
stream: `srt://<remote ip>:<remote port>`, followed by `?streamid=<id>` when
the connection carries a stream id. Two inputs receiving the same stream
share one sync source; two inputs receiving different streams in one clock
are two sync sources, and the clock reports the conflict
([clock spec, pacing §2](../../../clock/spec/pacing.md#2-sync-source)). The
declaration is withdrawn when the peer ends.

Without `self_sync`, the input's self-sync is static and it declares none.

The pace comes from §5: the streaming thread waits for the reader, which
receives at the pace the library delivers.

## 8. Other source behaviour

- Remaining time is unknown.
- Aborting the track adds a track mark to the buffer.
