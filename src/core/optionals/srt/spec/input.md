# `input.srt`

Receives a byte stream over SRT, as a listener to one caller or as a caller
to one listener, and decodes it into a source.

## 1. Interface

`input.srt(<common>, start, max, self_sync, dump, content_type, id)`.

| Parameter                              | Type    | Default                | Meaning                                                     |
| -------------------------------------- | ------- | ---------------------- | ----------------------------------------------------------- |
| [common.md §1](common.md#1-parameters) |         |                        | `mode` defaults to `"listener"`.                            |
| `start`                                | bool    | `true`                 | Start as soon as the input wakes up.                        |
| `max`                                  | float   | `10.`                  | Maximum buffered duration, in seconds.                      |
| `self_sync`                            | bool    | `true`                 | Act as a sync source while connected (§7).                  |
| `dump`                                 | string  | `""`                   | File receiving a copy of the received bytes; `""` for none. |
| `content_type`                         | string  | `"application/ffmpeg"` | MIME type used to find a stream decoder.                    |
| `id`                                   | string? | null                   | Operator id.                                                |

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

**start**: clear the stop flag, connect. **stop**: set the stop flag,
disconnect.

When it wakes up, the buffer's maximum length is set to `max` seconds. The
buffer drops its excess when it grows past it, logged at level 2.

## 3. Readiness

The input can produce a frame when it is started, not stopping, and
connected ([connection.md §6](connection.md#6-sockets)).

## 4. Decoder

A decoder is created on the first frame request after a connection, on the
streaming thread:

1. Find the stream decoder for `content_type` and the input's content type.
   None: error, handled by §5. Parameters in the MIME type, as in
   `application/ffmpeg;format=s16le,ch_layout=mono,sample_rate=48000`, are the
   decoder's business ([FFmpeg layer spec](../../ffmpeg/spec/README.md)).
2. Set the socket to blocking send and receive.
3. Give the decoder a read function (below) and no seek, position or length.
4. Add a track mark to the buffer: each connection starts a new track.

**Read function**, asked for up to `len` bytes:

1. If stopping, fail.
2. If the received bytes are used up:
   1. if the end of the stream was seen, signal end of file;
   2. receive one message of at most `payload_size` bytes. This blocks up to
      the receive timeout of `read_timeout`, and fails on timeout. A length of
      0 marks the end of the stream;
   3. keep the bytes; if a dump file is open, append them to it.
3. Return up to `len` of the kept bytes.

The input always receives message by message, whatever `messageapi` says.

## 5. Producing frames

1. Decode until the buffer holds one frame's length.
2. Return one frame from the buffer.

Any error in §4 or §5 (no decoder, a failed read, end of file, a decoding
error, no socket):

1. Log with backtrace `Feeding failed: <exn>`.
2. Disconnect; unless stopping, connect again.
3. Return what the buffer holds, up to one frame, followed by an end of track.

A second with nothing received therefore ends the track and reconnects, with
the default `read_timeout`.

## 6. Connect and disconnect handlers

Registered at construction, before any script callback:

- on `on_connect`: if `dump` is set, open it for writing in binary,
  truncating it;
- on `on_disconnect`: close the decoder and forget it; close the dump file.

The buffer is the operator's, not the connection's: it keeps its content and
layout across reconnections.

## 7. Pacing

With `self_sync`:

- on `on_connect`, the input declares the process-wide SRT sync source,
  self-paced, named `srt`;
- on `on_disconnect`, it withdraws it.

Its self-sync is dynamic: it is a sync source only while it declares one.
Without `self_sync`, its self-sync is static and it declares none.

Every input uses the same sync source, so two inputs connected in one clock
count as one pacer ([clock spec, pacing §2](../../../clock/spec/pacing.md#2-sync-source)).
The history records no reason for the shared value.

The pacing comes from §4: the streaming thread blocks in each receive until
the peer's data arrives at the pace libsrt delivers it.

## 8. Other source behaviour

- Remaining time is unknown.
- Aborting the track adds a track mark to the buffer.
