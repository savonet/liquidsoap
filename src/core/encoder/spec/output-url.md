# `output.url` at the encoder seam (as built)

Seam view only. The normative text is
[ffmpeg io.md §1.4 and §11](../../optionals/ffmpeg/spec/io.md#11-outputurl).
This file records the one thing that sets `output.url` apart for the
redesign: **the encoder does the I/O itself**; the bytes never cross the
seam.

## 1. Formats accepted and how the factory is obtained

`%ffmpeg` only; any other format: invalid-value error `Encoding format does
not support output url!`. The registry is **not** used: the output builds
the FFmpeg encoder instance directly, from the format with its output set to
the URL, wraps it in the generic serialisation, and hands it an interrupt
callback (abort flag, process shutdown unless the instance is being closed,
or a write in progress past `timeout`). No streaming-format step.

`output.youtube.live.rtmp` and the other standard-library wrappers are
`output.url` with a computed URL.

## 2. Instance lifecycle

One instance per connection attempt; built on a scheduler task (the open of
the URL happens inside the build), never on the streaming thread. Initial
metadata: the output's current metadata at build time. Stopped (draining,
trailer written to the URL) at the output's stop; stopped without draining
after a failure; stopped unused when a stop won the race against the build.

## 3. Call sequence

Encoded-output base, metadata first
([output-encoded.md §3](output-encoded.md#3-call-sequence)).

- Encode step, connected: if the current metadata differs (physically) from
  what the instance was built with or last given, `encode_metadata` first;
  then the frames held while connecting, in order, then the piece, each
  `encode(piece)` under the write-timeout guard. Returned bytes are
  **ignored** (they are empty: the muxer writes to the URL).
- Encode step, connecting: the piece is held (up to `timeout` seconds of
  stream; older held pieces are dropped and counted). Waiting: dropped and
  counted, or a new attempt is scheduled and the piece held.
- Metadata step: recorded as current metadata; connected: `encode_metadata`.
- Stop: `stop()` under the write-timeout guard; returned bytes ignored.

`header()`, `restart_offset()`, `encode_into` and `bitrate()` are never
called.

## 4. The bytes

Written by FFmpeg's I/O to the URL, from inside `encode`, `encode_metadata`
and `stop` on the streaming thread. The output neither sees, buffers, frames
nor seeks them; whether the destination is seekable is FFmpeg's protocol's
business. A `%ffmpeg` Ogg format to a non-`file:` URL does not chain: a
metadata change does not start a new link ([ffmpeg encoder.md](../../optionals/ffmpeg/spec/encoder.md)).

Each connection is a new instance, so each connection's stream starts with
its container header and fresh timestamps; the stream between a failure
and the next connection is dropped, except up to `timeout` seconds held
during the connect itself.

## 5. Reopen, reconnect

[io.md §11.1–§11.3](../../optionals/ffmpeg/spec/io.md#111-states):
`restart_delay` (default 2 s) after an output fault; `null` stops the
output; a configuration error before the first successful connection is
fatal.

## 6. Failures

An exception from `encode` or `encode_metadata` stops the instance (errors
ignored) and goes to the fault classification of io.md §11.3; a fault at
stop is logged at `severe`, `Error while stopping: <exn>`.

## 7. Threads and locks

Build on a `Threaded` scheduler task; encode, metadata and stop on the
streaming thread (stop may also come from the thread that stops the
output). The state is one atomic cell changed by compare-and-set; the
instance serialises its calls.

## 8. Tests

`src/core/optionals/ffmpeg/test/conformance/conformance.ml` (io §11
checks: a stopped output leaves a complete file, a failing output with
`restart_delay=null` stops once, a stalled write is given up after
`timeout`), and `tests/regression/GH2850.liq`.
