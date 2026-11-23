# `output.srt` at the encoder seam (as built)

Seam view only. The normative tree is
[optionals/srt/spec/](../../optionals/srt/spec/README.md), in particular
[output.md §4–§6, §10](../../optionals/srt/spec/output.md#4-encoder); this
file records how it drives the [stream face](encoder.md) and what it does
with the bytes. The code matches that tree on every point checked here;
asymmetries with the other network outputs are in
[findings/net-callers.md](findings/net-callers.md).

## 1. Formats accepted and how the factory is obtained

The format value goes straight to the registry; none: invalid-value error
`Cannot get a stream encoder for that format`. **No streaming-format step**
(unlike `output.icecast` and `output.harbor`): a `%ffmpeg` mp3 stream keeps
its ID3v2 tag at the start.

## 2. Instance lifecycle

One shared instance for all peers, at most one at a time.

- **Created** lazily, on the first encode or metadata call that finds at
  least one peer and no instance; name = the output's id, empty initial
  metadata. Every peer present then is marked joined: its stream starts at
  the instance's first byte.
- **Stopped** when the last peer ends by failure (final bytes discarded), or
  when the output ends its peers itself (stop, `disconnect()`, reset,
  shutdown): final bytes delivered to every joined peer.
- No peer: no instance, frames and metadata are not encoded and are lost.
- A peer ending by failure while others remain does not touch the instance.

## 3. Call sequence

Encoded-output base, metadata first
([output-encoded.md §3](output-encoded.md#3-call-sequence)).
All under one output lock:

- Encode step: add the piece's duration to the output's stream time; if an
  instance runs (or is created now), `encode(piece)`, then
  `restart_offset()`, then distribute (§4). The send step does nothing.
- Metadata step: `encode_metadata(m)` on the running (or newly created)
  instance.
- Joining peer, at the next output: `restart_offset()` and, when it is
  none, `header()`.
- End: `stop()`.

`encode_into` and `bitrate()` are never called.

## 4. The bytes

Each peer has its own stream of bytes owed:

- A joined peer: every output in full.
- A peer joining now: if the output reports a restart at `r`, the output
  from `r` on (the bytes before `r`, end of the previous Ogg link, are not
  sent to it); otherwise `header()` followed by the whole output.
- The final bytes on a clean end: appended to every joined peer.

Each stream is cut into payloads of exactly `payload_size` bytes counted from
the peer's first byte, with a shorter payload when bytes have waited
`max_payload_delay` seconds of stream time, and sent one message per payload
by a sender thread per peer ([srt output.md §5, §10](../../optionals/srt/spec/output.md#5-sending)).
A peer whose backlog exceeds 8 MiB is failed.

Checked (`%ffmpeg(format="mp3")`, listener mode, two `ffmpeg` callers):
the first peer's bytes start with a 44-byte ID3v2.4 tag (`49 44 33 04 …`,
`TSSE Lavf…`) and then MPEG frames; a peer joining 2 s later receives an
MPEG frame first (`ff fb …`), no ID3v2 tag.

## 5. Reopen, reconnect

Every connection of a caller, and every client of a listener, is a peer.
A reconnecting caller after a failure, when it was the only peer, finds no
instance and gets a new one: a fresh stream from its first byte. A client
joining a running instance gets a mid-stream join (§4).

## 6. Failures

Encoder errors in encode or metadata: not caught, generic output failure.
Stop error: logged at `severe`, `Error while stopping encoder: <exn>`, with
the backtrace; the final bytes are then empty and the peers are ended anyway.

## 7. Threads and locks

Encode, metadata and distribution on the streaming thread under the output
lock; the clean-end stop on the thread that ends the peers, under the same
lock; sends on one thread per peer, each stream under its own lock
([srt concurrency.md](../../optionals/srt/spec/concurrency.md)).

## 8. Tests

[srt conformance.md](../../optionals/srt/spec/conformance.md). Seam-binding:
`tests/srt/srt-late-joiner.liq` (Ogg and WAV late clients decode, an Ogg
client joining right after a metadata change decodes the new link),
`srt-client-failure.liq` (one client failing leaves the other's stream
continuous, with one header), `srt-shutdown-drain.liq` (the last bytes are
the encoder's final bytes), `srt-output-payloads.liq` (payload sizes),
`srt-caller-reconnect.liq` (reconnections decode).
