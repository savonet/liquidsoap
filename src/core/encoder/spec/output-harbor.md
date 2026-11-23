# `output.harbor` at the encoder seam (as built)

Seam view only. The normative tree is
[outputs/spec/harbor/](../../outputs/spec/harbor/README.md): admission,
delivery, ICY, the ring, both encoder modes. This file records how the two
modes drive the [stream face](encoder.md) and where each byte goes, for the
redesign; contradictions with that tree are in
[findings/net-callers.md](findings/net-callers.md). OCaml traces:
[language-notes/net-callers.md](language-notes/net-callers.md).

## 1. Formats accepted and how the factory is obtained

The same step as `output.icecast`
([output-icecast.md §1.1–1.2](output-icecast.md#1-formats-accepted-and-how-the-factory-is-obtained)):
streaming-format rewrite (no ID3v2/Xing for `%ffmpeg` mp3), registry
lookup, content type from `format=` or guessed. The content type is used as
a plain string in the response. There is no `send_icy_metadata` guess:
ICY is negotiated per listener
([harbor icy.md](../../outputs/spec/harbor/icy.md)).

Script-level caller: `icecast.server` (`src/libs/extra/icecast_server/`)
creates one `output.harbor` per mount, re-encoding (or copying) what a
feeder sends with the feeder's format
([harbor icecast-server.md](../../outputs/spec/harbor/icecast-server.md)).

## 2. Instance lifecycle

| Mode                                 | Instances          | Created                                        | Stopped                                                       |
| ------------------------------------ | ------------------ | ---------------------------------------------- | ------------------------------------------------------------- |
| Shared (default)                     | One per output run | Output start, initial metadata = last metadata | Output stop                                                   |
| Dedicated (`dedicated_encoder=true`) | One per listener   | Admission, initial metadata = last metadata    | Output stop (bytes kept), listener teardown (bytes discarded) |

The name is the output's id. In shared mode the instance encodes whether or
not anyone listens (the store keeps the bytes for bursts). In dedicated mode
no listener means no instance and no encoding.

## 3. Call sequence

The encoded-output base cuts frames into pieces, metadata first
([output-encoded.md §3](output-encoded.md#3-call-sequence)).

**Shared.** Streaming thread: per piece, `encode`, then `restart_offset()`
read at once under the same lock; the send step appends the bytes. Metadata:
`encode_metadata`. Stop: `stop()`, then `restart_offset()`. Admission
(server thread): `header()` under the same lock, once per listener.
`encode_into` is never used in this mode.

**Dedicated.** Admission (server thread): factory, then `header()` on the
fresh instance before it has encoded anything. Streaming thread (or one
scheduler task per batch of 32 listeners, the clock waiting for all):
per listener and piece, `encode_into` when the encoder offers it, else
`encode`; listeners still owed a preroll are skipped. Metadata:
`encode_metadata` on every live listener's instance. `restart_offset` is
never read.

## 4. The bytes

**Shared.** Each output is appended to one byte store (a ring of `buffer`
bytes). Each non-empty output's start offset is recorded as a candidate join
point; a reported `restart_offset` `r` records a **stream start** at
`tail + r`. A listener joins at a candidate offset (burst) or at the tail; it
gets `header()` first unless it joins exactly at the stream start, whose
bytes begin with their own header pages
([harbor shared-encoder.md §3–§5](../../outputs/spec/harbor/shared-encoder.md)).
The bytes at an output start other than a stream start are not a decodable
start by themselves for Ogg (they are mid-link); for a constant-frame codec
they are frame-aligned because each encoder output is. The final bytes of
`stop()` are appended as an ordinary output, so every listener receives
them (Ogg EOS).

Checked: a listener joining an `%vorbis` shared output mid-link receives the
current link's BOS identification page and comment/setup page (from
`header()`), then the link's audio pages, then the next link's BOS at the
next metadata change, all in one body.

**Dedicated.** Each listener's queue receives: the response head, the
preroll (if any), `header()` of its instance, then each encoder output copied
into reusable chunks (`encode_into`) or queued as one string. A listener
admitted between two cycles misses no byte of its own stream, since its
instance starts with it. The final bytes of `stop()` are queued at output
stop and discarded at listener teardown.

Checked: a dedicated listener receives its instance's BOS page first; its
first link is short (from join to the next metadata change).

Both modes assume `header()` and the instance's first output do not
duplicate each other: dedicated mode sends `header()` of an instance that
has not yet encoded, then that instance's first output.

## 5. Reopen, reconnect

No reconnection: listeners come and go. An output stop and start creates a
new shared instance; listeners kept across it (`keep_listeners`) are
[harbor modes.md](../../outputs/spec/harbor/modes.md)'s. A shared listener
whose preroll straddles an encoder restart is disconnected (`encoder
restarted during its preroll`).

## 6. Failures

Encoder exceptions on the streaming thread are not caught by the modes:
they are the generic output's
([outputs/spec/output.md §6](../../outputs/spec/output.md#6-failure)); in
dedicated mode the first failure of any batch task is re-raised on the
clock after all batches finish. A listener's instance stop at teardown
that fails is logged at info (`Listener <id> encoder stop failed: <exn>`).

## 7. Threads and locks

Shared: one lock serialises the instance's use, its stop, and the store's
writes; admissions call `header()` under it from the server thread.
Dedicated: one lock per listener serialises its instance's encode, metadata
and stop; encoding of different listeners runs in parallel on scheduler
tasks. See [harbor concurrency.md](../../outputs/spec/harbor/concurrency.md).

## 8. Tests

[harbor tests.md](../../outputs/spec/harbor/tests.md). Those binding the
seam: join points and headers (`tests/harbor/harbor_output_join_points.liq`:
a late joiner starts where a decoder can, Ogg joiners get the identification
header, mp3 joiners get no ID3v2 or Xing), final bytes
(`harbor_output_final_bytes.liq`), dedicated encoder
(`harbor_output_dedicated_encoder.liq`, `harbor_output_dedicated_chunks.liq`),
and the FFmpeg conformance check of a late Opus joiner
(`src/core/optionals/ffmpeg/test/conformance/conformance.ml`).
