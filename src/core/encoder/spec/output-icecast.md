# `output.icecast` and `output.shoutcast` at the encoder seam (as built)

Pass-1 description of how the Icecast/Shoutcast source client drives the
[stream face](encoder.md) of an encoder and what it does with the bytes. It
owns: the format → content-type step shared with `output.harbor` (§1.2), the
encoded-output base's call sequence as the network outputs see it (§3.1),
and everything Icecast-specific. The source-client protocol bytes are those
of the vendored `cry` library, described here only where they frame encoder
bytes. OCaml traces: [language-notes/net-callers.md](language-notes/net-callers.md).
Findings: [findings/net-callers.md](findings/net-callers.md).

`output.shoutcast` is a standard-library wrapper
(`src/libs/icecast.liq`): `output.icecast` with `protocol="icy"` and the
`icy_id` it is given. Everything below holds for both.

## 1. Formats accepted and how the factory is obtained

### 1.1 Factory

At evaluation, from the unlabelled format argument:

1. The format is rewritten by the **streaming-format step**: a `%ffmpeg`
   format whose container `format` is `"mp3"` gets the container options
   `id3v2_version = 0` and `write_xing = 0`, each only when the script did not
   set it. Every other format is unchanged. The script's format value is not
   modified (the copy is local). Reason, in the code: a listener joining
   mid-stream must not receive an ID3v2 header or a Xing frame. This step is
   also stated, normatively, in
   [ffmpeg io.md §12](../../optionals/ffmpeg/spec/io.md#12-other-outputs-special-casing-ffmpeg).
2. The registry is asked for a factory for the rewritten format. None:
   invalid-value error on the format argument, `No encoder found for that
format`.
3. The factory is bound to the format value's position; the instance
   factory kept by the output takes `(name, initial metadata)`.

Every format the registry answers is accepted, provided §1.2 finds a content
type: `%mp3`, `%shine`, `%fdkaac`, `%flac`, `%wav`, `%avi`, `%ogg(...)`,
`%ffmpeg`, `%external`, and `%ndi` only with an explicit `format=` (it has no
MIME of its own).

### 1.2 Content type

The `format` argument (string, default `""`). Non-empty: it is the content
type, verbatim. Empty: guessed from the format:

| Format                                   | Content type                                                   |
| ---------------------------------------- | -------------------------------------------------------------- |
| `%mp3`, `%shine`                         | `audio/mpeg`                                                   |
| `%fdkaac`                                | `audio/aac`                                                    |
| `%flac`                                  | `audio/flac`                                                   |
| `%wav`                                   | `audio/wav`                                                    |
| `%avi`                                   | `video/avi`                                                    |
| `%ogg(...)`                              | `application/ogg`                                              |
| `%ffmpeg` with `mime_type` set           | that MIME                                                      |
| `%ffmpeg` with a container `format` name | the MIME of Liquidsoap's container table for that name, if any |
| `%ffmpeg` otherwise, `%external`, `%ndi` | none                                                           |

None: invalid-value error on the format argument, `No format (mime) found,
please specify one.`

`%ogg` streams are always announced `application/ogg`, never `audio/ogg`,
whatever they carry. This table is shared with `output.harbor`, which uses the
content type as a plain string; Icecast maps the four Ogg/MPEG strings to
`cry`'s constants and passes others through unchanged.

### 1.3 ICY metadata mode

`send_icy_metadata` (nullable bool). When null it is guessed from the content
type: `true` for `audio/mpeg`, `audio/wav`, `audio/aac`, `audio/flac`;
`false` for `application/ogg`, `audio/ogg`, `video/ogg`; any other content
type (for example an explicit `format="audio/opus"`, or `%ffmpeg` to
`video/mp2t`) is an invalid-value error on `send_icy_metadata`: `Could not
guess send_icy_metadata for this format, please specify either true or
false.`

This flag decides where metadata goes (§3.3): out of band to the server, or
into the encoder.

### 1.4 Stream info announced

At each connection, after the instance is created and before any encode, the
`ice-audio-info` header is built from the format, plus the instance for
`%ffmpeg`:

| Format                      | `bitrate`                   | `quality`           | `samplerate`, `channels`  |
| --------------------------- | --------------------------- | ------------------- | ------------------------- |
| `%mp3` CBR / ABR / VBR      | CBR rate / ABR mean / —     | — / — / VBR quality | format                    |
| `%shine`, `%fdkaac`         | format bitrate              | —                   | format                    |
| `%flac`                     | —                           | compression level   | format                    |
| `%ogg(%vorbis)`             | CBR or ABR nominal rate     | VBR quality (float) | format                    |
| other `%ogg`                | —                           | —                   | —                         |
| `%wav`, `%avi`, `%external` | —                           | —                   | format                    |
| `%ffmpeg`                   | instance `bitrate() / 1000` | —                   | `audio` stream if encoded |
| `%ndi`                      | —                           | —                   | —                         |

For `%ffmpeg` the instance has encoded nothing when it is asked, and its
`bitrate()` is then unknown: the header never carries a bitrate for
`%ffmpeg`, even with an explicit `b=` (checked, see findings).

## 2. Instance lifecycle

**One instance per connection.** The output holds at most one instance, and
holds one exactly while it holds a connection (or is establishing one).

- **Created** at the start of each connection attempt, before the socket is
  opened, with the output's id as name and **empty** initial metadata. The
  last metadata, if any, is handed afterwards through `encode_metadata`
  (§3.3), not as initial metadata.
- **Stopped** whenever the connection ends: a send error, a failed connect,
  the output's stop. Its final bytes are discarded (§4.4).
- The output's own start does nothing: the first connection is attempted
  lazily, by the first send (§3.2). The output's stop stops the instance and
  closes the connection; the next start reconnects lazily with a new
  instance.

So every connection to the server carries a stream from a fresh instance,
from its first byte.

## 3. Call sequence

### 3.1 The encoded-output base

`output.icecast`, `output.harbor`, `output.srt` and `output.url` inherit the
[encoded-output base](output-encoded.md#3-call-sequence): a frame is cut at
its metadata positions into non-empty pieces (no zero-length piece ever
reaches these outputs' encode step), and per piece the base calls the
output's metadata step, encode step and send step, in metadata-first order
by default or data-first when the output asks.

### 3.2 Per piece

`output.icecast` uses data-first order when `send_icy_metadata` is true
(reason, in the code: an ICY update needs an established connection, which
the send establishes), metadata-first otherwise.

- **Encode step.** Connected and holding an instance: `encode(piece)`.
  Otherwise: no call; the piece produces nothing and is lost.
- **Send step.** Disconnected and the retry time has passed (initially 0):
  connect (§5), then write the bytes. Connected: write the bytes.
  Disconnected before the retry time: nothing.

Because encode runs before send, the piece during which a connection is
established is never encoded: **the first piece of every connection is
lost**, including the very first one after start-up.

`header()`, `restart_offset()` and `encode_into` are never called.
`bitrate()` is called once per connection for `%ffmpeg` only (§1.4).

### 3.3 Metadata step

- `send_icy_metadata = true`: the metadata is filtered to the keys of
  `icy_metadata` (default `song title artist genre date album tracknum
comment dj next`), each value converted to the output charset (`encoding`,
  default UTF-8). If no `song` key results, `icy_song(metadata)` adds one
  (default: `artist - title`, or whichever is set, or none). If connected,
  an out-of-band update is sent (§4.3). Disconnected: dropped. **The
  encoder is never told.**
- `send_icy_metadata = false`: with an instance, `encode_metadata(m)`
  (in-band; an Ogg encoder starts a new link); without one, dropped.

On connect, when `send_last_metadata_on_connect` (default true) and the
source has a last metadata, that metadata goes through the metadata step
above, right after the handshake and before any piece is encoded. Errors
there are ignored silently.

## 4. The bytes

### 4.1 Request head

Built by `cry`. HTTP protocol (`protocol="http"`, the default; `method`
`source` | `put` | `post`): request line `<VERB> <mount> HTTP/1.0`, or
`HTTP/1.1` when `chunked=true`; then `Host`, `Authorization: Basic`
(user defaults to `source`), the `ice-*` headers, `User-Agent`, the extra
`headers`, `Content-Type`, `Transfer-Encoding: chunked` when chunked, and
`Expect: 100-continue` for PUT. Accepted answers: 2xx (100 for PUT).

ICY protocol (`protocol="icy"`): connects to `port + 1`, writes
`[user:]password[:#sid]\r\n` (`:#sid` only when `icy_id ≠ 1`), expects a
line starting `OK`, reads an optional `icy-caps:1` line, then writes the
`icy-*` headers and `content-type` with no space after the colon, ended by
an empty line.

Worked example (checked, against a local fake server):
`output.icecast(%vorbis, port=18000, mount="m", s)` sends

```
SOURCE /m HTTP/1.0
Host: localhost:18000
ice-name: m
Authorization: Basic c291cmNlOmhhY2ttZQ==
ice-public: 1
User-Agent: Liquidsoap/2.5.0+git@02fa52985 (Unix; OCaml 5.5.2)
ice-audio-info: channels=2;quality=0.3;samplerate=44100
Content-Type: application/ogg
```

followed by an empty line, then the encoder bytes.

### 4.2 Body

The encoder's bytes are written as they come, on the streaming thread, one
write per fragment of the returned byte sequence; nothing is buffered by the
output. Each write waits for the socket to become writable, up to `timeout`
(default 30 s), and loops until the fragment is written.

- Not chunked: raw bytes.
- Chunked: each non-empty fragment is one chunk, `<HEX length>\r\n<bytes>\r\n`;
  closing the connection writes `0\r\n\r\n` first.
- `dumpfile`, when set: the same bytes, after a successful write, are also
  appended to a local file opened (truncating) at each connection and closed
  at each disconnection. A reconnection therefore overwrites the dump.

There is no ICY interleaving on the source side: ICY metadata blocks are the
server's business towards its listeners.

### 4.3 Out-of-band metadata

Each update opens a new TCP connection (connection timeout as configured,
`timeout` for the exchange) to the same host and port and sends:

- HTTP: `GET /admin/metadata?mode=updinfo&mount=<mount>&charset=<cs><&k=v...> HTTP/1.0`
  with **all the source connection's headers** (`ice-*`, `Authorization`,
  `User-Agent`, extra headers) plus `Host`;
- ICY: `GET /admin.cgi?mode=updinfo&pass=<password><&k=v...>[&sid=n] HTTP/1.0`
  with only `User-Agent: <ua> (Mozilla compatible)`; allowed only when the
  server announced `icy-caps:1`, else it fails.

Keys and values are URL-encoded, empty values skipped. A non-200 answer is an
error. Example (checked), `%ffmpeg` mp3 with title `T1`, artist `A`:

```
GET /admin/metadata?mode=updinfo&mount=/m&charset=UTF-8&title=T1&artist=A&song=A%20%2D%20T1 HTTP/1.0
```

The update is synchronous: the streaming thread waits for its answer.

### 4.4 What a server, and its listeners, receive

Per connection the server receives one stream from a fresh instance, from
the instance's first output: for Ogg, the BOS identification and comment
pages first; for MPEG audio, an MPEG frame first (with the streaming-format
step, no ID3v2 tag and no Xing frame for `%ffmpeg` mp3; checked: first bytes
`ff fb`).

Mid-connection restarts happen only inside the encoder: with
`send_icy_metadata = false`, every metadata change makes an Ogg encoder end
its link (EOS page) and start a new one (BOS + headers) in the same body.
Checked with `%vorbis` and metadata every 2 s: the body is a chain of links,
each `BOS id-header, comment/setup, audio..., EOS`.

At every **disconnection** the instance's final bytes are discarded: the last
link of the body has no EOS page and the encoder's buffered audio (about one
Ogg page, here up to ~1 s) is lost. Checked: both the server-cut connection
and the shutdown-closed connection of the experiment end on a non-EOS page.

At a **reconnection** the server sees a new source connection whose body
starts again with full headers; what the server's listeners then receive is
the server's decision (Icecast moves them to the new stream; Liquidsoap's own
`icecast.server` is [harbor icecast-server.md](../../outputs/spec/harbor/icecast-server.md)).
The stream between the failure and the reconnection is not encoded and is
lost, as is the first piece of the new connection.

## 5. Reopen, reconnect

- A connection is attempted by the send step when disconnected and the
  current whole-second time is past the retry time. The retry time is set
  from the sub-second clock, compared against whole seconds, so the
  effective wait is the delay plus up to one second (checked: delay 3 s,
  reconnection about 3.4 s after the failure was logged).
- Connect: create the instance, open the dump, connect and hand-shake (§4.1),
  log `Connection setup was successful.` (important), replay the last
  metadata (§3.3), run `on_connect`.
- Any failure there: logged at `severe` (`Connection failed: <exn>`, the
  backtrace at info), then the connection is torn down (§2) and the error
  policy applies.
- **Error policy** (`call_on_error`): the delay defaults to 3 s; the single
  `on_error` handler (a new registration replaces the previous one) may
  call `restart_in(d)`: `d ≥ 0` retries after `d` seconds, `null` never
  retries (retry time infinite; the output stays started and silently drops
  every frame), `d < 0` re-raises the error on the streaming thread, which
  is then the generic output's failure
  ([outputs/spec/output.md §6](../../outputs/spec/output.md#6-failure)).
  With a retry, `Will try to reconnect in <d> seconds.` (important).
- Teardown (send error, failed connect, stop): stop the instance and discard
  its bytes (any error ignored), forget it; if connected, log `Closing
connection...` (important), write the chunked terminator if chunked, close
  the socket (errors logged at severe), run `on_disconnect`; close the dump.

## 6. Failures

| Where                        | What happens                                                                               |
| ---------------------------- | ------------------------------------------------------------------------------------------ |
| Factory lookup, content type | Invalid-value error at evaluation (§1).                                                    |
| Instance creation            | Inside connect: logged `Connection failed`, error policy.                                  |
| `encode`, `encode_metadata`  | Not caught: raised on the streaming thread, generic output failure.                        |
| Socket write                 | Logged severe `Error while sending data: <exn>!`, teardown, error policy.                  |
| Out-of-band metadata update  | Logged severe `Metadata update may have failed with error: <exn>`; the connection is kept. |
| `stop` of the instance       | Ignored.                                                                                   |
| Close                        | Logged severe, `on_disconnect` still runs.                                                 |

## 7. Threads and locks

Everything runs on the streaming thread: encoding, the connect and handshake,
the writes, the out-of-band metadata request. The output declares blocking
I/O, so its clock is animated by a thread of its own rather than a shared
task. No lock of the output's own; the instance serialises its calls
([encoder.md](encoder.md)).

## 8. Tests

- **Error policy** (`tests/regression/LS503.liq`): an unresolvable host runs
  `on_error`. Binding: the handler runs; incidental: the host name.
- **Connection and shutdown** (`GH4629.liq`): an Icecast source connecting
  to a harbor input; shutting the output down disconnects it. Binding: the
  input sees the disconnect.
- **Ogg/FLAC into a harbor input** (`fallible_ogg.liq`): the harbor input
  decodes what the output sends; binding: the input starts.
- **Late Ogg/Opus listener through `icecast.server`**
  (`tests/harbor/icecast_server_ogg_late_listener.liq`), and source
  reconnection to the same mount (`icecast_server_source_reconnect.liq`):
  these bind the server side; the source side is only the stimulus.
- **Argument handling** (`shoutcast-args.liq`): the wrapper evaluates.

Not covered by any test: the loss of the first piece of a connection, the
discarded final bytes, that a reconnection starts with full headers, the
out-of-band request bytes, the `ice-audio-info` contents. A harness needs a
fake server that records each connection (head and body) and can cut one
(the experiment of this file used one, in a scratch directory).
