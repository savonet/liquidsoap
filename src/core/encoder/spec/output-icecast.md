# `output.icecast` and `output.shoutcast`

Part A. The source client: an output that pushes one writer's stream to an
Icecast or Shoutcast server, one connection at a time, and keeps the
server's view of the stream (its metadata, its stream properties) up to
date. This file answers the caller numbering of
[encoder.md §11](encoder.md#11-caller-files). It owns: the source client's
declarations and content type, its sink (the connection, its dialects and
their wire bytes), the out-of-band metadata update, the writer's lifetime
across connections, the final bytes at a disconnection, and the source
client's restart policy.

It relies on: the encoder contract ([encoder.md](encoder.md)); sinks
([sink.md](sink.md)); format helpers and validation
([formats.md](formats.md)); the output base
([output.md](../../outputs/spec/output.md)); faults
([faults.md](../../../spec/faults.md)); concurrency
([concurrency.md](../../../spec/concurrency.md)); the metadata charset
conversion of harbor's ICY metadata ([§4.3](#43-charset)).

`output.shoutcast` is a standard-library wrapper: `output.icecast` with the
ICY dialect and the stream id it is given. Everything below holds for both.

**Compatibility.** What the source client sends a server (the request head,
the authentication, the chunked framing, the update requests) is an
external commitment and is kept byte for byte, except where a change
improves compatibility with servers; each such change is stated where it is
made. The body is the writer's stream: only its validity is binding.

## 1. Formats and declarations

1. At construction, the output asks the registry for a factory for its
   format argument ([encoder.md §1](encoder.md#1-registry)). None: an
   invalid-value error on the format argument.
2. **Declarations** ([encoder.md §5.1](encoder.md#51-what-an-output-declares)):
   its sinks are `fresh` and `live(L)`, with `L` the output's
   `live_latency_bound` ([sink.md §1](sink.md#1-capabilities)), fixed at
   1 s and not a script parameter; it serves no
   reader that starts mid-stream. The server's listeners do start
   mid-stream, but the server serves them with its own parser, chosen from
   the content type (item 4).
3. **Checks** ([encoder.md §5.3](encoder.md#53-the-check)), each an
   invalid-value error on the format argument at evaluation:
   - a configuration that needs a seekable destination is refused;
   - a configuration that needs no byte sink is refused;
   - a preamble the script set explicitly is refused, since the sink is
     live (`%mp3(id3v2=…)`, an `%ffmpeg` mp3 container with `id3v2_version`
     above 0 or `write_xing=1`). Left at its default, the writer drops it by
     itself ([encoder.md §3.5](encoder.md#35-header-forms-and-preambles),
     item 5).

   The output never rewrites the format to steer the writer.

4. **Content type.** The `format` parameter (string, default `""`), when
   non-empty, is the content type, sent verbatim. Empty: the format's `mime`
   helper ([formats.md §9](formats.md#9-helpers)). When neither gives one,
   the output fails at evaluation with an invalid-value error on `format`
   that asks for one. Reason: the server chooses its parser, and players
   their decoder, from the content type.
5. **Metadata mode** (§4.1) is fixed at construction from
   `send_icy_metadata`, the content type and the writer's declarations. It
   never fails: a content type the output cannot classify defers to the
   writer's declaration.

## 2. The sink

The sink is one connection to the server. A new sink, with a new
connection, serves each writer (§3).

The sink is one worker per output, running the blocking protocol client
with its timeouts, behind the bounded queue of §2.2; it does not use
non-blocking I/O. Reason: the protocol client's request heads are the
external commitment stated above (Compatibility), kept byte for byte by
reusing it, and the worker is work allowed to block
([sink.md §3](sink.md#3-never-blocking-the-streaming-thread)).

### 2.1 What the sink owns

- **Address.** The host name is resolved on work allowed to block. Every
  address it resolves to is tried in turn, in the order of the output's
  address-family preference (its parameter, else the global setting), each
  bounded by the connection timeout; an address that refuses falls through
  to the next at once. A malformed address (a port out of range, an empty
  host) is a configuration error, reported at evaluation when the value is
  known then, else as a hard error at the first attempt.
- **Transport.** Plain TCP or TLS, as the transport parameter says. Every
  connect, handshake, read and write is bounded: the connection timeout
  for everything up to the end of the handshake, the write timeout
  (`timeout`, default 30 s) for each write and for each answer awaited.
  The bound holds on every supported platform and transport: a black-holed
  address returns within the connection timeout, a refusing port at once,
  a TLS handshake with a server that does not speak TLS within the
  connection timeout.
- **Dialect** (§2.3, §2.4): the request head, the authentication, the
  framing of the body.
- **Identity**: the stream's name, description, genre, URL and public flag,
  sent in the head. Each text is converted to the output's charset (§4.3)
  before it is sent; a value containing a line break is not sent, and the
  output logs it once at `severe`.
- **Dump.** When `dumpfile` is set, the bytes the connection delivers to the
  server are also written to that file, opened with truncation when the
  connection opens and closed when it closes. A failure of the dump is
  logged at `severe` once per connection, the dump is closed, and the
  stream goes on.

### 2.2 Bounds

The connection holds what the writer gives it while it opens, and what the
server has not yet taken ([sink.md §3](sink.md#3-never-blocking-the-streaming-thread)):

| Parameter      | Value for this output                  | Protects                                                |
| -------------- | -------------------------------------- | ------------------------------------------------------- |
| `open_hold`    | the connection timeout, in stream time | Memory, while the connection and its handshake are made |
| `sink_backlog` | `timeout`, in stream time              | Memory, and the age of what a slow server receives      |

Past either, the connection has failed (§8). The sink confirms `fresh` and
`live(L)` when it opens; nothing in a connection changes them.

### 2.3 HTTP dialect

`protocol="http"` (the default); `method` is `source`, `put` or `post`.

- Request line `<VERB> <mount> HTTP/1.0`, or `HTTP/1.1` when `chunked` is
  true.
- Then `Host`, `Authorization: Basic` (user `source` unless given), the
  `ice-*` identity and stream-property headers (§2.5), `User-Agent`, the
  script's extra `headers`, `Content-Type` (§1 item 4),
  `Transfer-Encoding: chunked` when chunked, and `Expect: 100-continue` for
  `put`; then an empty line.
- An answer `2xx` (or `100` for `put`) opens the connection; any other
  answer is a refusal (§8).
- **Body.** Not chunked: the writer's bytes, as they are. Chunked: each
  range of bytes handed to the socket is one chunk,
  `<hex length>\r\n<bytes>\r\n`, and a clean close (§6) writes `0\r\n\r\n`
  after the final bytes.

### 2.4 ICY dialect

`protocol="icy"`, used by `output.shoutcast`.

- The connection is made to `port + 1`. The client writes
  `[user:]password[:#sid]\r\n` (`:#sid` only when the stream id `icy_id` is
  not 1), expects a line starting `OK`, reads an optional `icy-caps:1`
  line, then writes the `icy-*` identity and stream-property headers and
  `content-type`, each as `name:value` with no space after the colon, and
  an empty line.
- **Body**: the writer's bytes, as they are.

### 2.5 Stream properties

The head carries the stream's properties (`ice-audio-info` over HTTP, the
matching `icy-*` headers over ICY): `bitrate`, `quality`, `samplerate`,
`channels`. The head is sent once per connection and cannot be amended, so
the writer is built before the head is sent (§3), and each property is read
then:

- `bitrate` is the writer's `bitrate` property, in kbit/s. When the
  configuration fixes the bit rate, the writer knows it from its creation
  ([encoder.md §3.8](encoder.md#38-bitrate)), whatever its kind: a `%ffmpeg`
  format with an explicit `b` announces it as `%mp3(bitrate=…)` does.
- `samplerate` and `channels` are those of the configuration's encoded
  audio; `quality` is the configuration's variable-bit-rate quality or
  compression level where it has one.
- A property unknown at that moment is left out of the head, never sent as
  a placeholder.

### 2.6 Bytes delivered

The connection delivers exactly the bytes the writer wrote, in order: for
every range handed to it, whatever its position in the writer's buffer, the
same bytes reach the socket, the chunk framing and the dump, and a chunk's
length is the length of the range it frames. Nothing is added to the body
but the chunk framing; no ICY block is interleaved (ICY blocks are the
server's, towards its listeners).

## 3. Writer lifetime

**One writer per connection** ([encoder.md §2.1](encoder.md#21-one-writer-per-destination-stream)).
A connection carries one stream, from its writer's first byte; a new
connection gets a new writer and starts with its full header, so that every
connection's body decodes alone.

- **When.** The output connects lazily: the first piece the output receives
  while started, holding no connection and not waiting to retry (§7),
  starts an attempt. At that piece the output builds the writer, starts
  opening the sink on work allowed to block, and writes the piece into the
  sink, which holds it until the connection opens. No piece is lost to the
  connection being made. Reason: a source that fails at once opens no
  connection, and the first bytes of a stream are its header.
- **Initial metadata**: [output-encoded.md §3.2](output-encoded.md#32-initial-metadata).
  The sink is live, so the writer writes it as tags only where it is the
  in-band channel (Ogg comment headers in the in-band mode, §4.1); cover
  art never reaches the writer
  ([encoder.md §3.5](encoder.md#35-header-forms-and-preambles), item 7).
- **Ended** when the connection ends: cleanly at the output's stop (§6),
  abandoned when the connection or the writer fails (§8).
- A writer ended before its first piece writes nothing, and its sink opens
  no connection.
- Between a failure and the end of the retry wait there is no writer: the
  pieces received are dropped and counted in stream time, and the next
  connection logs the stream time dropped at `important`. Metadata received
  then becomes the current metadata
  ([output-encoded.md §3.1](output-encoded.md#31-current-metadata)).

## 4. Pieces and metadata

The output hands its writer each piece with its metadata first
([output.md §3](../../outputs/spec/output.md#3-a-streaming-cycle)), in
every metadata mode: an out-of-band update does not need the connection to
be open when it is handed over (§4.2).

### 4.1 Mode

| `send_icy_metadata` | Mode                                                                                                                                                                                                            |
| ------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `true`              | out of band: each metadata is sent to the server (§4.2); the writer is never offered it                                                                                                                         |
| `false`             | in band where the writer offers it ([encoder.md §9](encoder.md#9-in-band-metadata)); no update is ever sent to the server                                                                                       |
| null (default)      | `true` for `audio/mpeg`, `audio/aac`, `audio/flac` and `audio/wav`; `false` for `application/ogg`, `audio/ogg` and `video/ogg`; for any other content type, in band when the writer offers it, else out of band |

With `false` and a writer that offers no in-band metadata, a metadata change
reaches the server only as the initial metadata of the next connection's
writer; the output logs that once, at `info`, when it starts.

### 4.2 Out-of-band updates

- **What.** The metadata is filtered to the keys of `icy_metadata`, each
  value converted to the output's charset (§4.3). When no `song` key
  results, `icy_song` adds one from the result (by default
  `artist - title`, or whichever of the two is set). Empty values are left
  out. An update with no key left is not sent.
- **When.** An update belongs to the connection whose stream carries the
  piece it labels. It is sent once every byte written before it has been
  handed to the socket, and before any later update; bytes written after it
  keep flowing meanwhile ([sink.md §5](sink.md#5-out-of-band-metadata)). An
  update handed over while the connection opens is sent once it is open,
  after the bytes before it.
- **On a new connection.** When `send_last_metadata_on_connect` (default
  true) holds and the output has a current metadata from before the
  connection's first piece, that metadata is the connection's first update.
  Metadata handed with or after the first piece is always sent.
- **Bounded.** An update never runs on the streaming thread. At most one
  update per output is in flight; it is bounded by the connection timeout
  for its connection and by `timeout` for its answer. A newer update that
  arrives while another waits to be sent replaces it. Reason: an update to
  a slow or unreachable endpoint must not delay the stream, and the server
  needs the current value, not the history.
- **HTTP.** A new connection to the same host and port, sending
  `GET /admin/metadata?mode=updinfo&mount=<mount>&charset=<charset>&<k>=<v>… HTTP/1.0`
  with `Host` and the source connection's headers (identity,
  `Authorization`, `User-Agent`, extra headers). Keys and values are
  URL-encoded. A `200` answer is success.
- **ICY.** `GET /admin.cgi?mode=updinfo&pass=<password>&<k>=<v>…[&sid=<n>] HTTP/1.0`
  with `User-Agent: <agent> (Mozilla compatible)`; `sid` when the stream id
  is not 1. Sent only when the server announced `icy-caps:1` at the
  handshake; otherwise no update is attempted on that connection, which is
  logged once at `info` when the first update is due.
- **Failure.** An update that fails, or whose answer is not a success, is
  logged at `severe` as possibly not applied (some servers apply an update
  and answer nothing), at most once per `fault_log_interval` with a count
  ([faults.md §4](../../../spec/faults.md#4-item-faults-escalate)), and is
  not escalated: the source connection stays up
  ([sink.md §7](sink.md#7-faults)). The first update of a connection fails
  the same way.

### 4.3 Charset

This section owns the charset rule of the source client and of harbor's
ICY metadata. Metadata values and identity texts are converted, value by
value, to the charset named by `encoding` (UTF-8 when empty). A value's own
charset is auto-detected; a value detected as binary is left unchanged; keys
are not converted. A value holding a character the target charset cannot
represent is sent unconverted, and the output logs it once, at `info`.
Reason: a lossy conversion mode is not offered by the charset conversion
the output uses; a value sent as it is stays readable by most players. The HTTP update names the
target charset in its `charset` argument.

### 4.4 In band

In the in-band mode the output calls the writer's `metadata` before the
piece the metadata labels; for native `%ogg` this starts a new link in the
same body ([encoder.md §9](encoder.md#9-in-band-metadata)). A metadata
received while there is no writer becomes the current metadata, carried by
the next writer's initial metadata.

## 5. Markers and readers

The sink serves one reader, the server, from byte 0. It ignores stream
starts and access points, and uses `end` to close (§6). It does not use the
shared stream.

## 6. End

- **Clean end.** At the output's stop, a reset, or the final stop
  ([output.md §2](../../outputs/spec/output.md#2-states)), the writer is
  ended `clean` and its final bytes go to the server: the end-of-stream
  pages of an Ogg body, a container's trailer, the codec's held audio. The
  connection delivers them ([sink.md §4](sink.md#4-markers-and-the-end)),
  then the chunk terminator when chunked, then closes. Delivery after `end`
  is bounded by `final_delivery_bound`, 2 s for this sink
  ([sink.md §4](sink.md#4-markers-and-the-end)); past it, the connection
  closes without the rest, logged at `severe`.
- **Abandoned end.** When the connection has failed, the writer is ended
  `abandoned` and the connection closes without waiting for a byte. When
  the writer has failed and the connection is up, the bytes the writer
  wrote before the fault are delivered within `final_delivery_bound`, with
  no chunk terminator, then the connection closes.
- **Teardown** completes whatever step fails
  ([sink.md §7](sink.md#7-faults)). Its order: the output's state becomes
  disconnected; the writer is ended; the final bytes and the terminator are
  delivered (clean end only); the socket and the dump are closed; the
  `on_disconnect` callbacks run. A step that fails is logged at `severe`
  and the next step runs. Every descriptor, buffer and pending update of
  the connection is released exactly once; nothing the output holds grows
  with the number of connections or failed attempts.
- **Callbacks.** `on_connect` runs once per connection, after its handshake
  succeeded; `on_disconnect` runs exactly once for each connection that ran
  `on_connect`, after it is closed. A failed attempt runs neither.

## 7. Reopen, reconnect, append

- Every connection is a new destination stream with a new writer (§3).
  The stream between a failure and the next connection does not exist; it
  is counted and logged (§3).
- **Retry wait.** After a failure, the restart policy (§8) gives a delay
  `d`. The retry time is `d` after the failure, on a monotonic clock; the
  first piece received at or after it starts the next attempt (§3). The
  wait observed is `d` to within one streaming cycle.
- **Reset, or stop then start.** A new connection is not opened before the
  previous one has closed (§6), so that the server has released the mount:
  the next attempt's open waits for that close, and the new writer's pieces
  are held meanwhile under `open_hold`.
- **Append.** Nothing.

## 8. Faults and restart policy

| Fault                                                                                                                       | Class               | Outcome                                                                  |
| --------------------------------------------------------------------------------------------------------------------------- | ------------------- | ------------------------------------------------------------------------ |
| no factory; no content type; a refused declaration or explicit setting (§1)                                                 | configuration error | the script fails at evaluation                                           |
| a malformed address                                                                                                         | configuration error | at evaluation when known then, else a hard error at the first attempt    |
| the writer's creation refuses the configuration ([encoder.md §8](encoder.md#8-faults))                                      | configuration error | hard error, naming the output, the parameter and the format's position   |
| the writer's creation fails for a cause that can clear                                                                      | output fault        | the policy                                                               |
| the name does not resolve; every address refuses or times out; the TLS handshake fails                                      | output fault        | the policy                                                               |
| the server refuses the source (an HTTP answer other than §2.3's: `401`, `403`, a mount in use; an ICY line other than `OK`) | output fault        | the policy                                                               |
| `open_hold` or `sink_backlog` exceeded; a write fails                                                                       | output fault        | the writer is ended abandoned; the policy                                |
| an item fault of the writer                                                                                                 | item fault          | dropped and counted by the writer ([encoder.md §8](encoder.md#8-faults)) |
| an output fault of the writer (its own failure, a run of item faults past the bound)                                        | output fault        | the writer is ended abandoned, the connection closes; the policy         |
| an out-of-band update fails                                                                                                 | not escalated       | §4.2                                                                     |
| delivering the final bytes or the terminator, closing, the dump                                                             | not escalated       | logged at `severe`; teardown completes (§6)                              |

**The policy.** Every output fault, whatever layer raised it, reaches the
policy exactly once per attempt, logged at `severe` with the cause. The
policy is the script's `on_error` handler, given the error; it may call
`restart_in(d)`:

| Call                     | Outcome                                                                                                       |
| ------------------------ | ------------------------------------------------------------------------------------------------------------- |
| none                     | retry after `retry_delay`                                                                                     |
| `restart_in(d)`, `d ≥ 0` | retry after `d` seconds, logged at `important`                                                                |
| `restart_in(null)`       | the output gives up: it stops itself (its `on_stop` callbacks run) and stays stopped until `start()`          |
| `restart_in(d)`, `d < 0` | the output fails under the clock's source error rule ([output.md §6](../../outputs/spec/output.md#6-failure)) |

| Parameter     | Recommended | Protects                                            |
| ------------- | ----------- | --------------------------------------------------- |
| `retry_delay` | 3 s         | The server and the log, against a reconnection loop |

A raising `on_error` handler is logged at `severe` with its backtrace and
counts as no call ([faults.md §0](../../../spec/faults.md#0-the-rule)).
Whether the output is connected, waiting to retry or has given up is
visible to the script at every moment, and is the sink's actual state.

**Liveness.** A server drops a source that sends nothing for its source
timeout. The writer writes at least once per `L`
([encoder.md §3.9](encoder.md#39-live-latency-bound)) and the sink delivers
what it holds without waiting for more, so digital silence keeps the source
connected whatever the codec. `live_latency_bound` MUST stay well below the
servers' source timeouts (Icecast's default is 10 s). A source that becomes
unavailable stops a fallible output
([output.md §2](../../outputs/spec/output.md#2-states)), which closes its
connection cleanly; a script that wants the connection kept through a gap
keeps its source available (`mksafe`, `fallback`).

## 9. Concurrency

- **Streaming work** ([encoder.md §7](encoder.md#7-concurrency)): every call
  on the writer (creation, `write`, `metadata`, `end`), handing bytes and
  out-of-band updates to the sink, and the decisions of §3 and §7. None of
  it waits for the network, a name server, a disk or a callback.
- **Sink work**, allowed to block, on the output's one sink worker (§2):
  resolution, connect, TLS, handshake, writes, the dump, the out-of-band
  updates, the delivery of the final bytes and the close.
- **User code** (`on_connect`, `on_disconnect`, `on_error`, `icy_song`) runs
  as work allowed to block, never under a lock the streaming work takes
  ([concurrency.md §1](../../../spec/concurrency.md#1-work)). The policy's
  answer reaches the streaming work as a request.
- **Requests** from other threads (`start`, `stop`, `shutdown`) are carried
  out by the streaming work between pieces
  ([encoder.md §7](encoder.md#7-concurrency), item 2).
- **Isolation.** A server that stalls, refuses or answers slowly costs its
  own output only: another output on the same clock keeps its real-time
  rate, and the scheduler's other work runs within its deadlines, whatever
  the number of stalled source clients.

## 10. Tests

A harness needs a recording fake server speaking both dialects (HTTP,
plain and chunked, and ICY), over plain TCP, that records every
connection's head and body and every update request, and can accept late,
refuse, answer `401`, cut a connection, and answer an update late. Every
check asserts the number of connections and requests it recorded, and fails
when it recorded none. TLS and a server that stalls are not exercised.
Reason: the transport and the bounds of §2.1 belong to the protocol client,
and a stalled server needs real-time waits too long for the suite.

- **Fresh stream per connection.** `%vorbis`; the server cuts the connection
  three times: each body starts with the identification header and decodes
  alone; `on_connect` and `on_disconnect` pair one to one with connections.
- **No lost first piece.** A source of known content: the samples decoded
  from each connection's body equal those produced from that connection's
  first piece on, with a server that accepts after 1 s included.
- **Final bytes.** A clean stop while connected: the body ends with the
  writer's final bytes (for Ogg, an end-of-stream page; when chunked,
  followed by `0\r\n\r\n`), and the audio decoded equals the audio
  produced.
- **Gap.** Metadata, `stop`, `start` and `restart_in` during a retry wait:
  none raises; the next connection's first update (or first link) carries
  the current metadata.
- **Teardown.** For each dialect, kill the server mid-write
  and bring it back: the output reconnects after the delay, to within one
  cycle. 10 000 attempts against a refusing port: descriptors and memory
  stay flat, and the policy is reached once per attempt.
- **Policy.** Refused, timeout, `401`, a write error, a writer output fault,
  at the first connection and at a reconnection: each reaches `on_error`
  once; `restart_in(0.5)` reconnects after 0.5 s, to within one cycle;
  `restart_in(null)` stops the output, whose state then says so.
- **Metadata placement.** Metadata at position 0 against a server that
  accepts after 1 s: the first update follows the head and carries it. An
  update endpoint that answers late: the body keeps its real-time rate, and
  a burst of changes leaves at most one update in flight. A metadata whose keys are all filtered out sends nothing. A
  stream id of 2 is named in the connection and in the update.
- **Charset.** Titles, names, genres and descriptions with multi-byte and
  non-Latin-1 characters, under UTF-8 and ISO-8859-1: the recorded bytes are
  in the declared charset; a value with a character the charset cannot
  represent is sent unconverted and logged once.
- **Head.** For each dialect, the head byte for byte (with a fixed
  `User-Agent`): a space after the colon over HTTP, none over ICY.
  `%ffmpeg(format="mp3", %audio(codec="libmp3lame", b="128k"))` and
  `%mp3(bitrate=128)` both announce a bitrate of 128.
- **Byte ranges.** A writer double that hands the sink ranges at non-zero
  offsets in its buffers: the body, the chunks and the dump hold exactly
  the bytes written, chunked or not.
- **Preambles.** `%mp3` and `%ffmpeg` mp3: the body starts with an MPEG
  frame, no ID3v2 tag and no encoder-information frame;
  `%mp3(id3v2=true)` and an explicit `write_xing=1` are refused at
  evaluation.
- **Silence.** Digital silence through every writer, against a server with
  a 2 s source timeout, for 10 s: the source stays connected.
- **Content type.** For every format, the announced type is the `mime`
  helper's or the override, and a late listener of a reference server
  decodes.
