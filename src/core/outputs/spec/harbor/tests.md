# Tests

The existing suite, described by principle. Each test is a script run by the
standard script harness: it starts a real output on a fixed local port,
connects real clients, and ends by declaring pass or fail; a test that
declares nothing fails by timeout. Test names, given in parentheses, trace
back to the source only.

## Harness and doubles

- **Real server, real sockets.** Every test binds a loopback port of its own;
  two tests must never share one. A new harness needs: a way to run an
  output on a port, a way to know it is listening, and a deadline per test.
- **Client doubles**, all needed by a new harness:
  - a _decoding client_ that connects over HTTP and decodes the stream, with
    a connect event (the existing tests use another script-side HTTP input);
  - a _raw client_ that sends a hand-written request head and reads bytes
    with a timeout, to assert on exact bytes;
  - a _streaming HTTP getter_ that collects the body until a byte count;
  - for load: an external probe that connects, identifies the codec, and
    disconnects; and a client that connects and never reads.
- **A sine source** as content: deterministic, infallible, endless.
- **A metadata injector** on a timer, to force Ogg link changes.

Incidental in every test: log text, exact timings and delays (they are slack
for scheduling), port numbers, and the order in which asynchronous callbacks
run.

## Admission and callbacks

- **Authentication accepts valid credentials** (`harbor_output_auth`). With an
  `auth` function accepting one user and password, a client giving them in the
  URL is admitted. _Binding_: the connect callback fires. _Not covered_:
  refusal, the 401 bytes, credentials in the query string.
- **Callback records** (`harbor_output_on_connect_disconnect`). One listener;
  the output stops one second after it connects. _Binding_: the disconnect
  record has the same `id` and `connected_at` as the connect record; `ip` is
  `127.0.0.1` with no port; `uri` is the mount path; `bytes_sent` is 0 at
  connect and positive at disconnect; `duration` is positive at disconnect;
  `listeners()` is empty inside a synchronous disconnect handler.
- **Concurrent admission in dedicated mode**
  (`harbor_output_dedicated_encoder`). Two clients connect at once. _Binding_:
  both connect callbacks fire.

## Delivery and bursts

- **Burst on join** (`harbor_output_burst`). A raw client joining a running
  128 kbps mp3 output receives at least 30000 bytes within one second.
  _Binding_: the byte count exceeds what one second of live stream gives
  (~16 KB), so it proves a burst was sent. _Incidental_: the exact count.
- **No burst still streams** (`harbor_output_burst_null`). With `burst=null`,
  a client connects. _Binding_: the connect callback fires.
- **Burst respects Ogg structure** (`harbor_output_burst_ogg_pages`, also
  regression `GH5546`). An Ogg/Opus stream whose metadata changes every
  second, through the native Ogg encoder and through `%ffmpeg`. A late client
  collects 40000 bytes. _Binding_: the bytes parse as consecutive Ogg pages
  from offset 0; the first page begins a logical stream; every page after a
  beginning-of-stream page carries that stream's serial number; at least two
  stream changes are seen; at least five whole pages. The regression variant
  binds that a decoding client, joining after many link changes, decodes more
  than 50 frames without error. _Why_: a burst starting mid-page or in an
  earlier link is skipped by tolerant decoders, so only a byte-level check
  catches it.
- **Stop with a listener** (regression `GH4840`). A listener is connected and
  the output is stopped. _Binding_: the output's stop callback fires; the
  listener reports no error before it.

## ICY

- **Block placement** (`harbor_output_icy_metaint`). `metaint=1024`, constant
  title `icy test`; a raw client sends `Icy-MetaData: 1`. _Binding_: the
  response head contains `icy-metaint: 1024\r\n`; in the body, byte 1024 is
  `0x02` and the following 32 bytes contain `StreamTitle='icy test';`; after
  1024 further bytes, the byte is `0x00`. This pins the block format, the
  position counting from the body start (codec header included), and the
  empty block for unchanged metadata.

## Lifecycle and scale

- **Shards and stop** (`harbor_output_many_listeners`). Three writers, twelve
  decoding clients. After the twelfth connects, the output stops. _Binding_:
  twelve connect callbacks, then twelve disconnect callbacks.
- **Exactly-once callbacks under churn** (`harbor_output_listener_lifecycle`).
  Three writers, eight reconnecting clients, the output stopped and restarted
  ten times (200 ms stopped, 500 ms started), then stopped. _Binding_, three
  seconds after the last stop: more than eight connects in total; disconnects
  equal connects; `listeners()` is empty. _Why_: listeners admitted while a
  stop is in progress are the case where a callback can be lost or doubled.
  _Limit_: recorded when it was added as passing with and without the fix for
  that race; it does not reliably reach the window.
- **Load** (`scale-test.sh` / `scale-test.js`, run by hand, not by CI). For a
  given encoder and mode: 30 probes connect with random staggers of 0.2–2 s,
  and 3 clients connect and never read. _Binding_: every probe identifies an
  audio stream; every non-reading client is closed by the server within 60 s;
  the server process is still alive at the end.

## `icecast.server`

End-to-end tests through [icecast-server.md](icecast-server.md): a real
`icecast.server`, real source clients (`output.icecast`), real listeners. A
rebuild must pass them unchanged.

- **Ogg late listener** (`icecast_server_ogg_late_listener`). An Ogg/Opus
  source, `<burst-size>4096</burst-size>`; a raw client joins after 4 s
  asking `Icy-MetaData: 1`. _Binding_: no `icy-metaint` in the response head;
  the body starts with `OggS` and its first 64 bytes contain `OpusHead`.
- **Fallback keeps listeners** (`icecast_server_fallback`). A listener on `/a`;
  a reload adds `/b` as `/a`'s fallback; `/a`'s source leaves. _Binding_: the
  listener is not disconnected and its `bytes_sent` keeps growing; once both
  sources leave, no mount remains.
- **Logs and admin page** (`icecast_server_logs`). _Binding_: the admin page
  refuses without credentials (401) and lists the listener with a 16-hex-digit
  hashed address; the access log line matches
  `<hash> - - [<date>] "GET /live HTTP/1.x" 200 <positive bytes> "-" "<agent>" <seconds>`;
  the playlist log carries the title; the error log has the source login line.
- **Reload** (`icecast_server_reload`). Configuration changes apply to new
  sources without dropping connected ones. Exercises the operator only through
  mount creation.
- **Shutdown with a source connected** (`icecast_server_shutdown`). _Binding_:
  the process does not hang.

Not covered: a source reconnecting to the same mount (handler removal versus
re-registration, [icecast-server.md §4](icecast-server.md#4-lifecycle-of-a-mounts-output));
listeners counted against a limit; a TLS port.

## Byte ring

Unit tests of the ring alone (`byteRing_test`), no server involved:

- **Round trip and wrap.** Appends of 10 bytes, reads at several offsets.
  _Binding_: tail, and exact bytes read across the wrap point.
- **Unavailable ranges.** _Binding_: a range older than tail minus capacity,
  or beyond the tail, reads as unavailable.
- **Growth.** A chunk larger than the capacity. _Binding_: the new capacity
  is twice the chunk, and older bytes still within reach read back intact.
- **One writer, one reader, concurrently.** 200 000 bytes of a known pattern,
  in chunks of 1–97 bytes, into a 1024-byte ring. _Binding_: every range
  reported valid has the exact pattern bytes; the final tail is the total.
  A reader that is overtaken skips ahead and continues.

## Gaps in coverage

Not exercised by any test: authentication refusal and its reply bytes;
query-string credentials; the timeout of a stalled listener (except by the
manual load test); the `buffer` overflow disconnection; restart behaviour of
the shared ring; `dumpfile`; `headers`; `url`/`StreamUrl`; `encoding`; in-stream
metadata in dedicated mode; title truncation; HTTPS transports; two outputs on
one mount; the 404/503 replies.

A disabled regression test (`GH3276`) once covered track boundaries through a
`%vorbis` harbor stream; it is skipped unconditionally.
