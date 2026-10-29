# Conformance

The checks a rebuild is accepted against, by principle. Each runs a real
output on a loopback port of its own, connects real clients, and ends by
declaring pass or fail; a check that declares nothing fails by timeout. A
check MUST assert that it observed something (a count of listeners,
callbacks or bytes) before reporting a pass. Names in parentheses trace to
existing tests and carry no content.

Incidental in every check: log text, exact timings and delays, port numbers,
the order in which asynchronous callbacks run.

## Harness and doubles

- **Real server, real sockets**, a port per check, a way to know the output
  is listening, a deadline per check.
- **Clients**: a _decoding client_ with a connect event; a _raw client_ that
  sends a hand-written request head and reads bytes with a timeout; a
  _throttled client_ that reads at a set rate; a _non-reading client_.
- **A sine source**: deterministic, infallible, endless.
- **A metadata injector** on a timer, to force Ogg link changes and title
  changes.
- **Fault injection** for the races of
  [known-complexity.md K6](known-complexity.md#k6-stopping-while-listeners-connect-stream-and-leave):
  a stop forced between any two admission steps, and between a pass's flush
  and its removal. Each such check MUST be seen to fail against a build with
  the guard removed.

## Admission and callbacks

- **Authentication** (`harbor_output_auth`). Valid credentials in the header
  and in the query string are admitted. Missing ones get 401 with the
  `WWW-Authenticate` challenge; wrong ones get 401; the mount-taken error
  gets 403. _Binding_: status codes, the challenge, neither callback on
  refusal.
- **Admission predicate.** `accept` returning false gets
  `403 Forbidden` and neither callback; true admits.
- **Callback records** (`harbor_output_on_connect_disconnect`). _Binding_: the
  disconnect record has the same `id` and `connected_at` as the connect
  record; `ip` is numeric with no port; `uri` is the mount path; `bytes_sent`
  is 0 at connect and, at disconnect, equals the bytes the client received,
  response head included; `duration` is positive at disconnect;
  `listeners()` is empty inside a synchronous disconnect handler.
- **Raising handlers.** A raising `on_connect` or `on_disconnect` handler does
  not stop the others or the teardown.
- **Concurrent admission in dedicated mode** (`harbor_output_dedicated_encoder`).
  Both connect callbacks fire.
- **Clock pacing.** An output on a clock with sync `none` fails to start with
  an error naming the clock; with `allow_unsynced_clock=true` it starts.
- **Construction errors.** Empty mount, `burst > buffer` in shared mode,
  a header value with a line break, an unknown `encoding`: each is an
  invalid-value error naming its argument. `burst > buffer` in dedicated
  mode is accepted.

## Routing

- **Literal mount.** `mount="dup.mp3"` serves `/dup.mp3` and not `/dupXmp3`.
- **Duplicate mount.** Two outputs on one port and mount: the later serves;
  stopping it, the earlier serves; stopping the earlier one while the later
  serves leaves the later serving.
- **Shared port.** An output, an HTTP route and an input on one port all
  serve.
- **Immediate restart.** A stop followed at once by a start serves again.
- **Unsupported version.** `GET /<mount> ICE/1.0` gets 505 and the process
  keeps running.

## Delivery

- **Burst on join** (`harbor_output_burst`). A raw client joining a running
  128 kbps mp3 output receives at least 30000 bytes within one second.
  _Binding_: more than one second of live stream gives.
- **No burst still streams** (`harbor_output_burst_null`).
- **Join points** (`harbor_output_burst_ogg_pages`, regression `GH5546`):
  [known-complexity.md K1](known-complexity.md#k1-a-late-listener-must-join-at-a-point-a-decoder-can-start-from),
  including after a restart.
- **Overflow and timeout.** A throttled ICY client slower than the stream is
  disconnected once it is more than `buffer` behind, and its bytes up to then
  are a contiguous stream with a block exactly every `metaint` bytes. A
  non-reading client is closed within `timeout` plus the retry interval.
- **Large output.** An encoder output larger than `buffer` does not
  disconnect a caught-up listener.
- **Final bytes.** Stopping an Ogg output delivers its end-of-stream pages to
  a reading listener.
- **Stop with a listener** (regression `GH4840`). The stop callback fires; the
  listener reports no error before it.

## ICY

- **Block placement** (`harbor_output_icy_metaint`). `metaint=1024`, constant
  title `icy test`; a raw client sends `Icy-MetaData: 1`. _Binding_: the
  head contains `icy-metaint: 1024`; byte 1024 of the body is `0x02` and the
  following 32 bytes contain `StreamTitle='icy test';`; after 1024 further
  bytes, the byte is `0x00`.
- **Per-listener change detection**:
  [known-complexity.md K8](known-complexity.md#k8-icy-negotiation-and-per-listener-metadata-state).
- **Long values.** A 4000-byte title and a 300-byte url: the block's length
  byte is 255, the stream continues, other listeners are served.
- **Stamping at encode.** A listener lagging by several seconds receives a
  title change at the stream position where it took effect, not when it was
  produced.
- **Dedicated mode metadata.** An `%ogg(%vorbis)` dedicated listener's bytes
  carry each new title.

## Lifecycle and scale

- **Stop with many listeners** (`harbor_output_many_listeners`). Twelve
  decoding clients, then stop: twelve connects, twelve disconnects.
- **Churn** (`harbor_output_listener_lifecycle`). Reconnecting clients,
  repeated stop/start: connects equal disconnects, `listeners()` empty after
  the last stop, open descriptors back to their baseline. Not binding alone
  without the fault-injection checks above.
- **Shutdown.** Process shutdown with listeners connected and no frame
  running closes every listener.
- **Load** ([known-complexity.md K2](known-complexity.md#k2-per-listener-work-must-not-scale-the-streaming-cycle),
  [K5](known-complexity.md#k5-idle-cost-must-not-grow-with-the-machine),
  [K10](known-complexity.md#k10-memory-over-long-runs-with-listeners)).

## `icecast.server`

End-to-end through [icecast-server.md](icecast-server.md): a real
`icecast.server`, real source clients, real listeners. These are acceptance
checks; a rebuild passes them.

- **Ogg late listener** (`icecast_server_ogg_late_listener`). Ogg/Opus,
  `<burst-size>4096</burst-size>`, a raw client joins after 4 s asking ICY.
  _Binding_: no `icy-metaint`; the body starts with `OggS` and its first 64
  bytes contain `OpusHead`.
- **Fallback keeps listeners** (`icecast_server_fallback`). The listener is
  not disconnected and its `bytes_sent` keeps growing; once both sources
  leave, no mount remains.
- **Logs and admin page** (`icecast_server_logs`). The admin page refuses
  without credentials (401) and lists the listener with a 16-hex-digit hashed
  address; the access log line matches
  `<hash> - - [<date>] "GET /live HTTP/1.x" 200 <positive bytes> "-" "<agent>" <seconds>`.
- **Reload** (`icecast_server_reload`). New configuration applies to new
  sources without dropping connected ones.
- **Shutdown with a source connected** (`icecast_server_shutdown`). The process
  does not hang.
- **Source reconnect.** A source leaves and returns on the same mount: the
  new output serves; the old output's removal does not remove its route.

## Stream store

Unit checks of the store alone ([ring.md](ring.md)):

- round trip and wrap: exact bytes across the wrap point;
- ranges outside the retained window read as invalid;
- an append longer than the store keeps the required window readable;
- one writer, one reader, concurrently, with appends of every size up to
  more than the store's size: every range reported valid has the exact
  pattern bytes. The append sizes MUST reach the case of an append still in
  progress over the reader's range.
