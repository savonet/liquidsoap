# Findings

Kept out of the as-built text. Each is marked **checked** (reproduced against
a running build of commit `b3ef10327`) or **read only** (a suspect from
reading; it needs a second read that tries to refute it before anything is
built on it).

## Defects

### D1. A `GET` with protocol `ICE/1.0` crashes the process — checked, outside scope

The harbor server accepts `ICE/1.0` as a request version, but its handler path
only expects `1.0`/`1.1` and fails an assertion when another version reaches
a registered handler. The assertion escapes every handler and kills the
scheduler: `PANIC: Liquidsoap has crashed, exiting.` One unauthenticated
request, `GET /<mount> ICE/1.0\r\n\r\n`, to any registered harbor HTTP path,
takes the whole process down. It is in the shared server, so it also affects
`harbor.http.*` routes. Reproduced with `printf 'GET /ice ICE/1.0\r\n\r\n' | nc`.

### D2. Long title plus long url overflows the ICY length byte — checked

The block length is `floor(len / 16) + 1`; truncation caps the text at
exactly 4080 bytes (3867 for the title part, 213 for the url part), which
gives 256, out of range for a byte. Building the block then raises.
Observed in shared mode with a 4000-byte title and a 300-byte url: the shard's
writer logs `Write task error: Invalid_argument("Char.chr")` on every pass;
the ICY listener receives nothing, not even the response head, and is never
timed out (the exception ends the pass before the timeout check). Read only:
other listeners after it in the same shard are starved too; in dedicated mode
the exception is raised on the streaming thread inside send. Also read only:
any text whose length is a multiple of 16 gets 16 needless zero bytes.

### D3. Dedicated mode never sends in-stream metadata — checked

Metadata is stored for ICY but never passed to the per-listener encoders.
Observed with `%ogg(%vorbis)` and titles changing every second: a shared-mode
listener's bytes carry `title=link2` … `title=link10`; a dedicated-mode
listener's bytes over the same period carry none.

### D4. `dumpfile` is empty in dedicated mode — checked

The file is created and closed but never written. Observed: 0 bytes after
10 seconds with a connected listener.

### D5. Two outputs on one mount — checked

Registration accepts a second handler for the same port and pattern. The first
one registered serves every request; the second never gets a listener.
Stopping either removes both handlers: observed, after stopping the second
output, the first was still running but its port was closed (connection
refused). Read only: after a restart, the restarted output's handler goes to
the end of the list, so the other output then serves the mount.

### D6. The mount is an unescaped regular expression — checked

`mount="dup.mp3"` also serves `/dupXmp3`. Any regex syntax in a mount changes
which paths match.

### D7. An empty mount fails with `index out of bounds` — checked

The runtime error names no parameter. Observed: `Error 14: Uncaught runtime
error: type: invalid, message: "index out of bounds"`.

### D8. The 404 and 503 replies never end — read only

Both are built as a _close_ reply whose producer returns the same status line
forever ([contracts.md §1](contracts.md#1-harbor-server)), with no header
terminator. The server keeps writing `HTTP/1.x 404 Not found\r\n` until the
client goes away. Reached only when a request matches the handler while the
output is stopping. Not reproduced: the window is narrow.

### D9. Torn ring reads can be reported valid — read only

[ring.md §4](ring.md#4-guarantees-and-limits): the writer overwrites bytes
before publishing the tail, and the reader's after-copy check cannot see an
append in progress. With a single encoder output longer than `buffer` (a large
video keyframe with a small `buffer`), a reader at the edge of its allowed lag
can send corrupted bytes. Not reproduced. The ring's concurrent unit test
appends chunks of at most 97 bytes into a 1024-byte ring, so it cannot reach
this case.

### D10. Shared state survives a restart — read only

The ring, the stream start and the chunk starts are kept across stop/start,
while the encoder is new. A listener joining shortly after a restart with a
burst can get the new encoder's header followed by bytes of the previous
encoder's stream; the stream start of the old encoder can also skip the new
header. Not reproduced.

### D11. Final encoder bytes are dropped — read only

On stop, the shared encoder's final bytes (for Ogg, the end-of-stream pages)
and each dedicated encoder's final bytes are discarded; listeners are closed
without them.

### D12. The writers setting shows a default it does not use — read only

`settings.harbor.output.writers` displays the runtime's recommended domain
count as its default; when unset, the scheduler's maximum domain count is
used instead. The two differ whenever `settings.scheduler.domains` is set.

## Asymmetries

### A1. ICY timing differs between modes — read only

The shared mode stamps a block with the metadata current at flush time; the
dedicated mode with the metadata current at encode time. A shared-mode
listener behind by `buffer` bytes (≈ 20 s of 128 kbps) sees a title change up
to that long before the audio changes; a burst is stamped with the current
title.

### A2. `burst` is validated in dedicated mode, where it is ignored — read only

`burst > buffer` fails construction in both modes; the dedicated mode never
bursts.

### A3. Overflow accounting differs — read only

The shared mode limits the ring lag only (the carry — response head, header,
partial ICY block — is outside the limit). The dedicated mode limits the whole
pending queue, but the initial bytes are appended without the check.

### A4. The `auth` address and the listener `ip` differ — read only

`auth` receives a reverse-DNS name when `settings.harbor.reverse_dns` is on;
the listener record's `ip` is always numeric.

### A5. ICY blocks at the end of a chunk — read only

The insertion used for headers and dedicated output places a block at a
boundary reached exactly at the end of the bytes; the shared flush places a
block only when stream bytes follow it. Both give the same byte stream once
more data arrives.

## Gaps

All read only, from the code and the tests.

- **G1. No duplicate-mount check** for HTTP handlers, while harbor sources have
  one (`Mountpoint is already taken!`). See D5.
- **G2. ICY text is not escaped and is cut on bytes.** A `'` in a title ends
  the quoted value for strict parsers; truncation can split a UTF-8 sequence.
- **G3. Only `GET`.** `HEAD` and `OPTIONS` on the mount are not answered by
  the output.
- **G4. Credentials in the query string.** `?pass=…&user=…` is accepted as
  credentials for listeners; nothing states whether that is intended for
  listeners as well as for metadata updates.
- **G5. Extra headers are unvalidated.** `headers` are written verbatim, so a
  value with a line break injects headers or ends the head. The script is
  trusted, so this matters only when header values come from outside.
- **G6. A departed client is detected only when owed bytes.**
- **G7. `icy-metadata` must be exactly `1`.** `true` or ` 1` disables ICY.
- **G8. TLS stalls a shard.** One slow listener on the TLS transport blocks
  the writes of its whole shard, for up to `settings.harbor.timeout` per write,
  which also exceeds the listener's own `timeout` ([delivery.md §4](delivery.md#4-writing-to-a-socket)).
- **G9. A large single chunk grows the ring for good.** A chunk larger than
  the ring's capacity grows it to twice the chunk's size, and it never
  shrinks.
- **G10. No bound on process memory or threads.** The spec bounds the ring
  and per-listener pending bytes only. Reported growth with listeners came
  from outside the managed heap and scaled with the thread count
  ([known-complexity.md K10](known-complexity.md#k10-memory-over-long-runs-with-listeners)).
- **G11. An immediate restart can fail to bind.** The server closes a port's
  listening sockets asynchronously when its last handler goes, so a stop
  followed at once by a start can get `EADDRINUSE`. Reported in the pull
  request that added the churn test, which waits 200 ms to avoid it.

## `icecast.server`

### I1. ICY is no longer offered on Ogg — checked by the suite

Recorded for the history: until the fix, Ogg mounts honoured `Icy-MetaData: 1`
and broke Icecast relays and GStreamer clients. Covered now by
[icy.md §1](icy.md#1-negotiation) and the Ogg late-listener test.

### I2. No burst unless configured — checked

A mount whose configuration sets no `<burst-size>` gets `burst=null`: listeners
receive no burst at all. Observed through `icecast.server` with a 128 kbps mp3
source: about 16 KB in a listener's first second, the live rate only. Icecast's
own default `burst-size` is 64 KB, so behaviour differs from the server it
emulates (that default is from memory, to verify).

### I3. No way to refuse a listener without credentials — read only

The operator admits every listener unless `auth` is set, and with `auth` set
it asks for credentials before calling it. Icecast's `<clients>` and
`<max-listeners>` therefore cannot be enforced through it, and are documented
as not implemented.

### I4. Responses lack Icecast's listener headers — read only

Icecast answers listeners with `icy-name`, `icy-description`, `icy-genre`,
`icy-br`, `icy-pub`, `Server` and caching headers taken from the source.
`icecast.server` passes only the configured `<http-headers>`, so players and
directories see none of the stream's identity (Icecast's set is from memory,
to verify).

### I5. A mount can be shadowed by a status route — read only

The status routes (`/`, `/status.json`, `/admin/listeners.json`) are
registered first and win over a mount with the same path.

### I6. Frame duration bounds latency — read only

`icecast.server` sets 0.2 s frames, so encoded data reaches the writers in
0.2 s steps and a listener's latency has that granularity on top of the burst.

## To verify

- **V1.** The Windows claim behind waking writers every cycle instead of
  waiting for writability ([delivery.md §2](delivery.md#2-wake-ups)) comes
  from a code comment. Not checked on Windows.
- **V2.** What an unknown `encoding` name does: whether it fails at the first
  metadata, on the streaming thread, or is logged and ignored. Not run.
- **V3.** HTTP/1.1 responses with neither length nor `Connection: close`
  rely on clients treating the body as close-delimited. Common players do;
  not checked against a list of clients.
- **V4.** Whether `header()` of every encoder returns what a mid-stream
  joiner needs ([contracts.md §4](contracts.md#4-encoder)) is the encoders'
  contract, not checked here beyond Ogg (by the existing tests).
