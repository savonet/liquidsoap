# Known complexity

What the history of `output.harbor` shows to be easy to get wrong: 165 commits
on the operator since 2010, of which about twenty fix behaviour, plus the
issues and pull requests behind them. Each entry is stated without reference
to any implementation, then reconciled with the as-built spec.

## K1. A late listener must join at a point a decoder can start from

Seen 6 times.

- **Situation.** One encoder feeds many listeners. A listener connects while
  the stream is running and is sent a codec header followed by recent stream
  bytes. The container has structure: pages, and logical streams (Ogg links)
  that each start with their own header pages and change at metadata changes;
  or stream-level preambles (ID3v2, Xing) meant only for the start of a file.
- **What went wrong.** A listener received the current link's header
  followed by pages of the previous link: undecodable (`Cannot identify new
stream`). A burst started in the middle of a page, right after the header.
  Before that, a joiner got no header at all, or a header fixed at start that
  no longer matched the stream, so Ogg streams were empty or garbled for every
  client. An mp3 joiner received an ID3v2 tag and a Xing frame mid-stream.
  On an Ogg/Opus mount relayed by the Icecast emulation, a joiner after the
  burst no longer reached the stream's start got pages with no `OpusHead`:
  the captured container header was empty, and in-band link headers hid it in
  the tests that changed metadata often.
- **Why it is hard.** The valid join points are known only to the encoder, and
  they move as the stream goes. Tolerant decoders resynchronise on the next
  page, so a listening test sounds fine while the bytes are invalid. The
  failure appears only when a structural boundary falls inside the burst
  window, which a short test may never hit.
- **How to tell.** Stream Ogg with a metadata change every second, through
  every Ogg-producing encoder; join after many changes with a burst shorter
  than the stream so far. _Binding_: the received bytes, parsed directly, are
  whole consecutive pages from the first byte; the first page begins a
  logical stream; every page belongs to the logical stream whose beginning
  came last; at least two stream changes are seen. Also: an mp3 joiner's
  bytes contain no ID3v2 tag and no Xing frame. Also, with a source whose
  metadata never changes (a single Ogg link) and a short burst: a joiner's
  first page is `OggS` with the codec's identification header. Decoding
  success alone is not binding.

**Reconciled.** Covered: join point
[shared-encoder.md §2](shared-encoder.md#2-initial-bytes), chunk starts and
stream start [shared-encoder.md §5](shared-encoder.md#5-encode-and-send), the
encoder's side [contracts.md §4](contracts.md#4-encoder), mp3 preambles
[contracts.md §2](contracts.md#2-format-and-mime-derivation). Not covered:
the same situation across a stop and start, where join points from the
previous encoder remain —
[findings.md D10](findings.md#d10-shared-state-survives-a-restart--read-only).

## K2. Per-listener work must not scale the streaming cycle

Seen 4 times.

- **Situation.** Thousands of listeners on one output, some slow, some on
  slow links. The stream is produced in fixed cycles (here 50 per second).
- **What went wrong.** With per-listener copies made on the streaming thread,
  the cycle overran from about 24 000 listeners and the clock fell behind.
  With all socket writes in one task, one core saturated at about 26 000
  writes per cycle while the others idled. Each listener held its own copy of
  the stream, 0.2 MB apiece. Earlier, writes on the streaming thread let one
  slow client stall everyone.
- **Why it is hard.** Any cost per listener per cycle multiplies by the cycle
  rate and the audience; it is invisible in tests with a dozen clients.
  Encrypted transports may turn a non-blocking write into a blocking one.
- **How to tell.** A load run with thousands of listeners and a few that never
  read. _Binding_: the clock reports no catch-up; memory grows with listeners
  by a bounded amount each, independent of stream length; time to first byte
  stays bounded; non-reading listeners are disconnected; the process stays
  up. _Incidental_: the exact thresholds.

**Reconciled.** Covered: one append per cycle
[shared-encoder.md §5](shared-encoder.md#5-encode-and-send), shards and
writers [delivery.md §1–§3](delivery.md#1-shards), actors
[concurrency.md §3](concurrency.md#3-guarantees). As stated, the dedicated mode
still encodes per listener on the streaming thread
([dedicated-encoder.md §5](dedicated-encoder.md#5-encode-and-send)), and a TLS
write blocks a shard
([findings.md G8](findings.md#gaps)).

## K3. A listener that falls behind is disconnected, never trimmed

Seen once, with a lasting effect.

- **Situation.** A listener reads slower than the stream is produced, with ICY
  metadata on.
- **What went wrong.** Dropping the oldest pending bytes to stay within the
  limit glitched the audio and shifted every later ICY boundary: the client
  read stream bytes as metadata and metadata as stream from then on.
- **Why it is hard.** Dropping looks like graceful degradation. The framing
  counts bytes from the start of the body, so any gap corrupts it for the rest
  of the connection.
- **How to tell.** A throttled ICY client. _Binding_: it either receives a
  contiguous stream with a block exactly every `metaint` bytes, or is
  disconnected; never a block at another offset.

**Reconciled.** Covered: [delivery.md §6](delivery.md#6-disconnection),
[shared-encoder.md §4](shared-encoder.md#4-flush),
[dedicated-encoder.md §5](dedicated-encoder.md#5-encode-and-send).

## K4. How a writer learns it can write

Seen 5 times, including one revert.

- **Situation.** A task writes to many non-blocking sockets and must sleep
  when there is nothing to do, wake when there is, and keep going after an
  error.
- **What went wrong.** Waiting on writability of sockets with nothing to send
  spun at full CPU (they are always writable). On Windows, readiness turned
  edge-triggered once any non-socket descriptor was in the wait set, so
  pending data was never flushed; non-blocking mode could not be set on
  pipes; and a non-socket descriptor in the set made each wait leak native
  memory (several megabytes per minute with one listener). A writer that hit
  one exception stopped for good.
- **Why it is hard.** Readiness semantics differ by platform and by
  descriptor type, and the Linux test suite passes either way. A wake-up
  mechanism is itself a descriptor in the wait set.
- **How to tell.** On every supported platform: a listener whose socket buffer
  fills and drains keeps receiving; idle CPU is flat; memory with one listener
  over an hour is flat; a writer pass that raises does not prevent the next
  pass.

**Reconciled.** Covered: wake-ups by socket pair, no writability wait
[delivery.md §2](delivery.md#2-wake-ups); re-arm after an error
[delivery.md §3](delivery.md#3-the-writer-pass). The Windows facts are
unverified here ([findings.md V1](findings.md#to-verify)). An exception that
recurs on every pass still starves the shard
([findings.md D2](findings.md#d2-long-title-plus-long-url-overflows-the-icy-length-byte--checked)).

## K5. Idle cost must not grow with the machine

Seen once.

- **Situation.** An output with no listener, on a machine with many cores, so
  many writers.
- **What went wrong.** Every writer was woken on every cycle: 13% of a core
  for an idle output on 24 threads, against under 1% before; the cost followed
  the writer count.
- **Why it is hard.** Waking writers every cycle is needed while a slow
  listener has bytes pending; the waste appears only when there is nobody to
  serve.
- **How to tell.** _Binding_: an idle output's CPU use and context switches
  per second do not depend on the number of writers.

**Reconciled.** Covered: [delivery.md §2](delivery.md#2-wake-ups).

## K6. Stopping while listeners connect, stream and leave

Seen 9 times.

- **Situation.** The output stops or restarts (by script, telnet, a fallible
  source ending, or process shutdown) while listeners are streaming and
  others are part-way through connecting.
- **What went wrong.** Encoding after stop crashed the clock thread. A second
  start or stop raised instead of doing nothing. A listener that connected
  during a stop was never closed and never reported. A connection divided by
  zero when the writer set was emptied under it. A per-listener encoder leaked
  when a connection failed half-way. A socket was closed twice, which can
  close an unrelated connection that reused the descriptor. Sleeping clients
  were not woken at shutdown and kept their sockets.
- **Why it is hard.** Four actors touch a listener (stream, request, writer,
  teardown), and the windows are a few instructions wide. The existing churn
  test passes with and without the fix for the connect-during-stop race: it
  does not reach the window.
- **How to tell.** (a) Stop with listeners connected: the process survives;
  every listener gets one disconnect. (b) Repeated stop/start with
  reconnecting clients: connects equal disconnects; no listener left; the
  process's open descriptor count returns to its baseline. (c) Deterministic
  versions of the races: a stop injected between each step of admission, and
  between a writer's flush and its removal pass, each run against a build
  with the guard removed to see the check fail. Timing-based churn alone is
  not binding.

**Reconciled.** Covered: idempotent start/stop and stop order
[operator.md §7](operator.md#7-lifecycle); admission's check after publishing
[connection.md §5](connection.md#5-admission); stopping writers and close-once
[delivery.md §7–§8](delivery.md#7-stopping-the-writers); guarantees
[concurrency.md §3](concurrency.md#3-guarantees). Not covered: an immediate
restart can fail to bind because the port closes asynchronously
([findings.md G11](findings.md#gaps)).

## K7. Listener callbacks feed accounting

Seen 3 times.

- **Situation.** Scripts build access logs and listener counts from connect
  and disconnect callbacks, with several handlers, some of which raise.
- **What went wrong.** A disconnect callback was not called, or could be
  called for a listener whose connect callback had not run, so counts drifted
  and a second listener list kept by a script went out of sync. One raising
  handler prevented the others and the cleanup. The disconnect report carried
  no duration or byte count.
- **Why it is hard.** Disconnection has many causes on several threads, and a
  listener can fail between its connect callback and its first byte.
- **How to tell.** _Binding_: for every connect, exactly one disconnect, after
  it, with the same `id` and `connected_at`; a raising handler does not stop
  the others; disconnect carries `bytes_sent` including the response head.

**Reconciled.** Covered: [operator.md §4–§5](operator.md#4-callbacks),
[concurrency.md §3](concurrency.md#3-guarantees).

## K8. ICY negotiation and per-listener metadata state

Seen 4 times.

- **Situation.** Several ICY listeners, joining at different times, with
  metadata changing.
- **What went wrong.** No ICY metadata was sent at all. ICY was never
  negotiated because the request header was looked up with its original
  capitalisation after the server had normalised it. A metadata change was
  consumed by the first listener served and never reached the others. ICY
  blocks were spliced into Ogg streams for clients that asked: an Icecast
  relay and GStreamer's Ogg demuxer, which always ask and never strip, broke.
- **Why it is hard.** Header names are case-insensitive. Metadata belongs to
  the output, but "already sent" belongs to each listener. Asking for ICY does
  not mean the client can take it: the right answer depends on the format.
- **How to tell.** Two ICY listeners, one joining late, with the request header
  in mixed case. _Binding_: both get `icy-metaint`; both receive a block with
  each new title; an unchanged title gives empty blocks. An Ogg mount asked
  for ICY answers without `icy-metaint` and sends no blocks.

**Reconciled.** Covered: [icy.md §1](icy.md#1-negotiation),
[icy.md §4](icy.md#4-cadence-and-change-detection).

## K9. The status line and the request version

Seen 2 times.

- **Situation.** Clients send `HTTP/1.0`, `HTTP/1.1`, or other versions on the
  request line.
- **What went wrong.** The response's status line lacked the `HTTP/` prefix
  and clients reset the connection. An unsupported version used to be
  answered with a generic error rather than a 505.
- **Why it is hard.** The response echoes the request's version, so every
  accepted version must map to a valid status line.
- **How to tell.** _Binding_: the status line is `HTTP/1.0 200 OK` or
  `HTTP/1.1 200 OK` matching the request; any other version gets a 505 and
  the server keeps running.

**Reconciled.** Covered for 1.0 and 1.1:
[connection.md §4](connection.md#4-the-response-head). Contradicted for other
versions: `ICE/1.0` is accepted and then crashes the process
([findings.md D1](findings.md#d1-a-get-with-protocol-ice10-crashes-the-process--checked-outside-scope)).

## K10. Memory over long runs with listeners

Seen 3 times, root cause outside the operator in at least one.

- **Situation.** An output with one or more listeners runs for hours.
- **What went wrong.** Process memory grew steadily (tens of MB per minute
  on Windows; to 750 MB over three hours on Linux) while the managed heap
  stayed flat. Causes found: a platform wait primitive leaking per call, and
  allocator arenas multiplied by the thread count.
- **Why it is hard.** The growth is outside what the runtime's own memory
  statistics see, and grows with threads, which the writer count multiplies.
- **How to tell.** _Binding_: resident memory with one listener over several
  hours stays within a bound independent of uptime, on each platform.

**Reconciled.** Not covered: the spec bounds the shared ring
([shared-encoder.md](shared-encoder.md)) and per-listener pending bytes, but
states no bound on process memory or threads
([findings.md G10](findings.md#gaps)).

## K11. One port, many users

Seen 2 times.

- **Situation.** An output shares its port with HTTP routes, harbor inputs,
  or another output.
- **What went wrong.** Registering a route on a port already used by an
  output failed with `Address already in use`. Today a second output on the
  same mount is accepted silently and stopping either removes both.
- **Why it is hard.** The port is shared state owned by the server; each user
  sees only its own registration.
- **How to tell.** _Binding_: an output, an HTTP route and an input on one
  port all serve; a second output on the same mount is either refused at
  construction or served deterministically, and stopping one leaves the other
  serving.

**Reconciled.** Covered for sharing: [contracts.md §1](contracts.md#1-harbor-server).
Not covered for duplicate mounts:
[findings.md D5](findings.md#d5-two-outputs-on-one-mount--checked).
