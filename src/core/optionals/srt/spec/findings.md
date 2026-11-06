# Findings

Kept out of the as-built text. Each finding is marked:

- **checked**: confirmed by running a script against the build of
  `c89c6beea`;
- **second read**: confirmed by an independent read that tried to refute it;
- **read only**: from one read, still a suspect.

Nothing was run against a peer other than Liquidsoap itself. The second read
refuted no finding; where it found one milder or worse, the text says so.

## Defects

- **D1. `sockets()` raises on an unconnected input caller.** The input
  caller's list catches the wrong exception, so `s.sockets()` before a
  connection raises an internal error to the script. The output caller
  returns `[]`. Checked.
- **D2. Listener callbacks that call `sockets()` deadlock.** A synchronous
  `on_connect` runs inside the source lock; `sockets()` asks for it again and
  waits forever, then the streaming thread and shutdown wait too
  ([concurrency.md §2](concurrency.md#2-the-source-lock)). The same holds for
  `on_disconnect` and `on_socket "close"`. Checked for `on_connect`: on the
  termination signal, shutdown started and never finished, with the clock
  reported stuck in its tick.
- **D3. The encoder's final bytes are never sent.** Stopping the encoder
  appends its final bytes and then empties the queue
  ([output.md §6](output.md#6-stopping-the-encoder)). What is lost depends on
  the encoder: the FFmpeg encoder's container trailer and delayed packets,
  LAME's flushed audio frames; WAV has none. The last partial payload is lost
  as well, since only whole payloads are ever sent. Second read.
- **D4. An empty passphrase is rejected.** The documentation says a
  non-empty passphrase enables encryption, implying empty disables it; the
  length check rejects `""`. Checked.
- **D5. A listener can stop accepting for good.** The readiness loop takes
  the listening socket out before calling its handler, and the handler puts it
  back only after a successful accept. An accept error, a failure to format
  the peer address, or a ready event of another kind than the registered one
  leaves it out until connect runs again
  ([listener.md §4](listener.md#4-connect)). Second read; the triggers are
  rare.
- **D6. Accepting while stopping closes the client twice** and re-registers
  the listening socket first. The second close is logged at level 5 only, and
  the disconnect that follows removes the registration again, except in a
  race. Milder than first stated. Second read.
- **D7. The readiness loop can fall asleep.** Any wait or processing error
  other than a timeout puts the loop to sleep until the next registration,
  freezing every listener of the process and dropping the rest of the ready
  batch ([poll.md §3](poll.md#3-one-iteration)). A listener removed on
  another thread between the wait and the handler lookup is enough. Second
  read.
- **D8. A caller attempt in flight survives disconnect.** Disconnect does not
  interrupt the attempt; when the connect succeeds, the attempt fires
  `on_connect` and stores the socket after `on_disconnect` has fired, leaving
  a stopped operator connected. An input with `self_sync` declares its sync
  source while stopped. The stale socket is closed only by the next attempt,
  without `on_disconnect` ([caller.md §8](caller.md#8-disconnect)). Second
  read.
- **D9. `connect()` on a connected caller replaces the connection silently.**
  The old socket closes without `on_disconnect`. The output's encoder and
  queued bytes carry over: the new peer receives a stream from the middle,
  without header. The input keeps a decoder reading the old, closed socket;
  its next read fails and the disconnect that follows drops the new
  connection too, so every such call costs one more reconnect cycle
  ([caller.md §4](caller.md#4-connect)). Worse than first stated. Second
  read.
- **D10. A raising listener `on_connect` leaves a closed client listed**
  ([failures.md §3](failures.md#3-admitting)). An input recovers at the next
  frame, which fails and disconnects; an output keeps the dead client until a
  send to it fails. `on_socket "close"` later fires for a socket already
  closed. Checked for the input.
- **D11. The readiness loop's own description says sockets return to
  blocking mode when they leave it**; nothing does that. No effect: only
  listening sockets enter the loop, and accepted sockets are made blocking
  explicitly ([poll.md §2](poll.md#2-registering-and-removing)). Second read.
- **D12. Two disconnects at once can fail the output.** The caller's task
  handle is unprotected: a disconnect from the streaming thread after a send
  failure and one from a script or shutdown can both stop the same task, and
  the second stop raises out of the send. Read only.
- **D13. Documentation text.** `prefer_address` names the setting
  `srt.prefer_connection` (it is `srt.prefer_address`); `connection_timeout`
  says "`client` mode" (the mode is `caller`); `write_timeout` says "no data
  was received"; `bistats` is described as "Socket bstats"; the poll timeout
  setting says "seconda"; a bad `settings.srt.prefer_address` is reported
  against the `prefer_address` argument. Read only.

## Asymmetries

- **A1. `on_disconnect`.** The caller fires it only when it held a socket,
  including after a peer failure. The listener fires it on every disconnect,
  even with no client, and never when it loses one client.
- **A2. Peer failure.** The caller reconnects; the listener drops the peer.
  Both restart the shared encoder and empty the queue, so with several
  listener clients one failure discards the queued bytes of every healthy
  client, and each then receives a new header mid-stream (with WAV, a second
  RIFF header). Worse than first stated. Second read.
- **A3. Headers.** A caller's peer always receives a stream from its start.
  A listener's second and later clients join mid-stream without the header:
  nothing asks the encoder for its separately available header. MPEG-TS
  repeats its tables and copes; WAV, Ogg and MP4 do not. Second read.
- **A4. Locking.** Listener callbacks run under the source lock; caller
  callbacks under none.
- **A5. The socket in callbacks.** `on_socket` receives the socket with its
  methods but typed without them; `listen_callback` and `sockets()` give it
  without methods.
- **A6. Client limits.** The input listener admits one client and listens
  with a backlog of 1. The output listener without `max_clients` admits any
  number, also with a backlog of 1.
- **A7. Stop flag.** The listener reads the operator's stop flag; a caller
  attempt reads only its task flag.
- **A8. Timeouts.** The caller sets send and receive timeouts before
  connecting; the listener sets them on each accepted socket, never on the
  listening socket. `streamid` and `connection_timeout` exist for the caller
  only, which the parameter list does not show.
- **A9. `messageapi`.** It selects the output's send call; the input always
  receives message by message.

## Gaps

- **G1. One sync source for all inputs.** Every connected `input.srt` with
  `self_sync` declares the same sync source, and the clock tells sync sources
  apart by identity, so two connected SRT inputs in one clock, each blocking
  on its own socket, raise no conflict ([input.md §7](input.md#7-pacing)).
  The history records no reason for the shared value; a shared clock for all
  SRT inputs was added and removed two weeks later. Checked: two connected
  inputs in one clock, one "paced by" line, no error.
- **G2. Sending blocks the clock.** Each payload send can block the
  streaming thread up to `write_timeout`, per socket, in sequence
  ([output.md §5](output.md#5-chunking-and-sending)). One slow listener
  client delays every other one and the clock. Read only.
- **G3. Unsynchronised state.** The output byte queue, the input decoder and
  dump file, the caller's task handle, the readiness handler table and the
  binding's record of the readiness set are written from several threads
  without a common lock ([concurrency.md §3](concurrency.md#3-shared-state)).
  A queue flush from another thread between the output's length check and its
  copy raises an out-of-bounds error out of the send and fails the output.
  Second read.
- **G4. A listener that cannot listen stops the process**, unrelated outputs
  included. Checked for the output (port already listened on) and the input
  (bind to an address the host does not have).
- **G5. No validation** of `payload_size`, `port`, `pbkeylen` or the
  timeouts' sign; a timeout under one millisecond becomes 0.
- **G6. User code on libsrt's handshake thread.** `listen_callback` runs
  there; a slow one delays every handshake of the process.
- **G7. Loaded but unused.** The readiness set and its task exist from
  program load even when no SRT operator is built; the set is created before
  libsrt is started.
- **G8. A fallible output listener drops its clients** each time its source
  becomes unavailable, and closes its listening socket
  ([output.md §3](output.md#3-lifecycle)).
- **G9. `disconnect()` on a started output** leaves it started and silent.
  `start()` does nothing to a started output; only `connect()`, or a stop and
  then a start, reconnects. Second read.
- **G10. Connecting blocks the input's clock.** The decoder is created on
  the streaming thread and may read the stream header there, each read
  blocking up to `read_timeout`. Seen: two WAV inputs took about five seconds
  of probing on the streaming thread.
- **G11. Sockets leak when `on_socket` raises at creation.** The listener
  leaks one socket and its start fails. The caller retries every
  `polling_delay`, so an `on_socket "connect"` that always raises leaks one
  socket per attempt, without end. Worse than first stated. Second read.
- **G12. Encoders forgotten without being stopped.** The output's disconnect
  handler drops the encoder without stopping it
  ([output.md §7](output.md#7-disconnect-handler)); its resources wait for
  the garbage collector, and whether an external encoder's process outlives
  it is unknown. Read only.
- **G13. Test coverage** ([conformance.md §3](conformance.md#3-untested)),
  and the reconnection test is skipped. Every test runs Liquidsoap against
  itself; a user once reported the two operators failing to reach each other
  while third-party SRT tools worked with either end.
- **G14. A silent peer cycles the connection.** The input reports itself
  ready whenever it is connected ([input.md §3](input.md#3-readiness)). A
  peer that keeps the connection but sends nothing makes each read time out,
  which ends the track, disconnects and reconnects, once per `read_timeout`.
  The issue that introduced the timeouts also asked for readiness to mean
  "data available"; that half was not done. Read only.
- **G15. The default addresses differ in family.** The caller's default host
  `localhost` may resolve to IPv6 under the system preference while the
  listener's default bind address is the IPv4 wildcard; every test pins IPv4
  for this reason. Read only.
- **G16. Long blocking work in a shared pool.** Each caller's connect task
  blocks a worker of the scheduler's threaded queue for up to its connection
  timeout per attempt, and the readiness loop for up to its poll timeout;
  enough failing callers can starve unrelated work on that queue
  ([concurrency.md §1](concurrency.md#1-threads)). Read only.
- **G17. The client limit can be exceeded.** The admission predicate counts
  listed clients only; a handshake admitted after another client is accepted
  but before it is listed passes too. Two clients on an input listener make
  its next frame fail, which drops both and reconnects. Read only; see V7.

## To verify

Claims about libsrt made from memory, not checked against its documentation
or a run:

- **V1.** Encryption options set on a listening socket apply to the sockets
  it accepts.
- **V2.** Adding a socket to an epoll set it is already in updates its flags
  without error (an input listener's reconnect does this).
- **V3.** Creating an epoll set before `srt_startup` works.
- **V4.** What `messageapi = false` means for a live-mode socket.
- **V5.** What a receive does when the incoming message exceeds the buffer,
  i.e. a receiver whose `payload_size` is smaller than the sender's.
- **V6.** libsrt's default send and receive timeouts are infinite, as
  "indefinite if `null`" says.
- **V7.** How the listen backlog interacts with the admission predicate.
- **V8.** The two core shutdown hooks run in registration order.
- **V9.** What the conversion of unsigned 64-bit statistics does past the
  native integer range.
- **V10.** Whether the readiness wait reports a broken socket in both the
  read and the write list. If it does, the loop removes it twice, and the
  second removal can fail and trigger D7.
