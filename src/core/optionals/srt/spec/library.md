# libsrt lifecycle, settings and address resolution

Process-wide behaviour, shared by every SRT operator.

## 1. Initialisation

The first construction of any SRT operator runs a one-time initialisation,
exactly once even when several threads construct operators at the same
time. It registers two application hooks and does nothing else:

1. An **application start** hook, named `srt initialization`, which:
   1. starts libsrt;
   2. if `settings.srt.log` is `true`, maps `settings.srt.log.verbosity` to a
      libsrt log level (`critical`, `error`, `warning`, `notice`, `debug`; any
      other value logs `Invalid value for "srt.log.verbosity"!` at level 2 and
      uses `error`), sets it, and installs the log handler of §2.
2. A **final cleanup** hook, named `srt cleanup`, which runs after every SRT
   socket of the process is closed, releases the readiness set of
   [poll.md](poll.md), then cleans libsrt up.

libsrt starts when the application starts, after daemonising, because a
forked child does not inherit libsrt's threads. It starts only if a script
constructed an SRT operator, so a process that never uses SRT (`--check`,
`--version`, a script without SRT) never calls libsrt and never runs its
exit-time destructors. Start and cleanup MUST be balanced in the same
process.

No libsrt call happens before libsrt is started: the readiness set and the
readiness loop are created on first use ([poll.md §1](poll.md#1-model)).

## 2. Log routing

libsrt delivers its log messages on its own threads. The handler MUST NOT
block those threads on the host runtime: it queues each message, and a
thread of the host logs it. Logging removes every trailing run of spaces,
carriage returns and line feeds, then logs the text under the `srt` log name
at the level `settings.srt.log.level`, whatever the message's own libsrt
severity.

## 3. Settings

| Setting                            | Type   | Default     | Read                                                                         |
| ---------------------------------- | ------ | ----------- | ---------------------------------------------------------------------------- |
| `settings.srt.prefer_address`      | string | `"system"`  | At each operator construction whose `prefer_address` is null.                |
| `settings.srt.log`                 | bool   | `true`      | At application start.                                                        |
| `settings.srt.log.verbosity`       | string | `"warning"` | At application start.                                                        |
| `settings.srt.log.level`           | int    | `4`         | At every libsrt log message.                                                 |
| `settings.srt.poll.timeout`        | float  | `0.1`       | At every readiness wait: its longest duration, in seconds.                   |
| `settings.srt.enforced_encryption` | bool   | `true`      | At every socket creation ([connection.md §3](connection.md#3-socket-setup)). |

`prefer_address` accepts `"system"`, `"ipv4"`, `"ipv6"`; validation is in
[common.md §1](common.md#1-parameters).

## 4. Shutdown

At core shutdown, one step:

1. Set the process-wide **shutdown flag**: no operator wants a connection
   from then on ([common.md §5](common.md#5-connection-wanted)).
2. Release every operator still alive
   ([connection.md §9](connection.md#9-stopping)).

Operators are found through a registry that MUST NOT keep them alive: an
operator built and dropped by a script is collected, and the registry
forgets it. A registry that holds operators strongly leaks every
dynamically built one.

## 5. Address resolution

Both roles resolve a host name and port the same way:

1. Ask the system resolver for `host` and the port as a decimal string, with
   hints: socket type datagram, and family IPv4 for `ipv4`, IPv6 for `ipv6`,
   none for `system`.
2. Keep every answer, in the resolver's order. At log level 5, log
   `Address <host>:<port> resolved to: <ip>:<port>, ...`.
3. No answer is a transient failure ([connection.md §4](connection.md#4-connect))
   with the message `Could not resolve address: <host>:<port>`.

The caller tries the answers in order
([caller.md §4](caller.md#4-connect)): on a dual-stack host its default
`localhost` reaches a listener bound to either family. The listener binds the
first answer.

## 6. What is relied upon from the binding

- A failing libsrt call raises an error carrying the libsrt error code and its
  message, and clears libsrt's last error. The code is what failure
  classification reads ([connection.md §4](connection.md#4-connect)).
- Blocking calls (connect, accept, send, receive, readiness wait) release the
  runtime while they block, so other threads run.
- Closing a socket makes every call blocked on it return with an error.
- The admission predicate installed on a listening socket is called on a
  libsrt thread during each handshake with the handshake version, the peer
  address, the stream id (absent when the caller set none) and the new
  socket. `true` admits; `false` or an exception rejects. Closing the
  listening socket frees the predicate.
- A send buffer is copied before the runtime is released, so the collector
  cannot move it while libsrt reads it.
- The readiness wait returns the read-ready and write-ready sockets, and
  fails with a timeout error when nothing became ready, and with an
  empty-set error when the set holds no socket.

## 7. libsrt behaviour assumed

Claims about libsrt this spec builds on, not checked against its
documentation or a run. Each says what the spec needs if it does not hold.

- Options set on a listening socket, encryption and latency included, apply
  to the sockets it accepts. If not, accepted sockets get them in
  [listener.md §3](listener.md#3-socket-setup).
- Closing a socket removes it from every readiness set without notice;
  waiting on an empty set fails at once; removing an absent socket fails.
  The loop tolerates all three ([poll.md §3](poll.md#3-one-iteration)).
- A socket accepted from a non-blocking listening socket inherits
  non-blocking mode.
- Whether a connected socket uses timestamp-based packet delivery can be read
  after the handshake. If not, a live-mode connection is taken as
  time-controlled ([input.md §7](input.md#7-pacing)).
- Closing a socket wakes a receive blocked on it (§6). If not, the reader
  bounds each receive by a short internal timeout and checks whether its
  peer was ended.
- The library can tell when a socket's send buffer is empty, so a clean end
  closes after delivery ([output.md §6](output.md#6-stopping)).
- The largest live-mode payload is 1456 bytes; valid key lengths are 16, 24
  and 32.
- What `messageapi = false` means for a live-mode socket.
- Unsigned 64-bit statistics past the native integer range saturate at its
  maximum ([socket-value.md §4](socket-value.md#4-statistics)).
