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
   [poll.md](poll.md), waits until no thread is inside a blocking libsrt
   call, then cleans libsrt up. Cleaning libsrt up while a thread is inside
   one of its calls corrupts the process's memory: if a call has not
   returned within `cleanup_wait`, the cleanup is skipped and logged at
   level 2, and the process exits without it. Recommended value: 5 seconds.

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

- A failing libsrt call raises an error carrying the libsrt error code, the
  system error behind it when there is one, and libsrt's message, and
  clears libsrt's last error. Failure classification reads the code and the
  system error ([connection.md §4](connection.md#4-connect)).
- After a refused connect, the socket gives the library's rejection reason
  as text.
- Blocking calls (connect, accept, send, receive, readiness wait) release the
  runtime while they block, so other threads run.
- Closing a socket makes every call blocked on it return with an error.
- The admission predicate installed on a listening socket is called on a
  libsrt thread during each handshake with the handshake version, the peer
  address, the stream id (absent when the caller set none) and the new
  socket. `true` admits; `false` or an exception rejects. Closing the
  listening socket frees the predicate.
- The largest live-mode payload is 1456 bytes: the 1500-byte MTU less the
  UDP and SRT headers, as libsrt's public header defines it.
- A send buffer is copied before the runtime is released, so the collector
  cannot move it while libsrt reads it.
- The readiness wait returns the read-ready and write-ready sockets, and
  fails with a timeout error when nothing became ready, and with an
  empty-set error when the set holds no socket.
- Statistics include the number of packets still in a socket's send buffer.

## 7. libsrt behaviour relied upon

Checked against libsrt 1.5 with a probe of the binding. Each item says what
the spec does with it.

- Options set on a listening socket apply to the sockets it accepts:
  latency, key length, encryption, and blocking mode. Accepted sockets
  inherit the listening socket's non-blocking mode, so their owner sets the
  mode it uses ([poll.md §2](poll.md#2-registering-and-removing)).
- Removing a socket from a readiness set succeeds whether or not it is
  there, closed sockets included; waiting on an empty set fails at once.
  The loop tolerates both ([poll.md §3](poll.md#3-one-iteration)).
- A blocking accept does not honour the receive timeout. The listener
  accepts only when the readiness loop reports a pending connection.
- Closing a socket wakes a receive blocked on it, which fails with
  connection lost.
- A remote close is seen by the receiver as connection lost, never as a
  zero-length message; data the sender queued but libsrt had not sent yet is
  lost.
- Closing a socket discards what its send buffer still holds. A clean end
  waits until the send buffer statistics reach zero, bounded by
  `write_timeout`, before closing ([output.md §6](output.md#6-stopping)).
- A receiver drops the data it holds back for timestamp-based delivery when
  the sender's close arrives before that data is due: a sender that closes
  as soon as its send buffer is empty loses its last latency's worth of
  data, and with a short stream all of it. A clean end therefore also waits
  for the peer's latency ([output.md §6](output.md#6-stopping)).
- The timestamp-based delivery option, read on a connected socket, reports
  that socket's own configuration. A receiver cannot tell whether its sender
  turned timestamps off: the option and the statistics both report the
  receiver's setting. The input paces on the receiving socket's
  configuration ([input.md §7](input.md#7-pacing)).
- Valid key lengths are 0 (the library's default), 16, 24 and 32; others
  are rejected as invalid parameters.
- Cleaning the library up while another thread is inside one of its calls,
  a connect blocked on an unreachable peer for instance, corrupts the heap
  ([§1](#1-initialisation)).
- A closed socket stays known to the library for a short while: calls on it
  may still succeed until its status reads closed or nonexistent, and fail
  from then on ([socket-value.md §1](socket-value.md#1-the-value)).
- A live-mode socket without the message API fails every send and receive
  ([common.md §1](common.md#1-parameters)).
- A message larger than the socket's payload size is refused; the payload
  size accepts 1 to 1456.
- Several SRT sockets of one process may bind the same port; the second to
  listen on it fails with a duplicate-listen error, which is transient.
- Unsigned 64-bit statistics past the native integer range saturate at its
  maximum ([socket-value.md §4](socket-value.md#4-statistics)).
