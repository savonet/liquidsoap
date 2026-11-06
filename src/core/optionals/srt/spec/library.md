# libsrt lifecycle, settings and address resolution

Process-wide behaviour, shared by every SRT operator.

## 1. Initialisation

The first construction of any SRT operator runs a one-time initialisation,
guarded so that concurrent constructions run it once. It registers two
application hooks and does nothing else:

1. An **application start** hook, named `srt initialization`, which:
   1. starts libsrt;
   2. if `settings.srt.log` is `true`, maps `settings.srt.log.verbosity` to a
      libsrt log level (`critical`, `error`, `warning`, `notice`, `debug`; any
      other value logs `Invalid value for "srt.log.verbosity"!` at level 2 and
      uses `error`), sets it, and installs the log handler of §2.
2. A **final cleanup** hook, named `set cleanup`, which releases the shared
   readiness set of [poll.md](poll.md) and then cleans libsrt up.

libsrt therefore starts when the application starts, after daemonising, and
only if a script constructed an SRT operator. The history records both as
deliberate changes ("Start srt after daemonizing", "Conditionally start srt")
without stating a reason.

Two things exist from program load, whether or not SRT is used: the shared
readiness set (created through libsrt before libsrt is started) and the
readiness loop's task, registered on the scheduler and asleep
([poll.md §1](poll.md#1-model)).

## 2. Log routing

libsrt log messages are delivered to the handler on a thread the binding
starts. The handler removes every trailing run of spaces, carriage returns and
line feeds, then logs the text under the `srt` log name at the level
`settings.srt.log.level`, whatever the message's own libsrt severity.

## 3. Settings

| Setting                            | Type   | Default     | Read                                                                         |
| ---------------------------------- | ------ | ----------- | ---------------------------------------------------------------------------- |
| `settings.srt.prefer_address`      | string | `"system"`  | At each operator construction whose `prefer_address` is null.                |
| `settings.srt.log`                 | bool   | `true`      | At application start.                                                        |
| `settings.srt.log.verbosity`       | string | `"warning"` | At application start.                                                        |
| `settings.srt.log.level`           | int    | `4`         | At every libsrt log message.                                                 |
| `settings.srt.poll.timeout`        | float  | `0.1`       | At every readiness wait, in seconds.                                         |
| `settings.srt.enforced_encryption` | bool   | `true`      | At every socket creation ([connection.md §3](connection.md#3-socket-setup)). |

`prefer_address` accepts `"system"`, `"ipv4"`, `"ipv6"`; validation is in
[common.md §1](common.md#1-parameters).

## 4. Shutdown

Two **core shutdown** hooks are registered when the module loads, in this
order:

1. `srt shutdown` sets a process-wide **shutdown flag**. Every operator's stop
   flag reads as set from then on ([common.md §5](common.md#5-stop-flag)).
2. `Srt disconnect` calls the role's disconnect
   ([connection.md §8](connection.md#8-disconnect)) on every operator still
   alive. Operators are tracked in a weak registry keyed by an instance number
   taken from a process-wide counter at construction; an operator collected by
   the garbage collector drops out of it.

With the shutdown flag set, a listener's disconnect also closes its listening
socket.

## 5. Address resolution

Both roles resolve a host name and port the same way:

1. Ask the system resolver for `host` and the port as a decimal string, with
   hints: socket type stream, and family IPv4 for `ipv4`, IPv6 for `ipv6`,
   none for `system`.
2. Use the first answer. At log level 5, log
   `Address <host>:<port> resolved to: <ip>:<port>`.
3. No answer raises a runtime error of kind `srt` with the message
   `getaddrinfo could not resolve address: <host>:<port>`.

The stream type in the hints only narrows the answers to one per address;
the socket that uses the address is SRT over UDP.

## 6. What is relied upon from the binding

- A failing libsrt call raises an error carrying the libsrt error code and its
  message, and clears libsrt's last error.
- Blocking calls (connect, accept, send, receive, readiness wait) release the
  runtime while they block, so other threads run.
- The admission predicate installed on a listening socket is called on a
  libsrt thread during each handshake with the handshake version, the peer
  address, the stream id (absent when the caller set none) and the new socket.
  `true` admits; `false` or an exception rejects. Closing the listening socket
  frees the predicate.
- Receiving a message returns its length in bytes, `0` when libsrt reports no
  data.
- Sending returns the number of bytes libsrt accepted.
- The readiness wait returns the read-ready and write-ready sockets, each list
  sorted and without duplicates, and fails with a timeout error when nothing
  became ready within the timeout and with an empty-set error when the set
  holds no socket.
