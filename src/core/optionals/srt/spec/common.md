# What both operators share

## 1. Parameters

Both `input.srt` and `output.srt` take these parameters. The role and kind
columns say who reads each one; the others ignore it.

| Parameter             | Type                                                                          | Default                               | Role     | Kind   | Meaning                                                                                                   |
| --------------------- | ----------------------------------------------------------------------------- | ------------------------------------- | -------- | ------ | --------------------------------------------------------------------------------------------------------- |
| `mode`                | string                                                                        | input `"listener"`, output `"caller"` | both     | both   | `"listener"` or `"caller"`.                                                                               |
| `host`                | string                                                                        | `"localhost"`                         | caller   | both   | Address to connect to.                                                                                    |
| `port`                | int                                                                           | `8000`                                | both     | both   | Port to bind (listener) or connect to (caller).                                                           |
| `bind_address`        | string                                                                        | `"0.0.0.0"`                           | listener | both   | Local address to bind.                                                                                    |
| `prefer_address`      | string?                                                                       | null                                  | both     | both   | Address family for resolution; null reads `settings.srt.prefer_address`.                                  |
| `ipv6only`            | bool?                                                                         | null                                  | listener | both   | See [listener.md §3](listener.md#3-socket-setup).                                                         |
| `listen_callback`     | `(hs_version: int, peeraddr: string, streamid: string?, srt_socket) -> bool`? | null                                  | listener | both   | Admission predicate.                                                                                      |
| `streamid`            | string?                                                                       | null                                  | caller   | both   | Stream id sent in the handshake.                                                                          |
| `passphrase`          | string?                                                                       | null                                  | both     | both   | §2.                                                                                                       |
| `pbkeylen`            | int?                                                                          | null                                  | both     | both   | §2.                                                                                                       |
| `enforced_encryption` | bool?                                                                         | null                                  | both     | both   | §2.                                                                                                       |
| `polling_delay`       | float                                                                         | `2.`                                  | both     | both   | Seconds between tries after a transient failure ([connection.md §4](connection.md#4-connect)).            |
| `connection_timeout`  | float?                                                                        | null                                  | caller   | both   | Connect timeout in seconds; null leaves libsrt's default.                                                 |
| `read_timeout`        | float?                                                                        | `1.`                                  | both     | input  | Longest wait for data while the input paces its clock ([input.md §7](input.md#7-pacing)); null: no limit. |
| `write_timeout`       | float?                                                                        | `1.`                                  | both     | output | Longest a peer may accept no data before it fails ([output.md §5](output.md#5-sending)); null: no limit.  |
| `payload_size`        | int                                                                           | `1316`                                | both     | both   | libsrt payload size, and the output's message size ([output.md §10](output.md#10-wire-bytes)).            |
| `messageapi`          | bool                                                                          | `true`                                | both     | both   | libsrt message API flag.                                                                                  |

`1316` is seven 188-byte MPEG-TS packets, libsrt's live-mode default payload.
The operators' documentation states these meanings, with the names of the
modes and settings exactly as they are.

**Durations** become integer milliseconds rounded up, so a positive duration
never becomes 0, which the library reads as "do not wait".

**Validation**, at operator construction. Each failure is an invalid-value
error on the parameter it names, and the message states the valid values:

| Check                                                                                 | Message                                                         |
| ------------------------------------------------------------------------------------- | --------------------------------------------------------------- |
| `mode` is neither `"listener"` nor `"caller"`                                         | ``Invalid mode! Should be one of: `"listener"` or `"caller"`.`` |
| `prefer_address` not one of `"system"`, `"ipv4"`, `"ipv6"`                            | ``Valid values are: `"system"`, `"ipv4"` or `"ipv6"`.``         |
| `port` outside 1–65535                                                                | `Port must be between 1 and 65535!`                             |
| `payload_size` outside 1 to the library's largest live payload                        | `Payload size must be between 1 and <max>!`                     |
| `pbkeylen` not one of 0, 16, 24, 32                                                   | `Key length must be one of: 0, 16, 24 or 32!`                   |
| a non-empty `passphrase` shorter than 10 bytes                                        | `Passphrase must be at least 10 characters long!`               |
| `passphrase` longer than 79 bytes                                                     | `Passphrase must be at most 79 characters long!`                |
| `polling_delay`, `connection_timeout`, `read_timeout` or `write_timeout` not positive | `Value must be positive!`                                       |

When `prefer_address` is null, a bad `settings.srt.prefer_address` is
reported against the setting, not the argument. Lengths are byte counts.

## 2. Encryption

`passphrase`, `pbkeylen` and `enforced_encryption` are libsrt socket options,
each set only when given. A null or empty `passphrase` means no encryption.
A `pbkeylen` of 0 is libsrt's default key length, the same as null.
They are set:

- on the caller's socket before it connects
  ([caller.md §3](caller.md#3-socket-setup));
- on the listener's listening socket before it listens
  ([listener.md §3](listener.md#3-socket-setup)); accepted sockets inherit
  them.

Every socket first receives `settings.srt.enforced_encryption`
([connection.md §3](connection.md#3-socket-setup)); a given
`enforced_encryption` overrides it.

## 3. Callbacks

Each operator has three callbacks of its own, on top of its kind's generic
ones (`on_start`, `on_stop`, and `on_output` for the output):

| Callback        | Arguments                    | Fired                                                                                                           |
| --------------- | ---------------------------- | --------------------------------------------------------------------------------------------------------------- |
| `on_connect`    | none                         | once per peer, before it is live ([connection.md §5](connection.md#5-connected))                                |
| `on_disconnect` | none                         | once per peer that fired `on_connect`, after its socket closed ([connection.md §8](connection.md#8-disconnect)) |
| `on_socket`     | `mode: string`, `srt_socket` | `"connect"`, `"listen"`: socket created; `"incoming"`: socket accepted; `"close"`: socket about to close        |

**Registration** is a method on the operator:
`x.on_connect(synchronous=b, fn)`. It returns a value whose `release()`
unregisters `fn`; a second `release()` does nothing.

- `synchronous=true`: `fn` runs before the operator goes on with the event
  that fired it, as [concurrency.md §2](concurrency.md#2-callbacks-and-locks)
  states.
- `synchronous=false`: firing queues `fn` on the scheduler's threaded queue,
  and it runs there later.

**Order.** For each event, the operator's own reaction completes first, then
the callbacks run in registration order.

**Exceptions** from a callback are logged at level 2 with the callback's
name, and the operator goes on as if it had returned. `listen_callback` is
the exception: raising rejects the handshake
([listener.md §3](listener.md#3-socket-setup)).

**Deprecated arguments.** When deprecated features are enabled (the default),
each of the three, and `on_start`/`on_stop`, is also an optional argument of
the operator. Passing one logs at level 2
``The `<name>` argument is deprecated! Please use the source's `<name>` method.``
and registers it as `synchronous=true`.

**The socket argument** carries the methods of
[socket-value.md](socket-value.md), and is typed with them, wherever a
socket reaches a script.

## 4. Methods

| Method         | Type                            | Effect                                                                                                                                             |
| -------------- | ------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------- |
| `sockets()`    | `() -> [(string * srt_socket)]` | The live peers ([connection.md §6](connection.md#6-sockets)), each address rendered as its IP without port. Never raises; `[]` when there is none. |
| `connect()`    | `() -> unit`                    | Clear the hold (§5).                                                                                                                               |
| `disconnect()` | `() -> unit`                    | Set the hold (§5).                                                                                                                                 |

Every method MAY be called from any thread, callbacks included, and returns
without waiting for another callback. Events a method causes fire after the
callback that called it has returned.

Both kinds also carry the generic start/stop methods (`is_started`, `start`,
`stop`) and the generic source methods. `start` and `stop` queue a transition
that runs before the next streaming cycle; `connect` and `disconnect` take
effect at once.

## 5. Connection wanted

The operator **wants a connection** while:

- it is started (the kind's generic start/stop state);
- it is not **held**;
- the process is not shutting down ([library.md §4](library.md#4-shutdown)).

| Hold set by    | Hold cleared by               |
| -------------- | ----------------------------- |
| `disconnect()` | `connect()`, the kind's start |

When the predicate becomes true the role connects; when it becomes false
the role releases ([connection.md §8](connection.md#8-disconnect)). A held,
started operator stays started and exchanges no data: an input is not ready,
an output encodes nothing. `connect()` on a stopped operator clears the hold
and nothing else; the connection follows the next start.
