# What both operators share

## 1. Parameters

Both `input.srt` and `output.srt` take these parameters. The role column says
which role reads each one; the other role ignores it without warning.

| Parameter             | Type                                                                          | Default                               | Role     | Meaning                                                                                                |
| --------------------- | ----------------------------------------------------------------------------- | ------------------------------------- | -------- | ------------------------------------------------------------------------------------------------------ |
| `mode`                | string                                                                        | input `"listener"`, output `"caller"` | both     | `"listener"` or `"caller"`.                                                                            |
| `host`                | string                                                                        | `"localhost"`                         | caller   | Address to connect to.                                                                                 |
| `port`                | int                                                                           | `8000`                                | both     | Port to bind (listener) or connect to (caller).                                                        |
| `bind_address`        | string                                                                        | `"0.0.0.0"`                           | listener | Local address to bind.                                                                                 |
| `prefer_address`      | string?                                                                       | null                                  | both     | Address family for resolution; null reads `settings.srt.prefer_address`.                               |
| `ipv6only`            | bool?                                                                         | null                                  | listener | See [listener.md §3](listener.md#3-socket-setup).                                                      |
| `listen_callback`     | `(hs_version: int, peeraddr: string, streamid: string?, srt_socket) -> bool`? | null                                  | listener | Admission predicate.                                                                                   |
| `streamid`            | string?                                                                       | null                                  | caller   | Stream id sent in the handshake.                                                                       |
| `passphrase`          | string?                                                                       | null                                  | both     | §2.                                                                                                    |
| `pbkeylen`            | int?                                                                          | null                                  | both     | §2.                                                                                                    |
| `enforced_encryption` | bool?                                                                         | null                                  | both     | §2.                                                                                                    |
| `polling_delay`       | float                                                                         | `2.`                                  | caller   | Seconds between connection attempts.                                                                   |
| `read_timeout`        | float?                                                                        | `1.`                                  | both     | Receive timeout in seconds; null leaves libsrt's default.                                              |
| `write_timeout`       | float?                                                                        | `1.`                                  | both     | Send timeout in seconds; null leaves libsrt's default.                                                 |
| `connection_timeout`  | float?                                                                        | null                                  | caller   | Connect timeout in seconds; null leaves libsrt's default.                                              |
| `payload_size`        | int                                                                           | `1316`                                | both     | libsrt payload size, and the output's message size ([output.md §5](output.md#5-chunking-and-sending)). |
| `messageapi`          | bool                                                                          | `true`                                | both     | libsrt message API flag; also selects the output's send call.                                          |

`1316` is seven 188-byte MPEG-TS packets, libsrt's live-mode default payload.

**Conversions.** The three timeouts become integer milliseconds as
`truncate(1000 × seconds)`. Nothing checks their sign or range, nor
`payload_size`, `port` or `pbkeylen`.

**Validation**, at operator construction, each failure an invalid-value error
on the named argument with this exact message:

| Check                                                                | Message                                                         |
| -------------------------------------------------------------------- | --------------------------------------------------------------- |
| `mode` is neither `"listener"` nor `"caller"`                        | ``Invalid mode! Should be one of: `"listener"` or `"caller"`.`` |
| effective `prefer_address` not one of `"system"`, `"ipv4"`, `"ipv6"` | ``Valid values are: `"system"`, `"ipv4"` or `"ipv6"`.``         |
| `passphrase` shorter than 10 bytes                                   | `Passphrase must be at least 10 characters long!`               |
| `passphrase` longer than 79 bytes                                    | `Passphrase must be at most 79 characters long!`                |

When `prefer_address` is null and the setting holds a bad value, the error
still names the `prefer_address` argument. Lengths are byte counts. An empty
passphrase is rejected by the first check.

## 2. Encryption

`passphrase`, `pbkeylen` and `enforced_encryption` are libsrt socket options,
each set only when given:

- on the caller's socket before it connects
  ([caller.md §3](caller.md#3-socket-setup));
- on the listener's listening socket before it listens
  ([listener.md §3](listener.md#3-socket-setup)). Accepted sockets receive
  nothing directly.

Every socket first receives `settings.srt.enforced_encryption`
([connection.md §3](connection.md#3-socket-setup)); a given
`enforced_encryption` overrides it afterwards on the sockets listed above.

## 3. Callbacks

Each operator registers three callbacks of its own, on top of its kind's
generic ones (`on_start`, `on_stop`, and `on_output` for the output):

| Callback        | Arguments                    | Fired                                                                                                    |
| --------------- | ---------------------------- | -------------------------------------------------------------------------------------------------------- |
| `on_connect`    | none                         | [connection.md §5](connection.md#5-connected)                                                            |
| `on_disconnect` | none                         | [connection.md §8](connection.md#8-disconnect), and per role                                             |
| `on_socket`     | `mode: string`, `srt_socket` | `"connect"`, `"listen"`: socket created; `"incoming"`: socket accepted; `"close"`: socket about to close |

**Registration** is a method on the operator:
`x.on_connect(synchronous=b, fn)`. It returns a value whose `release()`
unregisters `fn`; a second `release()` does nothing.

- `synchronous=true`: `fn` runs inline, on the thread that fires the event and
  inside whatever lock it holds ([concurrency.md](concurrency.md)).
- `synchronous=false`: firing queues a task with no delay on the scheduler's
  threaded queue, and `fn` runs there later.

Exceptions from a synchronous `fn` propagate into the code that fired it
([failures.md](failures.md)).

**Order.** Callbacks fire in registration order. Each operator registers its
own internal handlers at construction ([input.md §6](input.md#6-connect-and-disconnect-handlers),
[output.md §7](output.md#7-disconnect-handler)), so they run before any script
callback.

**Deprecated arguments.** When deprecated features are enabled (the default),
each of the three, and `on_start`/`on_stop`, is also an optional argument of
the operator. Passing one logs at level 2
``The `<name>` argument is deprecated! Please use the source's `<name>` method.``
and registers it as `synchronous=true`.

**The socket argument.** `on_socket` receives the socket carrying all the
methods of [socket-value.md](socket-value.md), though its declared type is the
plain `srt_socket`; a script reaches the methods through `srt.socket(s)`.

## 4. Methods

| Method         | Type                            | Effect                                                                                                                                               |
| -------------- | ------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------- |
| `sockets()`    | `() -> [(string * srt_socket)]` | The role's sockets list ([connection.md §6](connection.md#6-sockets)), each address rendered as its IP alone, without port, sockets without methods. |
| `connect()`    | `() -> unit`                    | Clear the stop flag, then the role's connect.                                                                                                        |
| `disconnect()` | `() -> unit`                    | Set the stop flag, then the role's disconnect.                                                                                                       |

Both kinds also carry the generic start/stop methods (`is_started`, `start`,
`stop`) and the generic source methods. `start` and `stop` queue a transition
that runs before the next streaming cycle; `connect` and `disconnect` act at
once on the calling thread.

## 5. Stop flag

Each operator has a **stop flag**, initially clear. The operator is
**stopping** when its stop flag is set or the process shutdown flag
([library.md §4](library.md#4-shutdown)) is set.

| Set by                          | Cleared by                    |
| ------------------------------- | ----------------------------- |
| the kind's stop, `disconnect()` | the kind's start, `connect()` |

The caller role keeps a second flag of its own for its connect task
([caller.md §2](caller.md#2-state)).
