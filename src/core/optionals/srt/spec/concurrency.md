# Concurrency

## 1. Threads

| Thread                         | Runs                                                                                                                                                                                                                                                            |
| ------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| streaming thread (the clock's) | start and stop of both kinds; output encoding and sending ([output.md §5](output.md#5-chunking-and-sending)), so the output's peer failure; input decoding and receiving ([input.md §4](input.md#4-decoder)), so the input's disconnect and reconnect on error. |
| readiness loop                 | the listener's accept handler, so `on_socket "incoming"` and the listener's `on_connect` ([poll.md](poll.md)).                                                                                                                                                  |
| caller connect task            | connection attempts, so the caller's `on_connect` ([caller.md §4](caller.md#4-connect)).                                                                                                                                                                        |
| libsrt handshake thread        | the admission predicate, so `listen_callback` and the client-limit count ([listener.md §3](listener.md#3-socket-setup)).                                                                                                                                        |
| libsrt log thread              | the log handler ([library.md §2](library.md#2-log-routing)).                                                                                                                                                                                                    |
| script threads                 | `connect()`, `disconnect()`, `sockets()`, the socket value's methods.                                                                                                                                                                                           |
| scheduler threaded queue       | callbacks registered with `synchronous=false`.                                                                                                                                                                                                                  |
| shutdown                       | the registry's disconnects ([library.md §4](library.md#4-shutdown)).                                                                                                                                                                                            |

The readiness loop and the connect tasks share the scheduler's threaded
queue with other work. Blocking libsrt calls release the runtime
([library.md §6](library.md#6-what-is-relied-upon-from-the-binding)).

## 2. The source lock

Each operator has one lock, provided by the generic source and used only by
the SRT code, taken in one of two modes:

- **short**: for reads and small writes;
- **mutating**: for longer work. A thread waiting for the lock while it is
  held mutating sleeps; while it is held short, it spins.

The lock is not reentrant in either mode: a thread holding it that asks for it
again waits forever.

| Taken                                                                   | Mode     |
| ----------------------------------------------------------------------- | -------- |
| listener: connected test, `sockets()`                                   | short    |
| listener: client-limit count in the admission predicate                 | short    |
| listener: get or create the listening socket and register it            | short    |
| listener: prepend an accepted client                                    | short    |
| listener: fire `on_connect`                                             | mutating |
| listener: disconnect, including `on_socket "close"` and `on_disconnect` | mutating |
| listener: peer failure, including `on_socket "close"`                   | mutating |
| output: write the stop flag in start and stop                           | short    |
| output: get or create the encoder                                       | short    |

The caller role takes no lock; its socket and flags are atomic cells.

Consequences, for the listener:

- A synchronous `on_connect`, `on_disconnect` or `on_socket "close"` runs
  inside the lock. One that calls `sockets()` waits forever, and so does
  every later user of the lock: the streaming thread at its next connected
  test, and process shutdown.
- While any of these callbacks runs, the streaming thread waits at its next
  connected test, so a slow synchronous callback stalls the clock.

## 3. Shared state

| State                      | Written by                                                               | Protection                                  |
| -------------------------- | ------------------------------------------------------------------------ | ------------------------------------------- |
| stop flags                 | streaming thread, script threads, shutdown                               | atomic                                      |
| caller socket              | connect task, any thread that disconnects                                | atomic                                      |
| caller connect task handle | connect and disconnect, any thread                                       | none                                        |
| listener clients           | readiness loop, streaming thread, any thread that disconnects            | source lock                                 |
| listener listening socket  | connect, any thread that disconnects                                     | atomic cell, creation under the source lock |
| output byte queue          | streaming thread; `on_disconnect` handler on any thread that disconnects | none                                        |
| output encoder             | streaming thread; `on_disconnect` handler                                | atomic cell, creation under the source lock |
| input decoder, dump file   | streaming thread; handlers on any thread that connects or disconnects    | none                                        |
| readiness handler table    | readiness loop; any thread that connects or disconnects a listener       | none                                        |
