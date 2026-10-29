# Concurrency and ownership

What work exists, its scheduler class, what it waits on, who may touch which
part of a listener, and the guarantees that follow. The rules are owned by the
documents linked; this file states how they fit together.

Every decision about domains, thread counts and where work runs belongs to
the scheduler ([contracts.md §5](contracts.md#5-scheduler)). The operator
states only the classes below and creates no thread of its own.

## 1. Work

| Work              | Class                                                                  | Waits on                              | Does                                                                                                                                   |
| ----------------- | ---------------------------------------------------------------------- | ------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| **Streaming**     | the output's clock                                                     | the clock                             | metadata, encode, send ([modes.md §5–§6](modes.md#5-encode-and-send))                                                                  |
| **Request**       | the harbor server's request task; may block during `auth` and `accept` | the request socket, user code         | authentication, predicate, listener creation, `on_connect`, initial bytes, publication ([connection.md §6](connection.md#6-admission)) |
| **Delivery pass** | fast I/O (non-blocking class), one at a time per group                 | the group's wake-up, the retry delay  | flush, timeout, removal, socket close ([delivery.md §3](delivery.md#3-the-delivery-pass))                                              |
| **Teardown**      | work allowed to block, one per listener                                | nothing                               | per-listener encoder stop, `on_disconnect` ([delivery.md §6](delivery.md#6-disconnection))                                             |
| **Start/stop**    | whatever calls `start` / `stop` (script, clock, server)                | the output's start/stop serialisation | [operator.md §7](operator.md#7-lifecycle)                                                                                              |
| **Shutdown hook** | the core's shutdown                                                    | nothing                               | marks every listener closed ([operator.md §7](operator.md#7-lifecycle))                                                                |

User code (`auth`, `accept`, synchronous callbacks) runs only on request,
teardown and start/stop work, never in a delivery pass. Encoder code runs on
the streaming clock, the request (dedicated creation and header), start/stop
and teardown, never in a delivery pass.

## 2. Ownership of a listener

| Part                               | Written by                                                                                                  |
| ---------------------------------- | ----------------------------------------------------------------------------------------------------------- |
| record, socket, ICY negotiation    | the request, at creation; immutable afterwards                                                              |
| ICY position, last generation sent | the request before publication; then the delivery pass (shared) or the streaming clock (dedicated)          |
| last progress, `bytes_sent`        | the delivery pass; `bytes_sent` readable by anyone                                                          |
| shared `offset`, carried bytes     | the request before publication; the delivery pass after                                                     |
| dedicated queue                    | the request (initial bytes), the streaming clock and stop (appends), the delivery pass (drops)              |
| dedicated encoder                  | the request (creation, header), the streaming clock (encode, metadata), stop or teardown (stop); serialised |
| closed flag                        | anyone, race-free; exactly one marking wins                                                                 |
| socket close                       | admission before publication; the group after; exactly once                                                 |

## 3. Guarantees

1. **`on_connect` before any byte.** Synchronous `on_connect` handlers finish
   before the listener is published, so before any byte reaches its socket.
2. **`on_disconnect` exactly once, after `on_connect`.** Every path after
   `on_connect` ends with the listener marked closed
   ([connection.md §7](connection.md#7-failure-paths)); every published
   listener is eventually marked closed (write error, overflow, timeout,
   failure, stop, shutdown); and teardown runs once per listener. Marking can
   only follow `on_connect`, so synchronous `on_disconnect` handlers follow
   the synchronous `on_connect` ones. Requests refused before `on_connect` get
   neither.
3. **`bytes_sent` is final in `on_disconnect`**: teardown runs after the
   listener's socket is closed ([delivery.md §6](delivery.md#6-disconnection)).
4. **No listener outlives its group.** After stop, every listener is marked
   closed and its socket closed, including one published concurrently with
   the stop ([delivery.md §7](delivery.md#7-stopping)).
5. **A socket is closed once, by its owner, when no other work uses it**
   ([delivery.md §8](delivery.md#8-removal-and-socket-close)).
6. **The streaming clock never waits on a listener.** It appends, wakes and
   returns; wake-ups do not block. In dedicated mode it also encodes per
   listener ([dedicated-encoder.md](dedicated-encoder.md)), which is CPU work,
   not I/O.
7. **One slow listener delays nobody.** A delivery pass spends one
   non-blocking write on it per pass, on every transport.
8. **Faults stay local.** A fault while serving one listener closes that
   listener only ([delivery.md §3](delivery.md#3-the-delivery-pass)); a
   request fault affects only its request.

## 4. What is not synchronised

- An admission reads the shared mode's markers, output starts and tail while
  send may be updating them. Since markers are published before the bytes
  they point to, the join point is a valid candidate or the tail
  ([shared-encoder.md §5](shared-encoder.md#5-encode-and-send)).
- An admission may find the encoder state stopped between its checks; it then
  replies 503 before `on_connect`, or ends closed after it
  ([connection.md §6](connection.md#6-admission)).
- `listeners()` is a snapshot that may be stale on return.
