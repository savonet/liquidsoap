# Concurrency and ownership

Who runs what, who may touch which part of a listener, and the guarantees
that follow. The rules themselves are owned by the documents linked; this file
states how they fit together.

## 1. Actors

| Actor                 | Runs on                                                                                                          | Does                                                                                                                                  |
| --------------------- | ---------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------- |
| **Streaming thread**  | the output's clock                                                                                               | metadata, encode, send ([modes.md §5–§6](modes.md#5-encode-and-send)); the shutdown sweep ([operator.md §7](operator.md#7-lifecycle)) |
| **Request task**      | the harbor server, one per connection; moved to the `Threaded` queue before authentication and after publication | authentication, listener creation, `on_connect`, initial bytes, publication ([connection.md §5](connection.md#5-admission))           |
| **Writer**            | one `Blocking` scheduler task per shard                                                                          | flush, timeout, removal, socket close ([delivery.md §3](delivery.md#3-the-writer-pass))                                               |
| **Teardown task**     | one `Threaded` scheduler task per closed listener                                                                | per-listener encoder stop, `on_disconnect` ([delivery.md §6](delivery.md#6-disconnection))                                            |
| **Start/stop caller** | whatever thread calls `start` / `stop` (script, clock, server)                                                   | [operator.md §7](operator.md#7-lifecycle), under the output's start/stop lock                                                         |

Writers of different shards run in parallel on different domains; a writer
never runs twice at once for the same shard.

## 2. Ownership of a listener

| Part                                            | Written by                                                                                                                                            |
| ----------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------- |
| record, socket, ICY negotiation, timeout        | request task, at creation; immutable afterwards                                                                                                       |
| ICY position, last metadata sent, last progress | request task before publication; the shard's writer after (shared mode); the streaming thread for ICY state, the writer for progress (dedicated mode) |
| shared `offset`, `carry`                        | request task before publication; the shard's writer after                                                                                             |
| dedicated pending queue                         | request task (initial bytes); streaming thread (appends); writer (drops) — the queue has its own lock                                                 |
| dedicated encoder                               | request task (creation, header); streaming thread (encode); teardown task (stop) — under the encoder lock                                             |
| closed flag                                     | anyone, by atomic compare-and-set; the winner runs the disconnection steps                                                                            |
| `bytes_sent`                                    | the writer, by atomic add; read by anyone                                                                                                             |
| socket close                                    | the shard's writer, the stop path, a failed admission — once, by the socket's own once-flag                                                           |

Shard member lists are replaced by compare-and-swap: admission prepends, the
writer removes closed members, and the stopping writer swaps the list for
the empty one.

## 3. Guarantees

1. **`on_connect` before any byte.** Synchronous `on_connect` handlers finish
   before the listener's initial bytes are queued and before it is
   published, so before any byte reaches the socket.
2. **`on_disconnect` exactly once, after `on_connect`.** For every listener
   whose `on_connect` ran, `on_disconnect` runs exactly once: every path out
   of admission after `on_connect` marks the listener closed
   ([connection.md §6](connection.md#6-failure-paths)); every published
   listener is eventually marked closed (write error, overflow, timeout,
   stop, shutdown); and only the first marking queues the teardown task.
   Since the marking can only happen after `on_connect` returned, synchronous
   `on_disconnect` handlers always follow the synchronous `on_connect` ones.
   Listeners refused before creation get neither.
3. **No listener outlives its writer.** After stop, every listener is marked
   closed and its socket closed, including one published concurrently with
   the stop ([delivery.md §7](delivery.md#7-stopping-the-writers)).
4. **A socket is closed once, by its owner, after it left the select set.**
   ([delivery.md §8](delivery.md#8-removal-and-socket-close)).
5. **Slow work stays off the streaming thread and the writers.** Encoder
   teardown and `on_disconnect` run on a `Threaded` task; authentication runs
   on the `Threaded` queue. The dedicated mode's per-listener encoding does run
   on the streaming thread, one listener after the other.
6. **One slow listener does not delay listeners of other shards.** It delays
   its own shard only by one non-blocking write per pass (plain TCP), or by a
   full blocking write (TLS, [delivery.md §4](delivery.md#4-writing-to-a-socket)).

## 4. What is not synchronised

- An admission reads the shared mode's markers (stream start, chunk starts) and
  the ring's tail as separate atomic reads, while send may be updating them.
  The resulting offset is either a valid chunk start, the stream start, or the
  tail ([shared-encoder.md §5](shared-encoder.md#5-encode-and-send)).
- The encoder reference of the shared mode is read without the start/stop lock
  by encode, send, metadata and admission. When a stop clears it between an
  admission's creation step and its initial-bytes step, the initial-bytes step
  raises and the listener takes the failed-admission path
  ([connection.md §6](connection.md#6-failure-paths)).
- `listeners()` reads every shard's member list without a lock; the result is
  a snapshot that may be stale on return.
