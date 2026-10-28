# Delivery

How bytes owed to listeners reach their sockets. This machinery is common to
both modes; what a listener is owed and how it is written is the mode's
([modes.md §3–§4](modes.md#3-pending-test)).

## 1. Shards

Listeners are partitioned into **shards**. Each shard has:

- a member list, read and replaced atomically (compare-and-swap on the whole
  list);
- a **wake channel**: a connected socket pair whose write end is non-blocking,
  or _stopped_ once the writer has been told to stop;
- a drain buffer of 1024 bytes for emptying the wake channel;
- a staging buffer whose size the mode chooses
  ([modes.md §10](modes.md#10-staging-size)).

On start, when the output has no shards, it creates `W` of them, where `W` is
`settings.harbor.output.writers` when set, otherwise the scheduler's maximum
domain count, and at least 1. Each shard gets one **writer**: a scheduler task
of priority `Blocking` waiting for either the wake channel to be readable or
one second to pass. Each listener belongs to exactly one shard for its whole
life ([connection.md §5](connection.md#5-admission), step 6).

A socket pair is used rather than a pipe because only sockets can be made
non-blocking on Windows, and a blocking wake-up could stall the streaming
thread.

## 2. Wake-ups

Waking a shard writes one byte to its wake channel. A failed write is
ignored: a full channel means a wake-up is already pending, a closed one means
the writer is stopping.

**Who wakes.**

- _Every streaming cycle_: after the mode has consumed the encoded data of a
  frame chunk ([modes.md §5](modes.md#5-encode-and-send)), every shard that has
  at least one member is woken, whether or not new data was produced, so that
  data still pending for slow listeners keeps being retried.
- _Admission_: the shard a listener is published to
  ([connection.md §5](connection.md#5-admission), step 11).
- _Disconnection_: every shard with members
  ([§6](#6-disconnection)).

A shard with no members is not woken; its writer still runs once a second.

Writers do not wait on listener sockets becoming writable. Writability is
discovered by trying a non-blocking write on each pass. The reason recorded in
the code: on Windows, once any non-socket descriptor is in the scheduler's
set, the select emulation becomes edge-triggered and reports writability only
once after connect, so pending data would never be flushed.

## 3. The writer pass

Each time the writer runs (woken, or one second elapsed):

1. If the wake channel is _stopped_, see [§7](#7-stopping-the-writers).
2. If the wake channel is readable, read up to 1024 bytes from it and discard
   them.
3. Take `now` (wall clock).
4. For each member of the shard, in list order (newest first): if it is not
   marked closed and the mode says it has pending bytes
   ([modes.md §3](modes.md#3-pending-test)):
   1. flush it ([modes.md §4](modes.md#4-flush));
   2. if it is still not marked closed and `now − last progress >
timeout`, log `Listener <id> timed out (no progress for <timeout>s)`
      and mark it closed ([§6](#6-disconnection)).
5. Remove the closed members ([§8](#8-removal-and-socket-close)).
6. Re-arm: wait again for the wake channel or one second.

An exception escaping steps 2–5 is logged (`Write task error: <exception>`)
and the pass ends there; the writer still re-arms. The members after the one
whose flush raised are not served in that pass, and closed members are not
removed.

## 4. Writing to a socket

A write offers a byte range to the listener's socket and classifies the
outcome:

| Outcome                                | Result | Effect                                               |
| -------------------------------------- | ------ | ---------------------------------------------------- |
| `k > 0` bytes accepted                 | `k`    | last progress := now; `bytes_sent += k`              |
| 0 bytes accepted                       | 0      | none                                                 |
| would block (`EAGAIN` / `EWOULDBLOCK`) | 0      | none                                                 |
| `EPIPE` or `ECONNRESET`                | −1     | log `Socket write for <id>: <error> (disconnecting)` |
| any other error                        | −1     | log `Socket write error for <id>: <error>`           |

A result of −1 makes the mode mark the listener closed. One flush issues at
most one write.

On the plain TCP and SSL transports the write is non-blocking; the SSL
transport reports its would-block conditions as `EAGAIN`. The TLS transport
encrypts the whole range and writes it all before returning, waiting up to
`settings.harbor.timeout` (default 120 s); a timeout is an error (−1). Meanwhile
the other listeners of the shard wait.

## 5. No-progress timeout

`last progress` starts at the listener's creation and is updated on every
write that accepts at least one byte. A listener is timed out by the check of
[§3](#3-the-writer-pass) step 4.2: only while it has pending bytes, only right
after a flush attempt. A listener owed nothing is never timed out, and a
listener whose socket accepts any byte at all within `timeout` is never timed
out.

## 6. Disconnection

**Causes.** A write error ([§4](#4-writing-to-a-socket)); falling behind by
more than `buffer` bytes (the mode's overflow, logged `Listener <id> buffer
overflow (client too slow), disconnecting`); the no-progress timeout; a failed
admission ([connection.md §6](connection.md#6-failure-paths)); the output
stopping; process shutdown ([operator.md §7](operator.md#7-lifecycle)).

A client closing its end is noticed only through a write error, so only when
it is owed bytes.

**Marking closed** is the only operation the causes perform. It is
idempotent: the first caller to flip the listener's closed flag (atomic
compare-and-set from false to true) does the rest, later callers do nothing.
The first caller:

1. records the disconnection time and logs `Listener <id> disconnected`;
2. wakes every shard that has members, so the owning writer removes the
   listener promptly;
3. queues one scheduler task at priority `Threaded`, with no delay, that:
   1. stops the listener's per-listener encoder state
      ([modes.md §9](modes.md#9-listener-teardown)), logging
      `Listener <id> encoder stop failed: …` if that raises;
   2. runs the `on_disconnect` handlers with the listener record, its
      `duration` measured to the recorded disconnection time.

The socket is not closed here ([§8](#8-removal-and-socket-close)): a writer
of another shard may be in the middle of a select on its descriptor, and a
closed descriptor number can be reused by a new connection.

A listener marked closed is skipped by every later writer pass, and is no
longer listed by `listeners()`.

## 7. Stopping the writers

On output stop, after every listener has been marked closed
([operator.md §7](operator.md#7-lifecycle)):

1. For each shard, swap its wake channel to _stopped_. If it was not already,
   write one byte to the old write end (to wake the writer) and close the write
   end; failures are ignored.
2. Drop the shard array: the output now has no shards.

A writer that wakes with its channel _stopped_:

1. takes its whole member list, leaving it empty;
2. marks each of them closed (no-op for those already closed) and closes its
   socket;
3. closes the read end of its wake channel;
4. ends: it does not re-arm.

A listener published to a shard after step 1 is caught by admission's own
check ([connection.md §5](connection.md#5-admission), step 10), so no
listener is left in a shard whose writer has ended.

## 8. Removal and socket close

At the end of each pass ([§3](#3-the-writer-pass) step 5), the writer splits
its member list into open and closed listeners and installs the open ones
with a compare-and-swap, retrying from a fresh read if an admission changed
the list meanwhile. It then closes the socket of every closed listener it
removed.

A listener's socket close is guarded by its own once-flag: whichever of the
writer, the stop path or a failed admission closes first actually closes it;
the others do nothing. Errors on close are ignored.

Only the shard's writer, the stop path of [§7](#7-stopping-the-writers) and
the failure paths of admission close a listener's socket.
