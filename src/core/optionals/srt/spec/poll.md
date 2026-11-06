# The readiness loop

One loop serves every SRT operator in the process. Today only listening
sockets are registered with it, for read readiness, so its one job is to
accept incoming connections ([listener.md §4](listener.md#4-connect)).

## 1. Model

- A **readiness set**: one libsrt epoll set for the whole process.
- A **handler table**: socket → (event kind, handler). The event kind is
  `read` or `write`.
- A **task** on the scheduler's threaded queue, which runs one iteration (§3)
  each time it is scheduled. After an iteration it is either scheduled again
  at once or put to sleep until the next wake-up (§2).

## 2. Registering and removing

**Register** (socket, event kind, handler):

1. Make the socket non-blocking for sending and for receiving.
2. Store the handler for the socket, replacing any previous entry.
3. Add the socket to the readiness set for that one event kind.
4. Wake the task.

**Remove** (socket): drop its handler entry; if the socket is in the
readiness set, take it out.

Registration is **one-shot**: an iteration takes a ready socket out of the
readiness set before calling its handler, so a handler that wants further
events registers the socket again. The handler entry stays in the table after
the one-shot removal.

Nothing returns a socket to blocking mode when it leaves the set. A listening
socket stays non-blocking for its whole life; accepted sockets are made
blocking by the listener itself.

## 3. One iteration

1. If the readiness set holds no socket: sleep until woken.
2. Wait for readiness for at most `settings.srt.poll.timeout` seconds,
   converted to milliseconds by truncation.
3. For the read-ready sockets, then for the write-ready sockets, in the order
   the wait returned them, for each socket:
   1. take it out of the readiness set;
   2. look up its handler entry;
   3. if the entry's event kind is the event being processed, call the
      handler with the socket. An exception from the handler is logged with
      its backtrace as `Error while executing asynchronous callback: <exn>`
      and the iteration continues.
4. Schedule the next iteration at once.

Outcomes of the wait:

| Wait result     | Next                                                                                  |
| --------------- | ------------------------------------------------------------------------------------- |
| sockets ready   | step 3, then again at once                                                            |
| timeout         | again at once                                                                         |
| empty set       | sleep until woken                                                                     |
| any other error | log `Error while processing SRT socket pool: <exn>` with backtrace, sleep until woken |

An error raised by step 3.2 (no handler entry for a ready socket) is an
"other error": the rest of that batch is skipped and the loop sleeps until the
next registration wakes it.

## 4. Threads

Handlers run on the scheduler thread that runs the task, one at a time. The
handler table is read there and written by whichever thread registers or
removes a socket; it has no lock of its own
([concurrency.md §3](concurrency.md#3-shared-state)).
