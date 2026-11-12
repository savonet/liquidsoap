# The readiness loop

One loop serves every SRT operator in the process. Listening sockets are
registered with it to accept incoming connections
([listener.md §4](listener.md#4-connect)). A rebuild MAY also use it to
complete caller connections or drain output queues without blocking a
thread per socket; the rules below hold for any use.

## 1. Model

- A **readiness set**: one libsrt epoll set for the whole process.
- A **handler table**: socket → (event kinds, handler). The event kinds are
  `read` and `write`.
- A **thread** of its own, running iterations (§3) for as long as the set
  holds a socket, and asleep otherwise.

The set, the table and the thread are created on first registration, after
libsrt is started ([library.md §1](library.md#1-initialisation)). The set and
the table are one state with one owner: libsrt's set and the table never
disagree about which sockets are registered, and nothing else keeps a count
of them.

## 2. Registering and removing

**Register** (socket, event kinds, handler): make the socket non-blocking,
store the handler, replacing any previous entry for that socket, add the
socket to the set for those events, and wake the thread.

A registration lasts until it is removed: a handler is called each time its
socket is ready, without registering again.

**Remove** (socket): drop its entry and take it out of the set. A socket
libsrt already dropped from the set (because it was closed) is removed
without error. Once remove returns, the socket's handler is not running and
will not run again. A socket MUST be removed before it is closed.

A socket leaves the set in the mode its owner needs: whoever registered it
sets the blocking mode it uses afterwards.

## 3. One iteration

1. Wait for readiness for at most `settings.srt.poll.timeout` seconds.
2. For each socket reported ready, once per socket even when it appears in
   both lists: look up its entry; with one, call the handler with the socket
   and the events that are ready; without one, take the socket out of the
   set.

| Wait result     | Next                                                                       |
| --------------- | -------------------------------------------------------------------------- |
| sockets ready   | step 2, then the next iteration                                            |
| timeout         | the next iteration                                                         |
| empty set       | sleep until a registration wakes the thread                                |
| any other error | log at level 2 `Error while processing SRT sockets: <exn>`, next iteration |

An exception from a handler is logged at level 2 with its backtrace and the
iteration goes on with the next socket. The loop MUST keep serving while its
set holds a socket: no error stops it or puts it to sleep.

## 4. Handlers

Handlers run on the loop's thread, one at a time. A handler MUST NOT block
and MUST NOT run a script callback: it hands slow work and user code to
another thread ([concurrency.md §1](concurrency.md#1-threads)), so one
operator never delays another operator's sockets.
