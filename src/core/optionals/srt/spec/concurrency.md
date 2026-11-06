# Concurrency

## 1. Threads

| Thread                         | Runs                                                                                                                                                                                                                                                   |
| ------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| streaming thread (the clock's) | start and stop of both kinds; output encoding and handing payloads to peer queues ([output.md §5](output.md#5-sending)); taking input frames from the buffer, and waiting for data while the input paces ([input.md §5](input.md#5-producing-frames)). |
| readiness loop                 | readiness handlers ([poll.md §4](poll.md#4-handlers)).                                                                                                                                                                                                 |
| connection work                | caller attempts, listener setup retries, the steps of a new peer up to live, ending peers; so `on_socket`, `on_connect` and `on_disconnect` ([connection.md §5](connection.md#5-connected)).                                                           |
| input reader                   | receiving and decoding for one peer ([input.md §4](input.md#4-receiving-and-decoding)).                                                                                                                                                                |
| output sending                 | moving queued payloads into the library, and the drain of a clean end ([output.md §5–§6](output.md#5-sending)).                                                                                                                                        |
| libsrt handshake thread        | the admission predicate, so `listen_callback` ([listener.md §3](listener.md#3-socket-setup)).                                                                                                                                                          |
| libsrt log thread              | queuing log messages ([library.md §2](library.md#2-log-routing)).                                                                                                                                                                                      |
| script threads                 | `connect()`, `disconnect()`, `sockets()`, the socket value's methods.                                                                                                                                                                                  |
| scheduler threaded queue       | callbacks registered with `synchronous=false`.                                                                                                                                                                                                         |
| shutdown                       | releasing every operator ([library.md §4](library.md#4-shutdown)).                                                                                                                                                                                     |

How the rows map to threads is the implementation's choice, under these
rules:

- **The streaming thread never blocks on a peer.** It never connects,
  accepts, sends or receives. Its waits are the input's wait for data while
  it paces, bounded by `read_timeout` and cut short when the peer ends, and
  the listener's first setup during start (resolving and binding its own
  address), so that a permanent failure fails the start
  ([listener.md §4](listener.md#4-connect)).
- **No blocking call holds a shared bounded worker.** An attempt blocked up
  to its connect timeout, a receive, a send drain or the readiness wait MUST
  NOT occupy a worker of a pool other work depends on: enough failing
  callers would otherwise starve unrelated work.
- **One operator never delays another.** User code (callbacks, socket
  methods called from them) and one peer's slowness delay only their own
  operator, and for the listener only their own peer. The exception is
  `listen_callback`, which libsrt runs on its single handshake thread.

## 2. Callbacks and locks

- **No user code runs under an operator lock.** Every callback fires after
  the transition that caused it is complete and every lock it took is
  released.
- **Methods work from callbacks.** A synchronous callback MAY call any
  method of any operator, its own included, and the call returns
  ([common.md §4](common.md#4-methods)).
- **Order per operator.** The events of one operator fire in the order of
  the transitions that caused them, and its synchronous callbacks never run
  concurrently with each other. Per peer, `on_socket` creation, `on_connect`,
  `on_socket "close"`, `on_disconnect` come in that order.
- **What "synchronous" holds back.** A synchronous `on_connect` completes
  before its peer is live; a synchronous `on_socket` creation event completes
  before the socket connects, binds or carries data. Nothing else waits for a
  synchronous callback, the streaming thread least of all.

## 3. Shared state

Every piece of state written from more than one thread has one owner and
atomic transitions:

| State                               | Owner                                                                                             |
| ----------------------------------- | ------------------------------------------------------------------------------------------------- |
| hold flag, shutdown flag            | atomic values                                                                                     |
| role state (caller §2, listener §2) | one value per operator, changed by atomic transitions ([connection.md §2](connection.md#2-state)) |
| peer list                           | part of the role state                                                                            |
| output encoder                      | the streaming thread; other threads only mark peers joining or ended                              |
| output peer queues                  | each queue has one producer (streaming thread) and one consumer (sending)                         |
| input buffer                        | one producer (the reader), one consumer (the streaming thread)                                    |
| input decoder, dump file            | the peer's reader                                                                                 |
| readiness set and table             | the readiness loop ([poll.md §1](poll.md#1-model))                                                |

**Check-then-act** over a peer is never a crash: a socket used after its
peer ended yields a failure that §7 of
[connection.md](connection.md#7-peer-failure) ignores, since the peer has
already failed once.
