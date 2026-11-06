# Failures

Every failure the operators meet, and what an observer sees. Log messages are
in the sections linked.

## 1. At construction

| Failure                                                    | Effect                                       |
| ---------------------------------------------------------- | -------------------------------------------- |
| invalid parameter ([common.md §1](common.md#1-parameters)) | Script error at evaluation; nothing starts.  |
| no encoder for the output's format                         | Script error at evaluation.                  |
| non-fallible output on a fallible source                   | Script error at evaluation (generic output). |

## 2. Starting

| Failure                                                                                   | Effect                                                                                                                                                                                                                                                                         |
| ----------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| listener: resolution, bind or listen fails ([listener.md §3](listener.md#3-socket-setup)) | The error leaves start, on the streaming thread. The clock logs `Source <id> failed (no handler)` and stops; the process shuts down, unrelated outputs included. Checked for the output (two listeners on one port) and the input (bind to an address the host does not have). |
| caller: resolution or connect fails                                                       | Logged at level 3; retried every `polling_delay`, forever ([caller.md §4](caller.md#4-connect)).                                                                                                                                                                               |

## 3. Admitting

| Failure                                       | Effect                                                                                                                                |
| --------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------- |
| admission predicate returns `false` or raises | The handshake is rejected; nothing logged by the operator.                                                                            |
| accept fails                                  | Logged at level 5; the listener stops accepting until connect runs again ([listener.md §4](listener.md#4-connect)).                   |
| setting up an accepted client fails           | Client closed, logged at level 5.                                                                                                     |
| synchronous `on_connect` raises (listener)    | Logged at level 5. The client is already listed and has been closed: it stays listed until a send to it fails or the next disconnect. |
| synchronous `on_connect` raises (caller)      | The attempt fails and is retried every `polling_delay`, so the caller never stays connected.                                          |
| `on_socket` raises at socket creation         | Creation fails: an attempt failure (caller), a start failure (listener).                                                              |

## 4. Streaming

| Failure                                                                               | Effect                                                                                                                                                     |
| ------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------- |
| output: a send fails or times out                                                     | Encoder restarted for every peer; that peer is dropped (listener) or the connection is remade (caller) ([output.md §5](output.md#5-chunking-and-sending)). |
| output: stopping the encoder fails                                                    | Logged; the queue is emptied anyway.                                                                                                                       |
| input: nothing received for `read_timeout`, end of stream, decoder missing or failing | Track ends, disconnect, reconnect unless stopping ([input.md §5](input.md#5-producing-frames)).                                                            |
| input: buffer exceeds `max`                                                           | Excess dropped, logged at level 2.                                                                                                                         |

## 5. Background

| Failure                                    | Effect                                                                                       |
| ------------------------------------------ | -------------------------------------------------------------------------------------------- |
| readiness wait fails other than by timeout | Logged; the loop sleeps until the next registration ([poll.md §3](poll.md#3-one-iteration)). |
| an accept handler raises                   | Logged; the loop goes on.                                                                    |
| libsrt log verbosity setting invalid       | Logged at level 2; level `error` used.                                                       |

## 6. Script calls

| Call                                               | Failure                                                                                          |
| -------------------------------------------------- | ------------------------------------------------------------------------------------------------ |
| socket value methods                               | libsrt errors raised as kind `srt`.                                                              |
| `sockets()` on an unconnected input in caller mode | Raises an internal not-connected error to the script. Checked.                                   |
| `sockets()` from a synchronous listener callback   | Waits forever ([concurrency.md §2](concurrency.md#2-the-source-lock)). Checked for `on_connect`. |
