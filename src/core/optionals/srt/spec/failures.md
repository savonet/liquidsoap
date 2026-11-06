# Failures

Every failure the operators meet, and what an observer sees. One principle
holds throughout: a failure affects the smallest thing it concerns (one
peer, one attempt, one operator), and only a cause the process cannot fix
stops anything for good.

## 1. At construction

| Failure                                                    | Effect                                       |
| ---------------------------------------------------------- | -------------------------------------------- |
| invalid parameter ([common.md §1](common.md#1-parameters)) | Script error at evaluation; nothing starts.  |
| no encoder for the output's format                         | Script error at evaluation.                  |
| non-fallible output on a fallible source                   | Script error at evaluation (generic output). |

## 2. Connecting

| Failure                                                                   | Effect                                                                                                                                                                                                               |
| ------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| permanent: a parameter the library rejects, permission refused            | Raised from the start or `connect()` that caused it, the clock handling a failed start; in background work, logged at level 2 and no further tries until wanted again ([connection.md §4](connection.md#4-connect)). |
| transient, listener: address in use or not available, name not resolved   | Logged at level 3; retried every `polling_delay` ([listener.md §4](listener.md#4-connect)). Other operators run.                                                                                                     |
| transient, caller: name not resolved, peer unreachable, timeout, rejected | Logged at level 3 with the cause; retried every `polling_delay` ([caller.md §4](caller.md#4-connect)).                                                                                                               |

## 3. Admitting

| Failure                                     | Effect                                                                                   |
| ------------------------------------------- | ---------------------------------------------------------------------------------------- |
| admission predicate returns `false`         | The handshake is rejected; nothing fires.                                                |
| `listen_callback` raises                    | The handshake is rejected; logged at level 2.                                            |
| a handshake completes past the client limit | The socket is closed at accept; nothing fires ([listener.md §4](listener.md#4-connect)). |
| accept fails                                | Logged at level 3; the listener keeps accepting.                                         |
| setting up an accepted socket fails         | That peer alone is ended.                                                                |

## 4. Streaming

| Failure                                                                                           | Effect                                                                                                                                                                             |
| ------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| output: a send fails, a peer stalls for `write_timeout`, or its queue passes `peer_backlog_limit` | That peer fails ([output.md §5](output.md#5-sending)); other peers are untouched. Caller: a new attempt ([caller.md §7](caller.md#7-peer-failure)).                                |
| output: stopping the encoder fails                                                                | Logged at level 2; the peers are ended anyway ([output.md §6](output.md#6-stopping)).                                                                                              |
| output: the encoder fails while encoding                                                          | The generic output's handling of an encoder error.                                                                                                                                 |
| input: end of stream, receive error, decoder missing or failing                                   | That peer fails; its decoded data is produced, then the track ends ([input.md §5](input.md#5-producing-frames)). Caller: a new attempt ([caller.md §7](caller.md#7-peer-failure)). |
| input: connected peer silent                                                                      | The input becomes not ready; the connection is kept. While pacing, the streaming thread waits up to `read_timeout`, then ends the track.                                           |
| input: buffer exceeds `max`                                                                       | Excess dropped, logged at level 2.                                                                                                                                                 |

A peer that vanishes without closing is detected by the library's own peer
idle timeout, and reported as broken.

## 5. Callbacks

| Failure                                        | Effect                                                                                                    |
| ---------------------------------------------- | --------------------------------------------------------------------------------------------------------- |
| any user callback but `listen_callback` raises | Logged at level 2 with the callback's name; the operator goes on ([common.md §3](common.md#3-callbacks)). |

## 6. Background

| Failure                                    | Effect                                                                       |
| ------------------------------------------ | ---------------------------------------------------------------------------- |
| readiness wait fails other than by timeout | Logged at level 2; the loop goes on ([poll.md §3](poll.md#3-one-iteration)). |
| a readiness handler raises                 | Logged at level 2; the loop goes on with the next socket.                    |
| libsrt log verbosity setting invalid       | Logged at level 2; level `error` used.                                       |

## 7. Script calls

| Call                                     | Failure                                                        |
| ---------------------------------------- | -------------------------------------------------------------- |
| socket value methods                     | libsrt errors, a closed socket included, raised as kind `srt`. |
| `sockets()`, `connect()`, `disconnect()` | Never raise; `sockets()` is `[]` with no live peer.            |
