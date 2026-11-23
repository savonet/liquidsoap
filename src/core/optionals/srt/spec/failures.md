# Failures

Every failure the operators meet, and what an observer sees. Faults follow
[faults.md](../../../../spec/faults.md): a fault is contained to the smallest
thing it concerns ([§0](../../../../spec/faults.md#0-the-rule)), for SRT one
peer, one attempt or one operator. In the classes of
[faults.md §2](../../../../spec/faults.md#2-classes): construction failures
are configuration errors, and so are permanent connection failures where
they are raised (see [Open](#open)); a lost or failed peer is an output fault
of that peer for the output, a source fault for the input; a transient
connection failure is retried.

## 1. At construction

| Failure                                                    | Effect                                                                                                                  |
| ---------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------- |
| invalid parameter ([common.md §1](common.md#1-parameters)) | Script error at evaluation; nothing starts.                                                                             |
| no encoder for the output's format                         | Script error at evaluation ([encoder.md §1](../../../encoder/spec/encoder.md#1-registry)).                              |
| listener role, configuration not streamable                | Script error at evaluation ([output.md §2](output.md#2-construction)).                                                  |
| non-fallible output on a fallible source                   | Script error at evaluation ([output.md §1](../../../outputs/spec/output.md#1-common-parameters-methods-and-callbacks)). |

## 2. Connecting

| Failure                                                                   | Effect                                                                                                                                                                                                                |
| ------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| permanent: a parameter the library rejects, permission refused            | Raised from the start or `connect()` that caused it, the clock handling a failed start; in background work, logged at `severe` and no further tries until wanted again ([connection.md §4](connection.md#4-connect)). |
| transient, listener: address in use or not available, name not resolved   | Logged at `important`; retried every `polling_delay` ([listener.md §4](listener.md#4-connect)). Other operators run.                                                                                                  |
| transient, caller: name not resolved, peer unreachable, timeout, rejected | Logged at `important` with the cause; retried every `polling_delay` ([caller.md §4](caller.md#4-connect)).                                                                                                            |

## 3. Admitting

| Failure                                     | Effect                                                                                   |
| ------------------------------------------- | ---------------------------------------------------------------------------------------- |
| admission predicate returns `false`         | The handshake is rejected; nothing fires.                                                |
| `listen_callback` raises                    | The handshake is rejected; logged at `severe`.                                           |
| a handshake completes past the client limit | The socket is closed at accept; nothing fires ([listener.md §4](listener.md#4-connect)). |
| accept fails                                | Logged at `important`; the listener keeps accepting.                                     |
| setting up an accepted socket fails         | That peer alone is ended.                                                                |

## 4. Streaming

| Failure                                                                                           | Effect                                                                                                                                                                                                               |
| ------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| output: a send fails, a peer stalls for `write_timeout`, or its queue passes `peer_backlog_limit` | That peer fails ([output.md §5](output.md#5-sending)); other peers are untouched. Caller: a new attempt ([caller.md §7](caller.md#7-peer-failure)).                                                                  |
| output: the writer's clean end fails                                                              | Logged at `severe`; the peers are ended anyway ([output.md §6](output.md#6-stopping)).                                                                                                                               |
| output: the writer reports an output fault                                                        | The writer is ended `abandoned`; every peer it serves ends as a peer failure; a new writer no sooner than `fault_retry_delay` ([output-srt.md §8](../../../encoder/spec/output-srt.md#8-faults-and-restart-policy)). |
| output: the writer reports an item fault                                                          | The item is dropped with a warning and counted ([encoder.md §8](../../../encoder/spec/encoder.md#8-faults)); the peers are untouched.                                                                                |
| input: end of stream, receive error, decoder missing or failing                                   | That peer fails; its decoded data is produced, then the track ends ([input.md §5](input.md#5-producing-frames)). Caller: a new attempt ([caller.md §7](caller.md#7-peer-failure)).                                   |
| input: connected peer silent                                                                      | The input becomes not ready; the connection is kept. While pacing, the streaming thread waits up to `read_timeout`, then ends the track.                                                                             |
| input: buffer exceeds `max`                                                                       | Excess dropped, logged at `severe`.                                                                                                                                                                                  |

A peer that vanishes without closing is detected by the library's own peer
idle timeout, and reported as broken.

## 5. Callbacks

| Failure                                        | Effect                                                                                                                                                                        |
| ---------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| any user callback but `listen_callback` raises | As every raising handler ([source-contract.md §10](../../../source/spec/source-contract.md#10-callbacks)): logged at `severe` with the callback's name; the operator goes on. |

## 6. Background

| Failure                                    | Effect                                                                        |
| ------------------------------------------ | ----------------------------------------------------------------------------- |
| readiness wait fails other than by timeout | Logged at `severe`; the loop goes on ([poll.md §3](poll.md#3-one-iteration)). |
| a readiness handler raises                 | Logged at `severe`; the loop goes on with the next socket.                    |
| libsrt log verbosity setting invalid       | Logged at `severe`; level `error` used.                                       |

## 7. Script calls

| Call                                     | Failure                                                        |
| ---------------------------------------- | -------------------------------------------------------------- |
| socket value methods                     | libsrt errors, a closed socket included, raised as kind `srt`. |
| `sockets()`, `connect()`, `disconnect()` | Never raise; `sockets()` is `[]` with no live peer.            |

## Open

Points where this tree and [faults.md](../../../../spec/faults.md) differ,
left for the maintainer to settle:

- A permanent failure in background connection work is logged and the role
  stops trying (§2). [faults.md §2](../../../../spec/faults.md#2-classes)
  makes a failure known not to recover a hard error whenever it shows.
- An input peer ending for a missing decoder is logged at `important`
  ([input.md §4](input.md#4-receiving-and-decoding)), and a peer failure is a
  source fault of the input; [faults.md §3](../../../../spec/faults.md#3-source-faults-reach-the-script)
  logs every source fault at `severe`.
