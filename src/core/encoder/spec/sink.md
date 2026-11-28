# Sinks

Part A. The destination an output provides to a container writer, and what
every output's destination does for it. This file owns: the capabilities a
sink declares and when they are confirmed, the operations a writer uses, the
rule that a sink never blocks the streaming thread, what a sink does with
markers and with the end, where out-of-band metadata goes relative to the
bytes, what a sink owns, and the faults it reports. Each output's sink is
described in its caller file's §2 ([encoder.md §11](encoder.md#11-caller-files)).

It relies on: the vocabulary, the markers and the writer's lifetime
([encoder.md](encoder.md)); the shared stream for readers starting
mid-stream ([shared-stream.md](../../outputs/spec/shared-stream.md));
concurrency ([concurrency.md](../../../spec/concurrency.md)); faults
([faults.md](../../../spec/faults.md)).

## 1. Capabilities

| Capability     | Meaning                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| -------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `seekable`     | Bytes already written can be overwritten, and read back while the writer ends ([encoder.md §3.6](encoder.md#36-draining-and-final-bytes)). A regular file opened for writing.                                                                                                                                                                                                                                                                                      |
| `fresh`        | The writer's stream starts at the destination's first byte.                                                                                                                                                                                                                                                                                                                                                                                                        |
| `continuing`   | The writer extends an existing stream (a file opened for appending) ([encoder.md §3.5](encoder.md#35-header-forms-and-preambles)). `seekable` on a regular file, where the existing header can be read back and rewritten at the writer's end.                                                                                                                                                                                                                     |
| `live(L)`      | Readers consume the stream as it is produced and may read it from the middle; the writer holds encoded data for at most `L` of stream time ([encoder.md §3.9](encoder.md#39-live-latency-bound)). Never `seekable`.                                                                                                                                                                                                                                                |
| `segmented(D)` | The sink cuts the stream into segments of target duration `D` at the writer's access points ([encoder.md §4](encoder.md#4-the-segmenting-face)); it holds each chunk's split request, which the writer reads ([HLS encoder contract §2](../../outputs/hls/spec/encoder-contract.md#2-calls-and-order)). Never `seekable`. It is a declared capability for the check at evaluation and the input of the segmenting factory, not a capability the stream face reads. |
| `delegated(u)` | The sink takes no bytes: the writer opens `u` itself ([encoder.md §6](encoder.md#6-the-delegated-destination)). Excludes every other capability.                                                                                                                                                                                                                                                                                                                   |

- Exactly one of `fresh` and `continuing` holds, except for a delegated sink,
  which has neither.
- An output declares its sinks' capabilities at construction, from its
  parameters alone ([encoder.md §5.1](encoder.md#51-what-an-output-declares)).
- A sink **confirms** its capabilities when it opens, before the writer is
  built. Confirmation removes `seekable` when the destination turns out not
  to be seekable (a FIFO, a character device, a pipe), and turns
  `continuing` into `fresh` when there is no stream to extend (an absent or
  empty file, a FIFO, a device); it never adds a capability. A sink learns
  this without waiting for a peer: the kind and size of a path are known
  without opening it. What the writer does then is
  [encoder.md §5.4](encoder.md#54-at-open).

| Parameter            | Recommended | Protects                                                                                                           |
| -------------------- | ----------- | ------------------------------------------------------------------------------------------------------------------ |
| `live_latency_bound` | 1 s         | A reader's or a server's view of a live stream: a server that judges a source alive by its bytes, a reader's delay |

`L` is the output's `live_latency_bound`: a fixed 1 s, not a script
parameter. An output whose protocol has a bound of its own of the same
nature declares it as `L` and says so in its caller file.

## 2. Operations

What a writer calls on its sink.

| Operation           | Sinks         | Effect                                                                                                                                                                                                                      |
| ------------------- | ------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `write(bytes)`      | not delegated | Appends the bytes at the current position.                                                                                                                                                                                  |
| `mark(marker)`      | not delegated | Places a marker of [encoder.md §3.4](encoder.md#34-bytes-and-markers-out) before the next byte.                                                                                                                             |
| `seek(offset)`      | `seekable`    | Moves the current position to `offset`, at most the destination's size. On a fresh sink, offsets count from the writer's first byte; on a continuing sink, from the destination's first byte, the existing stream included. |
| `size()`            | `seekable`    | The destination's size, existing bytes included: the highest position written.                                                                                                                                              |
| `read(offset, len)` | `seekable`    | The bytes at `offset`; only while the writer ends, on work allowed to block.                                                                                                                                                |

A continuing writer writes only at the end of the existing stream while it streams; it seeks into bytes it did not write only at its end, to update the existing header ([encoder.md §3.5](encoder.md#35-header-forms-and-preambles)).

A continuing writer reads nothing of the existing stream at creation:
`read` is only for the end. A format whose frames continue a numbering
from the stream's start cannot continue
([encoder.md §3.5](encoder.md#35-header-forms-and-preambles)).

- The sink applies writes, seeks and marks in the order they are called.
  What a reader of the destination finds is the result of that sequence.
- `write`, `mark`, `seek` and `size` return at once (§3). Their effect on the
  destination happens in call order, after they returned.
- The bytes handed to `write` are lent for the duration of the call: the
  sink copies what it keeps, and the writer may reuse its buffer once the
  call returned.
- A sink that serves one reader from byte 0 MAY ignore every marker but
  `end`.
- A sink MAY collect a call's bytes and markers and hand them on when the
  call returns. The order of §2 and the no-waiting rule of §3 still hold.

## 3. Never blocking the streaming thread

1. No operation of §2 but `read` waits for a peer, a disk, a process or
   another thread
   ([concurrency.md §2](../../../spec/concurrency.md#2-the-streaming-thread)).
   A sink whose destination can block queues what it is given and does its
   I/O as fast I/O on a non-blocking descriptor, or as work allowed to block
   ([concurrency.md §1](../../../spec/concurrency.md#1-work)).
2. Opening (a connection, a TLS handshake, a FIFO that waits for its reader,
   a file on a slow mount) never runs on the streaming thread. The writer
   is built and writes from the stream's first byte while the sink opens:
   the sink holds those bytes and delivers them, from the first, once open.
   None is dropped. Reason: the first bytes carry the stream's header, and a
   stream without them does not decode.
3. Every queue a sink keeps is bounded:

   | Parameter      | Recommended                                     | Protects                                                     |
   | -------------- | ----------------------------------------------- | ------------------------------------------------------------ |
   | `open_hold`    | the output's connection timeout, in stream time | Memory, while a destination is being opened                  |
   | `sink_backlog` | the output's write timeout, in stream time      | Memory, and the age of the bytes a slow destination receives |

   Past `open_hold`, the open has failed; past `sink_backlog`, the
   destination cannot keep up. Each is a sink fault (§7). A sink that serves
   several readers bounds each reader's backlog instead, and a reader past
   its bound is that reader's fault (§7); the output's tree names that bound
   (harbor's `buffer`, SRT's `peer_backlog_limit`).

4. A sink's work never runs user code under a lock the streaming thread
   takes.

## 4. Markers and the end

| Sink                                                                 | Stream starts and access points                                                          | `end`                                                         |
| -------------------------------------------------------------------- | ---------------------------------------------------------------------------------------- | ------------------------------------------------------------- |
| one reader from byte 0 (file, process, connection, dedicated reader) | MAY ignore them                                                                          | deliver everything, then close                                |
| readers starting mid-stream                                          | the shared stream records them ([shared-stream.md](../../outputs/spec/shared-stream.md)) | readers are owed the final bytes behind what they already owe |
| segmented                                                            | cut points and init segments ([encoder.md §4](encoder.md#4-the-segmenting-face))         | the last segment closes with the final bytes                  |

- After `end`, a sink delivers every byte it was given, the final bytes
  included, before it closes. A clean end of an output delivers the
  writer's final bytes to every destination that is still connected,
  network destinations included. Reason: an Ogg stream without its
  end-of-stream pages, or a container without its trailer, loses its last
  second and is not a complete stream.
- Delivery after the writer's last byte is bounded by
  `final_delivery_bound`; a destination that does not take the bytes within
  it is closed without them, logged at `severe`. This file owns the
  parameter; its value depends on the sink:

  | Parameter              | Recommended                                                                                      | Protects                                                                               |
  | ---------------------- | ------------------------------------------------------------------------------------------------ | -------------------------------------------------------------------------------------- |
  | `final_delivery_bound` | 2 s for a network peer or a process; for a file sink, the output's `sink_backlog` in stream time | The process's shutdown and the next run, against a destination that stops taking bytes |

  A file sink's clean end also writes back and reads back at disk speed
  (finalisation, an index moved to the front): a disk that cannot finish
  within `sink_backlog` is a stalled destination. A tree that owns a bound
  of the same nature uses it as its `final_delivery_bound` and says so
  (harbor's last delivery pass, SRT's `shutdown_drain`).

- A sink whose writer was ended abandoned writes no final byte. It still
  delivers the bytes the writer wrote before the fault, where its
  destination takes them (a file, a process or a peer whose fault was the
  writer's), within `final_delivery_bound`; a destination that failed
  itself is closed at once.

## 5. Out-of-band metadata

1. An output that carries metadata out of band (ICY blocks, a server's
   metadata update) hands the metadata to its sink after the writer's call
   for the piece that precedes it, and before its call for the piece it
   labels. The sink places it after every byte written so far and before
   the bytes that follow. Reason: the writer writes a call's output before
   the call returns ([encoder.md §3.2](encoder.md#32-order-of-a-calls-output)),
   so this one rule serves both channels: in-band metadata is handed to the
   writer before its piece, and out-of-band metadata leaves after the bytes
   that precede it.
2. Sending out-of-band metadata is the sink's work: it never runs on the
   streaming thread, it is bounded by the output's timeout, and its failure
   leaves the stream's connection up and is logged by the output, which also
   reports it through its error callback where it has one.
3. Metadata still owed when a destination opens (the metadata in effect for
   a new connection) is sent once the destination is open, before the next
   change.

## 6. What a sink owns

- **Opening, naming and closing** the destination: file names (asked once
  per destination), directories, permissions, append and truncation, the
  connection, its protocol and its peers.
- **Retries**: when to open again after a failure, under the output's one
  restart policy ([encoder.md §8](encoder.md#8-faults)).
- **One destination stream per writer**: a reopen or a reconnect is a new
  destination stream, given a new writer
  ([encoder.md §2.1](encoder.md#21-one-writer-per-destination-stream)).
  Bytes of one writer never reach another writer's destination, and the
  first bytes of a destination are its writer's first bytes.
- **Readers**: their admission, their backlog, their failure. A reader's
  fault never reaches the writer.

A writer does none of these.

## 7. Faults

A sink reports its faults to the output as values carrying their class
([faults.md §2](../../../spec/faults.md#2-classes)) and the cause, naming the
destination. Such a value MAY be raised as an exception carrying it.

| Fault                                                                                                                                       | Class               | Outcome                                                                                                                          |
| ------------------------------------------------------------------------------------------------------------------------------------------- | ------------------- | -------------------------------------------------------------------------------------------------------------------------------- |
| the destination cannot be opened, for a cause that follows from the configuration and the host alone and cannot clear (a malformed address) | configuration error | hard error when it shows                                                                                                         |
| the destination cannot be opened (refused, unreachable, denied, `open_hold` exceeded)                                                       | output fault        | the writer is ended abandoned; the output's restart policy                                                                       |
| a write, a seek or a flush fails; `sink_backlog` exceeded                                                                                   | output fault        | the same                                                                                                                         |
| one reader of a sink serving several fails, or exceeds its backlog                                                                          | that reader's fault | that reader is closed; the stream, the writer and the other readers go on                                                        |
| delivering the final bytes fails, or closing fails                                                                                          | not escalated       | logged at `severe`; the end counts as abandoned (`on_close`'s `clean` is false where the output has one); the teardown completes |
| an out-of-band metadata request fails                                                                                                       | not escalated       | logged; the stream goes on                                                                                                       |

- Every fault of a sink reaches the output's one restart policy at most
  once, together with any fault of its writer for the same failure.
- Teardown completes whatever step fails: every descriptor, connection and
  queue of a failed attempt is released exactly once
  ([concurrency.md §4](../../../spec/concurrency.md#4-resources)), and nothing
  a sink holds grows with the number of failed attempts.
- The state the output reports (connected or not, open or not) is the
  sink's actual state at every moment.

## 8. Delegated destination

A delegated sink takes no bytes and no markers. The output keeps the
destination's naming, its directories, its lifecycle, its interrupt and its
restart policy; the writer opens, writes and closes it on its own task
([encoder.md §6](encoder.md#6-the-delegated-destination)). A failure the
writer meets there is an output fault the writer reports.

## 9. Conformance

- A destination opened slowly (a server that accepts after one second, a
  FIFO whose reader opens late) receives the stream from its first byte: its
  first bytes are the header, and the samples it decodes equal the samples
  the output encoded from its first piece.
- With a destination that never reads, the streaming thread's latency stays
  bounded and every other output of the clock keeps its rate; the stalled
  output reports a fault within `sink_backlog`.
- A clean stop of a connected network output delivers the writer's final
  bytes: an Ogg body ends with end-of-stream pages, a container with its
  trailer.
- Out-of-band metadata at position 0 of a stream reaches the destination
  after that stream's header; a metadata at a piece boundary is sent after
  the bytes of the pieces before it.
- 10 000 failed opens against a refusing port: open descriptors and memory
  stay flat, and each failure reaches the restart policy once.
- A path declared seekable that is a FIFO: the output starts, logs once
  that it writes the streaming form, and the reader decodes the stream.
- An append of one minute of `%wav` and `%mp3` to a file holding one
  minute of the same format: the result decodes as two minutes and holds
  one header; for `%wav` the header's counts cover both minutes, for
  `%mp3` the encoder-information frame is left as it was. The same append
  to an absent file writes a complete stream with its header. An append of
  native `%flac` is refused at evaluation, naming `%ogg(%flac)`.
