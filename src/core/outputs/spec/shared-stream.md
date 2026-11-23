# Shared stream

Part A. The component that serves one writer's stream to readers that start
at any moment: what it records from the writer's markers, where a reader
starts, what the reader is sent first, the bounded history, what happens
when the writer changes, and the guarantees of the store it keeps. Its
users are the shared mode of `output.harbor`
([shared-encoder.md](harbor/shared-encoder.md)) and the listener role of
`output.srt` ([SRT output.md](../../optionals/srt/spec/output.md)).

It relies on: the vocabulary and the markers of the encoder seam
([encoder.md](../../encoder/spec/encoder.md), in particular
[§3.4](../../encoder/spec/encoder.md#34-bytes-and-markers-out)); the sink
contract, which it meets ([sink.md](../../encoder/spec/sink.md)); concurrency
([concurrency.md](../../../spec/concurrency.md)).

What stays with each user, and is not stated here:

| User                         | Keeps                                                                                                                                         |
| ---------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------- |
| `output.harbor`, shared mode | listeners, admission and refusals, the preroll, ICY blocks and their stamping, delivery groups, the overflow rule, `buffer` and `burst`       |
| `output.srt`, listener role  | peers and their admission, payload cutting and the delay bound, per-peer queues and their bound, peer failure, whether peers outlive a writer |

## 1. Model

- The component has two parts: the **records** and the start choice (§1,
  §3, §4), and the **store** of the bytes (§2, §5). A user whose readers
  never take history (`B = 0`) and that keeps a queue per reader MAY use
  the records alone: per append, the component reports the first candidate
  of that append, if any, and its entry header, and the user copies the
  bytes from there into its readers' queues. §2 and §5 then apply to those
  queues, under the user's own bound. Reason: such a user already owns
  per-reader queues and their delivery; reading a shared store would layer
  a second delivery under its own for no observable gain. The SRT listener
  role is such a user.
- The component is the sink of one output, from the output's construction
  to its end. Its capabilities are `fresh` and `live(L)`
  ([sink.md §1](../../encoder/spec/sink.md#1-capabilities)). It outlives
  writers: the output gives it one writer per run.
- An output that uses it declares that it serves readers starting
  mid-stream, so a configuration that is not streamable is refused when the
  output is constructed
  ([encoder.md §5.3](../../encoder/spec/encoder.md#53-the-check)).
- **Offsets** are absolute: the `k`-th byte appended since the component was
  created has offset `k − 1`, whatever writer wrote it. The **tail** `T` is
  the offset of the next byte to be appended.
- From the markers, the component records:
  - `E`, the offset of the current writer's first byte (its first stream
    start);
  - each **stream start**: its offset and a copy of its header, taken once
    the header's bytes are appended; a stream start whose header length is
    `unknown` has an empty header
    ([encoder.md §3.4](../../encoder/spec/encoder.md#34-bytes-and-markers-out),
    item 8);
  - each **access point**: its offset;
  - the current writer's **end**, once written.
- A **candidate** is a stream start or an access point of the current
  writer. Readers start only at candidates.
- The **entry header** of a candidate is the header of the newest stream
  start at or before it. A stream start has none: its bytes begin with its
  header.

## 2. History

- The user states the window of offsets a reader may still be owed (for
  harbor, `[N − buffer, T)`, [shared-encoder.md](harbor/shared-encoder.md)).
  Every offset of that window stays readable until the window moves past it.
- Candidates and headers are needed only within the window. Older ones MAY
  be dropped, except the stream start in effect at the window's start and
  its header, which are kept as long as a candidate of the window needs
  them.
- Memory is bounded by the window plus the newest append, up to a constant
  factor, plus the headers of the stream starts the window needs. Space
  taken for one large append SHOULD be released once the window no longer
  needs it. Nothing the component keeps grows with uptime.

## 3. Starting a reader

A user starts a reader with an amount of history `B`, in bytes, possibly 0.

1. **No history** (`B = 0`): the reader's start is the first candidate at or
   after `T`. Until the writer writes one, the reader is owed nothing.
   Reason: the tail is not in general a place where a decoder can start.
2. **With history**: among the candidates in the window, the newest `c` with
   `c ≤ T − B`, the newest one that gives at least `B` bytes; if none, the
   oldest candidate in the window; if there is no candidate, as with no
   history.
3. The reader is owed the entry header of its start, unless the start is a
   stream start, then the stream from its start.
4. The entry header is the one the component recorded for that start. It is
   never asked of the writer, and it always belongs to the stream whose bytes
   follow it, even when the writer ended between the choice of the start and
   the reader's first byte.
5. Choosing a start never waits. A reader waiting for its first candidate
   is owed nothing; it waits at most the writer's spacing of access points
   ([encoder.md §3.4](../../encoder/spec/encoder.md#34-bytes-and-markers-out),
   item 5).

A user MAY constrain the history it asks for (harbor reduces the burst by a
preroll's length, and starts with no history while its source is
unavailable); the choice above applies to the history asked.

**Worked example.** Window `[3000, 10000)`, `T = 10000`, `E = 0`. Stream
starts at 0 and 8000; access points at 2000, 5000, 6500, 9000.

| `B`  | Start                      | Sent first                    |
| ---- | -------------------------- | ----------------------------- |
| 3000 | 6500                       | the header of the stream at 0 |
| 7000 | 5000 (oldest candidate)    | the header of the stream at 0 |
| 1500 | 8000                       | nothing: a stream start       |
| 0    | the next candidate written | depends on that candidate     |

## 4. Writer changes

- A new writer's first item is a stream start: `E` moves to its offset.
  Candidates of earlier writers are dropped at once: no reader starts in the
  stream of a writer that has ended.
- A reader that started earlier keeps its offset. Its bytes continue with
  the earlier writer's final bytes, then the new writer's stream start and
  header. Whether readers are kept across writers is the user's.
- Between a writer's end and the next writer's first byte, no reader can
  start. What a user does with a reader that asks then is its own (harbor
  refuses it).

## 5. Store and concurrency

The store's design is the one harbor's shared mode specifies
([ring.md §3](harbor/ring.md#3-a-design-that-meets-them)); every user of
the store uses that design.

The output's streaming work is the only writer of the component
([encoder.md §7](../../encoder/spec/encoder.md#7-concurrency)); readers read
it concurrently.

1. **No blocking.** A read never blocks an append or another read; an append
   never waits for a reader.
2. **No torn bytes.** A read reports invalid any range of which a byte may
   have been overwritten before or during the copy, including by an append
   not finished yet. A range reported valid holds exactly the bytes appended
   at those offsets. Bytes reported invalid never reach a reader; the user
   treats them as its overflow.
3. **Publication order.** Records (stream starts with their headers, access
   points, `E`) are published before the bytes they point to, so a start
   chosen concurrently is either backed by bytes or at the tail, where it is
   simply the next byte to come.
4. **Retention** and **bounded memory** as in §2.

## 6. End

The writer's final bytes are appended as ordinary bytes: every reader is
owed them behind what it already owes. After the writer's `end`, the
component takes the next writer.

## 7. Conformance

- An Ogg stream with a metadata change every second, through every Ogg
  configuration; a reader starts after many changes with a history shorter
  than the stream so far. Binding: its bytes, parsed directly, are whole
  consecutive pages from the first; the first page begins a logical stream;
  every page belongs to the logical stream whose beginning came last; at
  least two stream changes are seen.
- A streamable configuration whose codec has a delay (AAC or MP3 in a
  container with a header), with no metadata change: a reader that starts
  after 1 s decodes from its first byte.
- A native FLAC stream: a reader that starts mid-stream receives `fLaC` and
  the metadata blocks first, then whole frames.
- An MP3 reader receives no ID3v2 tag and no encoder-information frame.
- With no history, a reader's first stream byte is a candidate.
- A reader whose start was chosen just before a writer change receives the
  header of the stream its bytes belong to, or is refused by the user.
- Over a run of many hours with readers coming and going, the component's
  memory stays within its bound.
