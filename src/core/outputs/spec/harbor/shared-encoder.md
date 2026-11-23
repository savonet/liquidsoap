# Shared encoder mode

`dedicated_encoder=false`, the default. One writer runs while the output is
started. Its sink is the output's shared stream
([shared-stream.md](../shared-stream.md)): the writer writes its bytes and
markers into it, and every listener reads it at its own offset. A new
listener starts at a candidate, is sent its entry header and, optionally, a
burst of recent stream. The streaming work does one append per writer call,
whatever the number of listeners.

Numbered as [modes.md](modes.md). Only a streamable configuration reaches
this mode ([operator.md §2](operator.md#2-validation-at-construction)).

**Output-wide state**:

- the **shared stream**, with its tail `T`, the current writer's first byte
  `E`, its candidates and their entry headers, all recorded from the
  writer's markers ([shared-stream.md §1](../shared-stream.md#1-model)). Its
  store's design is [ring.md](ring.md);
- **newest output** `N`: the offset of the first byte the writer's latest
  call appended (`N = T` while the writer has appended nothing). A call that
  appends nothing leaves `N` unchanged;
- **metadata markers**: pairs (offset, metadata event), the event in effect
  from that offset on ([icy.md §4](icy.md#4-cadence-and-change-detection)).

Harbor's window is `[N − buffer, T)`: every offset of it stays readable
([shared-stream.md §2](../shared-stream.md#2-history)). Metadata markers are
needed only within that window; older ones MAY be dropped, keeping the
marker in effect at the window's start.

## 1. Listener state

- `offset`: the offset of the next stream byte to send, set when the
  listener's start is chosen ([§2](#2-initial-bytes)); unset while it waits
  for a candidate;
- the unsent rest of its initial bytes, and of an ICY block of which only
  part was accepted;
- while it owes a preroll: the preroll and how much of it was accepted.

The per-listener state does not grow with uptime or stream length.

## 2. Initial bytes

With `B` the burst (0 when null). The listener's start and entry header are
chosen as [shared-stream.md §3](../shared-stream.md#3-starting-a-reader)
states, with history `B` over harbor's window; the choice reads only the
shared stream's records and never calls the writer. A listener with no
burst starts at the next candidate the writer writes, and is owed nothing
until then.

1. **Preroll.** A listener with a preroll owes the response head then the
   preroll. Its start is chosen once all of the preroll has been accepted, in
   the delivery pass, with `B` reduced by the preroll's length, at least 0,
   as Icecast shortens the burst after an intro.
2. **ICY.** Blocks are inserted into the entry header as into any stream
   data, its bytes counted towards the position, each stamped with the
   metadata in effect at `offset`.
3. The listener owes: the response head, then the preroll, then the entry
   header, then the stream from `offset`.

**During a gap.** An output kept started while its source is unavailable
([operator.md §7](operator.md#7-lifecycle)) starts a listener with no
history (`B = 0`). The pre-gap stream is stale, and a burst of it would play
old audio before the silence. The listener owes the response head and its
preroll, if any, then the stream from the first candidate written after its
start was asked for.

## 3. Pending test

The listener has unsent initial or carried bytes, owes a preroll, or has an
`offset` with `offset < T`.

## 4. Flush

While the listener owes a preroll: offer the carried bytes then the preroll,
with empty ICY blocks where the position requires, advance by what was
accepted, and choose its start ([§2](#2-initial-bytes)) once all of it has
been. No overflow check applies: the listener has no offset yet.

Otherwise:

1. If `offset < N − buffer`, disconnect for overflow ([modes.md §4](modes.md#4-flush)).
2. Offer, in one write: the carried bytes, then stream bytes from `offset`
   up to `T`, with ICY blocks inserted where the position requires
   ([icy.md §4](icy.md#4-cadence-and-change-detection)). A block before the
   stream byte at offset `o` carries the metadata of the newest marker at or
   before `o`.
3. If the store reports that bytes being read may have been overwritten
   ([shared-stream.md §5](../shared-stream.md#5-store-and-concurrency),
   item 2), disconnect for overflow: such bytes MUST never reach a socket.
4. On −1, mark the listener closed. Otherwise advance by what was accepted:
   the next flush resumes at the first byte not accepted. An ICY block of
   which only part was accepted is carried and completed as it was, before
   any further stream byte.

Because the stamp of a block depends only on the offset it precedes, the
block is the same whatever pass builds it.

## 5. Encode and send

_Encode_ hands the piece to the running writer (`write`), which writes its
bytes and markers into the shared stream before it returns; with no running
writer, nothing happens. The first byte the call appended, if any, becomes
`N`.

_Send_ wakes the groups.

Metadata markers and `N` MUST be published before the bytes they point to,
as the shared stream's records are
([shared-stream.md §5](../shared-stream.md#5-store-and-concurrency), item 3).

A writer fault is handled by the output's restart policy
([operator.md §7](operator.md#7-lifecycle)): listeners stay connected and
continue with the next writer's stream start.

## 6. Metadata

Record the event and add a marker (current `T`, event). Offer the converted
values, with cover art removed, to the running writer under the gate of
[modes.md §6](modes.md#6-metadata). With no running writer, only the record
and the marker happen.

## 7. Start

The streaming work builds the writer from the factory
([contracts.md §4](contracts.md#4-encoder)), named with the output's id, with
its initial metadata: the last metadata at the start, which is what the
output base's initial-metadata rule gives for a writer built before any
piece
([output-encoded.md §3.2](../../../encoder/spec/output-encoded.md#32-initial-metadata)).
When there is a last metadata, add the marker (`T`,
last metadata). The writer's first stream start moves `E` and drops the
candidates of earlier writers
([shared-stream.md §4](../shared-stream.md#4-writer-changes)), so no
listener admitted after a restart receives a previous writer's bytes.

## 8. Stop

End the writer clean and append its final bytes ([§11](#11-final-bytes)).
Clear the writer: admissions are refused with 503 and encode does nothing.

## 9. Listener teardown

Nothing.

## 10. Staging

The implementation's choice ([modes.md §10](modes.md#10-staging)).

## 11. Final bytes

The writer's final bytes are appended to the shared stream as ordinary
bytes, so every listener is owed them behind what it already owes
([shared-stream.md §6](../shared-stream.md#6-end)).
