# Shared encoder mode

`dedicated_encoder=false`, the default. One encoder runs while the output is
started. Its output is appended to a shared stream store
([ring.md](ring.md)) and every listener reads the store at its own offset. A
new listener gets the codec header and, optionally, a burst of recent
stream. The streaming clock does one append per encoder output, whatever the
number of listeners.

Numbered as [modes.md](modes.md).

**Output-wide state**:

- the **stream store**, with its **tail** `T`: the absolute offset of the
  next byte to be appended. Offsets count every byte appended since the
  operator was created.
- **epoch start** `E`: the tail when the current encoder was created. No byte
  before `E` belongs to the current encoder.
- **output starts**: the offsets at which each output of the current encoder
  begins. The newest one is `N` (`N = T` when the encoder has produced
  nothing yet). A burst starts at one of them, so that a listener never joins
  in the middle of an encoder output (an Ogg page, for instance).
- **stream start** `S`, an offset or unset: where the encoder's current
  logical stream begins with its own header pages, as the encoder reports it
  ([contracts.md §4](contracts.md#4-encoder)). A burst must not start before
  it, or two logical streams would be mixed.
- **metadata markers**: pairs (offset, metadata event), the event in effect
  from that offset on ([icy.md §4](icy.md#4-cadence-and-change-detection)).

The store MUST keep readable every offset a listener may still be owed
without being in overflow: `[N − buffer, T)`. Markers and output starts are
needed only within that window; older ones MAY be dropped, keeping the
marker in effect at the window's start.

## 1. Listener state

- `offset`: the offset of the next stream byte to send, set when the
  listener joins;
- the unsent rest of its initial bytes, and of an ICY block of which only
  part was accepted;
- while it owes a preroll: the preroll, how much of it was accepted, and the
  encoder's header and epoch start `E` at admission.

The per-listener state does not grow with uptime or stream length.

## 2. Initial bytes

With `B` the burst. A listener with a preroll owes the response head then
the preroll, and joins when all of it has been accepted, in the delivery
pass: steps 1–3 run then, with `B` reduced by the preroll's length, at least
0, as Icecast shortens the burst after an intro. Encoder code never runs in a
delivery pass ([concurrency.md §1](concurrency.md#1-work)), so the header is
the one taken at admission. If `E` changed since admission and the join point
is not `S`, that header belongs to a stopped encoder: the listener is
disconnected, logged `Listener <peer:port>: encoder restarted during its
preroll, disconnecting`, unless the header is empty.

1. **Join point.** With no burst, `offset = T`. With a burst, the candidates
   are the output starts, plus `S` when it is set, that are at least
   `max(E, S)` (when `S` is set, otherwise `E`) and at least `N − buffer`.
   `E` itself is a candidate only when `S = E`: the encoder's first output
   may begin with header pages it does not report, and a joiner there would
   get the header twice.
   `offset` is the newest candidate with `offset ≤ T − B`, the newest one
   that gives at least `B` bytes; if none, the oldest candidate; if there is
   no candidate, `T`.
2. **Header.** If `offset = S`, no header is sent: the bytes there begin with
   the stream's own header pages. Otherwise the header is the encoder's
   current header ([contracts.md §4](contracts.md#4-encoder)).
3. **ICY.** Blocks are inserted into the header as into any stream data,
   its bytes counted towards the position, each stamped with the metadata in
   effect at `offset`.
4. The listener owes: the response head, then the header, then the stream
   from `offset`.

**Worked example.** `buffer = 6000`, `E = 0`, `T = 10000`, output starts
`[9000, 8000, 6500, 5000, 2000]`, so `N = 9000` and the window starts at 3000. Output starts in it: `[9000, 8000, 6500, 5000]`.

| `B`  | `S`   | offset                  | header sent |
| ---- | ----- | ----------------------- | ----------- |
| 3000 | unset | 6500                    | yes         |
| 7000 | unset | 5000 (oldest candidate) | yes         |
| 3000 | 8000  | 8000 (oldest candidate) | no          |
| 3000 | 2000  | 6500                    | yes         |
| null | unset | 10000                   | yes         |

## 3. Pending test

The listener has unsent initial or carried bytes, owes a preroll, or
`offset < T`.

## 4. Flush

While the listener owes a preroll: offer the carried bytes then the preroll,
with empty ICY blocks where the position requires, advance by what was
accepted, and join ([§2](#2-initial-bytes)) once all of it has been. No
overflow check applies: the listener has no offset yet.

Otherwise:

1. If `offset < N − buffer`, disconnect for overflow ([modes.md §4](modes.md#4-flush)).
2. Offer, in one write: the carried bytes, then stream bytes from `offset`
   up to `T`, with ICY blocks inserted where the position requires
   ([icy.md §4](icy.md#4-cadence-and-change-detection)). A block before the
   stream byte at offset `o` carries the metadata of the newest marker at or
   before `o`.
3. If the store reports that bytes being read may have been overwritten
   ([ring.md §2](ring.md#2-guarantees)), disconnect for overflow: such bytes
   MUST never reach a socket.
4. On −1, mark the listener closed. Otherwise advance by what was accepted:
   the next flush resumes at the first byte not accepted. An ICY block of
   which only part was accepted is carried and completed as it was, before
   any further stream byte.

Because the stamp of a block depends only on the offset it precedes, the
block is the same whatever pass builds it.

## 5. Encode and send

_Encode_ passes the piece to the encoder and returns its bytes, or nothing
when no encoder is running.

_Send_, with `L` the length of the bytes:

1. If the encoder reports a stream start at `r` in its latest output, set
   `S = T + r`.
2. If `L > 0`: record `T` as the newest output start, then append the bytes.
3. Wake the groups.

Markers and output starts MUST be published before the bytes they point to,
so that an admission running concurrently picks either an offset already
backed by bytes or the tail, which is then simply the next byte to come.

## 6. Metadata

Record the event, add a marker (current `T`, event), then pass the converted
values, with cover art removed, to the running encoder as in-stream metadata.
With no running encoder, only the record and the marker happen.

## 7. Start

Create the encoder from the factory ([contracts.md §4](contracts.md#4-encoder)),
named with the output's id, with the last metadata as initial metadata. Set
`E = T`; clear the output starts and `S`; when there is a last metadata, add
the marker (`T`, last metadata). Bytes of a previous encoder stay out of every
later join point, since every candidate is at least `E`.

## 8. Stop

Stop the encoder and append the bytes it returns as its last output
([§11](#11-final-bytes)). Clear the encoder: admissions are refused with 503
and encode returns nothing.

## 9. Listener teardown

Nothing.

## 10. Staging

The implementation's choice ([modes.md §10](modes.md#10-staging)).

## 11. Final bytes

The bytes returned by the encoder's stop are appended to the store as an
ordinary output, so every listener is owed them behind what it already owes.
