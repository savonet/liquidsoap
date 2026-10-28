# Shared encoder mode

`dedicated_encoder=false`, the default. One encoder runs while the output is
started. Its output is appended to a byte ring ([ring.md](ring.md)) and every
listener reads the ring at its own offset. A new listener gets the codec header
and, optionally, a burst of recent stream.

Numbered as [modes.md](modes.md).

**Output-wide state**, created with the operator and kept across restarts:

- the **ring**, capacity `2 × buffer` bytes. Twice the per-listener lag limit:
  a listener is disconnected once it lags by `buffer`, so the writer cannot
  wrap over bytes a reader is copying unless another `buffer` worth is
  appended meanwhile, which the reader detects.
- **stream start**: an absolute ring offset or unset. Where the encoder's
  current stream begins with its own header pages, as reported by the encoder
  ([contracts.md §4](contracts.md#4-encoder)). A burst must not start before
  it: two streams would be mixed.
- **chunk starts**: a list of absolute ring offsets, newest first, each the
  start of one encoder output. Only maintained when `burst` is set. A burst
  starts at one of them, so that a listener never joins in the middle of an
  encoder output (an Ogg page, for instance).

Offsets are absolute: the count of bytes appended to the ring since the
operator was created.

## 1. Listener state

- `offset`: absolute ring offset of the next stream byte to send;
- `carry`: a byte string to send before any ring byte, and `carry_ofs`, how
  much of it is sent. It holds the response head and codec header at first,
  and later the unsent rest of an ICY block.

Refused with 404 when no encoder is running.

## 2. Initial bytes

With `T` the ring's current tail and `B` the burst:

1. **Starting offset.**
   - No burst: `offset = T`.
   - Burst: keep the chunk starts `s` with `s ≥ T − buffer` (still readable),
     newest first. `offset` is the first of them with `s ≤ T − B` (the newest
     start that gives at least `B` bytes); if none, the last of them (the
     oldest readable); if there are none at all, `T`.
2. **Header.** If the stream start is set and `≥ offset`, move `offset` to the
   stream start and send no header: the bytes there begin with the stream's
   own header pages. Otherwise the header is the encoder's current header
   ([contracts.md §4](contracts.md#4-encoder)).
3. **ICY.** Insert ICY blocks into the header, counting its bytes towards the
   listener's position ([icy.md §4](icy.md#4-cadence-and-change-detection)),
   with the output's last metadata. A block is inserted wherever the position
   reaches `metaint` inside the header, including at its very end.
4. `carry = response head ++ header`, `carry_ofs = 0`.

**Worked example.** `buffer = 6000`, `T = 10000`, chunk starts
`[9000, 8000, 6500, 5000, 2000]`. Readable: `[9000, 8000, 6500, 5000]`.

| `B`  | stream start | offset                 | header sent |
| ---- | ------------ | ---------------------- | ----------- |
| 3000 | unset        | 6500                   | yes         |
| 7000 | unset        | 5000 (oldest readable) | yes         |
| 3000 | 8000         | 8000                   | no          |
| 3000 | 2000         | 6500                   | yes         |
| null | unset        | 10000                  | yes         |

## 3. Pending test

`carry_ofs < length(carry)` or `offset < T`.

## 4. Flush

With `T` the ring's tail read once, `behind = T − offset`, `room` the staging
size ([§10](#10-staging-size)) and `M` the output's last metadata, read once:

1. If `behind > buffer`, disconnect for overflow and stop.
2. **Stage the carry.** If `carry` has `c > 0` unsent bytes, copy
   `min(c, room)` of them to the staging buffer, as a _carry_ segment.
3. If the whole unsent carry was staged, **stage ring data**, using local
   copies of the ICY position `p` and last-sent metadata, and `remaining =
behind`. While `remaining > 0`:
   - if ICY is on and `p ≥ metaint`: build the block for `M`
     ([icy.md §4](icy.md#4-cadence-and-change-detection)); if it does not
     fit in the staging space left, stop; otherwise stage it as a _meta_
     segment, set the local last-sent to `M` and `p = 0`;
   - otherwise: `n = min(remaining, space left)`, and with ICY on also
     `n ≤ metaint − p`; if `n ≤ 0`, stop; copy the ring bytes at
     `T − remaining … +n` to staging; if the ring reports them overwritten
     ([ring.md §3](ring.md#3-read)), mark the stage _torn_ and stop;
     otherwise stage them as a _data_ segment, `p += n`,
     `remaining −= n`.

   A block is staged only when stream bytes follow it in the same pass.

4. If torn, disconnect for overflow.
5. Else, if anything was staged, write it all with one socket write. On −1,
   mark the listener closed. Otherwise hand the accepted count `w` to the
   segments in staging order:
   - _carry_ of `n` bytes: `k = min(w, n)` advances `carry_ofs`; an exhausted
     carry is emptied; continue with `w − k` only if `k = n`;
   - _data_ of `n` bytes: `k = min(w, n)` advances `offset` and the ICY
     position; continue only if `k = n`;
   - _meta_ block: resets the ICY position to 0 and records `M` as last sent,
     **whether or not any of it was accepted**; if part of it was not
     accepted, that rest becomes the new `carry` and accounting stops.

The listener's ICY position and last-sent metadata change only through step 5;
the locals of step 3 are discarded.

The ICY block sent for a byte reflects the last metadata at flush time, not at
the time the byte was encoded.

## 5. Encode and send

_Encode_ passes the piece to the encoder and returns its bytes, or nothing
when no encoder is running.

_Send_, with `T` the tail before appending and `L` the length of the bytes:

1. If the encoder reports a restart offset `r` for its latest output, set the
   stream start to `T + r`. This is done even when `L = 0`.
2. If `L > 0`:
   1. If `burst = B`: prepend `T` to the chunk starts, then keep, from the
      newest, every start `> T + L − B`, plus the first start that is not,
      and drop the rest.
   2. Append the bytes to the ring.
   3. Write them to the dump file if open ([§11](#11-dump-file)).
3. Wake the shards.

The markers of steps 1 and 2.1 are published before the bytes they point to;
an admission running concurrently may pick an offset at the tail, which is
then simply the next byte to come.

## 6. Metadata

Convert and store as last metadata, then pass the converted metadata, with
cover art removed, to the running encoder as in-stream metadata. With no
running encoder, only the store happens.

## 7. Start

Create the encoder from the factory ([contracts.md §4](contracts.md#4-encoder)),
naming it with the output's id and giving it empty initial metadata. The ring,
the stream start and the chunk starts are not reset.

## 8. Stop

Call the encoder's stop and discard the bytes it returns. Clear the encoder:
from now on, admissions are refused with 404 and encode returns nothing.

## 9. Listener teardown

Nothing.

## 10. Staging size

`buffer + 4081` bytes: room for everything a listener may be behind plus one
ICY block of maximum size (`1 + 255 × 16`).

## 11. Dump file

Every non-empty encoder output, exactly as appended to the ring: no response
head, no ICY blocks, no burst duplication. The file therefore holds the
encoded stream from start to stop.
