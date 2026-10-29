# The shared stream store

The store the shared mode appends encoder output to and every listener reads
from ([shared-encoder.md](shared-encoder.md)). The streaming clock is its only
writer; delivery passes of every group read it concurrently.

## 1. Model

- **Offsets** are absolute: the `k`-th byte appended since creation has
  offset `k − 1`. The **tail** `T` is the offset of the next byte to be
  appended.
- The store **retains** a window of offsets ending at `T`. The shared mode
  states which offsets must stay readable
  ([shared-encoder.md](shared-encoder.md), `[N − buffer, T)`); the store's
  size follows from it.
- `read(o, len)` copies offsets `o … o + len − 1` and reports whether every
  copied byte is the one appended at its offset.

## 2. Guarantees

1. **No blocking.** A read never blocks the writer or another read; an append
   never waits for a reader. The streaming clock's timing depends on it.
2. **No torn bytes.** A read MUST report invalid any range of which a byte
   may have been overwritten before or during the copy, including by an append
   that has not finished yet. A range reported valid holds exactly the bytes
   appended at those offsets. A torn range handed to a listener would corrupt
   its stream with no way to detect it; reported invalid, it becomes an
   overflow.
3. **Retention.** Every offset in the window the shared mode requires is
   readable until the window moves past it, including after an append longer
   than the store's current size.
4. **Bounded memory.** The store's memory is bounded by the required window,
   `buffer` plus the newest output, up to a constant factor. Space taken for
   one large output SHOULD be released once the window no longer needs it.

## 3. A design that meets them

Informative. A circular buffer of capacity `C`, offset `o` at index
`o mod C`, with the tail published atomically after each append:

- Before overwriting indices, the writer publishes the **overwrite horizon**:
  the lowest offset whose index the append is about to reuse,
  `T + L − C`. A reader checks after its copy that its start is at or above
  the horizon; checking against the tail alone misses an append still in
  progress.
- An append longer than what fits beside the window moves to a larger buffer,
  copying the window, and publishes the new buffer; a reader still holding
  the old one keeps reading intact bytes from it.
