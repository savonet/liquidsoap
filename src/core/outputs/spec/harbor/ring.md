# The byte ring

A fixed-capacity circular byte buffer with one writer and any number of
readers, none of which take a lock. The writer appends at the tail; a reader
copies a range of absolute offsets behind the tail and learns afterwards
whether the writer may have overwritten it. Used by the shared mode
([shared-encoder.md](shared-encoder.md)): the streaming thread writes, the
writers of every shard read.

## 1. Model

- **Offsets** are absolute: the byte appended `k`-th since creation has offset
  `k − 1`. The **tail** is the offset of the next byte to be appended, i.e.
  the number of bytes appended so far.
- The ring holds one **buffer** at a time: a byte array and its capacity `C`.
  Offset `o` lives at index `o mod C`.
- The current buffer and the tail are each published atomically, separately.
- A byte at offset `o` is **available** when `T − C ≤ o < T`, where `T` is the
  tail and `C` the capacity of the buffer the reader holds.

## 2. Append (writer only)

For a chunk of `L > 0` bytes (an empty chunk does nothing):

1. Read the tail `T` and the current buffer.
2. If `L > C`, **grow**: allocate a new buffer of capacity `2 × L`, copy into
   it the bytes at offsets `max(0, T − C) … T − 1` from the old one (each at
   its index in the new capacity), and publish the new buffer. The old buffer
   is not modified afterwards, so a reader holding it keeps seeing intact
   bytes.
3. Copy the chunk to indices `T mod C …`, wrapping once to index 0 if needed.
4. Publish the tail `T + L`.

The capacity only grows, and only for a single chunk larger than the current
capacity.

## 3. Read

`read(o, dst, len)` copies offsets `o … o + len − 1` into `dst` and returns
whether they are valid:

1. Read the current buffer (capacity `C`), then the tail `T`.
2. If `o < T − C` or `o + len > T`, return false without copying: the range
   was overwritten already, or is not written yet.
3. Copy from index `o mod C`, wrapping once if needed.
4. Read the tail again, `T'`. Return `o ≥ T' − C`: the range is reported
   valid when no append published during the copy has moved the
   overwrite horizon past its start.

## 4. Guarantees and limits

- A reader never blocks the writer and never blocks another reader.
- A range reported invalid at step 2 or 4 is never handed to a listener: the
  shared mode treats it as an overflow ([shared-encoder.md §4](shared-encoder.md#4-flush)).
- Step 4 sees only appends that have **published** their tail. An append in
  progress (between [§2](#2-append-writer-only) step 3 and step 4) has already
  overwritten indices without moving the tail. Bytes it overwrites belong to
  offsets `T − C … T − C + L − 1`, so a reader whose range starts within `L`
  bytes of the overwrite horizon can copy partly overwritten bytes and still
  get true. In the shared mode a reader is at most `buffer = C / 2` behind, so
  this needs a single chunk longer than `buffer`. Recorded in
  [findings.md](findings.md).
