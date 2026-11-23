# The shared stream store

The store behind the shared stream of the shared mode
([shared-encoder.md](shared-encoder.md)). The output's streaming work is its
only writer; delivery passes of every group read it concurrently.

## 1. Model

Offsets, the tail `T` and the window of offsets that must stay readable are
[shared-stream.md §1–§2](../shared-stream.md#1-model); harbor's window is
`[N − buffer, T)` ([shared-encoder.md](shared-encoder.md)).
`read(o, len)` copies offsets `o … o + len − 1` and reports whether every
copied byte is the one appended at its offset.

## 2. Guarantees

No blocking, no torn bytes, publication order, retention and bounded memory
are [shared-stream.md §5](../shared-stream.md#5-store-and-concurrency). A
range reported invalid is an overflow of the listener that read it
([shared-encoder.md §4](shared-encoder.md#4-flush)).

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
