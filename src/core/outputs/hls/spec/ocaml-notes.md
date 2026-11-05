# OCaml notes (Part B)

Notes on the OCaml implementation this was extracted from. None of it is a
rule of the HLS output, and none of it is a design to copy.

## Structure

- One class, an `Output.encoded` subclass, holds every concern of
  [README.md](README.md)'s list: argument parsing, variant derivation, the
  grid, cuts, file writing, retention, playlists, persistence, ID3 and tags.
  The base class supplies the start/stop state machine and the metadata-first
  chunking of each frame; the subclass supplies `encode` (returns, per
  variant, the chunk length and its pieces) and `send` (applies them).
- Per-variant state is one mutable record; segments are mutable records too.
  The closed-segment lists are an association list from variant name to a
  list reference, held by the object, apart from the variant records. That
  split is why rebuilding the variants at `stop` leaves the lists in place.
- Temporary files are objects with `abort`, `close` (publish), `read`,
  `truncate`, `position`. The file name of a media segment is a closure over
  the segment record, evaluated at publish time.
- A global (frame number, offset) position is advanced on every encode and
  never read: ID3 positions are derived from the segment start.

## Concurrency

The streaming cycle runs every call. Pending inserted tags are an `Atomic`
list updated by compare-and-set, since `insert_tag` runs on any thread.
Derived variant properties are mutex-protected lazy values, forced from the
streaming cycle or from `streams()`. Every encoder is wrapped so that its
calls are serialised by one mutex.

## Encoder adapters

`Encoder.dummy_hls` is the frame-unit adapter; `Encoder_utils.mk_id3_hls`
overrides its `init` and `insert_id3` for packed audio. The `%ffmpeg`
instance builds its own record in HLS mode.

## Persistence

The JSON uses Liquidsoap's own JSON module: arrays are `Tuple`, objects
`Assoc` association lists. The reader pattern-matches the top-level object
and each stream object as exact lists, which is why key order matters; it
looks segment keys up by name. Errors are a local `Invalid_state`, an
`assert` on names, or `List.iter2`'s length check.

## Errors

Construction errors are `Error.Invalid_value` on the argument value. The
bandwidth error is an `Invalid_value` raised from inside a lazy value, so it
surfaces in the streaming cycle. `reopen_on_error` is shared with the pipe
outputs.

## Comments that cite the FFmpeg spec

The segmenting code cites the FFmpeg encoder spec's §8.7 item by item. That
section is where the grid rule was written first.
