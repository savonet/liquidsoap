# Language notes: content

Advice for implementing [content.md](../content.md),
[stream-description.md](../stream-description.md) and
[side-data.md](../side-data.md) in OCaml on the FFmpeg bindings. Nothing here
is binding; Part A is.

## 1. Libraries

- Split the kinds into a library with no libav dependency (names, type
  registration, printing, parser hook, merge and compatibility hooks, dump
  form) and parameter implementations. The libav implementation links the
  bindings; the stand-in does not.
- The kind-and-name library must not be `(optional)` in dune: a machine
  without the bindings still typechecks scripts that name FFmpeg tracks.
- A library whose only effect is registration needs `-linkall`, or its
  initialisers never run.
- Guard double registration with one process-wide record of the registered
  implementation name, set by the first kind that registers.
- The stand-in's name lists (channel layouts, sample formats, pixel formats)
  can be generated at build time from a libav-enabled build and committed,
  so the stand-in stays libav-free.

## 2. Formats and merges

- A format that every unified holder sees change wants a union-find cell
  (the core's `Unifier`), with the merged parameters stored at the root.
- Make `merge` return a result naming the failing property instead of
  raising a bare `Failure`; the error message of content.md §3 needs it.
- Copy parameters: a polymorphic variant over the bindings' `Avcodec.params`
  of each medium, plus the frame rate and the subtitle time base. Compare
  extradata as `string`; derive the bitstream form with a small function of
  codec id and the first bytes.
- Read `relaxed_compatibility_check` at each comparison (a `Dtools.Conf.bool`
  read is cheap); do not cache it.

## 3. Runs and positions

- `data = { mutable params; mutable length; mutable runs : (int * run) list }`.
  Keep runs sorted by start; blit and collapse are pure list functions over
  `(start, run)` pairs, easy to test in isolation.
- Port the blit examples of content.md §6.6 as unit tests first; they cover
  the cutting, the gaps and the collapse rules.
- Copy runs carry `source_params`; physical equality (`==`) is the fast path
  for "same parameters", structural comparison of the compared properties
  the slow one.

## 4. Source ids

- `Atomic.fetch_and_add` works on `int Atomic.t` only. A 63-bit `int`
  counter (`Atomic.fetch_and_add next 1 + 1`) never wraps in practice;
  convert to `Int64` at the edge if a consumer needs that type.
- Do not keep a table from sources to ids. Store the id in the producer, or
  in the reference-counted record of inline-codecs.md §5.

## 5. Dumps and cache

- Dump: serialise raw formats with the printer and read them back with the
  parser; serialise copy as `""`.
- The script cache marshals the typed term; formats inside it carry their
  union-find cells. Bump the cache key when the parameter representation
  changes.

## 6. Logging

- Description logger: `Log.make ["decoder"; "ffmpeg"; "description"]`.
- A stream whose parameters fail to read is logged at warning with the
  exception; no backtrace is needed for an FFmpeg error.

## 7. Documentation in the source tree

An in-tree Markdown note on FFmpeg content (and one on side data) drifted
from the behaviour it described. Do not keep a second description next to
the code: point to this spec instead.

## 8. Tests to keep

- Unit tests of make, length, copy, sub, blit and collapse over the run
  layer, including the cases of content.md §6.6 and §6.9.
- Stand-in tests: every annotation of content.md §2.1 gives the same result
  as with libav.
- Stream description over a set of media fixtures, including a file with a
  subtitle and a data stream, and one with a stream whose parameters fail.
- `get_type` with a copy content type over two incompatible files.

## 9. Faults

- Run the conformers, the merges in producers and the side-data edits inside
  a handler that turns an exception into the fault class of content.md §4.3;
  no `raise` crosses a source's `get_frame` while it streams.
- Count item faults in the producer's own state, keyed by nothing global:
  a counter field reset on each successful item.
