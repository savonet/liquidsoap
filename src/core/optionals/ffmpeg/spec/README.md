# Liquidsoap FFmpeg layer — as-built specification

What Liquidsoap's FFmpeg layer does, read from the code: the `%ffmpeg`
encoder, the FFmpeg decoders, the FFmpeg content kinds, inline encoders and
decoders, filter graphs, bitstream filters, side data, converters and FFmpeg
I/O. It is written to be exact enough to rebuild the layer in OCaml on top
of the FFmpeg bindings without the code in view.

This is the **as-built** pass: it describes, it does not prescribe. Defects,
asymmetries and gaps found while reading are kept apart in
[findings/](findings/README.md).

**Scope.** `src/core/optionals/ffmpeg/` in full; the non-optional FFmpeg
touchpoints in `src/core/encoder/` (the `%ffmpeg` format record, extensions,
MIME types, codec strings), the FFmpeg converters plugged into
`src/core/media/`, the outputs that special-case `%ffmpeg`, and the FFmpeg
definitions in `src/libs/*.liq`.

**Out of scope, referenced.** The OCaml FFmpeg bindings
([spec](../../../../modules/synced/ffmpeg/spec/README.md)) and clocks
([spec](../../../clock/spec/README.md)). Generic frames, sources and the
encoder/decoder registries appear only as the contracts this layer relies on,
stated where they are used.

## Reading order

| #   | Document                                       | Owns                                                                                                                               |
| --- | ---------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| 1   | [common.md](common.md)                         | Settings, log bridge, time bases, time-base conversion, the Duration helper, pixel formats, scaling, hardware contexts             |
| 2   | [content.md](content.md)                       | The kinds `ffmpeg.copy`, `ffmpeg.audio.raw`, `ffmpeg.video.raw`: types, parameters, the two chunk layers, `stream_idx`, operations |
| 3   | [stream-description.md](stream-description.md) | The container/stream description model, `describe`, `get_type`                                                                     |
| 4   | [encoder-format.md](encoder-format.md)         | The `%ffmpeg(...)` syntax, its content type, rendering, extension and MIME                                                         |
| 5   | [encoder.md](encoder.md)                       | Container lifecycle, metadata, HLS, outputs; §12 the stream-encoder contract                                                       |
| 6   | [encoder-internal.md](encoder-internal.md)     | Encoded (internal and raw) tracks, numbered as encoder.md §12                                                                      |
| 7   | [encoder-copy.md](encoder-copy.md)             | Copied tracks, numbered as encoder.md §12; §12 source switching, keyframe readiness, per-source records                            |
| 8   | [timestamps.md](timestamps.md)                 | Every encoder-side PTS/DTS rule: units, lifetimes, offsets, monotonicity, what the muxer is told                                   |
| 9   | [decoder.md](decoder.md)                       | Acceptance, probing, stream mapping, the decode loop, metadata, duration, seek; §7 the per-stream decoder contract                 |
| 10  | [decoder-internal.md](decoder-internal.md)     | Decoding to PCM/YUV and text/bitmap subtitles, numbered as decoder.md §7                                                           |
| 11  | [decoder-raw.md](decoder-raw.md)               | Decoding to raw frames, numbered as decoder.md §7                                                                                  |
| 12  | [decoder-copy.md](decoder-copy.md)             | Passing packets through, numbered as decoder.md §7                                                                                 |
| 13  | [image-decoder.md](image-decoder.md)           | Still images                                                                                                                       |
| 14  | [inline-codecs.md](inline-codecs.md)           | `track.ffmpeg.encode/decode.*` and the `ffmpeg.liq` wrappers; §3 the inline producer                                               |
| 15  | [filters.md](filters.md)                       | Filter builtins from AVOptions, `ffmpeg.filter.create`, graph generations, clocks, helper graphs                                   |
| 16  | [bitstream-filters.md](bitstream-filters.md)   | Bitstream filter operators                                                                                                         |
| 17  | [side-data.md](side-data.md)                   | Side data with each content kind, autorotate, the side-data track operators                                                        |
| 18  | [converters.md](converters.md)                 | The `ffmpeg` samplerate and video converters                                                                                       |
| 19  | [io.md](io.md)                                 | `input.ffmpeg`, `input.http`, `input.harbor.dynamic`, `output.url`, `%ffmpeg` in Icecast/Harbor                                    |
| 20  | [tests.md](tests.md)                           | The test suite by principle: invariants, binding and incidental parts, fixtures                                                    |
| 21  | [known-complexity.md](known-complexity.md)     | Pitfalls from the history (timestamps, keyframe switching, per-source state), each reconciled with this spec                       |

Every rule has one owner; other files link to it. Seams and their
implementations share numbering: encoder.md §12 with encoder-internal.md and
encoder-copy.md, decoder.md §7 with the three decoder files.

## Two parts

**Part A** is the files above. **Part B**, notes on the OCaml
implementation (libraries, functors, classes, `Weak` tables, registration),
is in [language-notes/](language-notes/), one file per area. Part B is not
needed to follow Part A.

## Cross-cutting concerns

- **Timestamps** are owned by [timestamps.md](timestamps.md) for the
  encoder, by decoder.md §9 for decoding, and by [common.md](common.md) for
  units and conversion. Other files link there.
- **Source identity** (`stream_idx`) is defined in [content.md](content.md)
  §5. What changes it (a new decoder, a seek, a reconnect, a filter graph
  generation, an inline encoder) is listed there. What a change does to an
  encoder is in [encoder-copy.md](encoder-copy.md) §12 and
  [timestamps.md](timestamps.md) §2.
- **Keyframe switching.** The intent recorded beside the code is that copied
  tracks from one source switch together once all are ready, and tracks from
  different sources switch each on its own keyframe. What the code does,
  case by case, is [encoder-copy.md](encoder-copy.md) §12.3.
- **No document** is given to crash safety or security. The layer holds no
  durable state of its own. Its trust boundary is untrusted media and network
  input, and each file states how a failed or malformed input behaves at the
  point it is read.

## Findings

[findings/README.md](findings/README.md) ranks every finding (data loss
first) with a verification verdict and deduplication groups.
[findings/cross-file.md](findings/cross-file.md) records the contradictions
between files and how each was settled.
