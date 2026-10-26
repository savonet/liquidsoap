# Liquidsoap FFmpeg layer — specification

What Liquidsoap's FFmpeg layer must do: the `%ffmpeg` encoder, the FFmpeg
decoders, the FFmpeg content kinds, inline encoders and decoders, filter
graphs, bitstream filters, side data, converters and FFmpeg I/O. It is
written for someone building the layer in OCaml on top of the FFmpeg
bindings. Requirements use MUST, SHOULD and MAY in the RFC 2119 sense.

**Scope.** The FFmpeg optional layer; the `%ffmpeg` format record, its
extensions, MIME types and codec strings; the FFmpeg converters plugged into
Liquidsoap's media converters; the outputs that special-case `%ffmpeg`; and
the FFmpeg definitions of the standard library.

**Contracts this layer sits on.** The OCaml FFmpeg bindings
([spec](../../../../modules/synced/ffmpeg/spec/README.md)) and clocks
([spec](../../../clock/spec/README.md)). Generic frames, sources and the
encoder and decoder registries appear only as the contracts this layer relies
on, stated where they are used.

## Principles

These hold in every document. Each has one owner; the others link to it.

1. **Content keeps flowing.** Liquidsoap runs long-lived streams. A fault in
   the data is contained to the smallest unit and becomes a signal the script
   can act on: a dropped item with a warning, an end of track, a source that
   is not ready. A data fault never stops an output. A configuration error
   that cannot recover fails the script, whenever it shows. No source invents
   content. Owner: [faults.md](faults.md).
2. **Nothing held is lost.** Every stage drains what it holds before it ends.
   Owner: [draining.md](draining.md).
3. **One notion of source.** A new decoder, a seek, a reconnect, a declared
   timestamp discontinuity, a filter graph generation and a new inline encoder
   each take a new source id. Owner: [content.md §5](content.md#5-stream_idx-and-time_base).
4. **Copied tracks of one source switch together**, once each has the
   keyframe its codec needs. Tracks of different sources switch each on its
   own keyframe. Owner: [encoder-copy.md §12](encoder-copy.md#12-source-switching-and-per-source-state).
5. **One output timeline, strictly increasing DTS** per output stream across
   every source switch. Lengths and positions come from absolute timestamps,
   never from summed rounded steps. Owners: [timestamps.md](timestamps.md) and
   [common.md §6](common.md#6-duration-helper).
6. **Incompatible copied sources are caught by the type system first.** The
   `ffmpeg.copy` parameters carry the full stream header, so the format merge
   rejects a source the muxer cannot take. The encoder checks again at a
   switch. `settings.ffmpeg.content.copy.relaxed_compatibility_check` relaxes
   both checks to media type and codec id and leaves the rest to libav.
   Owners: [content.md §3.1](content.md#31-ffmpegcopy) and
   [encoder-copy.md §12.7](encoder-copy.md#127-parameters-across-sources).

**Compatibility surface.** The `%ffmpeg(...)` syntax; every
`settings.ffmpeg.*` and `settings.decoder.ffmpeg.*` key and its default; the
type names `ffmpeg.copy`, `ffmpeg.audio.raw`, `ffmpeg.video.raw` and their
parameter syntax; every operator and builtin name and its argument names.
Output stays valid for players, HLS clients and ingest servers; it need not
be byte-identical to any earlier implementation. Typing dumps and script
caches may change form.

## Reading order

| #   | Document                                       | Owns                                                                                                                 |
| --- | ---------------------------------------------- | -------------------------------------------------------------------------------------------------------------------- |
| 1   | [faults.md](faults.md)                         | The fault model: configuration error, item fault, source fault, output fault; escalation bounds; bounded waits       |
| 2   | [draining.md](draining.md)                     | What drains when a stage ends, in what order, and what is discarded on purpose                                       |
| 3   | [common.md](common.md)                         | Settings index, log bridge, time bases and `rescale`, the duration helper, pixel formats, scaling, hardware contexts |
| 4   | [content.md](content.md)                       | The content kinds, their parameters, merge and compatibility, runs, source ids, raw conformance                      |
| 5   | [stream-description.md](stream-description.md) | The description of an opened container, field naming, `get_type`                                                     |
| 6   | [encoder-format.md](encoder-format.md)         | The `%ffmpeg(...)` syntax, its content type, rendering, extension and MIME                                           |
| 7   | [encoder.md](encoder.md)                       | Encoder instance lifecycle, metadata, HLS, container restarts, faults; §12 the stream-encoder contract               |
| 8   | [encoder-internal.md](encoder-internal.md)     | Encoded tracks, numbered as encoder.md §12                                                                           |
| 9   | [encoder-copy.md](encoder-copy.md)             | Copied tracks, numbered as encoder.md §12; §12 source switching, groups, per-source state                            |
| 10  | [timestamps.md](timestamps.md)                 | Every encoder-side timestamp rule: the output and media timelines, rebasing, monotonicity                            |
| 11  | [decoder.md](decoder.md)                       | Acceptance, probing, mapping, the decode loop, positions, discontinuities, seek, faults; §7 the per-stream contract  |
| 12  | [decoder-internal.md](decoder-internal.md)     | Decoding to PCM/YUV and subtitles, numbered as decoder.md §7                                                         |
| 13  | [decoder-raw.md](decoder-raw.md)               | Decoding to raw frames, numbered as decoder.md §7                                                                    |
| 14  | [decoder-copy.md](decoder-copy.md)             | Passing packets through, numbered as decoder.md §7                                                                   |
| 15  | [image-decoder.md](image-decoder.md)           | Still images                                                                                                         |
| 16  | [inline-codecs.md](inline-codecs.md)           | `track.ffmpeg.encode/decode.*` and their wrappers; §3 the inline producer                                            |
| 17  | [filters.md](filters.md)                       | Filter builtins, `ffmpeg.filter.create`, graph generations, limits, helper graphs                                    |
| 18  | [bitstream-filters.md](bitstream-filters.md)   | Bitstream filter operators                                                                                           |
| 19  | [side-data.md](side-data.md)                   | Side data with each content kind, autorotate, the side-data operators                                                |
| 20  | [converters.md](converters.md)                 | The `ffmpeg` samplerate and video converters                                                                         |
| 21  | [io.md](io.md)                                 | `input.ffmpeg`, `input.http`, `input.harbor.dynamic`, `output.url`, `%ffmpeg` in Icecast and Harbor outputs          |
| 22  | [tests.md](tests.md)                           | Conformance: harness rules, fixtures, doubles, failure modes, cross-subsystem scenarios                              |
| 23  | [known-complexity.md](known-complexity.md)     | Pitfalls from the history, each answered by a rule and a conformance check                                           |

Seams and their implementations share numbering: encoder.md §12 with
encoder-internal.md and encoder-copy.md, decoder.md §7 with the three
per-stream decoder files. Each implementation file answers every clause of
its contract.

## Two parts

**Part A** is the documents above. **Part B**,
[language-notes/](language-notes/), is advice for an OCaml implementation on
the bindings, one file per area. Part A is complete without it.
