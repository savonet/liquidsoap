# Liquidsoap FFmpeg layer — specification

What Liquidsoap's FFmpeg layer must do: the `%ffmpeg` encoder, the FFmpeg
decoders, the FFmpeg content kinds, inline encoders and decoders, filter
graphs, bitstream filters, side data, converters and FFmpeg I/O. It is
written for someone building the layer in OCaml on top of the FFmpeg
bindings, under the [project conventions](../../../../spec/conventions.md).
It sits on the levels indexed in [src/spec](../../../../spec/README.md): the
project rules ([principles](../../../../spec/principles.md),
[faults](../../../../spec/faults.md),
[concurrency](../../../../spec/concurrency.md),
[testing](../../../../spec/testing.md)); the streaming model
([sources](../../../source/spec/README.md)), which its decoders, inline
codecs and filters produce under; [clocks](../../../clock/spec/README.md);
and the generic seams its encoders and outputs implement, the
[encoder interface](../../../encoder/spec/encoder.md) and the
[output base](../../../outputs/spec/output.md). A rule of those levels is
linked here, not restated.

**Scope.** The FFmpeg optional layer; the `%ffmpeg` format record, its
extensions, MIME types and codec strings; the FFmpeg converters plugged into
Liquidsoap's media converters; the outputs that special-case `%ffmpeg`; and
the FFmpeg definitions of the standard library.

**Contracts this layer sits on.** The OCaml FFmpeg bindings
([spec](../../../../modules/synced/ffmpeg/spec/README.md)) and clocks
([spec](../../../clock/spec/README.md)). The encoder registry and instance
rules are the [encoder interface](../../../encoder/spec/encoder.md)'s;
generic frames and the decoder registry appear only as the contracts this
layer relies on, stated where they are used.

## Principles

These hold in every document. Each has one owner; the others link to it.

1. **Keep streaming, unless the error is major**: owned by
   [faults.md](../../../../spec/faults.md#0-the-rule), which every fault
   table of this layer classifies against.
2. **Nothing held is lost**: owned by
   [draining.md](../../../source/spec/draining.md).
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
6. **Copied content is the user's contract** (user decision D2). Copy passes
   coded packets through untouched, so the layer cannot repair a source the
   output cannot carry: preparing compatible sources is the user's job, and
   copy is an advanced feature. The check when a source opens is advisory.
   It refuses what users expect to be fixed, such as the codec or the video
   size, unless a container is known to carry the change, and what a
   container is known not to carry. When in doubt about anything else it
   SHOULD accept, and FFmpeg judges at run time. A copied stream that turns
   out not to fit its output, with no clean restart, MUST fail hard with an
   unrecoverable error naming what differs, rather than keep streaming a
   broken result: it is one of the major errors of
   [faults.md §0](../../../../spec/faults.md#0-the-rule).
   `settings.ffmpeg.content.copy.relaxed_compatibility_check` reduces the
   check at open to media type and codec id. Owners:
   [content.md §3.1.1](content.md#311-where-copy-parameters-are-merged) and
   [encoder-copy.md §12.7](encoder-copy.md#127-parameters-across-sources).

**User decisions.** The tree cites the maintainer's decisions by label: D2
is principle 6; D3 is principle 3 (every new timeline or parameter set takes
a new source id); D4 is principle 4 (one producer's tracks share one id and
switch together); D5 is principle 5's strictly increasing DTS; D6 is that
per-source state costs no permanent allocation
([content.md §5](content.md#5-stream_idx-and-time_base)).

**Compatibility surface.** The `%ffmpeg(...)` syntax; every
`settings.ffmpeg.*` and `settings.decoder.ffmpeg.*` key and its default; the
type names `ffmpeg.copy`, `ffmpeg.audio.raw`, `ffmpeg.video.raw` and their
parameter syntax; every operator and builtin name and its argument names.
Output stays valid for players, HLS clients and ingest servers; it need not
be byte-identical to any earlier implementation. Typing dumps and script
caches may change form.

## Reading order

| #   | Document                                        | Owns                                                                                                                 |
| --- | ----------------------------------------------- | -------------------------------------------------------------------------------------------------------------------- |
| 1   | [faults.md](../../../../spec/faults.md)         | The fault model: configuration error, item fault, source fault, output fault; escalation bounds; bounded waits       |
| 2   | [draining.md](../../../source/spec/draining.md) | What drains when a stage ends, in what order, and what is discarded on purpose                                       |
| 3   | [common.md](common.md)                          | Settings index, log bridge, time bases and `rescale`, the duration helper, pixel formats, scaling, hardware contexts |
| 4   | [content.md](content.md)                        | The content kinds, their parameters, merge and compatibility, runs, source ids, raw conformance                      |
| 5   | [stream-description.md](stream-description.md)  | The description of an opened container, field naming, `get_type`                                                     |
| 6   | [encoder-format.md](encoder-format.md)          | The `%ffmpeg(...)` syntax, its content type, rendering, extension and MIME                                           |
| 7   | [encoder.md](encoder.md)                        | Encoder instance lifecycle, metadata, HLS, container restarts, faults; §12 the stream-encoder contract               |
| 8   | [encoder-internal.md](encoder-internal.md)      | Encoded tracks, numbered as encoder.md §12                                                                           |
| 9   | [encoder-copy.md](encoder-copy.md)              | Copied tracks, numbered as encoder.md §12; §12 source switching, groups, per-source state                            |
| 10  | [timestamps.md](timestamps.md)                  | Every encoder-side timestamp rule: the output and media timelines, rebasing, monotonicity                            |
| 11  | [decoder.md](decoder.md)                        | Acceptance, probing, mapping, the decode loop, positions, discontinuities, seek, faults; §7 the per-stream contract  |
| 12  | [decoder-internal.md](decoder-internal.md)      | Decoding to PCM/YUV and subtitles, numbered as decoder.md §7                                                         |
| 13  | [decoder-raw.md](decoder-raw.md)                | Decoding to raw frames, numbered as decoder.md §7                                                                    |
| 14  | [decoder-copy.md](decoder-copy.md)              | Passing packets through, numbered as decoder.md §7                                                                   |
| 15  | [image-decoder.md](image-decoder.md)            | Still images                                                                                                         |
| 16  | [inline-codecs.md](inline-codecs.md)            | `track.ffmpeg.encode/decode.*` and their wrappers; §3 the inline producer                                            |
| 17  | [filters.md](filters.md)                        | Filter builtins, `ffmpeg.filter.create`, graph generations, limits, helper graphs                                    |
| 18  | [bitstream-filters.md](bitstream-filters.md)    | Bitstream filter operators                                                                                           |
| 19  | [side-data.md](side-data.md)                    | Side data with each content kind, autorotate, the side-data operators                                                |
| 20  | [converters.md](converters.md)                  | The `ffmpeg` samplerate and video converters                                                                         |
| 21  | [io.md](io.md)                                  | `input.ffmpeg`, `input.http`, `input.harbor.dynamic`, `output.url`, `%ffmpeg` in Icecast and Harbor outputs          |
| 22  | [tests.md](tests.md)                            | Conformance: harness rules, fixtures, doubles, failure modes, cross-subsystem scenarios                              |
| 23  | [known-complexity.md](known-complexity.md)      | Pitfalls from the history, each answered by a rule and a conformance check                                           |

Seams and their implementations share numbering: encoder.md §12 with
encoder-internal.md and encoder-copy.md, decoder.md §7 with the three
per-stream decoder files. Each implementation file answers every clause of
its contract.

## Two parts

**Part A** is the documents above, complete on its own. The tree has no
Part B yet.
