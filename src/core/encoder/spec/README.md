# Encoders specification — as-built snapshot

**Status: pass 1 (as-built).** This tree describes what the encoders and the
outputs that drive them do today, read from the code, without judgement. It
is the snapshot the normative pass is written against: the normative text
replaces it, and the diff between the two is the record of what changed and
why. Until then it departs from
[conventions §1](../../../spec/conventions.md#1-normative-language): it is
descriptive, not normative. The [findings](findings/README.md) are its agenda.

Under the [conventions](../../../spec/conventions.md) and
[principles](../../../spec/principles.md) of the project.

## Scope

- The encoder seam: formats, the `%name(...)` language, the registry,
  factories, instances and their operations.
- Every encoder plug-in, and the Ogg muxer's own stream-encoder seam.
- Every output that drives an encoder, described at the seam: what it asks
  and what it does with the bytes.

**Out of scope, owned elsewhere** and linked, not restated: the generic
output ([output.md](../../outputs/spec/output.md)), draining
([draining.md](../../source/spec/draining.md)), the FFmpeg layer's codecs,
streams and timestamps ([FFmpeg tree](../../optionals/ffmpeg/spec/README.md)),
and the internals of harbor ([tree](../../outputs/spec/harbor/README.md)),
SRT ([tree](../../optionals/srt/spec/README.md)) and HLS
([tree](../../outputs/hls/spec/README.md)) beyond their use of the seam.

## Reading order

| File                                       | Content                                                                                                                                                             |
| ------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [encoder.md](encoder.md)                   | The seam: vocabulary, registry, operations, `protect`, which encoder implements what, the §1–§14 numbering, callers.                                                |
| [formats.md](formats.md)                   | Format values: syntax, typing, evaluation and validation, helpers, printing.                                                                                        |
| [output-encoded.md](output-encoded.md)     | The encoded-output base: cutting frames into pieces at metadata, the call order.                                                                                    |
| [wav.md](wav.md)                           | `%wav`.                                                                                                                                                             |
| [avi.md](avi.md)                           | `%avi`.                                                                                                                                                             |
| [ndi.md](ndi.md)                           | `%ndi` and `output.ndi`.                                                                                                                                            |
| [external.md](external.md)                 | `%external`: a child process.                                                                                                                                       |
| [mp3.md](mp3.md)                           | `%mp3` (LAME).                                                                                                                                                      |
| [shine.md](shine.md)                       | `%shine`.                                                                                                                                                           |
| [fdkaac.md](fdkaac.md)                     | `%fdkaac`.                                                                                                                                                          |
| [flac.md](flac.md)                         | `%flac`, native.                                                                                                                                                    |
| [ogg.md](ogg.md)                           | `%ogg` and its muxer.                                                                                                                                               |
| [ogg-streams.md](ogg-streams.md)           | The muxer's stream-encoder contract, answered by [Vorbis](ogg-vorbis.md), [Opus](ogg-opus.md), [Speex](ogg-speex.md), [FLAC](ogg-flac.md), [Theora](ogg-theora.md). |
| [ffmpeg.md](ffmpeg.md)                     | `%ffmpeg` at the seam, and its I/O.                                                                                                                                 |
| [output-file.md](output-file.md)           | `output.file`, including the pipe-output machinery.                                                                                                                 |
| [output-pipe.md](output-pipe.md)           | `output.external`, the `pipe` operator, the language hooks.                                                                                                         |
| [output-icecast.md](output-icecast.md)     | `output.icecast` and `output.shoutcast`.                                                                                                                            |
| [output-harbor.md](output-harbor.md)       | `output.harbor`, at the seam.                                                                                                                                       |
| [output-srt.md](output-srt.md)             | `output.srt`, at the seam.                                                                                                                                          |
| [output-url.md](output-url.md)             | `output.url`, at the seam.                                                                                                                                          |
| [output-hls.md](output-hls.md)             | `output.file.hls` and the HLS face, at the seam.                                                                                                                    |
| [known-complexity.md](known-complexity.md) | What the history shows to be hard, implementation free, grouped by theme.                                                                                           |
| [tests.md](tests.md)                       | The cross-cutting encoder tests; each file above describes its own.                                                                                                 |
| [findings/](findings/README.md)            | Defects, asymmetries, gaps and claims to verify, with their adversarial verification.                                                                               |
| [language-notes/](language-notes/)         | Part B: OCaml and binding traces, one file per area.                                                                                                                |

Encoder files answer the sections §1–§14 defined in
[encoder.md §8](encoder.md#8-section-numbering-of-implementation-files).
Caller files share their own numbering: §1 formats accepted, §2 instance
lifecycle, §3 call sequence, §4 the bytes, §5 reopen, reconnect and append,
§6 failures, §7 threads and locks, §8 tests.

## Where the bytes go

The centre of the redesign is §13 of each encoder file ("Where the bytes go")
and §4–§5 of each caller file.
[encoder.md §7](encoder.md#7-where-the-bytes-go-what-the-seam-assumes)
gathers what the seam itself says about it.

## Compatibility surface

The `%name(...)` syntax and its parameters, and the script-visible names of
the outputs, per
[conventions §5](../../../spec/conventions.md#5-compatibility-surface). The
seam is internal. The bytes each encoder produces are described exactly
because the normative pass decides which of them are compatibility surface.
