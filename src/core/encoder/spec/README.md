# Encoders specification

What every encoder MUST, SHOULD and MAY do, and what the outputs that drive
encoders do at that seam and with the bytes, under the
[conventions](../../../spec/conventions.md) and
[principles](../../../spec/principles.md) of the project.

## Scope

- The seam outputs use: the **container writer**, created with the declared
  capabilities of its destination, writing bytes and markers into a
  **sink** the output provides.
- Every writer: the native single-stream encoders, Ogg and its private
  stream-encoder contract, `%ffmpeg`, `%external`; and `%ndi`, which is not
  a writer.
- Every output that drives a writer, at the seam.
- Readers that start mid-stream on a shared stream:
  [shared-stream.md](../../outputs/spec/shared-stream.md).

**Out of scope, owned elsewhere** and linked: the generic output
([output.md](../../outputs/spec/output.md)), draining
([draining.md](../../source/spec/draining.md)), the FFmpeg layer's codecs,
streams and timestamps ([FFmpeg tree](../../optionals/ffmpeg/spec/README.md)),
and the own rules of harbor ([tree](../../outputs/spec/harbor/README.md)),
SRT ([tree](../../optionals/srt/spec/README.md)) and HLS
([tree](../../outputs/hls/spec/README.md)).

## Reading order, also the order to build in

| File                                                                                                                                 | Content                                                                                                                                                                                                                                                                                                                      |
| ------------------------------------------------------------------------------------------------------------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [encoder.md](encoder.md)                                                                                                             | The contract: vocabulary, registry, instances, the stream face (markers, header forms, preambles and tags, draining, delay and padding), the segmenting face, capabilities and the checks at evaluation, the delegated destination, concurrency, faults, in-band metadata, the numbering of implementation and caller files. |
| [formats.md](formats.md)                                                                                                             | Format values: one specification per parameter for type and value, explicit versus adapted defaults, validation timing, helpers, printing.                                                                                                                                                                                   |
| [sink.md](sink.md)                                                                                                                   | The sink contract: capabilities, writes, markers, seek and read-back, never blocking, the end and final delivery, faults.                                                                                                                                                                                                    |
| [shared-stream.md](../../outputs/spec/shared-stream.md)                                                                              | Readers starting mid-stream: access points, entry headers, bounded history.                                                                                                                                                                                                                                                  |
| [output-encoded.md](output-encoded.md)                                                                                               | What every encoder-driving output shares: current and initial metadata, pieces, exported metadata, cover art.                                                                                                                                                                                                                |
| [wav.md](wav.md), [avi.md](avi.md)                                                                                                   | `%wav`, `%avi`.                                                                                                                                                                                                                                                                                                              |
| [mp3.md](mp3.md), [shine.md](shine.md)                                                                                               | `%mp3` (LAME) and `%shine`: MPEG audio, ID3v2, the encoder-information frame.                                                                                                                                                                                                                                                |
| [fdkaac.md](fdkaac.md), [flac.md](flac.md)                                                                                           | `%fdkaac`, native `%flac`.                                                                                                                                                                                                                                                                                                   |
| [ogg.md](ogg.md), [ogg-streams.md](ogg-streams.md)                                                                                   | `%ogg` and its stream-encoder contract, answered by [Vorbis](ogg-vorbis.md), [Opus](ogg-opus.md), [Speex](ogg-speex.md), [FLAC](ogg-flac.md), [Theora](ogg-theora.md).                                                                                                                                                       |
| [ffmpeg.md](ffmpeg.md)                                                                                                               | `%ffmpeg` as a writer: muxer over a sink or a delegated destination.                                                                                                                                                                                                                                                         |
| [external.md](external.md)                                                                                                           | `%external`: a child process as a writer.                                                                                                                                                                                                                                                                                    |
| [ndi.md](ndi.md)                                                                                                                     | `%ndi` and `output.ndi`.                                                                                                                                                                                                                                                                                                     |
| [output-file.md](output-file.md)                                                                                                     | `output.file`: the file sink, naming, reopen, append.                                                                                                                                                                                                                                                                        |
| [output-pipe.md](output-pipe.md)                                                                                                     | `output.external`, the `pipe` operator, the language hooks.                                                                                                                                                                                                                                                                  |
| [output-icecast.md](output-icecast.md)                                                                                               | `output.icecast` and `output.shoutcast`.                                                                                                                                                                                                                                                                                     |
| [output-harbor.md](output-harbor.md), [output-srt.md](output-srt.md), [output-url.md](output-url.md), [output-hls.md](output-hls.md) | The other outputs at the seam.                                                                                                                                                                                                                                                                                               |
| [conformance.md](conformance.md)                                                                                                     | The checks every implementation is run against, their doubles, and whether each failed before.                                                                                                                                                                                                                               |
| [known-complexity.md](known-complexity.md)                                                                                           | What the history shows to be hard, each entry answered by a rule.                                                                                                                                                                                                                                                            |
| [language-notes/](language-notes/)                                                                                                   | Part B of the snapshot the specification was extracted from; not normative.                                                                                                                                                                                                                                                  |

Implementation files answer [encoder.md §10](encoder.md#10-implementation-files)
clause by clause; caller files answer [§11](encoder.md#11-caller-files).

## Vocabulary

Defined once, in [encoder.md](encoder.md#vocabulary): container writer,
sink, configuration, stream start, access point, entry header, streamable,
needs a seekable destination, file sink, continuing, file form and
streaming form, preamble, tags, in-band metadata. The vocabulary of the
[outputs](../../outputs/spec/README.md#vocabulary) applies.

## Compatibility surface

- Script-visible: the `%name(...)` formats and their parameters, and the
  outputs' parameters, methods and callbacks, per
  [conventions §5](../../../spec/conventions.md#5-compatibility-surface).
  This specification changes several of them; each change is stated where
  its rule is, with its migration.
- Bytes: only validity is binding. Outputs produce valid, decodable files
  and streams of their format; exact bytes may change.
- Wire: what Icecast and Shoutcast servers and harbor's HTTP listeners
  receive keeps its external commitments; it changes only where that
  improves compatibility.
- The seam is internal and changes freely.

## Cross-cutting concerns without a document of their own

- **Faults** are classified by the project's
  [faults.md](../../../spec/faults.md); encoder.md §8 and sink.md map the
  writer's and the sink's onto it.
- **Concurrency** is the project's
  [concurrency.md](../../../spec/concurrency.md); encoder.md §7 states the
  single caller.
