# HLS output specification

**As built.** What `output.file.hls` and its standard-library wrappers do
today, read from the code, exact enough to reproduce by hand. It describes
and does not judge: defects found on the way are in
[findings.md](findings.md). A normative pass will follow.

**Reader.** Someone rebuilding the HLS output inside Liquidsoap: sources,
clocks, frames, encoders and the HTTP server exist and are used through the
contracts stated here.

**Scope.** The `output.file.hls` operator; the encoder seam it drives, with
one file per implementation; the standard library's `output.file.hls`
wrapper, `hls.playlist.main` and `output.harbor.hls`. Out of scope:
`input.hls`, `output.youtube.live.hls`, the `%ffmpeg` encoder's internals
([FFmpeg layer spec](../../../optionals/ffmpeg/spec/README.md)) and the HTTP
server ([harbor spec](../../spec/harbor/README.md)).

## Separation of concerns

HLS mixes transport, playlist syntax, segmenting, container details and
storage. The documents keep them apart, one concern each, and link across.
This is a standing rule from the user for every later pass, **for the
spec and for the code**: the normative pass keeps the concerns in separate
documents with narrow contracts between them, and the reimplementation
builds them as separate modules along the same seams. Where today's
behaviour couples two concerns (the encoder's split decisions and the
output's grid, file names and playlist URIs, storage and serving), the
coupling is described where it happens and listed as a finding.

| Concern                             | Owner                                      |
| ----------------------------------- | ------------------------------------------ |
| Script interface, lifecycle, errors | [operator.md](operator.md)                 |
| What a variant is and announces     | [variants.md](variants.md)                 |
| Where segments are cut              | [segmenting.md](segmenting.md)             |
| Playlist bytes                      | [playlists.md](playlists.md)               |
| Files on disk                       | [files.md](files.md)                       |
| Saved state across restarts         | [persistence.md](persistence.md)           |
| ID3 and inserted tags               | [metadata.md](metadata.md)                 |
| Encoder seam                        | [encoder-contract.md](encoder-contract.md) |
| HTTP serving                        | [serving.md](serving.md)                   |

## Reading order

| File                                               | Content                                                                      |
| -------------------------------------------------- | ---------------------------------------------------------------------------- |
| [operator.md](operator.md)                         | Part A. Parameters, construction, lifecycle, the per-frame loop, errors.     |
| [variants.md](variants.md)                         | Part A. Variant fields and derived properties: bandwidth, codecs, extension. |
| [segmenting.md](segmenting.md)                     | Part A. Grid, split points, cuts, the target-duration bound, restarts.       |
| [playlists.md](playlists.md)                       | Part A. Media and main playlist bytes, with examples.                        |
| [files.md](files.md)                               | Part A. Names, atomic writes, retention, cleanup, file-change events.        |
| [persistence.md](persistence.md)                   | Part A. The state file's format, saving, restoring.                          |
| [metadata.md](metadata.md)                         | Part A. Track metadata as ID3, inserted tags, extra cuts.                    |
| [encoder-contract.md](encoder-contract.md)         | Part A. The encoder seam.                                                    |
| [encoder-frame-units.md](encoder-frame-units.md)   | Part A. `%wav`, `%avi`, `%flac`, `%ogg`, `%external`, `%ndi`.                |
| [encoder-packed-audio.md](encoder-packed-audio.md) | Part A. `%mp3`, `%shine`, `%fdkaac`, with the ID3 timestamp bytes.           |
| [encoder-ffmpeg.md](encoder-ffmpeg.md)             | Part A. `%ffmpeg`, mapped to the FFmpeg layer's spec.                        |
| [serving.md](serving.md)                           | Part A. `output.harbor.hls` and its static handler.                          |
| [conformance.md](conformance.md)                   | The current test suite, by principle; what it leaves uncovered.              |
| [known-complexity.md](known-complexity.md)         | What the history shows to be hard, reconciled with this spec.                |
| [findings.md](findings.md)                         | Defects, asymmetries, gaps, claims to verify. The agenda for the next pass.  |
| [ocaml-notes.md](ocaml-notes.md)                   | Part B. Notes on the OCaml implementation. Not normative.                    |

Encoder implementation files use the contract's section numbers: "§7" is
`insert_id3` in all of them.

**No document of their own**: concurrency (one streaming cycle runs every
call; only inserted tags cross threads, [metadata.md §3](metadata.md#3-inserted-tags));
crash safety (atomic publication is [files.md §3](files.md#3-writing-a-file),
the state file [persistence.md §3](persistence.md#3-saving)); security
(file modes, [files.md §3](files.md#3-writing-a-file); the served directory,
[serving.md §3](serving.md#3-the-static-handler)). Each is one rule today,
with its owner. Findings F-SRV-1 and F-FS-1 may justify a security document
in the normative pass.

## Vocabulary

- **Variant**: one entry of the output's stream list, with its own encoder,
  media playlist and segments.
- **Main ticks**: the internal time unit. **Frame**: the configured frame
  size in ticks. **Chunk**: the part of a frame between metadata positions.
- **Media time**: ticks a variant's encoder has received.
- **Grid**: the boundaries `k × D`, `D` the segment duration in ticks.
- **Split point**: a byte position, with its media time, where the encoder
  allows a segment to start. **Forced** split: made without the encoder's
  own condition.
- **Cut**: ending the open segment at a split point.
- **Container restart**: a split point where the encoder starts a new
  container, possibly with a new init segment.
- **Position number**: a segment's sequence number within its variant.
- **Publish**: renaming a completed temporary file onto its final name.
- **Retention bound**: `segments + segments_overhead`.

## Compatibility surface

Not settled yet; to be decided with the user before the normative pass.
Candidates: the parameter and method names and defaults of
[operator.md §1](operator.md#1-parameters); the default file names; the
state file of [persistence.md §2](persistence.md#2-format), which deployed
systems hold on disk; the file-change event states.
