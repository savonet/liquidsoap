# HLS output specification

**Normative.** What `output.file.hls` and its standard-library wrappers
MUST, SHOULD and MAY do, exact enough to rebuild them. RFC 8216 and Apple's
HLS authoring specification are cited where a rule comes from them.

**Reader.** Someone building the HLS output inside Liquidsoap: sources,
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
storage. Each concern has one document and a narrow contract with the
others, and an implementation builds them as separate modules along the
same seams.

| Concern                                   | Owner                                      |
| ----------------------------------------- | ------------------------------------------ |
| Script interface, lifecycle, errors       | [operator.md](operator.md)                 |
| What a variant is and announces           | [variants.md](variants.md)                 |
| Where segments are cut, for every variant | [segmenting.md](segmenting.md)             |
| Playlist bytes and numbering              | [playlists.md](playlists.md)               |
| Files on disk                             | [files.md](files.md)                       |
| Saved state across restarts               | [persistence.md](persistence.md)           |
| ID3, inserted tags, cuts on metadata      | [metadata.md](metadata.md)                 |
| Encoder seam                              | [encoder-contract.md](encoder-contract.md) |
| HTTP serving                              | [serving.md](serving.md)                   |

The contracts between them:

- **Segmenting → files**: "close this segment at this length", "open one at
  this time". Segmenting never names a file.
- **Files → playlists**: the list of retained segments with their position
  numbers, lengths, flags and file names; playlists never touch the disk
  except through the file store's write.
- **Encoder → segmenting**: pieces (bytes, split points, restarts) and
  properties; the encoder never sees a segment.
- **Files → serving**: the tracked set of files; serving never reads the
  directory to decide which files are the output's.

## Reading order

Build in this order, each layer with its checks of
[conformance.md](conformance.md) before the next.

| File                                               | Content                                                              |
| -------------------------------------------------- | -------------------------------------------------------------------- |
| [encoder-contract.md](encoder-contract.md)         | The encoder seam.                                                    |
| [encoder-packed-audio.md](encoder-packed-audio.md) | `%mp3`, `%shine`, `%fdkaac`, with the ID3 timestamp bytes.           |
| [encoder-ffmpeg.md](encoder-ffmpeg.md)             | `%ffmpeg`, mapped to the FFmpeg layer's spec.                        |
| [variants.md](variants.md)                         | Variant fields and derived properties: bandwidth, codecs, extension. |
| [segmenting.md](segmenting.md)                     | Grid, cut rule, target-duration bound, output-wide cuts, restarts.   |
| [metadata.md](metadata.md)                         | ID3, inserted tags, cuts on metadata.                                |
| [files.md](files.md)                               | Names, atomic writes, retention, cleanup, file-change events.        |
| [playlists.md](playlists.md)                       | Media and main playlist bytes, numbering, with examples.             |
| [persistence.md](persistence.md)                   | The state file, saving, restoring.                                   |
| [operator.md](operator.md)                         | Parameters, construction, lifecycle, the per-frame loop, errors.     |
| [serving.md](serving.md)                           | `output.harbor.hls`: what is served, content types, caching.         |
| [conformance.md](conformance.md)                   | The checks, by principle.                                            |
| [known-complexity.md](known-complexity.md)         | What the history shows to be hard, each entry answered by a rule.    |
| [ocaml-notes.md](ocaml-notes.md)                   | Part B. Notes on an earlier OCaml implementation. Not normative.     |

Encoder implementation files use the contract's section numbers: "§7" is
`insert_id3` in all of them.

**No document of their own**: concurrency (one streaming cycle runs every
call; `insert_tag` and the server's reads of the tracked files cross
threads, [metadata.md §3](metadata.md#3-inserted-tags),
[files.md](files.md));
crash safety (atomic publication, [files.md §3](files.md#3-writing-a-file);
the state file, [persistence.md §3](persistence.md#3-saving)); security
(names stay inside the directory, [files.md §2](files.md#2-file-names);
modes under the umask, [files.md §3](files.md#3-writing-a-file); what is served, [serving.md §3](serving.md#3-what-is-served)). Each is a few rules
with one owner.

## Vocabulary

- **Variant**: one entry of the output's stream list, with its own encoder,
  media playlist and segments.
- **Run**: from a start, or the rebuild after an error, to the next stop or
  error. Every variant gets a new encoder at the start of a run. **Final
  stop**: the stop at the end of the process.
- **Main ticks**: the internal time unit. **Frame**: the configured frame
  size in ticks. **Chunk**: the part of a frame between metadata positions.
- **Media time**: ticks given to the encoders since the run started, the
  same for every variant.
- **Grid**: the boundaries `k × D` of media time, `D` the segment duration.
- **Split point**: a byte position, with its media time, where a segment
  may start. **Forced** split: made without the encoder's own condition.
  **Free-split** variant: one with a split point within a codec frame of
  any requested time.
- **Cut**: ending the open segment at a split point. **Sync cut**: a cut
  made in every variant for the same media time.
- **Container restart**: a split point where the encoder starts a new
  container, possibly with a new init segment.
- **Position number**: a published segment's number within its variant;
  the media sequence is derived from it.
- **Publish**: renaming a completed temporary file onto its final name.

## Compatibility surface

- **Data on disk changes freely**: the state file, the bytes of segments
  and init segments, temporary files.
- **The user-facing API is hard to break**: parameters, callbacks, methods,
  file-change event states and paths, default file names. Each change to it
  is argued separately.
