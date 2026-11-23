# `output.file.hls` at the encoder seam (as built)

Seam view only. The normative tree is
[outputs/hls/spec/](../../outputs/hls/spec/README.md), whose
[encoder-contract.md](../../outputs/hls/spec/encoder-contract.md) is the
segmenting face. This file records how the output obtains and drives
encoders and where their bytes go; contradictions are in
[findings/net-callers.md](findings/net-callers.md).

## 1. Formats accepted and how the factory is obtained

Per variant, at evaluation:

1. The stream-face registry is asked for a factory, only to validate the
   format (`Unsupported format`); that factory is never used by the output.
2. The extension is derived.
3. The segmenting registry is asked for an HLS factory; none:
   `Variant <name>: transport of other media file formats is not defined
(RFC 8216 §3.1)`. Two plug-ins answer: `ffmpeg` (MPEG-TS, fMP4, packed
   audio, WebVTT muxers) and `packed-audio` (`%mp3`, `%shine`, `%fdkaac`
   with ADTS transport).
4. A **probe instance** is created (name = variant name, the segment
   duration), `init(id3, id3_version)` is called to learn the variant's ID3
   mode or reject its `id3` field, and the probe is stopped at once with
   `hls_stop()`, errors ignored, its result discarded.

The `packed-audio` adapter wraps a stream-face instance obtained from the
stream registry with empty initial metadata: its probe creates, then stops,
a real MP3/AAC encoder at evaluation.

## 2. Instance lifecycle

One instance per variant per run. A run starts at the output's start, or at
the first piece after an error wait; each run's instances are new. At a
stop, every instance gets `hls_stop()`. After a streaming error
(`reopen_on_error`), every instance is **dropped without any stop**; the
next run builds new ones ([hls operator.md §6](../../outputs/hls/spec/operator.md#6-errors-and-reopening)).

## 3. Call sequence

The encoded-output base cuts frames into pieces; zero-length pieces are not
encoded. Per run and variant: `init` once; per piece `insert_id3` for a
prepended-ID3 variant at each segment open, then `init_encode` until it
answers `Init`, then `split_encode`; in-stream metadata through
`insert_id3`; properties (`codec_attrs`, `independent_segments`,
`free_split`, `hls_bitrate`, `hls_video_size`) after `Init` and after each
`Restart`; `hls_stop` at stop. The stream face's `encode_metadata`,
`header`, `restart_offset` and `stop` are never called by the output; the
packed-audio adapter calls its inner `encode` and `stop`
([encoder-packed-audio.md](../../outputs/hls/spec/encoder-packed-audio.md)).

## 4. The bytes

The pieces are applied in order to the variant's segmenter: `Bytes` are
appended to the open segment, `Split` marks where a segment may be cut,
`Restart` records a new init segment and resyncs the other variants. Segment
and init files are written to a temporary name and moved into place
([hls files.md](../../outputs/hls/spec/files.md)); nothing is seeked. The
encoder never does I/O itself (the FFmpeg instance writes to an in-memory
buffer in HLS mode).

Restart points visible to a player: each run starts with new instances and a
discontinuity in every variant; an aborted segment's bytes, including what
the dropped instance held, are lost.

## 5. Reopen, reconnect

[hls operator.md §6](../../outputs/hls/spec/operator.md#6-errors-and-reopening):
`reopen_on_error` decides the wait; a new run then starts as at start.

## 6. Failures

A failing `hls_stop` at stop is logged at `important` (`Variant <name>:
stopping the encoder failed: <exn>`) and the stop completes. Any exception
while streaming, the encoder's included, aborts the run (§2).

## 7. Threads and locks

All encoder calls on the streaming thread, except the probe, which runs at
evaluation. Each instance is serialised by the segmenting registry's
wrapper.

## 8. Tests

[hls conformance.md](../../outputs/hls/spec/conformance.md), in particular
its encoder-seam section; `tests/media/hls_restart.liq`,
`hls_init_segments.liq`, `hls_timed_id3.liq`, `hls_codecs_at_stop.liq`,
`hls_fmp4_short_stop.liq`, `hls_write_error.liq`.
