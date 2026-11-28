# The `%ffmpeg` writer under HLS

Part A. `%ffmpeg` offers the segmenting face of
[encoder-contract.md](encoder-contract.md) itself, when built with a
`segmented(D)` sink. Its behaviour is specified in the FFmpeg layer's spec;
this file maps each contract section to it and states what the HLS output
depends on. Sections follow the contract's numbering.

## 1. Role

Access points are positions where every participating stream's next packet
is a key packet; encoded video places its key frames by its key-frame
interval, defaulted from `D` ([encoder.md §3](../../../optionals/ffmpeg/spec/encoder.md#3-building-an-instance)).
[encoder.md §8.2](../../../optionals/ffmpeg/spec/encoder.md#82-keyframe-hooks).

## 2. Calls and order

The `segmented(D)` capability selects the writer's segmenting behaviour and
its segmenting defaults
([encoder.md §8](../../../optionals/ffmpeg/spec/encoder.md#8-hls),
[ffmpeg.md §12](../../../encoder/spec/ffmpeg.md#12-segmenting-face)); no
other flag is passed. The container header is the init segment. No writer is
built at evaluation: segmentability and the ID3 mode are declarations
([ffmpeg.md §2](../../../encoder/spec/ffmpeg.md#2-declarations)).

## 3. ID3 mode

Declared by muxer: MPEG-TS, mode in-stream, none with `id3 = false`; ADTS,
MP3, AC-3 and E-AC-3, mode prepended, and `id3 = false` is rejected; any
other muxer, mode none, and `id3 = true` is rejected.
[encoder.md §8.1](../../../optionals/ffmpeg/spec/encoder.md#81-id3-mode).

## 4. The first stream start

No stream start until the container header is known; then the header as the
init segment (fMP4: `ftyp` + `moov`; none for MPEG-TS and packed audio).
[encoder.md §8.3](../../../optionals/ffmpeg/spec/encoder.md#83-init-segment).

## 5. Access points and restarts

Access points at key packets of streams that are not all-key, and at each
split request when every stream is all-key; MPEG-TS access points start
with a PAT and a PMT
([encoder.md §8.7](../../../optionals/ffmpeg/spec/encoder.md#87-segmenting-hls-output-side));
restarts at incompatible copy switches.
[encoder.md §8.4](../../../optionals/ffmpeg/spec/encoder.md#84-split-points-and-restarts),
[§13](../../../optionals/ffmpeg/spec/encoder.md#13-container-restart).

Access point times come from packet times. A copied stream's first key
packet can sit after the run's start by the source's codec delay, so its
access points can all fall just after their targets; the cut rule absorbs
that ([segmenting.md §4.1](segmenting.md#41-key-frame-variants)). Key
packets of a copied stream reach the output after their time by the
source's interleaving; the cut rule does not depend on when they arrive.

## 6. Properties

[encoder.md §8.6](../../../optionals/ffmpeg/spec/encoder.md#86-properties).
`free_split` is true when every participating stream is all-key (audio,
subtitles, intra-only codecs): the hook then marks an access point before
the first packet at or after each split request
([encoder.md §8.2](../../../optionals/ffmpeg/spec/encoder.md#82-keyframe-hooks)).
False otherwise.

## 7. `insert_id3`

MPEG-TS: writes a timed-ID3 packet at `time` and returns none. Packed
audio: returns a tag to prepend, built as in
[encoder-packed-audio.md §7](encoder-packed-audio.md#7-insert_id3).
[encoder.md §8.5](../../../optionals/ffmpeg/spec/encoder.md#85-timed-id3-in-mpegts).

## 8. Stop

`end(clean)` drains the writer
([encoder.md §7](../../../optionals/ffmpeg/spec/encoder.md#7-end)) and
writes the drained output: the candidates its drained packets take are
access points; bytes held after an undecided candidate are written as bytes.
Its first stream start, with the container's header, comes then when none
was written before. `end(abandoned)` writes no trailer.

## 9. Conformance

[encoder.md §8.8](../../../optionals/ffmpeg/spec/encoder.md#88-conformance)
and [conformance.md §4](conformance.md#4-encoder-seam).
