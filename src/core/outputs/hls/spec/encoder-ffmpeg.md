# The `%ffmpeg` encoder under HLS

Part A. `%ffmpeg` implements the seam of
[encoder-contract.md](encoder-contract.md) itself. Its behaviour is
specified in the FFmpeg layer's spec; this file maps each contract section
to it and states what the HLS output depends on. Sections follow the
contract's numbering.

## 1. Role

Split points are positions where every participating stream's next packet
is a key packet; encoded video makes a key frame at each boundary it is
given. [encoder.md §8.2](../../../optionals/ffmpeg/spec/encoder.md#82-keyframe-hooks).

## 2. Calls and order

With the HLS flag the instance runs in its HLS mode
([encoder.md §8](../../../optionals/ffmpeg/spec/encoder.md#8-hls)).
The container header is the init segment.

## 3. `init`

By muxer: MPEG-TS, mode in-stream unless `id3 = false`; ADTS, MP3, AC-3
and E-AC-3, mode prepended, and `id3 = false` is rejected; any other muxer,
mode none, and `id3 = true` is rejected.
[encoder.md §8.1](../../../optionals/ffmpeg/spec/encoder.md#81-init-id3_enabled-id3_version-).

## 4. `init_encode`

`Pending` until the container header is known; then the header as the init
segment (fMP4: `ftyp` + `moov`; none for MPEG-TS and packed audio).
[encoder.md §8.3](../../../optionals/ffmpeg/spec/encoder.md#83-init_encode-frame).

## 5. `split_encode`

Split points at key packets and at each request; MPEG-TS split points
start with a PAT and a PMT
([encoder.md §8.7](../../../optionals/ffmpeg/spec/encoder.md#87-segment-grid-hls-output-side)); `force` honoured except
when a key frame at the boundary is already on its way; restarts at
incompatible copy switches.
[encoder.md §8.4](../../../optionals/ffmpeg/spec/encoder.md#84-split_encode-frame),
[§13](../../../optionals/ffmpeg/spec/encoder.md#13-container-restart).

Split times come from packet times. A copied stream's first key packet can
sit after the run's start by the source's codec delay, so its split points
can all fall just after the boundaries; the cut rule absorbs that
([segmenting.md §4](segmenting.md#4-the-cut-rule)).

## 6. Properties

[encoder.md §8.6](../../../optionals/ffmpeg/spec/encoder.md#86-properties).
`free_split` is true when every participating stream is all-key (audio,
subtitles, intra-only codecs): the hook then takes a split point before the
first packet at or after each request
([encoder.md §8.2](../../../optionals/ffmpeg/spec/encoder.md#82-keyframe-hooks)).
False otherwise. `split_due` is true while a key frame made for a request has
not yet reached the muxer, or a restart is not yet returned: the cases in
which the instance declines `force`
([encoder.md §8.4](../../../optionals/ffmpeg/spec/encoder.md#84-split_encode-frame)).

## 7. `insert_id3`

MPEG-TS: writes a timed-ID3 packet at `time` and returns none. Packed
audio: returns a tag to prepend, built as in
[encoder-packed-audio.md §7](encoder-packed-audio.md#7-insert_id3).
[encoder.md §8.5](../../../optionals/ffmpeg/spec/encoder.md#85-timed-id3-in-mpegts).

## 8. Stop

`stop()` drains the instance
([encoder.md §7](../../../optionals/ffmpeg/spec/encoder.md#7-stop)) and
returns the drained output as pieces: the candidates its drained packets
take are split points; bytes held after an undecided candidate are returned
as bytes. Its init is the container's, when no `Init` was returned before.

## 9. Conformance

[encoder.md §8.8](../../../optionals/ffmpeg/spec/encoder.md#88-conformance)
and [conformance.md §4](conformance.md#4-encoder-seam).
