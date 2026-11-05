# The `%ffmpeg` encoder under HLS

Part A. As built. `%ffmpeg` implements the seam of
[encoder-contract.md](encoder-contract.md) itself. Its behaviour is
specified in the FFmpeg layer's spec; this file maps each contract section
to it and records what the HLS output depends on. Sections follow the
contract's numbering.

## 1. Role

Split points are positions where every participating stream's next packet
is a key packet; encoded video makes a key frame at each boundary it is
given. [encoder.md §8.2](../../../optionals/ffmpeg/spec/encoder.md#82-keyframe-hooks).

## 2. Calls and order

With the HLS flag the instance runs in its HLS mode
([encoder.md §8](../../../optionals/ffmpeg/spec/encoder.md#8-hls)); without
it, it would use the frame-unit adapter, which the HLS output never asks
for. `header()` returns nothing in HLS mode: the container header is the
init segment.

## 3. `init`

By muxer: MPEG-TS adds a timed-ID3 stream unless `id3 = false`; ADTS, MP3,
AC-3 and E-AC-3 require ID3 and prepend tags; any other muxer rejects
`id3 = true`. [encoder.md §8.1](../../../optionals/ffmpeg/spec/encoder.md#81-init-id3_enabled-id3_version-).

## 4. `init_encode`

`Pending` until the container header is known; then the header as the init
segment (fMP4: `ftyp` + `moov`; none for MPEG-TS and packed audio).
[encoder.md §8.3](../../../optionals/ffmpeg/spec/encoder.md#83-init_encode-frame).

## 5. `split_encode`

Split points at key packets and at each boundary; forced splits honoured
except when a key frame at the boundary is already on its way; restarts at
incompatible copy switches.
[encoder.md §8.4](../../../optionals/ffmpeg/spec/encoder.md#84-split_encode-frame),
[§13](../../../optionals/ffmpeg/spec/encoder.md#13-container-restart).

Split times come from packet times. A copied stream's first key packet can
sit after the variant's start by the source's codec delay, so its split
points can all fall just after the boundaries
([segmenting.md §5.2](segmenting.md#52-worked-example)).

## 6. Properties

[encoder.md §8.6](../../../optionals/ffmpeg/spec/encoder.md#86-properties):
`codec_attrs` from the streams' codec strings (none if any is missing);
`independent_segments` true unless a copied stream's codec is not
intra-only; `bitrate` the sum of the streams'; `video_size` when exactly one
stream has one.

## 7. `insert_id3`

MPEG-TS: writes a timed-ID3 packet into the container and returns none.
Packed audio: returns a tag to prepend, built as in
[encoder-packed-audio.md §7](encoder-packed-audio.md#7-insert_id3).
[encoder.md §8.5](../../../optionals/ffmpeg/spec/encoder.md#85-timed-id3-in-mpegts).

## 8. Header and stop

`header()` is empty under HLS. `stop()` drains the instance; its bytes go
to the open segment without split points
([encoder.md §7](../../../optionals/ffmpeg/spec/encoder.md#7-stop)).

## 9. Conformance

[encoder.md §8.8](../../../optionals/ffmpeg/spec/encoder.md#88-conformance)
and [conformance.md §4](conformance.md#4-encoder-seam).
