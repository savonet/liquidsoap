# `%avi` encoder (as built)

What the native AVI encoder does today: an uncompressed RIFF/AVI stream with
one raw I420 video stream and one 16-bit PCM audio stream, written as an
endless, unindexed, interleaved `movi` list. The shared seam is
[encoder.md](encoder.md). The same header and chunk writer is used by
[`%external`](external.md) when `video=true`; the bytes are owned here.

## 1. Format and options

Syntax: `%avi(...)`. Parameters, last one wins:

| Parameter      | Value      | Default                                                |
| -------------- | ---------- | ------------------------------------------------------ |
| `channels=n`   | static int | 2 (hard-coded, not `frame.audio.channels`, issue #933) |
| `samplerate=n` | int        | `frame.audio.samplerate`, read at instance creation    |
| `width=n`      | int        | `frame.video.width`, read at instance creation         |
| `height=n`     | int        | `frame.video.height`, read at instance creation        |

There is no `mono`/`stereo` at the value level: `%avi(mono)` passes the
script-level type computation (which accepts `mono`/`stereo` for the
channel count) and then fails at evaluation with "Unknown encoder parameter:
mono". Any other labelled parameter: "unknown parameter name (x) or invalid
parameter value (v)". No range check on any value (zero, negative or odd
sizes are accepted). Errors are kind `encoder`, raised at evaluation.

Type: one `audio` track of PCM with _channels_ channels and one `video`
track in the default video format (raw YUV 4:2:0 canvas), nothing else.

Printed as `%avi(samplerate=44100,channels=2,width=1280,height=720)`.
Extension `avi`, MIME `video/avi`, no format-level bitrate estimate; Icecast
info reports samplerate and channels.

There is no frame-rate option: the video rate is always
`frame.video.framerate`.

## 2. Factory and instance creation

The factory ignores the output name and the initial metadata. Creating an
instance forces samplerate, width and height, creates a stateful sample-rate
converter (ratio `samplerate / frame.audio.samplerate`), logs at `info`
"Encoding at WxH, C channels, R Hz." (logger `avi.encoder`), computes the
header (§6) and sets "header pending". Nothing fails.

## 3. `encode`

Input: one frame; its audio and video tracks; metadata and track marks are
ignored.

1. **Audio**: the frame's audio (from 0, for the frame's audio length) is
   resampled, converted to interleaved signed 16-bit little endian (clipped)
   and wrapped in one chunk `01wb`.
2. **Video**: every image the frame's video track holds (0, 1 or more,
   depending on frame duration vs. video rate) is, in order:
   resized to width×height keeping proportions (a fresh scaler per call),
   rendered onto an opaque canvas, then its Y plane, then U, then V are
   appended, each row trimmed to the visible width (stride removed), chroma
   planes being `width/2 × height/2` (integer division). If the rendered
   image is not exactly width×height the call fails with "Resizing is not
   yet supported by AVI encoder got WxH instead of W'xH'"; with
   proportional resizing onto a canvas of the target size this does not
   happen in practice. **All images of the frame are concatenated into one
   single `00db` chunk.**
3. Output: `00db` chunk, then `01wb` chunk; the header in front if pending
   (then cleared).

A chunk is `id (4 bytes) | length (4 bytes LE, bit 31 cleared) | payload |
one 0x00 pad byte if length is odd`.

Consequences, observed on the real binary:

- A frame with no image (default `frame.duration` 0.02 s with 25 fps: one
  frame out of two) gives an empty `00db` chunk (8 bytes, length 0).
- A frame with two images (`frame.duration` 0.08 s) gives one `00db`
  chunk of twice the image size; ffprobe then counts one video frame per
  chunk (5 frames read for 10 images).
- A zero-length frame gives `00db 00000000 01wb 00000000` (16 bytes), plus
  the header if pending.
- With an odd width or height the payload per image is
  `w·h + 2·⌊w/2⌋·⌊h/2⌋`, which is smaller than the size declared in the
  header (`w·h·6/4`): e.g. 33×17 gives 817 bytes per image, the header says 841.

## 4. `encode_into`

Nothing.

## 5. `encode_metadata`

Nothing; metadata is dropped. The header carries a fixed `INFO/ISFT`
(software) entry only.

## 6. `header`

Computed once per instance. Layout (all integers little endian; `dword`
writes 4 bytes with bit 31 cleared, `word` 2 bytes; `MAX` is the literal
`FF FF FF FF`, built from two words because 0xFFFFFFFF is not representable
on 32-bit OCaml):

```
"RIFF" MAX "AVI "
LIST hdrl (length 294)
  avih (56): µs per frame = 1000000 / video_rate (integer division)
             max bytes/s 0, padding 0, flags 0x100 (interleaved),
             total frames MAX, initial frames 0, streams 2,
             suggested buffer 0, width, height, 4 × reserved 0
  LIST strl (116)
    strh (56): "vids" "I420" flags 0, priority 0, language 0,
               initial frames 0, scale 1, rate video_rate, start 0,
               length MAX, buffer 0, quality MAX, sample size 0,
               rect (0, 0, width, height) as 4 words
    strf (40): BITMAPINFOHEADER: size 40, width, height (positive),
               planes 1, bit count 12, compression "I420",
               image size width*height*6/4, 0, 0, 0, 0
  LIST strl (94)
    strh (56): "auds" handler 0, flags 0, priority 0, language 0,
               initial frames 0, scale 1, rate samplerate, start 0,
               length MAX, buffer 0, quality MAX, sample size 2*channels,
               rect 0,0,0,0
    strf (18): WAVEFORMATEX: format 1 (PCM), channels, samplerate,
               byte rate 2*channels*samplerate, block align 2*channels,
               16 bits, extra size 0
LIST INFO
  ISFT <vendor string> (padded to even length)
"LIST" MAX "movi"
```

The vendor string is the build's (`Liquidsoap/2.5.0+git@02fa52985 (Unix;
OCaml 5.5.2)`, 50 bytes), so the header length varies between builds: 396
bytes for this one.

No `idx1` index, no `dmlh`/OpenDML extension, no `JUNK` alignment.

`header()` returns the empty string before the first `encode` (which
carries the header) and the same header after it. Unlike `%wav` there is no
option to omit it.

## 7. `restart_offset`

Always "none".

## 8. `bitrate`

Always "none".

## 9. `stop`

Returns no bytes. No index is written, no sizes are fixed; samples held by
the rate converter are dropped.

## 10. Bytes produced

`header` then, per `encode`, `00db` chunk then `01wb` chunk. Nothing at the
end.

Worked example (real binary, `noise()`, 64×48, 25 fps, stereo 44.1 kHz,
`frame.duration` 0.02 s, 0.32 s run, 93 964 bytes):

```
00000000: 5249 4646 ffff ffff 4156 4920 4c49 5354  RIFF....AVI LIST
00000010: 2601 0000 6864 726c 6176 6968 3800 0000  &...hdrlavih8...
00000020: 409c 0000 0000 0000 0000 0000 0001 0000  @...............
00000030: ffff ffff 0000 0000 0200 0000 0000 0000  ................
00000040: 4000 0000 3000 0000 0000 0000 0000 0000  @...0...........
00000050: 0000 0000 0000 0000 4c49 5354 7400 0000  ........LISTt...
00000060: 7374 726c 7374 7268 3800 0000 7669 6473  strlstrh8...vids
00000070: 4934 3230 0000 0000 0000 0000 0000 0000  I420............
00000080: 0100 0000 1900 0000 0000 0000 ffff ffff  ................
00000090: 0000 0000 ffff ffff 0000 0000 0000 0000  ................
000000a0: 4000 3000 7374 7266 2800 0000 2800 0000  @.0.strf(...(...
000000b0: 4000 0000 3000 0000 0100 0c00 4934 3230  @...0.......I420
000000c0: 0012 0000 0000 0000 0000 0000 0000 0000  ................
000000d0: 0000 0000 4c49 5354 5e00 0000 7374 726c  ....LIST^...strl
000000e0: 7374 7268 3800 0000 6175 6473 0000 0000  strh8...auds....
000000f0: 0000 0000 0000 0000 0000 0000 0100 0000  ................
00000100: 44ac 0000 0000 0000 ffff ffff 0000 0000  D...............
00000110: ffff ffff 0400 0000 0000 0000 0000 0000  ................
00000120: 7374 7266 1200 0000 0100 0200 44ac 0000  strf........D...
00000130: 10b1 0200 0400 1000 0000 4c49 5354 3e00  ..........LIST>.
00000140: 0000 494e 464f 4953 4654 3200 0000 4c69  ..INFOISFT2...Li
00000150: 7175 6964 736f 6170 2f32 2e35 2e30 2b67  quidsoap/2.5.0+g
00000160: 6974 4030 3266 6135 3239 3835 2028 556e  it@02fa52985 (Un
00000170: 6978 3b20 4f43 616d 6c20 352e 352e 3229  ix; OCaml 5.5.2)
00000180: 4c49 5354 ffff ffff 6d6f 7669 3030 6462  LIST....movi00db
```

Chunks in `movi`: `00db 4608, 01wb 3528, 00db 0, 01wb 3528, …` (16 of
each). 4608 = 64·48·1.5; 3528 = 882 samples × 4 bytes.

ffprobe reads `rawvideo 64x48 25/1` and `pcm_s16le 44100 2ch`; it reports
`nb_frames=4294967295` and a duration of thousands of seconds, from the
`MAX` fields.

## 11. Concurrency and failures

No thread or lock of its own beyond the seam's per-instance lock. The only
run-time failure is the resize mismatch in §3 (an exception out of
`encode`, which the caller handles).

## 12. Segmenting (HLS) face

Nothing (see [wav.md §12](wav.md#12-segmenting-hls-face)).

## 13. Where the bytes go

- **Cannot seek back, so lies about every length.** RIFF size, `movi`
  size, total frames and stream lengths are `FF FF FF FF`; there is no
  index. Readers rely on scanning chunks sequentially. Nothing is fixed at
  `stop`.
- **One stream from the first encode.** Header once per instance, in the
  first `encode`'s bytes; `header()` gives late joiners the same header,
  and every `encode` returns whole chunks, so a joiner that starts on an
  `encode` boundary can parse. Whether each file gets a header is the
  caller's (instance per file).
- **One chunk per call, not per picture.** The chunk boundary is the
  caller's frame boundary: what a reader sees as "a video frame" depends
  on `frame.duration`, not on the video rate.
- Liquidsoap's own AVI reader (`input.external.avi`) rejects this output:
  it requires the audio `strh` handler to be 1 ("Wrong auds fourcc.") and
  expects video chunks named `00dc`.
- The encoder never does I/O.

## 14. Tests

- **Requested dimensions reach the file.** (`tests/media/video_size.liq`:
  `%avi(width=47, height=52)` probes as 47×52.) Binding: the declared
  width/height equal the requested ones, including odd values. Incidental:
  ffprobe, 45 fps, the input file. It does not check the payload size per
  image (which disagrees with the header for odd sizes, §3).
- **Untested:** header bytes, chunk order, empty/multi-image frames,
  readability by `input.external.avi`, audio sample rate conversion. A new
  harness needs a frame double with a controllable number of images per
  frame and a chunk walker.
