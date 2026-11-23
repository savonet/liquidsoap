# Ogg Theora stream encoder

Answers [ogg-streams.md](ogg-streams.md) for Theora (libtheora through the
vendored binding). Container behaviour is [ogg.md](ogg.md). The only Ogg
video codec. Its only test is skipped as "deprecated".

## 1. Format and options

`%theora(...)`, also `%ogg(%theora(...))` and `%ogg(<audio>, %theora(...))`.
Parameters are checked in the order given, against the values set so far, so
a `picture_*` check before a `width`/`height` argument uses the defaults.

| parameter                                | default                  | validation and error                                                                                                                                                               |
| ---------------------------------------- | ------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `quality`                                | 40                       | 0..63, `Theora quality should be in 0..63`                                                                                                                                         |
| `bitrate`                                | unset                    | none; replaces quality (quality then 0)                                                                                                                                            |
| `width`, `height`                        | frame video width/height | multiple of 16 and < 1048576, `invalid frame width value (should be a multiple of 16)` (resp. height); also sets `picture_width`/`picture_height`                                  |
| `picture_width`                          | width                    | ≤ width, `picture width must not be larger than width`                                                                                                                             |
| `picture_height`                         | height                   | ≤ height, `picture height must not be larger than height`                                                                                                                          |
| `picture_x`                              | 0                        | ≤ min(width − picture_width, 255), `picture x must not be larger than width - picture width or 255, whichever is smaller`                                                          |
| `picture_y`                              | 0                        | ≤ height − picture_height, `picture y must not be larger than height - picture height`; picture_height − picture_y ≤ 255, `picture height - picture y must not be larger than 255` |
| `aspect_numerator`, `aspect_denominator` | 1, 1                     | none                                                                                                                                                                               |
| `keyframe_frequency`                     | 64                       | none                                                                                                                                                                               |
| `vp3_compatible`                         | unset                    |                                                                                                                                                                                    |
| `soft_target`                            | false                    |                                                                                                                                                                                    |
| `buffer_delay`, `speed`                  | unset                    | none                                                                                                                                                                               |
| `bytes_per_page`                         | none                     | no effect                                                                                                                                                                          |

The defaults for the sizes are the frame's video dimensions; they are not
validated at parse time (a frame width that is not a multiple of 16 fails at
the first link, §12). The format's video size (for outputs) is
(width, height). No frame-rate parameter (code comment `TODO: framerate !`):
the frame's video rate is used.

Printed form (checked):
`%theora(quality=40,width=1280,height=720,picture_width=1280,picture_height=720,picture_x=0,picture_y=0,aspect_numerator=1,aspect_denominator=1,keyframe_frequency=64,,soft_target=false,,)`:
unset optional fields leave empty commas, so it does not parse back; and
`%ogg` drops the video codec from its printed form when there is no audio
([ogg.md §1](ogg.md#printed-form)).

## 2. Instance creation

Per instance: nothing. Per link: libtheora info (frame and picture sizes and
offsets, frame rate = frame video rate / 1, aspect, colorspace unspecified,
keyframe granule shift = libtheora default (6), target bitrate (the bitrate or
0), quality (the quality or 0), version from the library, pixel format 4:2:0),
then the encoder with the rate-control parameters (keyframe frequency, VP3
compatibility, soft target, buffer delay, speed level) and the comments. A
width or height not a multiple of 16 fails here.

## 3. Identification header (BOS page)

All three header packets are put in the stream at registration; flushing puts
the first, the 42-byte identification packet (`\x80theora`, version 3.2.1,
macroblock counts, picture size, frame rate, aspect…), alone on the BOS page
(70 bytes at 320×240).

## 4. Skeleton fisbone

3 header packets, granule rate = frame rate numerator / denominator (25/1),
preroll 0, granule shift = the info's (6), `Content-type: video/theora\r\n`.

## 5. Secondary headers

The comment and setup packets already in the stream, flushed: one page
(3368 bytes at 320×240 with no metadata: 131 + 3196).

## 6. Comments

Vendor: libtheora's (`Xiph.Org libtheora 1.2.0 20250329 (Ptalarbvorm)` with
the installed library). Comments: `ENCODER=ocaml-theora by the savonet team
(http://savonet.sf.net/)` first (added by the binding), then every metadata
pair as `key=value`, in list order.

## 7. Data input and conversion

Each image of the block is passed as a Y'CbCr 4:2:0 buffer whose plane sizes
are the **format's** width and height (chroma halved) and whose strides are
the **image's**. Images are rendered at the frame's video size, so a format
size different from the frame size gives the encoder planes of the wrong
size: the binding raises when a plane is smaller than stride × height (§12).
After each image every packet libtheora has ready is put in the stream.

## 8. Packets, granule positions and pages

libtheora's granulepos: (index of the last keyframe) << 6 | (frames since
it). Pages by libogg's default threshold. A still image encodes to a 13-byte
keyframe packet then empty packets for repeated frames, so a data page closes
only at 255 lacing values, every 255 frames (≈10.2 s at 25 fps).

## 9. Page end time

(frame index of the granulepos + 1) / frame rate; a granulepos of 0 gives
time 0 (header pages); negative, unknown.

## 10. End of stream

1. If no image was given in the link, one image is encoded whose planes are
   allocated with `width` bytes (Y) and `width / 2` bytes (chroma), declared
   as `width × height` (and half) with strides `width` (and half). The binding
   checks plane sizes against stride × height and raises (§12): a Theora link
   with no image cannot be ended.
2. The stream is terminated by the Ogg binding's generic terminator
   ([ogg-opus.md §10](ogg-opus.md#10-end-of-stream)): pending pages flushed,
   then a 27-byte EOS page without segments whose granulepos is the last
   packet's raw value + 1 (example: 101 → 102). With a keyframe interval of
   64 and the last frame 63 frames after a keyframe, + 1 carries into the
   keyframe field. The binding's own Theora end-of-stream call is not used.

Nothing is lost from what was given.

## 11. Page sizing

libogg default threshold; `bytes_per_page` ignored.

## 12. Failures

- `Invalid theora width/height (should be a multiple of 16).` (a failure
  exception) at link start, from `encode` or `encode_metadata`;
- binding "invalid data" when an image plane is smaller than stride × height
  (format size larger than the frame size), from `encode`; and always from
  the empty-link fallback, from `encode_metadata` or `stop`;
- libtheora errors: binding exceptions.

## 13. Concurrency

`th_encode_ycbcr_in` runs with the OCaml runtime lock released. No global
state.

## 14. Tests

`tests/regression/GH4116.liq` (skipped). Nothing else.
