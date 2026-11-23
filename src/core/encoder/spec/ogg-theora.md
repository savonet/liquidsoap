# Ogg Theora stream encoder

Part A. Answers [ogg-streams.md](ogg-streams.md) for Theora, under the same
numbers. Container rules are [ogg.md](ogg.md). Claims about the Theora
specification marked "(from memory)" are stated from memory.

## 1. Format and options

`%theora(...)`, also `%ogg(%theora(...))` and `%ogg(<audio>, %theora(...))`.

`%theora` is deprecated ([formats.md §2](formats.md#2-names-and-kinds)):
it is accepted, encodes as below, and logs one warning naming `%ffmpeg`
with a current video codec as its replacement. Reason: libtheora is no
longer developed, and players decode current codecs better.

Being deprecated, it gets minimal image handling: a size it cannot encode
is refused, never adapted (no rounding of the coded size, no scaling, no
placement of a smaller image in a larger coded frame).

| Parameter                                | Values                                                         | Default                                                          |
| ---------------------------------------- | -------------------------------------------------------------- | ---------------------------------------------------------------- |
| `quality`                                | int 0..63                                                      | 40                                                               |
| `bitrate`                                | int, bits per second, positive                                 | unset; when set, rate control targets it and `quality` is unused |
| `width`, `height`                        | multiple of 16, below 1048576, equal to the frame's video size | the frame's video size (deferred)                                |
| `picture_width`, `picture_height`        | at most `width`, `height`                                      | `width`, `height`                                                |
| `picture_x`, `picture_y`                 | as the Theora header allows (below)                            | 0                                                                |
| `aspect_numerator`, `aspect_denominator` | positive ints                                                  | 1, 1                                                             |
| `keyframe_frequency`                     | int, positive                                                  | 64                                                               |
| `vp3_compatible`, `soft_target`          | bool                                                           | the library's                                                    |
| `buffer_delay`, `speed`                  | int                                                            | the library's                                                    |
| `bytes_per_page`                         | [ogg-streams.md §1](ogg-streams.md#1-format-and-options)       | 4096                                                             |

- `picture_x` ≤ min(`width` − `picture_width`, 255); `picture_y` ≤
  `height` − `picture_height` and `picture_height` − `picture_y` ≤ 255
  (Theora header limits, from memory).
- All checks run on the final values, whatever the order of the parameters,
  at the phase of their values; deferred defaults are checked at start-up.
- The coded size is the frame's video size. A `width` or `height`, explicit
  or default, that is not a multiple of 16 or differs from the frame's video
  size is refused, at the phase of its value (start-up for the deferred
  default), naming the parameter, the frame's video size and the format's
  position.
- The frame rate is the frame's video rate (deferred).
- Printed form: every set option, unset ones omitted; printed inside
  `%ogg(...)` it keeps the video stream
  ([ogg.md §1](ogg.md#1-format-and-options)).

Migration: a frame size that is not a multiple of 16 (1920×1080), which
failed at the output's start, is refused at start-up with a positioned
error; set a frame size that is a multiple of 16, or use `%ffmpeg`.

## 2. Configuration and library checks

Sizes, picture region and rates are checked statically or at start-up.
Whether libtheora accepts the rate-control settings is known at creation.

## 3. Creation

Per link: a libtheora encoder for the coded size, picture region, frame rate,
aspect ratio, 4:2:0 pixel format, quality or target bit rate, then the
rate-control settings, with a key-frame granule shift `s` such that
2^`s` ≥ `keyframe_frequency`, and the link's comments.

## 4. Header packets

The three Theora headers libtheora produces at creation: identification,
comment, setup.

## 5. Comments

As [ogg-streams.md §5](ogg-streams.md#5-comments), vendor string
libtheora's.

## 6. Input

- Images have the frame's video size, which is the coded size (§1). They are
  copied by rows whatever their stride and converted to Y'CbCr 4:2:0; they
  are never scaled or placed. The planes handed to libtheora always have the
  coded frame's size. An image of another size is a defect of the caller
  (internal error), never adapted.
- One image per image period. A share with no image is not handed to the
  codec.

## 7. Packets

- One packet per image, in order; libtheora's zero-length packets for a
  repeated image are data packets (a repeated frame, from memory) and are
  kept.
- Granule position: libtheora's, (key-frame number << `s`) | (images since
  it), per the Theora specification's counting (from memory).
- Times: start time = image index / frame rate, end time = (index + 1) /
  frame rate, from the link's start.
- A key-frame packet must begin a page (§11).

## 8. Delay and padding

None.

## 9. End

- The stream encoder ends with the last image's packet, which carries the
  final granule position.
- A link in which the stream received no image encodes one image first: the
  latest image the writer received before the link, or a black image of the
  coded size when there is none. Reason: a logical stream ends on the page
  of its last packet, and a link can be shorter than one image period; the
  repeated image is what a viewer already sees.

## 10. Properties

`bitrate`: `bitrate` from creation when set; else none. `delay`, `padding`:
none.

## 11. Page constraints

Every key-frame packet begins a new page: the muxer closes the stream's page
before it ([ogg.md §5.3](ogg.md#53-page-sizing)). This places the link's
access points ([ogg.md §5.6](ogg.md#56-markers)).

## 12. Concurrency

libtheora keeps no state shared between encoders.

## 13. Faults

- Rate-control settings refused at creation: configuration error.
- An image libtheora fails to encode: item fault.

## 14. Tests

- Images with padded strides: the decoded image equals the input; header
  sizes equal the payload.
- A frame size of 1920×1080 with defaults, and an explicit `width=1280`
  under a 1920×1088 frame: refused at start-up, naming the parameter and
  the frame's video size; no output starts.
- A frame size of 1920×1088 with defaults: decodable, picture 1920×1088.
- `%theora` logs its deprecation warning once per format evaluated.
- A link with no image (two metadata changes less than one image period
  apart, then a stop): every link ends validly, no call raises.
- With a still image: video pages close at least every `max_page_duration`;
  the audio is never held longer.
- Every key frame begins a page; a reader starting at an access point first
  decodes a key frame.
