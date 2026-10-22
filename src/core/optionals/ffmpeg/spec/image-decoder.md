# FFmpeg image decoder

This file owns the FFmpeg entry of the image file decoders: turning a still
image file into one picture. The generic `image` media decoder and other
consumers of image files (for instance video overlays of an image) call the
image decoders in priority order. It relies on the bindings' `open_input`,
`find_best_video_stream` and `read_input`
([avformat.md](../../../../modules/synced/ffmpeg/spec/avformat.md)) and on the
display stage of [side-data.md §3](side-data.md#3-autorotate-on-decoding). How images reach the media
decoder instead is in [decoder.md §3.2](decoder.md#32-images-through-the-media-decoder).

## 1. Registration and priority

| Registry            | Name     | Priority setting                           | Default |
| ------------------- | -------- | ------------------------------------------ | ------- |
| image file decoders | `ffmpeg` | `settings.decoder.priorities.image.ffmpeg` | `10`    |

Image decoders are sorted by decreasing priority. With defaults the list is
`ffmpeg (priority: 10), sdl (priority: 5), ppm (priority: 1)` (from the
implementation).

## 2. Acceptance check

`check_image file` is true when both hold:

1. The file's extension, as returned by the platform extension function (dot
   included, case kept), is `.` followed by an entry of
   `settings.decoder.file_extensions.ffmpeg.images`
   ([decoder.md §3.4](decoder.md#34-default-lists)). `IMG.PNG` is rejected; `img.png` is
   accepted.
2. The file opens with default options, has a best video stream, and one
   video frame can be read from it. Any failure logs at level 4 `Failed to
decode "<file>": <exn>` and gives false. The container is closed in every
   case.

The generic check tries the decoders in priority order and logs at level 3
`Image decoder "<name>" accepted "<file>"` for the first that accepts.
Example (from the implementation): `IMG.PNG` is accepted by `sdl`, `img.png`
by `ffmpeg`.

## 3. Decoding

`decode_image file`:

1. Opens the file with default options and takes the best video stream.
2. Builds a display stage from the stream's codec parameters and time base
   ([side-data.md §3](side-data.md#3-autorotate-on-decoding)).
3. Reads one item from that stream in frame mode.
4. Passes the frame through the display stage, then drains the stage; the
   first frame it delivers is kept.
5. The kept frame is converted, at its own width and height, from its pixel
   format to YUVA 4:2:0, with the configured scaling threads.
6. Closes the container and returns the picture.

The picture keeps the decoded size (after rotation and cropping by the
display stage); scaling to a frame size is the caller's. It always has an
alpha plane.

## 4. Errors

| Event                                                                   | Effect                   |
| ----------------------------------------------------------------------- | ------------------------ |
| no video stream, open or read failure                                   | the exception propagates |
| the display stage delivers no frame, or the read returns no video frame | not-found                |

The generic image decoding tries each image decoder in priority order,
without calling `check_image`, and uses the first that returns a picture.
When none does it logs at level 3 `Unable to decode "<file>" using image
decoder(s)!` and raises not-found.
