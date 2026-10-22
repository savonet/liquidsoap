# FFmpeg image decoder

This file owns the FFmpeg entry of the image file decoders: turning a still
image file into one picture. The generic `image` media decoder and other
consumers of image files (for instance video overlays of an image) call the
image decoders in priority order. It relies on the bindings' `open_input`,
`find_best_video_stream` and `read_input`
([avformat.md](../../../../modules/synced/ffmpeg/spec/avformat.md)) and on the
display stage of [side-data.md §3](side-data.md#3-autorotate-on-decoding).
How images reach the media decoder instead is in
[decoder.md §3.2](decoder.md#32-images-through-the-media-decoder).

## 1. Registration and priority

| Registry            | Name     | Priority setting                           | Default |
| ------------------- | -------- | ------------------------------------------ | ------- |
| image file decoders | `ffmpeg` | `settings.decoder.priorities.image.ffmpeg` | `10`    |

Image decoders are sorted by decreasing priority. With defaults the list is
`ffmpeg (priority: 10), sdl (priority: 5), ppm (priority: 1)`.

## 2. Acceptance check

`check_image file` is true when both hold:

1. The file's extension, lowercased as in
   [decoder.md §3.1](decoder.md#31-files), is in
   `settings.decoder.file_extensions.ffmpeg.images`
   ([decoder.md §3.4](decoder.md#34-default-lists)).
2. The file opens with default options, has a best video stream, and one
   video frame can be read from it. A failure logs at info level that the
   file cannot be decoded, with the error, and gives false. The container is
   closed in every case.

The generic check tries the decoders in priority order and logs at important
level the first decoder that accepts the file.

Conformance:

- `IMG.PNG` and `img.png` are both accepted by `ffmpeg`.

## 3. Decoding

`decode_image file`:

1. Opens the file with default options and takes the best video stream.
2. Builds a display stage from the stream's codec parameters and time base
   ([side-data.md §3](side-data.md#3-autorotate-on-decoding)).
3. Reads one frame from that stream in frame mode.
4. Passes the frame through the display stage, then drains the stage
   ([draining.md](draining.md)); the first frame the stage delivers is kept.
5. The kept frame is converted, at its own width and height, from its pixel
   format to YUVA 4:2:0, with the configured scaling threads.
6. Closes the container, in every case, and returns the picture.

The picture keeps the decoded size (after rotation and cropping by the
display stage); scaling to a frame size is the caller's. It always has an
alpha plane.

## 4. Errors

| Event                                                    | Class        | Effect                                  |
| -------------------------------------------------------- | ------------ | --------------------------------------- |
| no video stream, open or read failure                    | source fault | error logged with the cause; no picture |
| the display stage delivers no frame, or no frame is read | source fault | error logged; no picture                |

Classes are those of [faults.md §2](faults.md#2-classes). `decode_image`
never raises: it returns a picture or none. The generic image decoding tries
each image decoder in priority order and uses the first that returns a
picture. When none does it logs at important level that no image decoder
could decode the file, and reports no picture; the source that asked for the
image then follows [faults.md §3](faults.md#3-source-faults-reach-the-script).
