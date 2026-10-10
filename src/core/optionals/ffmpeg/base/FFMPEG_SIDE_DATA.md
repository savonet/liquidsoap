# FFmpeg side data in Liquidsoap

What Liquidsoap does with FFmpeg side data, and what a script can see and
change. The binding interface it relies on is specified in
`src/modules/synced/ffmpeg/spec/side-data.md`.

The key words MUST, SHOULD and MAY are used as in RFC 2119.

## 1. Where side data lives

| Content       | Stream-wide side data               | Per-item side data |
| ------------- | ----------------------------------- | ------------------ |
| `ffmpeg.copy` | the codec parameters of the content | each packet        |
| `ffmpeg.raw`  | each frame, on kinds marked global  | each frame         |
| internal      | none                                | none               |

Internal content has no side data. Whatever must be acted on is acted on when
frames become internal content.

## 2. What must be acted on

Derived from the `ffmpeg` command-line tool, which is the reference for what
an application does with side data.

| Side data                                                                                                                                | The tool                                                | Liquidsoap                                    |
| ---------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------- | --------------------------------------------- |
| display matrix                                                                                                                           | rotates and flips decoded video, then removes the entry | §3                                            |
| frame cropping (container level)                                                                                                         | crops decoded video                                     | §3                                            |
| other global kinds (HDR mastering display, content light level, stereo 3D, spherical, ICC profile, audio service type, ReplayGain, EXIF) | carries them from input stream to output stream         | §4                                            |
| downmix information                                                                                                                      | reconfigures its resampler                              | nothing: libavfilter reads it from the frames |
| skip samples, palette, parameter change, new extradata                                                                                   | nothing: decoders and muxers consume them               | nothing                                       |

## 3. Decoding

- **D1. Autorotate.** A video decoder that produces internal or `ffmpeg.raw`
  content MUST apply the display matrix of each decoded frame and the
  cropping of its stream, so that the picture it outputs is upright, then
  remove the matrix from the frame. This is on by default and controlled by
  the setting `settings.ffmpeg.autorotate`.
- **D2.** The transformation is the binding's `Avfilter.Utils` display
  converter, which owns the table of filters. Liquidsoap holds no copy of it.
- **D3.** The width, height and pixel aspect of the content are those of the
  picture after the transformation. A video stored 1920×1080 with a 90° matrix
  is 1080×1920 content. They come from the binding's
  `Avfilter.Utils.display_layout` on the stream parameters, through
  `Ffmpeg_avfilter_utils.Display.layout`; Liquidsoap derives no geometry
  itself. Where the layout is undecided, `ffmpeg.raw` content declares no
  width, height or pixel aspect.
- **D4. Undecided.** A rotation that is not a quarter turn, a cropping that
  leaves no picture and a frame in a hardware pixel format have no single
  right answer. Liquidsoap MUST NOT decide: it asks the function installed
  with `ffmpeg.autorotate.on_undecided` (§5.2). With no function, or when it
  returns `null`, a warning is logged and the picture is left as stored,
  display matrix included.
- **D5.** The image decoder follows D1 to D4, which covers EXIF orientation:
  FFmpeg's decoders turn it into a display matrix.
- **D6.** D1 has one owner: `Ffmpeg_avfilter_utils.Display`. The file
  decoder, `input.ffmpeg`, the harbor input, the image decoder and
  `ffmpeg.decode.*` on `ffmpeg.copy` content all go through it.

With autorotate off, internal content shows the picture as stored, and
`ffmpeg.raw` frames keep their display matrix.

### 3.1 Changes in the middle of a stream

The format of decoded video can change while a stream is decoded. The format
of a frame is `Avutil.Video.frame_format` in the bindings: size, pixel format,
pixel aspect, colour space, range, primaries, transfer characteristic and
chroma location. That record is the one list of what counts, and everything
built for a format compares with `Ffmpeg_utils.same_video_format`, which
leaves out what `settings.ffmpeg.format_change` disables (§5.1). The size and
the pixel format always count.

- **C1.** A decoder to internal content, and `ffmpeg.decode.video`, follow
  the change: frames decoded before it are delivered, and scaling is rebuilt
  for the new format. The picture is scaled into the same internal frame, so
  nothing downstream depends on the change and the track continues.
- **C2.** `ffmpeg.decode.video` builds its decoder from the codec, the size
  and the pixel format of its input. A change of one of them on one stream is
  treated as C1: the decoder and its display stage are rebuilt. A change of
  side data alone rebuilds the display stage and keeps the decoder, which has
  no reference to restart from in the middle of a group of pictures. The
  kept decoder still puts the display matrix it was created with on its
  frames, so the matrix of the stream takes its place on each of them.
  `ffmpeg.raw.decode.video` sends its frames through a display stage of its
  own: a raw frame that still carries a display matrix comes out upright.
- **C3.** The format of an `ffmpeg.raw` video track (width, height, pixel
  format, pixel aspect) is open until a first frame flows through, which sets
  it. Every producer of raw video, a decoder or a filter graph output,
  delivers its frames through `Ffmpeg_raw_content.video_conformer`: a later
  frame outside the set format is fitted to it, proportions kept and the rest
  padded. The track continues.
- **C4.** No decoder adds a track mark at a change. A track mark makes
  operators switch, fade or stop, and cuts the audio of the same source.
- **C5.** An input of an `ffmpeg.filter` graph is built for the format of
  the first frame it takes. A frame in another format ends the generation of
  the graph: every input signals the end of its stream, the outputs are
  drained, and the graph is built again from that frame. Frames arriving
  while the next generation waits for its other inputs are held and pushed
  once it is built. The graph logs a warning at level 2 naming the input and
  both formats: its filters lose their state. This holds for audio inputs,
  whose format is the sample format, the rate and the channel layout.
- **C6.** The raw video encoder fits the frames it takes to the size of its
  stream, and rebuilds that conversion when their format changes.
- **C8.** Decoders to internal content share
  `Ffmpeg_decoder_common.internal_video_converter`, which delivers the frames
  its frame rate conversion holds before building it again for a new format.
- **C7.** Nothing stretches a picture. Every conversion to a frame of another
  shape takes its size from `Ffmpeg_avfilter_utils.Fit.fitted_size`: the
  largest size that keeps the proportions of the picture as displayed, the
  pixel aspect of the source and of the destination accounted for. The
  picture is centred and the rest is black. Decoders to internal content,
  `ffmpeg.decode.video` and `ffmpeg.raw.decode.video` included, share
  `Ffmpeg_decoder_common.internal_scaler` for it.

## 4. Passing through

- **P1. Copy.** `ffmpeg.copy` content MUST reach the output with the stream
  side data of its codec parameters and the side data of each packet. A
  rotated video relayed in copy mode stays rotated for the player.
- **P2. Raw video encoding.** When a video encoder is created from the first
  `ffmpeg.raw` frame, the frame's side data of global kinds MUST be given to
  the encoder, so that it reaches the output stream. With autorotate on the
  display matrix is already gone (D1); with it off it is carried.
- **P3.** Global side data that changes after the encoder was created is not
  applied.
- **P4. Filters.** Frames enter `ffmpeg.filter.*` graphs with their side data
  and leave with whatever the filters propagate. Liquidsoap adds and removes
  nothing.
- **P5. Internal encoding.** Frames built from internal content have no side
  data.

Raw audio encoders are not given side data.

## 5. Script interface

### 5.1 Setting

| Setting                                      | Default | Effect                                                     |
| -------------------------------------------- | ------- | ---------------------------------------------------------- |
| `settings.ffmpeg.autorotate`                 | `true`  | D1                                                         |
| `settings.ffmpeg.format_change.color`        | `true`  | a change of colour properties is a change of format (§3.1) |
| `settings.ffmpeg.format_change.pixel_aspect` | `true`  | a change of pixel aspect is a change of format (§3.1)      |

### 5.2 Deciding the undecided

```liquidsoap
ffmpeg.autorotate.on_undecided(
  fun (question) ->
    if question.reason == "odd_rotation" then
      [("rotate", [("angle", "#{question.rotation ?? 0.}*PI/180")])]
    else
      null
    end
)
```

- `question.reason` is `"odd_rotation"`, with `rotation` the clockwise angle
  in degrees; `"invalid_cropping"`, with `cropping` the four borders; or
  `"hardware_frame"`.
- The function returns the FFmpeg filters to apply, as
  `(name, [(option, value)])`, or `null` for no decision (D4).
- It is asked once per decoder for a run of frames with the same question,
  and applies to the decoders created after it is installed.

### 5.3 Track operators

`track.ffmpeg.side_data` takes an `ffmpeg.copy` track, `track.ffmpeg.raw.side_data`
a raw video track. Both return the track.

```liquidsoap
video = track.ffmpeg.side_data(video)
video.side_data()  # [{kind="Display Matrix", data="…"}, …]
video.rotation()   # float?, counter-clockwise degrees

video = track.ffmpeg.side_data(rotation=180., video)
video = track.ffmpeg.side_data(remove=["Display Matrix"], video)
```

- `rotation` is a getter of `float?`. A non-null value sets the display
  matrix of the stream: on the codec parameters of a video copy track, on
  every frame for raw. `null` leaves it as it is. It does nothing on audio
  and subtitle tracks.
- `remove` lists kinds dropped from the stream.
- `side_data()` returns the stream-wide side data of the last content that
  went through, after the changes above: the codec parameters' for copy, the
  global kinds of the last frame for raw. `data` is the payload, opaque.
- `rotation()` is the typed view of the display matrix, `null` when there is
  none.
- Kinds are named as FFmpeg names them, which is what `ffprobe` prints. The
  two families have their own names: the display matrix is `Display Matrix`
  on a copy track and `3x3 displaymatrix` on a raw one.
- A script cannot build an entry from bytes: the binding has no such
  constructor, for memory safety.

Forcing a rotation before decoding, as the tool's `-display_rotation` does, is
`track.ffmpeg.side_data(rotation=…)` on the copy track followed by
`ffmpeg.decode.video`.

## 6. Tests

`tests/media/test_ffmpeg_side_data.liq`, on a 320×240 file stored with a
rotation of 90:

- relayed in copy mode, the output stream holds the same rotation, and
  `rotation()` reads it;
- a rotation set, and the matrix removed, on the copy track are found in the
  output;
- decoded to internal content and encoded, the output is 240×320 with no
  rotation;
- a rotation set on the raw track reaches the output stream through the
  encoder;
- a file stored with a rotation of 33 calls the function of
  `ffmpeg.autorotate.on_undecided` with `"odd_rotation"`.

`tests/media/test_ffmpeg_video_format_change.liq`: a stream whose size
changes after one second plays through as one track (C1).

Not covered: autorotate off, cropping, a JPEG with an EXIF orientation, C2
and C3.

`tests/media/test_ffmpeg_raw_format_change.liq`, on two files with continuous
timestamps, one changing size and one changing colour space after a second:
decoded to raw content, sent through an `ffmpeg.filter` graph and encoded
from raw frames, both halves are in the one output stream (C3, C5, C6).
