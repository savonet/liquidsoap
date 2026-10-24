# FFmpeg encoder

The `%ffmpeg` encoder supports all options for `ffmpeg`'s [muxers](https://ffmpeg.org/ffmpeg-formats.html#Muxers) and [encoders](https://www.ffmpeg.org/ffmpeg-codecs.html), including private options. Options are passed as key/value pairs, with values of type `string`, `int`, or `float`. Unrecognized or unused options raise an error at encoder instantiation. Here are some examples:

### Interleaved muxing

FFmpeg provides two muxing APIs: interleaved and non-interleaved. The interleaved API buffers packets to ensure audio and video streams stay in sync, preventing output that starts with a long audio chunk before any video. This comes at the cost of higher memory usage.

The non-interleaved API sends encoded packets directly to output without buffering, which can reduce latency and memory usage.

The `%ffmpeg` encoder supports both. It defaults to the interleaved API when encoding more than one stream. Set the mode explicitly with `%ffmpeg(interleaved=<true|false|"default">, ...)`.

Note that some video encoders buffer frames before producing output, which can cause issues even with interleaving enabled (the interleave buffer has a maximum size). With `libx264`, set `tune = "zerolatency"` to ensure the encoder starts producing data immediately.

### Encoding examples

- **AAC at `22050Hz` using the `fdk-aac` encoder and `mpegts` muxer**

```liquidsoap
%ffmpeg(format="mpegts",
  %audio(
    codec="libfdk_aac",
    samplerate=22050,
    b="32k",
    afterburner=1,
    profile="aac_he_v2"
  )
)
```

- **MP3 encoding using `libshine` at `48000Hz`**

```liquidsoap
%ffmpeg(format="mp3", %audio(codec="libshine", samplerate=48000))
```

- **AC3 audio and H264 video encapsulated in an MPEG-TS stream**

```liquidsoap
%ffmpeg(
  format="mpegts",
  %audio(codec="ac3", channel_coupling=0),
  %video(
    codec="libx264",
    b="2600k",
    "x264-params"="scenecut=0:open_gop=0:min-keyint=150:keyint=150",
    preset="ultrafast"
  )
)
```

- **AC3 audio and H264 video encapsulated in an MPEG-TS stream using ffmpeg raw frames**

```liquidsoap
%ffmpeg(
  format="mpegts",
  %audio.raw(codec="ac3", channel_coupling=0),
  %video.raw(
    codec="libx264",
    b="2600k",
    "x264-params"="scenecut=0:open_gop=0:min-keyint=150:keyint=150",
    preset="ultrafast"
  )
)
```

- **MP3 encoding using `libmp3lame` and video copy**

```liquidsoap
%ffmpeg(
  format="mp3",
  %audio(codec="libmp3lame"),
  %video.copy
)
```

The full syntax is as follows:

```liquidsoap
%ffmpeg(
  format=<format>,
  # Metadata to be passed when initializing the output
  metadata=[("label","value"), ...],
  # Audio section
  %audio(codec=<codec>, <option_name>=<option_value>, ...),
  # Or:
  %audio.raw(codec=<codec>, <option_name>=<option_value>, ...),
  # Or:
  %audio.copy(<option>),
  # Video section
  %video(codec=<codec>, <option_name>=<option_value>, ...),
  # Or:
  %video.raw(codec=<codec>, <option_name>=<option_value>, ...),
  # Or:
  %video.copy(<option>),
  # Generic options
  <option_name>=<option_value>, ...
)
```

Where:

- `<format>` is a string value (e.g. `"mpegts"`) as returned by `ffmpeg -formats`, or `none`. When `none` or omitted, the encoder guesses the format, for instance from the file extension when using `output.url`.
- `<codec>` is a string value (e.g. `"libmp3lame"`), as returned by the `ffmpeg -codecs` command.
- `<option_name>` is any syntactically valid variable name or string. Strings are used for option names of the form `foo-bar`.
- `%audio(...)` sets options for the audio codec. Unused options raise an exception. Any option supported by `ffmpeg` may be passed. Streams encoded with `%audio` use liquidsoap's internal frame format.
- `%audio.raw(...)` behaves like `%audio` but keeps audio data in ffmpeg's internal format. This avoids data copies and is required for [ffmpeg filters](./ffmpeg_filters.md). The parameters it does not set, such as the channel count and the sample rate, are taken from the first frame: a mono input stays mono unless `channels=2` is given.
- `%audio.copy` passes data through without decoding or re-encoding. This avoids CPU usage but the data cannot be processed by operators like `fade.{in,out}` or `smart_cross`. Every source of a copied stream must share its data format: see [switching between copied sources](./ffmpeg.md#switching-between-copied-sources).
- `%video(...)` sets options for the video codec. Unused options raise an exception. Any option supported by `ffmpeg` may be passed.
- `%video.raw` and `%video.copy` behave like their `%audio` counterparts.
- Generic options are passed to audio, video, and format (container) setup. Unused options raise an exception.

The streams of the container are created in the order of the tracks in the encoder.

### HLS output

The `%ffmpeg` encoder is the primary encoder for HLS output, as it is the only encoder that produces MPEG-TS muxed data, which most HLS clients require.

Segments are cut at a keyframe of every stream at or before each segment boundary. A copied video whose keyframes are too far apart is cut without one, with a warning, so that no segment exceeds the target duration; a playlist announces `EXT-X-INDEPENDENT-SEGMENTS` only when no copied stream can be cut that way. When a copied source brings codec headers an MP4 segment cannot carry, the variant starts a new segment after a discontinuity, with its own init segment, and re-encoded streams continue their timeline across it.

### File output

Some formats, such as `mp4`, require rewinding the stream to write a header after encoding finishes. With the `%ffmpeg` encoder, `output.file` and `output.url` let FFmpeg control the output file, so FFmpeg can write headers at the end. These formats can be written to files but cannot be streamed, for instance with `output.icecast`.

An Ogg output carries a metadata change by chaining a new logical stream after the current one, which players read as a new track. Players without support for chained Ogg stop after the first link. With `output.file(append=true)`, the encoder appends to an existing file.

### Copy options

The `%audio.copy` and `%video.copy` encoders have two mutually exclusive options to handle keyframes:

- `%audio.copy(wait_for_keyframe)` and `%video.copy(wait_for_keyframe)`: wait until at least one keyframe has been passed to start passing
  encoded packets from a new stream.
- `%audio.copy(ignore_keyframe)` and `%video.copy(ignore_keyframe)`: ignore all keyframes.

These options are useful when switching between encoded streams.

With `wait_for_keyframe`, the encoder discards packets at the start of a new stream until a keyframe is received. Playback pauses until it can resume cleanly without decoding glitches. The tracks of one source start together: the audio of a source is held until its video reaches a keyframe, and starts with it. The wait is bounded by `settings.ffmpeg.content.copy.max_keyframe_wait` (20 seconds by default); past it, the ready tracks start with a warning and the video joins at its next keyframe. This is the default.

With `ignore_keyframe`, encoded data is passed through immediately. Content starts right away but playback may stall until a keyframe is received.

Audio and subtitle tracks never wait for a keyframe of their own: every packet of theirs can start a stream.

### Hardware acceleration

The `%ffmpeg` encoder supports multiple hardware acceleration backends provided by `ffmpeg`.

If the encoder supports hardware acceleration without extra configuration, selecting the codec (for example `codec="h264_videotoolbox"` on macOS) may be all that is needed.

FFmpeg provides three types of hardware acceleration:

1. Internal hardware acceleration requiring no specific configuration.
2. Device-based hardware acceleration using a specific device.
3. Frame-based hardware acceleration using a specific pixel format.

The type of hardware acceleration to use for a given stream can be specified using the `hwaccel` option. Its value is one of: `"auto"`,
`"none"`, `"internal"`, `"device"` or `"frame"`.

For device-based acceleration, specify the device with `hwaccel_device`. For frame-based acceleration, specify the pixel format with `hwaccel_pixel_format`. In most cases, liquidsoap can infer these from the codec.

Here's an example:

```liquidsoap
enc = %ffmpeg(
  format="mpegts",
  %video(
    hwaccel="device",
    hwaccel_device="/dev/...",
    ...
  )
)
```

Hardware acceleration support is highly hardware-dependent and not all combinations have been tested. If you encounter issues, reach out to us to check whether your use case is supported.
