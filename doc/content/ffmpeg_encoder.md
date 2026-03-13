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

- `<format>` is a string value (e.g. `"mpegts"`) as returned by `ffmpeg -formats`, or `none`. When `none` or omitted, the encoder auto-detects the format.
- `<codec>` is a string value (e.g. `"libmp3lame"`), as returned by the `ffmpeg -codecs` command.
- `<option_name>` is any syntactically valid variable name or string. Strings are used for option names of the form `foo-bar`.
- `%audio(...)` sets options for the audio codec. Unused options raise an exception. Any option supported by `ffmpeg` may be passed. Streams encoded with `%audio` use liquidsoap's internal frame format.
- `%audio.raw(...)` behaves like `%audio` but keeps audio data in ffmpeg's internal format. This avoids data copies and is required for [ffmpeg filters](ffmpeg_filters.html).
- `%audio.copy` passes data through without decoding or re-encoding. This avoids CPU usage but the data cannot be processed by operators like `fade.{in,out}` or `smart_cross`. All streams must share the same data format.
- `%video(...)` sets options for the video codec. Unused options raise an exception. Any option supported by `ffmpeg` may be passed.
- `%video.raw` and `%video.copy` behave like their `%audio` counterparts.
- Generic options are passed to audio, video, and format (container) setup. Unused options raise an exception.

### HLS output

The `%ffmpeg` encoder is the primary encoder for HLS output, as it is the only encoder that produces MPEG-TS muxed data, which most HLS clients require.

### File output

Some formats, such as `mp4`, require rewinding the stream to write a header after encoding finishes. For historical reasons, such formats cannot be used with `output.file`. The `output.url` operator addresses this — the encoder controls the output file and can write headers at the end. The `%ffmpeg` encoder supports `output.url`.

### Copy options

The `%audio.copy` and `%video.copy` encoders have two mutually exclusive options to handle keyframes:

- `%audio.copy(wait_for_keyframe)` and `%video.copy(wait_for_keyframe)`: Wait until at least one keyframe has been passed to start passing
  encoded packets from a new stream.
- `%audio.copy(ignore_keyframe)` and `%video.copy(ignore_keyframe)`: Ignore all keyframes.

These options are useful when switching between encoded streams.

With `wait_for_keyframe`, the encoder discards packets at the start of a new stream until a keyframe is received. Playback pauses until it can resume cleanly without decoding glitches. This option is applied globally when possible: if a video track has keyframes but an audio track does not, the audio track discards packets until a video keyframe is received. This is the default.

With `ignore_keyframe`, encoded data is passed through immediately. Content starts right away but playback may stall until a keyframe is received.

Note that some audio encoders also produce keyframes.

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

Hardware acceleration support is highly hardware-dependent and not all combinations have been tested. If you encounter issues, feel free to reach out to check whether your use case is supported.
