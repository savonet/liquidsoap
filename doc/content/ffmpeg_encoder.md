# FFmpeg encoder

The `%ffmpeg` encoder should support all the options for `ffmpeg`'s [muxers](https://ffmpeg.org/ffmpeg-formats.html#Muxers)
and [encoders](https://www.ffmpeg.org/ffmpeg-codecs.html), including private configuration options. Configuration value are passed as
key/values, with values being of types: `string`, `int`, or `float`. If an option is not recognized (or: unused), it will raise an error
during the instantiation of the encoder. Here are some configuration examples:

### Interleaved muxing

FFmpeg provides two different APIs for muxing data, interleaved or not. The interleaved API buffers packets waiting to be
outputted to make sure that all streams, e.g. audio and video, have their packets as close to each other as possible. This
ensures that for instance, the stream does not start with a long chunk of audio data without any video content. However, this
can come with some increased memory usage due to buffering.

On the other hand, the non-interleaved API allows to send encoded packets directly to the output without intermediate buffering.
This can sometimes result in better latency and lower memory usage.

The `%ffmpeg` encoder can use either API. By default, it uses the interleaved API when encoding more than one stream. You can also
specify the interleaving mode by passing the `interleaved` parameter: `%ffmpeg(interleaved=<true|false|"default">, ...)`.

You might also want to take this into consideration when setting your encoder's parameters. Some video encoders can buffer frames for
a while before outputting the first encoded frame, which can also create issues even with the interleaved API enabled (the interleaving
buffer has a max size too!). Typically, with `libx264`, you can set `tune = "zerolatency"` to make sure that the encoder starts outputting
data right away.

### Encoding examples

- **AAC encoding at `22050kHz` using `fdk-aac` encoder and `mpegts` muxer**

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

- **Mp3 encoding using `libshine` at `48000kHz`**

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

- **Mp3 encoding using `libmp3lame` and video copy**

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

- `<format>` is either a string value (e.g. `"mpegts"`), as returned by the `ffmpeg -formats` command or `none`. When set to `none` or
  simply no specified, the encoder will try to auto-detect it.
- `<codec>` is a string value (e.g. `"libmp3lame"`), as returned by the `ffmpeg -codecs` command.
- `<option_name>` can be any syntactically valid variable name or string. Strings are typically used when the option name is of the
  form: `foo-bar`.
- `%audio(...)` is for options specific to the audio codec. Unused options will raise an exception. Any option supported by `ffmpeg` can be
  passed here. Streams encoded using `%audio` are using liquidsoap internal frame format and are fully handled on the liquidsoap side.
- `%audio.raw(...)` behaves like `%audio` except that the audio data is kept as ffmpeg's internal format. This can avoid data copy and is
  also the format required to use [ffmpeg filters](ffmpeg_filters.html).
- `%audio.copy` copies data without decoding or encoding it. This is great to avoid using the CPU, but in this case, the data cannot be
  processed with operators that modify it, such as `fade.{in,out}` or `smart_cross`. Also, all streams must agree on the same data format.
- `%video(...)` is for options specific to the video codec. Unused options will raise an exception. Any option supported by `ffmpeg` can be
  passed here.
- `%video.raw` and `%video.copy` have the same meaning as their `%audio` counterpart.
- Generic options are passed to audio, video and format (container) setup. Unused options will raise an exception. Any option supported
  by `ffmpeg` can be passed here.

### HLS output

The `%ffmpeg` encoder is the prime encoder for HLS output as it is the only one of our collection of encoder which can produce Mpeg-ts muxed
data, which is required by most HLS clients.

### File output

Some encoding formats, for instance `mp4`, require to rewind their stream and write a header after the fact, when encoding of the current
track has finished. For historical reasons, such formats cannot be used with `output.file`. To remedy that, we have introduced
the `output.url` operator. When using this operator, the encoder is fully in charge of the output file and can thus write headers after the
fact. The `%ffmpeg` encoder is one such encoder that can be used with this operator.

### Copy options

The `%audio.copy` and `%video.copy` encoders have two mutually exclusive options to handle keyframes:

- `%audio.copy(wait_for_keyframe)` and `%video.copy(wait_for_keyframe)`: Wait until at least one keyframe has been passed to start passing
  encoded packets from a new stream.
- `%audio.copy(ignore_keyframe)` and `%video.copy(ignore_keyframe)`: Ignore all keyframes.

These options are useful when switching from one encoded stream to the next.

With option `wait_for_keyframe`, the encoder discards any new packet at the beginning of a stream until a keyframe is passed. This means
that playback will be paused until it can be resumed properly with no decoding glitches. This option is implemented globally when possible,
i.e. in case of a video track with keyframes and an audio track with no keyframes, the audio track will discard packets until a video
keyframe has been passed. This is the default option.

With option `ignore_keyframe`, the encoder starts passing encoded data right away. Content is immediately added but playback might get stuck
until a new keyframe is passed.

It is worth noting that some audio encoders may also have keyframes.

### Hardware acceleration

The `%ffmpeg` encoder supports multiple hardware acceleration provided by `ffmpeg`.

If you are lucky and the encoder you are using provides support for hardware acceleration without any specific configuration, all you might
have to do is select `codec="..."` (for instance on macOS, `codec="h264_videotoolbox"`) and it should work immediately.

The type of hardware acceleration provided by ffmpeg are:

1. Internal hardware acceleration that works without any specific configuration. This is the happy path described above!
2. Device-based hardware acceleration that works with a specific device.
3. Frame-based hardware acceleration that work with a specific pixel format.

The type of hardware acceleration to use for a given stream can be specified using the `hwaccel` option. Its value is one of: `"auto"`,
`"none"`, `"internal"`, `"device"` or `"frame"`.

For device-based hardware acceleration, the device to use can be specified using `hwaccel_device`. For frame-based hardware acceleration,
the pixel format can be specified using `hwaccel_pixel_format`. In most cases, liquidsoap should be able to guess these values from the
codec.

Here's an example:

```liquidsoap
enc = %ffmpeg(
  format="mpegts",
  %video(
    hwaccel="device",
    hwaccel_devic="/dev/...",
    ...
  )
)
```

Hardware acceleration support is, of course, very hardware dependent, so we might not have tested all possible combinations. If you are
having issues setting it up, do not hesitate to get in touch with us to see if your use-case is properly covered.
