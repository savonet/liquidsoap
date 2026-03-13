# FFmpeg Support

Since the `2.0.x` release cycle, liquidsoap includes tight FFmpeg integration. This covers:

- [Decoders](#decoders)
- [Encoders](#encoders)
- [Filters](#filters)
- [Bitstream filters](#bitstream-filters)
- [Encoded data tweaks](#encoded-data-tweaks)
- [Examples](#examples)

FFmpeg support includes 3 types of content:

- **Internal content**, that is content available to all liquidsoap operators: `PCM` audio and `YUV420p` video
- **Raw content**, that is decoded content stored as ffmpeg internal frames.
  Only available to ffmpeg filters and raw encoders. Avoids data copies between liquidsoap and ffmpeg.
- **Copy content**, that is encoded content stored as ffmpeg internal packets.
  Only available to the ffmpeg copy encoder and bitstream filters. Requires solid knowledge of media codecs and containers.
  Avoids transcoding by passing encoded data end-to-end inside liquidsoap scripts.

## Enabling ffmpeg support

FFmpeg support is available via the external [ocaml-ffmpeg](https://github.com/savonet/ocaml-ffmpeg) binding package. Binary releases and Docker images include it by default.

If you are installing via [opam](https://opam.ocaml.org/), installing the `ffmpeg` package should do the trick:

```sh
% opam install ffmpeg
```

### fdk-aac support in ffmpeg

A common question is how to install `ffmpeg` with `fdk-aac` support. This requires the _ffmpeg shared libraries_ compiled with `libfdk-aac` — installing `libfdk-aac` alone is not enough. You may need to recompile `ffmpeg` to enable it.

When recompiling, pass `--enable-shared` to the `configure` script. Note that most `ffmpeg` downloads are _static builds_ and do not provide shared libraries.

On Linux, check which dynamic libraries liquidsoap uses with:

```shell
ldd /path/to/liquidsopap
```

On macOS, use `otool -L`. Look for `libavcodec` in the output, then run the same command on that library. If `libfdk-aac` appears, you're good to go.

On Debian, [deb-multimedia.org](https://www.deb-multimedia.org/) may provide an `ffmpeg` build with `libfdk-aac` enabled. Follow the instructions on that site for the latest guide. See also [this discussion](https://github.com/savonet/liquidsoap/discussions/3027#discussioncomment-6072338).

## Decoders

In most cases, the `ffmpeg` decoder requires no configuration. When enabled, it is the preferred decoder for all supported media. When using raw or copied content, it produces the required format automatically.

If you need to adjust its behavior:

`settings.decoder.decoders` controls which decoders are tried when decoding media files. Use it to restrict decoders, for instance to use only ffmpeg:

```liquidsoap
settings.decoder.decoders := ["FFMPEG"]
```

Priority for the decoder is set via:

```liquidsoap
settings.decoder.priorities.ffmpeg := 10
```

Use this to control whether ffmpeg is tried first, in combination with the other `settings.decoder.priorities.*` settings.

The `settings.decoder.ffmpeg.codecs.*` settings specify which decoder ffmpeg should use for each codec (there may be more than one available).

For instance, for the `aac` codec:

- `settings.decoder.ffmpeg.codecs.aac.available()` returns the list of available decoders, typically `["aac", "aac_fixed"]`.
- `settings.decoder.ffmpeg.codecs.aac` can be used to choose which decoder should be used, typically: `settings.decoder.ffmpeg.codecs.aac := "aac"`

When debugging issues with `ffmpeg`, it can be useful to increase the log verbosity.

```liquidsoap
settings.ffmpeg.log.verbosity := "warning"
```

This sets the verbosity of `ffmpeg` logs. Values from least to most verbose: `"quiet"`, `"panic"`, `"fatal"`, `"error"`, `"warning"`, `"info"`, `"verbose"`, `"debug"`.

Due to a technical limitation, `ffmpeg` logs cannot currently be routed through liquidsoap's logging facilities. They are printed directly to standard output, and `settings.ffmpeg.log.level` is not used.

### Decoder arguments

In some cases, such as sending raw PCM data, the ffmpeg decoder needs additional arguments to know the format, codec, and other parameters.

There are two ways to provide them:

- For _streams_, the `content_type` argument can be used. The convention is to use `"application/ffmpeg;<arguments>"`.
- For _files_, the `ffmpeg_options` metadata can be used, for instance using the `annotate` protocol: `annotate:ffmpeg_options="<arguments>":/path/to/file.raw`

Here is an example of a SRT input and output for sending raw PCM data between two instances:

Sender:

```liquidsoap
enc = %ffmpeg(
  format="s16le",
  %audio(
    codec="pcm_s16le",
    ac=2,
    ar=48000
  )
)

output.srt(enc, s)
```

Receiver:

```liquidsoap
s = input.srt(
  content_type="application/ffmpeg;format=s16le,ch_layout=stereo,sample_rate=48000"
)
```

If `output.file` were used instead, saving to `bla.raw`, the file can be read with a `single` source:

```liquidsoap
s = single("annotate:ffmpeg_options='format=s16le,ch_layout=stereo,sample_rate=44100':/tmp/bla.raw")
```

The same approach works with `playlist` or `request.dynamic`.

## Encoders

See detailed [ffmpeg encoders](ffmpeg_encoder.html) article.

## Filters

See detailed [ffmpeg filters](ffmpeg_filters.html) article.

## Bitstream filters

FFmpeg bitstream filters modify the binary content of _encoded data_. They adjust codec and container aspects for specific uses, such as rtmp/flv output. They are particularly important for live switches on encoded content (see the [Examples](#examples) section).

All bitstream filters are listed in the [FFmpeg documentation](https://www.ffmpeg.org/ffmpeg-bitstream-filters.html) and our [extra API reference](reference-extras.html). Here is an example:

```liquidsoap
% liquidsoap -h ffmpeg.filter.bitstream.h264_mp4toannexb

FFmpeg h264_mp4toannexb bitstream filter. See ffmpeg documentation for more
details.

Type: (?id : string?, source(video=ffmpeg.copy('a), 'b)) ->
source(video=ffmpeg.copy('a), 'b)

Category: Source / FFmpeg filter

Arguments:

 * id : string?
     Force the value of the source ID.

 * (unlabeled) : source(video=ffmpeg.copy('a), 'b)

Methods:
...
```

Consult the FFmpeg documentation for details on what each filter does and how to use it.

## Encoded data tweaks

Manipulating encoded content requires knowledge of the internal aspects of media codecs and containers. This section covers specific cases.

### Relaxed copy content compatibility check

By default, liquidsoap tracks the content in `ffmpeg.copy` streams and only allows decoders to return strictly compatible content, such as matching video resolution or audio sample rate.

Some containers like `mp4` allow streams where these parameters change between tracks. To permit this, relax the compatibility check:

```liquidsoap
settings.ffmpeg.content.copy.relaxed_compatibility_check := true
```

This is currently a global setting and may be refined per-stream in the future.

### Shared encoders

Liquidsoap provides operators to encode data using `%ffmpeg` and share it across multiple outputs. This is called _inline encoding_. Here is an example:

```liquidsoap
audio_source = single(audio_url)
video_source = single(image)

stream = source.mux.video(video=video_source, audio_source)

stream = ffmpeg.encode.audio_video(
    %ffmpeg(
        %audio(codec="aac", b="128k"),
        %video(codec="libx264", b="4000k")
    ),
    stream
)

flv = %ffmpeg(
    format="flv",
    %audio.copy,
    %video.copy,
)

# Send to one youtube output:
output.youtube.live.rtmp(
    encoder = flv,
    stream,
    ...
)

mpegts = %ffmpeg(
    format="mpegts",
    %audio.copy,
    %video.copy,
)

# And to a hls one:
output.file.hls(
  ["mpegts", mpegts],
  stream,
  ...
)
```

Working with encoded data requires some knowledge of ffmpeg internals and media codecs and containers. In this example, the stream will have issues because the `flv` format requires global data — called `extradata` in ffmpeg terms.

When working with a single encoder such as:

```liquidsoap
%ffmpeg(
  format="flv",
  %audio(codec="aac", b="128k"),
  %video(codec="libx264", b="4000k")
)
```

When initializing the encoders, liquidsoap knows the target container is `flv` and implicitly enables the global header for each encoder.

With inline encoding, the target container is not known at encode time — and the encoded stream may be sent to multiple containers with different requirements.

There are two ways to solve this:

If all containers accept global header, enable the flag in the encoder:

```liquidsoap
stream = ffmpeg.encode.audio_video(
    %ffmpeg(
        %audio(codec="aac", b="128k", flags="+global_header"),
        %video(codec="libx264", b="4000k", flags="+global_header")
    ),
    stream
)
```

If only one stream needs global header (as with `mpegts` here), use the `ffmpeg.filter.bitstream.extract_extradata` bitstream filter to apply it selectively:

```
audio_source = single(audio_url)
video_source = single(image)

stream = source.mux.video(video=video_source, audio_source)

stream = ffmpeg.encode.audio_video(
    %ffmpeg(
        %audio(codec="aac", b="128k"),
        %video(codec="libx264", b="4000k")
    ),
    stream
)

flv = %ffmpeg(
    format="flv",
    %audio.copy,
    %video.copy,
)

flv_stream = ffmpeg.filter.bitstream.extract_extradata(stream)

# Send to one youtube output:
output.youtube.live.rtmp(
    encoder = flv,
    flv_stream,
    ...
)

mpegts = %ffmpeg(
    format="mpegts",
    %audio.copy,
    %video.copy,
)

# And to a hls one:
output.file.hls(
  ["mpegts", mpegts],
  stream,
  ...
)
```

## Examples

See detailed [ffmpeg cookbook](ffmpeg_cookbook.html) article.
