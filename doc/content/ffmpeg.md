# FFmpeg Support

Since the `2.0.x` release cycle, liquidsoap integrates a tight support of ffmpeg. This includes:

- [Decoders](#decoders)
- [Encoders](#encoders)
- [Filters](#filters)
- [Subtitles](#subtitles)
- [Bitstream filters](#bitstream-filters)
- [Encoded data tweaks](#encoded-data-tweaks)
- [Examples](#examples)

Ffmpeg support includes 3 types of content:

- **Internal content**, that is content available to all liquidsoap operators: `PCM` audio and `YUV420p` video
- **Raw content**, that is decoded content but stored as ffmpeg internal frame.
  This type of content is only available to ffmpeg filters and raw encoders. It can be used to avoid data copies back and forth between liquidsoap and ffmpeg.
- **Copy content**, that is encoded content stored as ffmpeg internal packets.
  This type of content is only available to ffmpeg copy encoder and bitstream filters and requires a fairly good understanding of media codecs and containers.
  Copy contents can be used to avoid transcoding and pass encoded data end-to-end inside liquidsoap scripts.

## Enabling ffmpeg support

FFmpeg support is available via the external [ocaml-ffmpeg](https://github.com/savonet/ocaml-ffmpeg) binding package. If you are using any binary asset from
our release pages or via docker, this should already be included.

If you are installing via [opam](https://opam.ocaml.org/), installing the `ffmpeg` package should do the trick:

```sh
% opam install ffmpeg
```

### fdk-aac support in ffmpeg

One common question is how to install `ffmpeg` with `fdk-aac` support. This can get tricky because you need the _ffmpeg shared libraries_ compiled with `libfdk-aac`.
This means that installing `libfdk-aac` alone will not be enough, you might also need to recompile `ffmpeg` to take advantage of it.

When recompiling `ffmpeg`, make sure that the `--enable-shared` argument is passed to the `configure` script. Also, compiling the shared libraries is different
than downloading the `ffmpeg` command line. Most `ffmpeg` downloads include a _static build_ of ffmpeg that is, one that does not use or provide shared libraries.

On linux platforms, you can check what dynamic libraries liquidsoap is using using

```shell
ldd /path/to/liquidsopap
```

On macos, you can use `otool -L`. In the list of libraries, you should see `libavcodec`. In turn, you should be able to use the same command to inspect the libraries required by the `libavcodec` used by the `liquidsoap` binary. If this includes `libfdk-aac`, you're good to go!

On debian, you might be able to use [deb-multimedia.org](https://www.deb-multimedia.org/) to install a build of `ffmpeg` with `libfdk-aac` enabled. You are advised
to follow the instructions on the website for the latest up-to date guide. You may also refer to [this conversation](https://github.com/savonet/liquidsoap/discussions/3027#discussioncomment-6072338).

## Decoders

For the most part, you should never have to worry about the `ffmpeg` decoder. When enabled, it should be the preferred decoder for all supported media.
When using raw or copied content, the decoder is able to produce the required content without the need of any intervention on the user part.

Should you need to tweak it, here are a couple of pointers:

The `settings.decoder.decoders` settings controls which decoders are to be used when trying to decode media files.
You can use it to restrict which decoders are being used, for instance making sure only the ffmpeg decoder is used:

```liquidsoap
settings.decoder.decoders := ["FFMPEG"]
```

Priority for the decoder is set via:

```liquidsoap
settings.decoder.priorities.ffmpeg := 10
```

You can use this setting to adjust whether or not the ffmpeg decoder should be tried first when decoding media files, in particular in
conjunction with the other `settings.decoder.priorities.*` settings.

For each type of media codec, the `settings.decoder.ffmpeg.codecs.*` settings can be used to tell `ffmpeg` which decoder to use to
decode this type of content (there could more than one decoder for a given codec).

For instance, for the `aac` codec:

- `settings.decoder.ffmpeg.codecs.aac.available()` returns the list of available decoders, typically `["aac", "aac_fixed"]`.
- `settings.decoder.ffmpeg.codecs.aac` can be used to choose which decoder should be used, typically: `settings.decoder.ffmpeg.codecs.aac := "aac"`

When debugging issues with `ffmpeg`, it can be useful to increase the log verbosity.

```liquidsoap
settings.ffmpeg.log.verbosity := "warning"
```

This settings sets the verbosity of `ffmpeg` logs. Possible values, from less verbose to more verbose are:
`"quiet"`, `"panic"`, `"fatal"`, `"error"`, `"warning"`, `"info"`, `"verbose"` or `"debug"`

Please note that, due to a technical limitation, we are not yet able to route `ffmpeg` logs through
the liquidsoap logging facilities, which means that `ffmpeg` logs are currently only printed to the
process's standard output and that the `settings.ffmpeg.log.level` is currently not used.

### Decoder arguments

In some cases, for instance when sending raw PCM data, it might be required to pass some arguments to
the ffmpeg decoder to let it know what kind of format, codec, etc. it should decode.

There are two ways to do that:

- For _streams_, the `content_type` argument can be used. The convention is to use `"application/ffmpeg;<arguments>"`.
- For _files_, the `ffmpeg_options` metadata can be used, for instance using the `annotate` protocol: `annotate:ffmpeg_options="<arguments>":/path/to/file.raw`

Here's an example of a SRT input and output that can be used to send raw PCM data between two instances:

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

If, instead of using `output.srt` above, we were using `output.file` and saving to a file
named `bla.raw`, this file could be read with a `single` source this way:

```liquidsoap
s = single("annotate:ffmpeg_options='format=s16le,ch_layout=stereo,sample_rate=44100':/tmp/bla.raw")
```

This could also be done in a `playlist` or `request.dynamic` and etc.

## Encoders

See detailed [ffmpeg encoders](ffmpeg_encoder.html) article.

## Filters

See detailed [ffmpeg filters](ffmpeg_filters.html) article.

## Subtitles

See detailed [ffmpeg subtitles](ffmpeg_subtitles.html) article.

## Bitstream filters

FFmpeg bitstream filters are filters that modify the binary content of _encoded data_. They can be used to adjust certain aspects of
media codecs and containers to make them fit some specific use, for instance a rtmp/flv output etc. They are particularly important
when dealing with live switches of encoded content (see [Examples](#examples) section).

The list of all bitstream filters is documented on [FFmpeg](https://www.ffmpeg.org/ffmpeg-bitstream-filters.html) online doc and
our [extra API reference](reference-extras.html). Here's one such filter:

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

Please consult the FFmpeg documentation for more details about that each filter do and why/how to use them.

## Encoded data tweaks

Manipulating encoded content is powerful but can sometimes require some specific knowledge of internals aspects of media codecs and containers. This section
lists some specific cases.

### Relaxed copy content compatibility check

By default, liquidsoap keeps track of the content passed in a stream containing ffmpeg encoded content (`ffmpeg.copy`) and only allows file and stream decoders to return strictly compatible
content, e.g. same video resolution or audio samplerate.

Some containers such as `mp4`, however, do allow stream where video resolution or audio samplerate changes between tracks. In this case, you can
relax those compatibility checks using the following setting:

```liquidsoap
settings.ffmpeg.content.copy.relaxed_compatibility_check := true
```

This is a global setting for now and could be refined per-stream in the future if the needs arises.

### Shared encoders

`liquisoap` provides operators to encode data using `%ffmpeg` and reuse it across output. This is called _inline encoding_. Here's an example:

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

Working with encoded data, however, requires a bit of knowledge of ffmpeg internal and media codecs and containers. Here, for instance, this stream
will have issues because the `flv` format requires global data, something that in ffmpeg terms is called `extradata`.

When working with a single encoder such as:

```liquidsoap
%ffmpeg(
  format="flv",
  %audio(codec="aac", b="128k"),
  %video(codec="libx264", b="4000k")
)
```

We are aware when initializing the encoders that it is aimed for a `flv` container so the code implicitly enables the global header for each encoder.

However, when encoding inline, we do not know at the time of encoding the container that will be used to encapsulate the stream, even worst, it can be
used potentially with different containers with different requirements!

In our case here, you have two ways to solve the issue:

If you know that all the containers will be okay with global header, you can enable the corresponding flag in the encoder:

```liquidsoap
stream = ffmpeg.encode.audio_video(
    %ffmpeg(
        %audio(codec="aac", b="128k", flags="+global_header"),
        %video(codec="libx264", b="4000k", flags="+global_header")
    ),
    stream
)
```

However, it is also possible that one stream needs global header but not the other one, which is the case here with `mpegts`. In this case, you can
use the _bitstream filter_ `ffmpeg.filter.bitstream.extract_extradata` to extract global data to only one stream:

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
