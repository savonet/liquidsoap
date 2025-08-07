# Encoding Formats in Liquidsoap 🎙️

When you're ready to send audio (or video!) out into the world—whether to Icecast, a file, or another system—you'll need to encode it into a suitable format. MP3, Opus, FLAC, WAV... Liquidsoap supports many, and each comes with its own settings and behaviors.

At the heart of this process are **encoders**—special values that you pass to output operators to define the desired stream format.

This page introduces how encoders work in Liquidsoap, how they influence your pipeline, and what formats and options are available. Don’t worry if it feels abstract at first—it’s a central concept, and it will click as you explore more! 🚀

## Encoders: More Than Just Compression

In Liquidsoap, an encoder is more than just a codec or compression setting. It **defines the structure of the stream**—and that structure must match the source you're feeding into it.

For example, if you write:

```liquidsoap
output.file(%mp3, "/tmp/foo.mp3", playlist("songs"))
```

You might think: “I want to encode this playlist as MP3.” But under the hood, `%mp3` is a **format specification**. It says: _This output will encode stereo PCM audio in MP3 format._

So your `playlist("songs")` must produce that exact kind of data—PCM audio, stereo. If it doesn’t, Liquidsoap may try to convert it. But it can’t always.

🧠 **Important:** The encoder drives the output type. You must feed it a compatible source.

For instance:

- `%mp3` expects `format(audio=pcm(stereo))`
- `%theora` expects video
- `%opus(channels=1)` expects mono audio

So, if you use `%mp3` in your output, your source must produce stereo PCM audio. If it doesn't, Liquidsoap will try to adapt it:

- Mono sources? Liquidsoap duplicates the channel to make stereo.
- Stereo sources for a mono encoder? Liquidsoap averages both channels.

## Encoder Syntax

Encoders use a special syntax:

```liquidsoap
%encoder_name(parameter1=value1, parameter2=value2)
```

You can omit parameters if defaults are acceptable:

```liquidsoap
output.icecast(%mp3, my_source)
```

Parameters are optional (unless noted) and can be reordered. You can also write `mono=true` or `channels=1`; both are equivalent.

## ⚠️ Format Availability

Not all encoders are always available in every Liquidsoap build. Some require optional libraries.

If an encoder isn’t available, you’ll see something like:

```
Error 12: Unsupported encoder: %xyz().
You must be missing an optional dependency.
```

In this case, you might need to enable an external dependency (if you are installing via `opam`) or rebuild liquidsoap.

On our Windows build, only `%ffmpeg` is included due to linking limitations. Luckily, `%ffmpeg` supports many common formats.

## 🔍 Format Reference

# List of formats and their syntax

## MP3

Mp3 encoder comes in 3 flavors:

- `%mp3` or `%mp3.cbr`: Constant bitrate encoding
- `%mp3.vbr`: Variable bitrate, quality-based encoding.
- `%mp3.abr`: Average bitrate based encoding.

Parameters common to each flavor are:

- `stereo=true/false`, `mono=true/false`: Encode stereo or mono data (default: `stereo`).
- `stereo_mode`: One of: `"stereo"`, `"joint_stereo"` or `"default"` (default: `"default"`). Default means that the underlying library (`libmp3lame`) will pick the stereo mode based on compression ration and input channels.
- `samplerate=44100`: Encoded data samplerate (default: `44100`)
- `internal_quality=2`: Lame algorithms internal quality. A value between `0` and `9`, `0` being highest quality and `9` the worst (default: `2`).
- `id3v2=true`: Add an `id3v2` tag to encoded data (default: `false`).

Parameters for `%mp3` are:

- `bitrate`: Encoded data fixed bitrate

Parameters for `%mp3.vbr` are:

- `quality`: Quality of encoded data; ranges from `0` (highest quality) to `9` (worst quality).

Parameters for `%mp3.abr` are:

- `bitrate`: Average bitrate
- `min_bitrate`: Minimum bitrate
- `max_bitrate`: Maximum bitrate
- `hard_min`: Enforce minimal bitrate

Examples:

- Constant `128` kbps bitrate encoding: `%mp3(bitrate=128)`
- Variable bitrate with quality `6` and samplerate of `22050` Hz: `%mp3.vbr(quality=7,samplerate=22050)`
- Average bitrate with mean of `128` kbps, maximum bitrate `192` kbps and `id3v2` tags: `%mp3.abr(bitrate=128,max_bitrate=192,id3v2=true)`

Optionally, liquidsoap can insert a message within mp3 data. You can set its value using the `msg` parameter.
Setting it to `""` disables this feature. This is its default value.

## Shine

Shine is the fixed-point mp3 encoder. It is useful on architectures without a FPU, such as ARM.

```liquidsoap
%shine(channels=2,samplerate=44100,bitrate=128)
```

## WAV

```liquidsoap
%wav(stereo=true, channels=2, samplesize=16, header=true, duration=10.)
```

If `header` is `false`, the encoder outputs raw PCM. `duration` is optional
and is used to set the WAV length header.

Because Liquidsoap encodes a possibly infinite stream, there
is no way to know in advance the duration of encoded data. Since WAV header
has to be written first, by default its length is set to the maximum possible
value. If you know the expected duration of the encoded data and you actually
care about the WAV length header then you should use the `duration` parameter.

## FFmpeg

See detailed [ffmpeg encoders](ffmpeg_encoder.html) article.

## Ogg

The following formats can be put together in an Ogg container.
The syntax for doing so is `%ogg(x,y,z)` but it is also
possible to just write `%vorbis(...)`, for example, instead
of `%ogg(%vorbis(...))`.

All ogg encoders have a `bytes_per_page` parameter, which can be used to
try to limit ogg logical pages size. For instance:

```liquidsoap
# Try to limit vorbis pages size to 1024 bytes
%vorbis(bytes_per_page=1024)
```

### Vorbis

```liquidsoap
# Variable bitrate
%vorbis(samplerate=44100, channels=2, quality=0.3)
% Average bitrate
%vorbis.abr(samplerate=44100, channels=2, bitrate=128, max_bitrate=192, min_bitrate=64)
# Constant bitrate
%vorbis.cbr(samplerate=44100, channels=2, bitrate=128)
```

### Opus

Opus is a lossy audio compression made especially suitable for interactive real-time applications
over the Internet. Liquidsoap supports Opus data encapsulated into Ogg streams.

The encoder is named `%opus` and its parameters are as follows. Please refer
to the [Opus documentation](http://www.opus-codec.org/docs/) for information about
their meanings and values.

- `vbr`: one of `"none"`, `"constrained"` or `"unconstrained"`
- `application`: One of `"audio"`, `"voip"` or `"restricted_lowdelay"`
- `complexity`: Integer value between `0` and `10`.
- `max_bandwidth`: One of `"narrow_band"`, `"medium_band"`, `"wide_band"`, `"super_wide_band"` or `"full_band"`
- `samplerate`: input samplerate. Must be one of: `8000`, `12000`, `16000`, `24000` or `48000`
- `frame_size`: encoding frame size, in milliseconds. Must be one of: `2.5`, `5.`, `10.`, `20.`, `40.` or `60.`.
- `bitrate`: encoding bitrate, in `kbps`. Must be a value between `5` and `512`. You can also set it to `"auto"`.
- `channels`: currently, only `1` or `2` channels are allowed.
- `mono`, `stereo`: equivalent to `channels=1` and `channels=2`.
- `signal`: one of `"voice"` or `"music"`

### Theora

```liquidsoap
%theora(quality=40,width=640,height=480,
        picture_width=255,picture_height=255,
        picture_x=0, picture_y=0,
        aspect_numerator=1, aspect_denominator=1,
        keyframe_frequency=64, vp3_compatible=false,
        soft_target=false, buffer_delay=5,
        speed=0)
```

You can also pass `bitrate=x` explicitly instead of a quality.
The default dimensions are liquidsoap's default,
from the settings `frame.video.height/width`.

### Speex

```liquidsoap
%speex(stereo=false, samplerate=44100, quality=7,
       mode=wideband, # One of: wideband|narrowband|ultra-wideband
       frames_per_packet=1,
       complexity=5)
```

You can also control quality using `abr=x` or `vbr=y`.

### Flac

The flac encoding format comes in two flavors:

- `%flac` is the native flac format, useful for file output but not for streaming purpose
- `%ogg(%flac,...)` is the ogg/flac format, which can be used to broadcast data with icecast

The parameters are:

```liquidsoap
%flac(samplerate=44100,
      channels=2,
      compression=5,
      bits_per_sample=16)
```

`compression` ranges from 0 to 8 and `bits_per_sample` should be one of: `8`, `16`, `24` or `32`.
Please note that `32` bits per sample is currently not supported by the underlying `libflac`.

## FDK-AAC

This encoder can do both AAC and AAC+.

Its syntax is:

```liquidsoap
%fdkaac(channels=2, samplerate=44100, bandwidth="auto", bitrate=64, afterburner=false, aot="mpeg2_he_aac_v2", transmux="adts", sbr_mode=false)
```

Where `aot` is one of: `"mpeg4_aac_lc"`, `"mpeg4_he_aac"`, `"mpeg4_he_aac_v2"`,
`"mpeg4_aac_ld"`, `"mpeg4_aac_eld"`, `"mpeg2_aac_lc"`, `"mpeg2_he_aac"` or
`"mpeg2_he_aac_v2"`

`bandwidth` is one of: `"auto"`, any supported integer value.

`transmux` is one of: `"raw"`, `"adif"`, `"adts"`, `"latm"`, `"latm_out_of_band"` or `"loas"`.

Bitrate can be either constant by passing: `bitrate=64` or variable: `vbr=<1-5>`

You can consult the [Hydrogenaudio knowledge base](http://wiki.hydrogenaud.io/index.php?title=Fraunhofer_FDK_AAC) for more details
on configuration values and meanings.
