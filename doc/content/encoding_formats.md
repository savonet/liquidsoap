Encoding formats
================
Encoders are used to define formats into which raw sources should be encoded by 
an output. Syntax for encoder is: `%encoder(parameters...)` or, if you use
default parameters, `%encoder`.

Formats determine the stream content
------------------------------------
In most liquidsoap scripts, the encoding format determines what
kind of data is streamed.

The type of an encoding format depends on its parameter.
For example, `%mp3` has type `format(audio=2,video=0,midi=0)`
but `%mp3(mono)` has type `format(audio=1,video=0,midi=0)`.

The type of an output like `output.icecast`
or `output.file` is something like
`(...,format('a),...,source('a))->source('a)`.
This means that your source will have to have the same type as your format.

For example if you write
```liquidsoap
output.file(%mp3,"/tmp/foo.mp3",playlist("~/audio"))
```
then the playlist source will have to stream stereo audio.
Thus it will reject mono and video files.

Liquidsoap provides operators that can be used to convert sources 
into a format acceptable for a given encoder. For instance, the `mean`
operator transforms any audio source into a mono source and the `audio_to_stereo`
operator transforms any audio source into a stereo source.

Format variables (or lack of, rather..)
---------------------------------------
You can store an atomic format in a variable, it is a value like another:
`fmt = %mp3`. However, an atomic format is an atomic constant despite its
appearance. You cannot use a variable for one of its parameters: for
example
```liquidsoap
x = 44100
%vorbis(samplerate=x)
```

is not allowed,
you must write `%vorbis(samplerate=44100)`.

If you really need to use variables in encoder, for instance if bitrate is given by 
a user's configuration, you may alleviate that by generating a pre-defined list of possible
encoders and include it on top of your script using the `%include` directive.

List of formats and their syntax
================================
All parameters are optional, and the parenthesis are not needed
when no parameter is passed. In the following default values
are shown.
As a special case, the keywords `mono` and `stereo` can be used to indicate
the number of channels (whether is is passed as an integer or a boolean).

MP3
---
Mp3 encoder comes in 3 flavors:

* `%mp3` or `%mp3.cbr`: Constant bitrate encoding
* `%mp3.vbr`: Variable bitrate, quality-based encoding.
* `%mp3.abr`: Average bitrate based encoding.

Parameters common to each flavor are:

* `stereo=true/false`, `mono=true/false`: Encode stereo or mono data (default: `stereo`).
* `stereo_mode`: One of: `"stereo"`, `"joint_stereo"` or `"default"` (default: `"default"`)
* `samplerate=44100`: Encoded data samplerate (default: `44100`)
* `internal_quality=2`: Lame algorithms internal quality. A value between `0` and `9`, `0` being highest quality and `9` the worst (default: `2`).
* `id3v2=true`: Add an `id3v2` tag to encoded data (default: `false`). This option is only valid if liquidsoap has been compiled with taglib support. 

Parameters for `%mp3` are:

* `bitrate`: Encoded data fixed bitrate

Parameters for `%mp3.vbr` are:

* `quality`: Quality of encoded data; ranges from `0` (highest quality) to `9` (worst quality).

Parameters for `%mp3.abr` are:

* `bitrate`: Average bitrate
* `min_bitrate`: Minimum bitrate
* `max_bitrate`: Maximum bitrate
* `hard_min`: Enforce minimal bitrate

Examples:

* Constant `128` kbps bitrate encoding: `%mp3(bitrate=128)`
* Variable bitrate with quality `6` and samplerate of `22050` Hz: `%mp3.vbr(quality=7,samplerate=22050)`
* Average bitrate with mean of `128` kbps, maximum bitrate `192` kbps and `id3v2` tags: `%mp3.abr(bitrate=128,max_bitrate=192,id3v2=true)`


Optionally, liquidsoap can insert a message within mp3 data. You can set its value using the `msg` parameter.
Setting it to `""` disables this feature. This is its default value.

Shine
-----
Shine is the fixed-point mp3 encoder. It is useful on architectures without a FPU, such as ARM. 
It is named `%shine` or `%mp3.fxp` and its parameters are:

```liquidsoap
%shine(channels=2,samplerate=44100,bitrate=128)
```

WAV
---
```liquidsoap
%wav(stereo=true, channels=2, samplesize=16, header=true, duration=10.)
```

If `header` is `false`, the encoder outputs raw PCM. `duration` is optional
and is used to set the WAV length header.

Because Liquidsoap encodes a possibly infinite stream, there
is no way to know in advance the duration of encoded data. Since WAV header
has to be written first, by default its length is set to the maximum possible 
value. If you know the expected duration of the encoded data and you actually 
care about the WAV length header then you should use this parameter.

FFmpeg
------
The `%ffmpeg` encoder is the latest addition to our collection. You need to have [ffmpeg-av, ffmpeg-avfilter, ffmpeg-swscale and ffmpeg-swresample](https://github.com/savonet/ocaml-ffmpeg) installed and up-to date to enable the encoder during liquidsoap's build.

The encoder should support all the options for `ffmpeg`'s [muxers](https://ffmpeg.org/ffmpeg-formats.html#Muxers) and [encoders](https://www.ffmpeg.org/ffmpeg-codecs.html), including private configuration options. Configuration value are passed as key/values, with values being of types: `string`, `int`, or `float`. If an option is not recognized (or: unused), it will raise an error during the instantiation of the encoder. Here are some configuration examples:

* **AAC encoding at `22050kHz` using `fdk-aac` encoder and `mpegts` muxer**
```liquidsoap
%ffmpeg(format="mpegts",
  %audio(codec="libfdk_aac",samplerate=22050,b="32k",
         afterburner=1,profile="aac_he_v2"))
```

* **Mp3 encoding using `libshine` at `48000kHz`**
```liquidsoap
%ffmpeg(format="mp3",%audio(codec="libshine",samplerate=48000))
```

* **AC3 audio and H264 video encapsulated in a MPEG-TS stream**
```liquidsoap
%ffmpeg(format="mpegts",
  %audio(codec="ac3",channel_coupling=0),
  %video(codec="libx264",b="2600k",
         "x264-params"="scenecut=0:open_gop=0:min-keyint=150:keyint=150",
         preset="ultrafast"))
```

* **AC3 audio and H264 video encapsulated in a MPEG-TS stream using ffmpeg raw frames**
```liquidsoap
%ffmpeg(format="mpegts",
  %audio.raw(codec="ac3",channel_coupling=0),
  %video.raw(codec="libx264",b="2600k",
             "x264-params"="scenecut=0:open_gop=0:min-keyint=150:keyint=150",
             preset="ultrafast"))
```

* **Mp3 encoding using `libmp3lame` and video copy**
```liquidsoap
%ffmpeg(format="mp3",
  %audio(codec="libmp3lame"),
  %video.copy)
```

The full syntax is as follows:

```liquidsoap
%ffmpeg(format=<format>,
  # Audio section
  %audio(codec=<codec>,<option_name>=<option_value>,..),
  # Or:
  %audio.raw(codec=<codec>,<option_name>=<option_value>,..),
  # Or:
  %audio.copy,
  # Video section
  %video(codec=<codec>,<option_name>=<option_value>,..),
  # Or:
  %video.raw(codec=<codec>,<option_name>=<option_value>,..),
  # Or:
  %video.copy,
  # Generic options
  <option_name>=<option_value>,..
)
```
Where:

* `<format>` is either a string value (e.g. `"mpegts"`), as returned by the `ffmpeg -formats` command or `none`. When set to `none` or simply no specified, the encoder will try to auto-detect it.
* `<codec>` is either a string value (e.g. `"libmp3lame"`), as returned by the `ffmpeg -codecs` command or `none`. When set to `none`, for audio, `channels` is set to `0` and, for either audio or video, the stream is assumed to have no such content.
* `<option_name>` can be any syntactically valid variable name or string. Strings are typically used when the option name is of the form: `foo-bar`.
* `%audio(..)` is for options specific to the audio codec. Unused options will raise an exception. Any option supported by `ffmpeg` can be passed here. Streams encoded using `%audio` are using liquidsoap internal frame format and are fully handled on the liquidsoap side.
* `%audio.raw(..)` behaves like `%audio` except that the audio data is kept as ffmpeg's internal format. This can avoid data copy and is also the format required to use [ffmpeg filters](ffmpeg_filters.html)..
* `%audio.copy` copies data without decoding or encoding it. This is great to avoid using the CPU but, in this case, the data cannot be processed through operators that modify it such as `fade.{in,out}` aor `smart_cross`. Also, all stream must agree on the same data format.
* `%video(..)` is for options specific to the video codec. Unused options will raise an exception. Any option supported by `ffmpeg` can be passed here.
* `%video.raw` and `%video.copy` have the same meaning as their `%audio` counterpart.

* Generic options are passed to audio, video and format (container) setup. Unused options will raise an exception. Any option supported by `ffmpeg` can be passed here. 

The `%ffmpeg` encoder is the prime encoder for HLS output as it is the only one of our collection of encoder which can produce Mpeg-ts muxed data, which is required by most HLS clients.

Some encoding formats, for instance `mp4` require to rewing their stream and write a header after the fact, when encoding of the current track has finished. For historical reasons, such formats
cannot be used with `output.file`. To remedy that, we have introduced the `output.url` operator. When using this operator, the encoder is fully in charge of the output file and can thus write headers
after the fact. The `%ffmpeg` encoder is one such encoder that can be used with this operator.

Ogg
---
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

Quality ranges from -0.2 to 1,
but quality -0.2 is only available with the aotuv implementation of libvorbis.

### Opus

Opus is a lossy audio compression made especially suitable for interactive real-time applications 
over the Internet. Liquidsoap supports Opus data encapsulated into Ogg streams.

The encoder is named `%opus` and its parameters are as follows. Please refer
to the [Opus documentation](http://www.opus-codec.org/docs/) for information about
their meanings and values.

* `vbr`: one of `"none"`, `"constrained"` or `"unconstrained"`
* `application`: One of `"audio"`, `"voip"` or `"restricted_lowdelay"`
* `complexity`: Integer value between `0` and `10`.
* `max_bandwidth`: One of `"narrow_band"`, `"medium_band"`, `"wide_band"`, `"super_wide_band"` or `"full_band"`
* `samplerate`: input samplerate. Must be one of: `8000`, `12000`, `16000`, `24000` or `48000`
* `frame_size`: encoding frame size, in milliseconds. Must be one of: `2.5`, `5.`, `10.`, `20.`, `40.` or `60.`. 
* `bitrate`: encoding bitrate, in `kbps`. Must be a value between `5` and `512`. You can also set it to `"auto"`.
* `channels`: currently, only `1` or `2` channels are allowed.
* `mono`, `stereo`: equivalent to `channels=1` and `channels=2`.
* `signal`: one of `"voice"` or `"music"`

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

* `%flac` is the native flac format, useful for file output but not for streaming purpose
* `%ogg(%flac,...)` is the ogg/flac format, which can be used to broadcast data with icecast

The parameters are:

```liquidsoap
%flac(samplerate=44100, 
      channels=2, 
      compression=5, 
      bits_per_sample=16)
```

`compression` ranges from 0 to 8 and `bits_per_sample` should be one of: `8`, `16`, `24` or `32`.
Please note that `32` bits per sample is currently not supported by the underlying `libflac`.

FDK-AAC
-------
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

Gstreamer
---------
The `%gstreamer` encoder can be used to encode streams using the `gstreamer` multimedia framework.
This encoder extends liquidsoap with all available GStreamer formats which includes most, if not all,
formats available to your operating system.

The encoder's parameters are as follows:

```liquidsoap
%gstreamer(channels=2,
           audio="lamemp3enc",
           has_video=true,
           video="x264enc",
           muxer="mpegtsmux",
           metadata="metadata",
           log=5,
           pipeline="")
```

Please refer to the [Gstreamer encoder](gstreamer_encoder.html) page for a detailed explanation
of this encoder.

External encoders
-----------------
For a detailed presentation of external encoders, see [this page](external_encoders.html).

```liquidsoap
%external(channels=2,samplerate=44100,header=true,
          restart_on_crash=false,
          restart_on_metadata,
          restart_after_delay=30,
          process="progname")
```

Only one of `restart_on_metadata` and `restart_after_delay` should
be passed. The delay is specified in seconds.
The encoding process is mandatory, and can also be passed directly
as a string, without `process=`.


