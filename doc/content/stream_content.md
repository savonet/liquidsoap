# Stream contents

In liquidsoap, a stream may contain any number of audio, video and
event MIDI channels (though this has not been tested in a while!).
As part of the type checking of your script,
liquidsoap checks that you make a consistent use of stream contents,
and also guesses what kind of stream your script is intended to
work on. As with other inferred parameters, you do not necessarily
need to read about stream contents typing if you're still learning
the ropes of liquidsoap, but you might eventually need to know a
little about it.

In liquidsoap script language, there are three sorts of objects
that rely on stream types: sources, requests and encoding formats.
A [source](sources.html) produces a stream,
and it is important what kind of stream
it produces when composing it with other sources.
A [request](requests.html) is an abstract notion of file,
often meant to be decoded, and it is useful to know into what
kind of stream it is meant to be decoded.
Finally, a [format](encoding_formats.html) describes how a stream
should be encoded (_e.g._, before output in a file or via icecast),
and the stream content is also useful here for the format
to make sense.

In this page, we explain how liquidsoap uses stream types
to guess and check what you're doing.

## Content types

Liquidsoap supports various type of content to be produced as the
script runs.

### Internal content

_internal_ content generally refers to content that the liquidsoap application
can produce and manipulate.

For audio, the default internal content is `pcm` floats
using OCaml native 64-bits float array representations. This is the format that allows the
fastest manipulation.

For video, the default is a C in-memory arrays of plannar YUV420 data.

For users concerned with memory consumption, we also support two additional audio formats,
`pcm_s16` and `pcm_f32` using, resp., signed 16-bit integers and 32-bit floating point numbers.
These formats may increase CPU usage, however, as we do need to convert back and forth when
using them in audio manipulation operators such as `amplify`, `crossfade` and etc. See [this
link](memory.html#audio-data-format) for more details.

### Opaque content

Liquidsoap also supports content type that are opaque to the application, provided by the `ffmpeg` decoder. There
are two:

- FFmpeg raw frames, which are decoded plain FFmpeg frames
- FFmpeg packets, also referred to as FFmpeg copy content. These are packets of encoded content

These type of content are consumed by FFmpeg specific operators and it is possible to convert back and
forth if you want to use them with our internal operators. However, their best use-case is to keep them as-is end-to-end
to optimize for memory and/or CPU usage.

See the [FFmpeg support](ffmpeg.html) doc for more information.

## Global parameters

You might have noticed that our description of internal stream contents is
missing some information, such as sample rate, video size, etc.
Indeed, that information is not part of the stream types, which is
local to each source/request/format, but global in liquidsoap.
You can change it using the `frame.audio/video.*`
settings, shown here with their default values:

```liquidsoap
audio.samplerate := 44100
video.frame.width := 320
video.frame.height := 240
video.frame.rate := 25
```

By default, video dimensions are automatically detected from the first decoded
video file. This behavior can be disabled by setting
`settings.video.detect_dimensions` to `false` or by explicitly setting
`video.frame.width` or `video.frame.height`.

## Checking stream contents

Checking the consistency of use of stream contents is done as part
of type checking. There is not so much to say here, except that you
have to read type errors. We present a few examples.

For example, if you try to send an ALSA input to a SDL input using
`output.sdl(input.alsa())`, you'll get the following:

```
At line 1, char 22-23:
  this value has type
    source(audio=pcm('a))
  but it should be a subtype of
    source(video=canvas)
```

It means that a source with a video channel was expected
by the SDL output, but the ALSA output can only offer sources
producing audio.

## Conversions

get a type error on seemingly meaningful code, and you'll wonder how
to fix it. Often, it suffices to perform a few explicit conversions.

Consider another example involving the SDL output, where we also try
to use AO to output the audio content of a video:

```liquidsoap
s = single("file.mp4")

# Output video here
output.file(
  %ffmpeg(%video(codec="libx264"),
  "/path/to/video.flv",
  s
)

# Output audio here
output.file(
  %ffmpeg(%audio(codec="aac"))
  "/path/to/video.aac",
  s
)
```

This won't work because the first output expects a video-only
stream while the second one expected an audio-only stream

The solution is to split the stream in two, dropping the irrelevant content:

```liquidsoap
s = single("file.mp4")

# Output video here
output.file(
  %ffmpeg(%video(codec="libx264"),
  "/path/to/video.flv",
  source.drop.audio(s)
)

# Output audio here
output.file(
  %ffmpeg(%audio(codec="aac"))
  "/path/to/video.aac",
  source.drop.video(s)
)
```

Another conversion is muxing.
It is useful to add audio/video channels to a pure video/audio stream.
For this, see `source.mux.video` and `source.mux.audio`.

## Type annotations

You now have all the tools to write a correct script.
But you might still be surprised by what stream content liquidsoap
guesses you want to use.
This is very important, because even if liquidsoap finds a type
for which it accepts to run, it might not run as you intend:
a different type might mean a different behavior
(not the intended number of audio channels, no video, etc).

Before reading on how liquidsoap performs this inference,
you can already work your way to the intended type by using type
annotations.

For example, with `output.alsa(input.alsa())`,
you'll see that liquidsoap decides that stereo audio should be used,
and consequently the ALSA I/O will be initialized with two channels.
If you want to use a different number of channels,
for example mono, you can explicitly specify it using:

```liquidsoap
output.alsa((input.alsa():source(audio=pcm(mono))))
```

## Guessing stream contents

When all other methods fail, you might need to understand a little more
how liquidsoap guesses what stream contents should be used for
each source.

First, liquidsoap guesses as much as possible
(without making unnecessary assumption) from what's been given in the
script.
Usually, the outputs pretty much determine what sources should contain.
A critical ingredient here is often the
[encoding format](encoding_formats.html). For example, in

```liquidsoap
output.icecast(%vorbis,mount="some.ogg",s)
```

`%vorbis` has type `format(audio=pcm(stereo))`, hence `s`
should have type `source(audio=pcm(stereo))`. This works in more complex
examples, when the types are guessed successively for several intermediate
operators.

After this first phase, it is possible that some contents are still
undetermined. For example in `output.alsa(input.alsa())`,
any number of audio channels could work, and nothing helps us determine
what is intended. At this point, the default numbers of channels are
used. They are given by the setting
`frame.audio/video/midi.channels` (whose defaults are respectively
`2`, `0` and `0`). In our example,
stereo audio would be chosen.
