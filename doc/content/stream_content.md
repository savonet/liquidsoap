# Stream contents

In liquidsoap, a stream may contain any number of audio, video and
MIDI channels. As part of the type checking of your script,
liquidsoap checks that you make a consistent use of stream contents,
and also guesses what kind of stream your script is intended to
work on. As with other inferred parameters, you do not necessarily
need to read about stream contents typing if you're still learning
the ropes of liquidsoap, but you might eventually need to know a
little about it.

The content of a stream is described by the audio, video and MIDI
arities. An arity might be fixed or variable. Fixed arities are usual
natural numbers, described a number of channels that does change over
time. For example, the stream type `(2,0,0)` describes
streams that always have 2 audio channels and no channel of another
type. Variable arities describes numbers of channels that vary over
time. For example, the stream type `(*,0,0)` describes
a stream which contains only audio, but whose number of channels
might change at anytime -- think of playing files, some of which
being stereo, some mono, and some videos without any audio content.
The stream type `(*+1,0,0)` also describes a variable
number of audio channels, but with the guarantee that there will
always be at least one.

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

## Global parameters

You might have noticed that our description of stream contents is
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

## Checking stream contents

Checking the consistency of use of stream contents is done as part
of type checking. There is not so much to say here, except that you
have to read type errors. We present a few examples.

For example, if you try to send an ALSA input to a SDL input using
`output.sdl(input.alsa())`, you'll get the following:

```
At line 1, char 22-23:
  this value has type
    source(audio=?A+1,video=0,midi=0)
    where ?A is a fixed arity type
  but it should be a subtype of
    source(audio=0,video=1,midi=0)
```

It means that a source with exactly onevideo channel was expected
by the SDL output, but the ALSA output can only offer sources
producing audio.
By the way,
`?A+1 where ?A is fixed` means that the ALSA input will
accept to produce any number of channels, fixed once for
all: it will attempt to initialize the soundcard with that number of
channels and report a runtime error if that fails.

## Conversions

The above example did not make much sense, but in some cases you'll
get a type error on seemingly meaningful code, and you'll wonder how
to fix it. Often, it suffices to perform a few explicit conversions.

Consider another example involving the SDL output, where we also try
to use AO to output the audio content of a video:
`liquidsoap output.ao(output.sdl(single("file.ogv")))`.
This won't work, because the SDL output expects a pure video stream,
but AO wants some audio. The solution is to split the stream in
two, dropping the irrelevant content:

```liquidsoap
s = single("file.ogv")
output.sdl(drop_audio(s))
output.ao(drop_video(s))
```

Currently, the video dropping is useless because AO tolerates
(and ignores) non-audio channels.

If you want to support both mono and stereo (and more) files within
the same playlist, you'll need your `playlist`
or `single` instance to have type
`source(*+1,0,0)`.
But this content type won't be supported by most operators, which
require fixed arities. What you need to do is use `audio_to_stereo`
which will normalize your variable arity audio into a fixed stereo audio.

The last conversion is muxing.
It is useful to add audio/video channels to a pure video/audio stream.
See `mux_video`, `mux_audio` and `mux_midi`.

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
output.alsa((input.alsa():source(1,0,0)))
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

`%vorbis` has type `format(2,0,0)`, hence `s`
should have type `source(2,0,0)`. This works in more complex
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
