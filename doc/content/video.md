---
header-includes: |
  \DeclareUnicodeCharacter{03C0}{$\pi$}
---

Basically streaming videos does not change anything compared to streaming audio:
you just have to use video files instead of sound files! For instance, if you
want to stream a single file to an icecast server in ogg format (with theora and
vorbis as codecs for audio and video) you can simply type:

```{.liquidsoap include="content/liq/video-simple.liq"}

```

And of course you could have used a `playlist` instead of `single` to have
multiple files, or used other [formats](encoding_formats.html) for the stream.

In order to test a video stream, it is often convenient to use the `output.sdl`
operator (or `output.graphics`) which will open a window and display the video
stream inside. These can handle streams with video only, you can use the
`drop_audio` operator to remove the sound part of a stream if needed.

You should be expecting much higher resource needs (in cpu time in particular)
for video than for audio. So, be prepared to hear the fan of your computer! The
size of videos have a great impact on computations; if your machine cannot
handle a stream (i.e. it's always catching up) you can try to encode to smaller
videos for a start.

### Encoding with FFmpeg

The `%ffmpeg` encoder is the recommended encoder when working with video. Not only does it support a wide range
of audio and video formats but it can also send and receive data to many different places, using `input.ffmpeg.`
and `output.url`. On top of that, it also supports all the [FFmpeg filters](https://ffmpeg.org/ffmpeg-filters.html)
and passing encoded data, if your script does not need re-encoding.

The syntax for the encoder is detailed in the [encoders page](encoding_formats.html). Here are some examples:

```liquidsoap
# AC3 audio and H264 video encapsulated in a MPEG-TS bitstream
%ffmpeg(format="mpegts",
  %audio(codec="ac3",channel_coupling=0),
  %video(codec="libx264",b="2600k",
         "x264-params"="scenecut=0:open_gop=0:min-keyint=150:keyint=150",
         preset="ultrafast"))

# AAC audio and H264 video encapsulated in a mp4 file (to use with
# `output.file` only, mp4 container cannot be streamed!
%ffmpeg(format="mp4",
  %audio(codec="aac"),
  %video(codec="libx264",b="2600k"))

# Ogg opus and theora encappsulated in an ogg bitstream
%ffmpeg(format="ogg",
  %audio(codec="libopus"),
  %video(codec="libtheora"))

# Ogg opus and VP8 video encapsulated in a webm bitstream
%ffmpeg(format="webm",
  %audio(codec="libopus"),
  %video(codec="libvpx"))
```

### Streaming with FFmpeg

The main input to take advantage of FFmpeg is `input.ffmpeg`. It should be able to decode pretty much any url and file that the `ffmpeg` command-line
can take as input. This is, in particular, how `input.rtmp` is defined.

For outputting, one can use the regular outputs but some of them have special features when used with `%ffmpeg`:

- `output.file` is able to properly close a file after it is done encoding it. This makes it possible to encode in formats that need a proper header after encoding is done, such as `mp4`.
- `output.url` will only work with the `%ffmpeg` encoder. It delegates data output to FFmpeg and can support any url that the `ffmpeg` command-line supports.
- `output.file.hls` and `output.harbor.hls` should only be used with `%ffmpeg`. The other encoders do work but `%ffmpeg` is the only encoder able to generate valid `MPEG-TS` and `MP4` data segments for the HLS specifications.

## Useful tips & tricks

Video is a really exciting world where there are lots of cool stuff to do.

### Transitions

Transitions at the beginning or at the end of video can be achieved using
`video.fade.in` and `video.fade.out`. For instance, fading at the beginning of
videos is done by

```{.liquidsoap include="content/liq/video-transition.liq" from=1 to=-1}

```

### Adding a logo

You can add a logo (any image) using the `video.add_image` operator, as follows:

```{.liquidsoap include="content/liq/video-logo.liq" from=1 to=-1}

```

### Inputting from a webcam

If your computer has a webcam, it can be used as a source thanks to the
`input.v4l2` operator. For instance:

```{.liquidsoap include="content/liq/video-webcam.liq"}

```

### Video in video

Suppose that you have two video sources `s` and `s2` and you want to display a
small copy of `s2` on top of `s`. This can be achieved by

```{.liquidsoap include="content/liq/video-in-video.liq" from=1 to=-1}

```

### Scrolling text

Adding scrolling text at the bottom of your video is as easy as

```{.liquidsoap include="content/liq/video-text.liq"}

```

You might need to change the `font` parameter so that it matches a font file
present on your system.

### Effects

There are many of effects that you can use to add some fun to your videos:
`video.greyscale`, `video.sepia`, `video.lomo`, etc. [Read the
documentation](reference.html) to find out about them. If you have compiled
Liquidsoap with [frei0r](http://www.piksel.org/frei0r/) support, and have
installed frei0r plugins, they will be named `video.frei0r.*`. You can have a
list of those supported on your installation as usual, using `liquidsoap --list-plugins`.

### Presenting weather forecast

You can say that a specific color should be transparent using
`video.transparent`. For instance, you can put yourself in front of a blue
screen (whose RGB color should be around 0x0000ff) and replace the blue screen
by an image of the weather using

```{.liquidsoap include="content/liq/video-weather.liq" to=-1}

```

## Detailed examples

### The anonymizer

Let's design an ``anonymizer'' effect: I want to blur my face and change my voice
so that nobody will recognise me in the street after seeing the youtube
video. Here is what we are going to achieve:

<center><iframe width="560" height="315" src="//www.youtube.com/embed/E7Fb0wV3h5Q" frameborder="0" allowfullscreen></iframe></center>This video was produced thanks to the following script:

```{.liquidsoap include="content/liq/video-anonymizer.liq"}

```

### Controlling with OSC

In this example we are going to use OSC integration in order to modify the
parameters in realtime. There are many OSC clients around, for instance I used
[TouchOSC](http://hexler.net/software/touchosc) :

<center><iframe width="560" height="315" src="//www.youtube.com/embed/EX1PTjiuuXY" frameborder="0" allowfullscreen></iframe></center>Here is how the video was made:

```{.liquidsoap content="content/liq/video-osc.liq"}

```

### Blue screen

You want to show yourself in front of a video of a bunny, as in

<center><iframe width="640" height="360" src="//www.youtube.com/embed/zHikXRNMQu4?feature=player_detailpage" frameborder="0" allowfullscreen></iframe></center>The idea is to film yourself in front of a blue screen, make this blue screen
transparent and put the resulting video in front of the bunny video (actually, I
don't have a blue screen at home, only a white wall but it still kinda works).

```{.liquidsoap include="content/liq/video-bluescreen.liq"}

```
