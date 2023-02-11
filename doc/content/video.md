---
header-includes: |
  \DeclareUnicodeCharacter{03C0}{$\pi$}
...

Basically streaming videos does not change anything compared to streaming audio:
you just have to use video files instead of sound files! For instance, if you
want to stream a single file to an icecast server in ogg format (with theora and
vorbis as codecs for audio and video) you can simply type:

```liquidsoap
source = single("video.mp4")

output.icecast(
        %ffmpeg(format="ogg",
          %audio(codec="libvorbis"),
          %video(codec="libtheora")
        ),
        host="localhost",
        port=8000,
        password="hackme",
        mount="/videostream",
        source)
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

```liquidsoap
source = video.fade.in(transition="fade",duration=3.,source)
```

### Adding a logo

You can add a logo (any image) using the `video.add_image` operator, as follows:

```liquidsoap
source = video.add_image(
       width=30,height=30,
       x=10,y=10,
       file="logo.jpg",
       source)
```

### Inputting from a webcam

If your computer has a webcam, it can be used as a source thanks to the
`input.v4l2` operator. For instance:

```liquidsoap
output.sdl(input.v4l2())
```

### Video in video

Suppose that you have two video sources `source` and `source2` and you want to
display a small copy of `source2` on top of `source`. This can be achieved by

```liquidsoap
source2 = video.scale(scale=0.2,x=10,y=10,source2)
source = add([source,source2])
```

### Scrolling text

Adding scrolling text at the bottom of your video is as easy as

```liquidsoap
source = video.add_text.sdl(
       font="/usr/share/fonts/truetype/ttf-dejavu/DejaVuSans.ttf",
       "Hello world!", source)
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

```liquidsoap
img = single("weather.jpg")
cam = input.v4l2()
cam = video.transparent(color=0x0000ff,precision=0.2,cam)
source = add([img,cam])
```

## Detailed examples

### The anonymizer

Let's design an ``anonymizer'' effect: I want to blur my face and change my voice
so that nobody will recognise me in the street after seeing the youtube
video. Here is what we are going to achieve:

<center><iframe width="560" height="315" src="//www.youtube.com/embed/E7Fb0wV3h5Q" frameborder="0" allowfullscreen></iframe></center>This video was produced thanks to the following script:

```liquidsoap
# Input from webcam
cam = input.v4l2()

# Detect faces (this generates a white disk over faces)
mask = video.frei0r.opencvfacedetect(cam)
# Pixellize the video
censored = video.frei0r.pixeliz0r(blocksizex=0.1,blocksizey=0.1,cam)
# Generate a mask for video without the face
unmask = video.frei0r.invert0r(mask)
# Put the pixellized face over the video
s = video.frei0r.addition(
  video.frei0r.multiply(mask,censored),
  video.frei0r.multiply(unmask,cam))
# We have to bufferize the source because its clock it GStreamer's clock
s = buffer(buffer=0.1,mksafe(s))

# Input audio from microphone
mic = input.pulseaudio(clock_safe=false)
# Transpose sound to generate a funny voice
mic = soundtouch(pitch=1.5,mic)
# Add sound to video
s = mux_audio(audio=mic,s)

# Let's hear the sound
output.pulseaudio(fallible=true,s)
# Let's see the video
output.sdl(fallible=true,drop_audio(s))

s = mksafe(s)
# Output the video/sound into a file in theora/vorbis format
output.file(%ogg(%theora(quality=63),%vorbis), "anonymous.ogv", s)
```

### Controlling with OSC

In this example we are going to use OSC integration in order to modify the
parameters in realtime. There are many OSC clients around, for instance I used
[TouchOSC](http://hexler.net/software/touchosc) :

<center><iframe width="560" height="315" src="//www.youtube.com/embed/EX1PTjiuuXY" frameborder="0" allowfullscreen></iframe></center>Here is how the video was made:

```liquidsoap
# Set the OSC port to match TouchOSC's default port
settings.osc.port := 8000

# Input from the webcam
s = input.v4l2_with_audio()
s = mksafe(s)

# We get the angle from fader 3
angle = osc.float("/1/fader3", 0.)
# we rescale the position of fader 3 so that it corresponds to a 2π rotation
angle = fun() -> angle() * 3.1416 * 2.
# ...and we rotate the video according to the angle
s = video.rotate(speed=0.,angle=angle,s)
# Change brightness according to fader 1
s = video.frei0r.brightness(brightness=osc.float("/1/fader1",0.5),s)
# Change contrast according to fader 2
s = video.frei0r.contrast0r(contrast=osc.float("/1/fader2",0.5),s)

# We have to buffer here otherwise we get clocks problems
s = buffer(s)

# Output sound and video
output.pulseaudio(fallible=true,s)
output.sdl(fallible=true,drop_audio(s))
```

### Blue screen

You want to show yourself in front of a video of a bunny, as in

<center><iframe width="640" height="360" src="//www.youtube.com/embed/zHikXRNMQu4?feature=player_detailpage" frameborder="0" allowfullscreen></iframe></center>The idea is to film yourself in front of a blue screen, make this blue screen
transparent and put the resulting video in front of the bunny video (actually, I
don't have a blue screen at home, only a white wall but it still kinda works).

```liquidsoap
# The video of the bunny
s = single("big_buck_bunny_720p_stereo.ogg")
# Input from the webcam
cam = input.v4l2()
# Flip the video around a vertical axis so that it is easier
# to position yourself
cam = video.frei0r.flippo(x_axis=true,cam)
# Make the white background transparent
# I had to tweak the precision parameter so that I will be seen
# but not the wall
cam = video.transparent(color=0xffffff,precision=0.64,cam)
# Superpose the two videos
s = add([s,cam])
# Output to SDL
output.sdl(fallible=true,drop_audio(s))
```
