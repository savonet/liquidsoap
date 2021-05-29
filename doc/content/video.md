---
header-includes: |
  \DeclareUnicodeCharacter{03C0}{$\pi$}
...
Basically streaming videos does not change anything compared to streaming audio:
you just have to use video files instead of sound files! For instance, if you
want to stream a single file to an icecast server in ogg format (with theora and
vorbis as codecs for audio and video) you can simply type:
```liquidsoap
source = single("video.avi")

output.icecast(
        %ogg(%theora(quality=25,width=320,height=240),%vorbis),
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
set("osc.port",8000)

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

### Encoding with GStreamer codecs

Gstreamer codecs can be used to encode videos and audio as any natively
supported format. For instance, suppose that you want to stream using harbor in
x264 / mp3. This can be achieved as follows:

```liquidsoap
# Set the values for video size and fps.
# On my standard computer, higher values means
# that we cannot encode in realtime.
set("frame.video.width", 320)
set("frame.video.height",240)
set("frame.video.samplerate",12)

# The video we want to stream.
s = single("big_buck_bunny_720p_stereo.ogg")

output.harbor(
  format="video/mpeg",
  icy_metadata="false",
  mount="/test",
  %gstreamer(video="x264enc speed-preset=1",audio="lamemp3enc"),
  s)
```

The video can be read after that at
[http://localhost:8000/test](http://localhost:8000/test) and of course an
`output.icecast` or `output.file` could have been used instead of
`output.harbor` depending on your needs.

### Streaming with GStreamer

The usual way to stream a video is using icecast, as for audio. However, it can
happen that you want to use weird formats or ways to to stream. In this case,
using GStreamer as output (as opposed to simply a codec as above) might be a
good idea. For instance, suppose that you want to stream mp4 video using
RTP. This can be done as follows:

```liquidsoap
s = single("test.mp4")
output.gstreamer.video(pipeline="videoconvert ! avenc_mpeg4 ! rtpmp4vpay config-interval=2 ! udpsink host=127.0.0.1 port=5000", s)
```

The stream can then be read with vlc for instance using `vlc test.sdp`. Here,
the contents of the file `test.sdp` is

```
v=0
m=video 5000 RTP/AVP 96
c=IN IP4 127.0.0.1
a=rtpmap:96 MP4V-ES/90000
```

## Frequently asked questions


### `audio=1+_`

When I try

```liquidsoap
s = input.v4l2_with_audio()
output.sdl(s)
```
I get the error
```
At line 2, char 13:
  this value has type
    active_source(audio=1+_,...) (inferred at ../scripts/gstreamer.liq, line 20, char 30-121)
  but it should be a subtype of
    active_source(audio=0,...)
```

This error means that the stream `s` has an audio channel (as indicated by
`audio=1+_`) whereas `output.sdl` wants no audio channel. Namely, it's type is

```
$ liquidsoap -h output.sdl

Display a video using SDL.

Type: (?id:string,?fallible:bool,?on_start:(()->unit),
 ?on_stop:(()->unit),?start:bool,
 source(audio=0,video=1,midi=0))->
active_source(audio=0,video=1,midi=0)
```

which means that it wants 0 audio channel, 1 video channel and 0 midi
channel. The solution to correct the script is simply to remove the audio
channel using the `drop_audio` operator:

```liquidsoap
s = input.v4l2_with_audio()
output.sdl(drop_audio(s))
```

## Advanced parameters

### Default size for videos

Internally, Liquidsoap uses a video format which is the same for all frames. You
can change it by doing

```liquidsoap
set("frame.video.width",320)
set("frame.video.height",240)
set("frame.video.samplerate",24)
```

Using higher values result in higher quality videos produced, but this also
means more computations to perform!
