# The Gstreamer encoder

The `%gstreamer` encoder can be used to encode streams using the `gstreamer` multimedia framework.
This encoder extends liquidsoap with all available GStreamer formats (provided they are
compatible with liquidsoap's model of streams, see Caveats section below), which includes a
huge array of encoders.

## Presentation

A basic understanding of gstreamer's pipelines and configuration should be expected in order to
understand the following documentation.

The encoder's parameters are as follows:

```
%gstreamer(channels=2,
           audio="lamemp3enc",
           has_video=true,
           video="x264enc",
           muxer="mpegtsmux",
           metadata="metadata",
           log=5,
           pipeline="")
```

Without using the `pipeline` argument, the `audio` and `video` arguments are used to build the
gstreamer pipeline used to encode. By setting the `log` parameter to a lower value or by using
`log.level := ...`, you should be able to see some example.

## Basic examples

Here are a couple of examples:

An MP3 encoder that expects sources of type `audio=2, video=0, midi=0`:

```
% liquidsoap 'output.file(%gstreamer(audio="lamemp3enc",
                                     muxer="",
                                     video="",
                                     log=3),...)'
(...)
2012/12/13 19:16:23 [encoder.gstreamer:3] Gstreamer encoder pipeline: appsrc
  name="audio_src" block=true caps="audio/x-raw,format=S16LE,layout=interleaved,
  channels=2,rate=44100" format=time ! queue ! audioconvert ! audioresample !
  lamemp3enc ! appsink name=sink sync=false emit-signals=true
```

A x264 encoder that expects sources of type `audio=0, video=1, midi=0`:

```
% liquidsoap 'output.file(%gstreamer(audio="",
                                     muxer="mpegtsmux",
                                     video="x264enc",
                                     log=3),...)'
(...)
2012/12/13 19:14:43 [encoder.gstreamer:3] Gstreamer encoder pipeline:  appsrc
  name="video_src" block=true caps="video/x-raw,format=RGBA,width=320,height=240,
  framerate=25/1,pixel-aspect-ratio=1/1" format=time blocksize=307200 ! queue !
  videoconvert ! videoscale add-borders=true ! videorate ! x264enc !
  mpegtsmux name=muxer ! appsink name=sink sync=false emit-signals=true
```

An MPEG TS encoder that expects sources of type `audio=2, video=1, midi=0`:

```
% liquidsoap 'output.file(%gstreamer(audio="lamemp3enc",
                                     muxer="mpegtsmux",
                                     video="x264enc",
                                     log=3),...)'
(...)
2012/12/13 19:18:09 [encoder.gstreamer:3] Gstreamer encoder pipeline: appsrc
  name="audio_src" block=true caps="audio/x-raw,format=S16LE,
  layout=interleaved,channels=2,rate=44100" format=time ! queue ! audioconvert
  ! audioresample ! lamemp3enc ! muxer. appsrc name="video_src" block=true
  caps="video/x-raw,format=RGBA,width=320,height=240,framerate=25/1,
  pixel-aspect-ratio=1/1" format=time blocksize=307200 ! queue ! videoconvert
  ! videoscale add-borders=true ! videorate ! x264enc ! muxer. mpegtsmux
  name=muxer ! appsink name=sink sync=false emit-signals=true
```

An ogg/vorbis+theora encoder that expects source of type `audio=1, video=1, midi=0`:

```
% liquidsoap 'output.file(%gstreamer(audio="vorbisenc",
                                     muxer="oggmux",
                                     video="theoraenc",
                                     channels=1,
                                     log=3),...)'
(...)
2012/12/13 19:21:17 [encoder.gstreamer:3] Gstreamer encoder pipeline: appsrc
  name="audio_src" block=true caps="audio/x-raw,format=S16LE,layout=interleaved,
  channels=1,rate=44100" format=time ! queue ! audioconvert ! audioresample !
  vorbisenc ! muxer. appsrc name="video_src" block=true caps="video/x-raw,
  format=RGBA,width=320,height=240,framerate=25/1,pixel-aspect-ratio=1/1"
  format=time blocksize=307200 ! queue ! videoconvert ! videoscale add-borders=true
  ! videorate ! theoraenc ! muxer. oggmux name=muxer ! appsink name=sink
  sync=false emit-signals=true
```

For advanced users, the `pipeline` argument can be used to override the whole pipeline. For instance:

```
% liquidsoap 'output.file(%gstreamer(pipeline="appsrc name=\"audio_src\"
    block=true caps=\"audio/x-raw,format=S16LE,layout=interleaved,
    channels=1,rate=44100\" format=time ! lamemp3enc ! appsink name=sink
    sync=false emit-signals=true",channels=1,log=3),...)'
(...)
```

## Content type inference

When starting its sources and outputs, liquidsoap determines the content type of each source (audio, video and midi channels).
During that process, encoders have to inform liquidsoap what type of sources they are expecting. It works as follows for the `%gstreamer`
encoder:

- If the `audio` parameter is a string different than `""` then the encoder expects a stream with `channels` audio channels.
- If the `video` parameter is a string different than `""` then the encoder expects a stream with `1` video channel.
- If the `pipeline` parameter is a string different than `""` then the encoder expects a stream with `channels` audio channels and a video channels only if `has_video` is true.

The `has_video` parameter is only used when using the `pipeline` parameter.

## Metadata

The `%gstreamer` encoder tries to also encode metadata attached to the stream. This requires that you specify a pipeline element
named according to the `metadata` parameter (default: `"metadata"`) that can be used with GStreamer's `tag_setter` API. Here are two such examples:

An ogg/vorbis encoder with vorbis tags:

```
% liquidsoap 'output.file(%gstreamer(audio="vorbisenc ! vorbistag name='metadata'",
                                     muxer="oggmux",
                                     video=""),...)'
```

An MP3 encoder with id3v2 tags:

```
% liquidsoap 'output.file(%gstreamer(audio="lamemp3enc",
                                     muxer="id3v2mux",
                                     video="",
                                     metadata="muxer"),...)'
```

In the last example, we tell the `%gstreamer` encoder that the element for injecting metadata is named
`"muxer"` because, for id3v2 tags, the gstreamer muxer element is also the element used to inject metadata
and the `"muxer"` name is implicitly added by liquidsoap to the muxer element. You can see that by printing
out the constructed pipeline, as shown before.

## Caveats

When using the `%gstreamer` encoder, one must think of it as an encoder for an infinite stream. This, in particular,
means that not all containers (muxers) will work. For instance, the AVI and MP4 containers need to write in their
header information that are only known with finite streams, such as the stream total's time and etc.. These containers
are usually not fit for streaming, which is liquidsoap's main functionality.
