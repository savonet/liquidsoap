# FFmpeg cookbook

Here are some examples of what is possible to do with the ffmpeg support in liquidsoap:

## Relaying without re-encoding

With ffmpeg support, Liquidsoap can relay encoded streams without re-encoding them, making it possible to re-send a stream to multiple destinations. Here's an example:

```{.liquidsoap include="ffmpeg-relay.liq"}

```

We cannot use `mksafe` here because the content is not plain `pcm` samples, which this operator is designed to produce. There
are several ways to make the source infallible, however, either by providing a `single(...)` source with the same encoded content
as we expect from `encoded_source` or by creating an infallible source using `ffmpeg.encode.audio`.

## On-demand relaying without re-encoding

Another refinement on the previous example is the capacity to relay a stream only when listeners are connected to it,
all without re-encoding the content.

To make it work, you will need a format that can be handled by `ffmpeg` for that purpose. `mp3` is a good example.

In the script below, you need to match the encoded format of the stream with a blank file (or any other file).
The `output.harbor` will then relay the data from the file if no one is connected and start/stop the underlying
input when there are listeners:

```{.liquidsoap include="ffmpeg-relay-ondemand.liq"}

```

## Shared encoding

Liquidsoap can also encode in one place and share the encoded with data with multiple outputs, making it possible to
minimize CPU resources. Here's an example adapted from the previous one:

```{.liquidsoap include="ffmpeg-shared-encoding.liq"}

```

Shared encoding is even more useful when dealing with video encoding, which is very costly. Here's a fun example
sharing audio and video encoding and sending to different destinations, both via Icecast and to YouTube/Facebook
via the rtmp protocol:

```{.liquidsoap include="ffmpeg-shared-encoding-rtmp.liq"}

```

## Add transparent logo and video

See: https://github.com/savonet/liquidsoap/discussions/1862

## Live switch between encoded content

_This is an ongoing development effort. Please refer to the online support channels if you are experiencing issues with this kind of feature._

Starting with liquidsoap `2.1.x`, it is gradually becoming possible to do proper live switches on encoded content and send the
result to different outputs.

Please note that this requires a solid knowledge of media codecs, containers and ffmpeg bitstream filters. Different input and output
containers store codec binary data in different ways and those are not always compatible. This requires the use of bitstream filters
to adapt the binary data and, it's possible some new filters will need to be written to support more combinations of input/output and codecs.

Here's a use case that has been tested: live switch between a playlist of mp4 files and a rtmp flv input:

```{.liquidsoap include="live-switch.liq"}

```

- We need the `h264_mp4toannexb` on each stream to make sure that the mp4 data conforms to what the mpegts container expect
- We need to disable ffmpeg's automatic insertion of bitstream filters via `-autobsf`. FFmpeg does not support this kind of live switch at the moment and its automatically inserted filters won't work, which is why we're doing it ourselves.

That's it! In the future we want to extend this use-case to also be able to output to a `rtmp` output from the same data. And more!
