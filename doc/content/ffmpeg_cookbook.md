# FFmpeg cookbook

Here are examples of what is possible with FFmpeg support in liquidsoap:

## Relaying without re-encoding

With FFmpeg support, liquidsoap can relay encoded streams without re-encoding them, enabling delivery to multiple destinations. Here is an example:

```{.liquidsoap include="ffmpeg-relay.liq"}

```

`mksafe` cannot be used here because the content is not plain PCM samples, which that operator is designed to produce. To make the source infallible, either provide a `single(...)` source with matching encoded content, or create an infallible source using `ffmpeg.encode.audio`.

## On-demand relaying without re-encoding

This extends the previous example to relay a stream only when listeners are connected, without re-encoding.

The format must be one that `ffmpeg` can handle; `mp3` is a good choice.

In the script below, the encoded format of the stream must match a blank file (or any other file). `output.harbor` serves data from the file when no listeners are connected and starts or stops the underlying input as listeners come and go:

```{.liquidsoap include="ffmpeg-relay-ondemand.liq"}

```

## Shared encoding

Liquidsoap can encode once and share the result across multiple outputs, minimizing CPU usage. Here is an example adapted from the previous one:

```{.liquidsoap include="ffmpeg-shared-encoding.liq"}

```

Shared encoding is especially useful for video, which is computationally expensive. Here is an example sharing audio and video encoding across multiple destinations — both Icecast and YouTube/Facebook via RTMP:

```{.liquidsoap include="ffmpeg-shared-encoding-rtmp.liq"}

```

## Add transparent logo and video

See: https://github.com/savonet/liquidsoap/discussions/1862

## Live switch between encoded content

_This is an ongoing development effort. If you encounter issues, please reach out via the online support channels._

Starting with liquidsoap `2.1.x`, live switching on encoded content with delivery to multiple outputs is gradually becoming possible.

This requires solid knowledge of media codecs, containers, and ffmpeg bitstream filters. Different containers store codec binary data in incompatible ways, requiring bitstream filters to adapt the data. Additional filters may need to be written to support more input/output and codec combinations.

Here is a tested use case: live switch between a playlist of MP4 files and an RTMP FLV input:

```{.liquidsoap include="live-switch.liq"}

```

- The `h264_mp4toannexb` filter is needed on each stream to ensure the MP4 data conforms to what the MPEG-TS container expects.
- FFmpeg's automatic bitstream filter insertion must be disabled via `-autobsf`. FFmpeg does not support this kind of live switch natively and its auto-inserted filters will not work.

Future work includes extending this to also support RTMP output from the same data.
