# HLS Output

Starting with liquidsoap `1.4.0`, it is possible to send your streams as [HLS output](https://en.wikipedia.org/wiki/HTTP_Live_Streaming).
Since version `2.4.3`, fragemented MP4 `fMP4` is also fully supported for audio and video streams.

The main operator is `output.file.hls`. Here's an example using it, courtesy of [srt2hls](https://github.com/mbugeia/srt2hls):

```{.liquidsoap include="output.file.hls.liq"}

```

Let's see what's important here:

- `streams` describes the encoded streams. It's a list of: `(stream_name, encoder)`. `stream_name` is used to generate
  the corresponding media playlists. Encoders can be any encoder supported by liquidsoap. However, the [HLS RFC](https://tools.ietf.org/html/rfc8216)
  limits the list of possible codecs to `mp3` and `aac`. Furthermore, for the best possible compatible, it is recommended
  to send data encapsulated in a `MPEG-TS` stream. Currently, the only encoder capable of doing this in liquidsoap is `%ffmpeg`.
- `persist_at` is used to allow liquidsoap to restart while keeping the existing segments and playlists. When
  shutting down, liquidsoap stores the current configuration at `persist_at` and uses it to restart the HLS stream when
  restarting.
- `segments` and `segments_overhead` are used to keep track of the generated segments. Each media playlist will contain
  a number of segments defined by `segments` and an extra set of segments, defined by `segments_overhead`, is kept past the playlist size for those
  listeners who are still listening on outdated segments.

There are more useful options, in particular `on_file_change`, which can be used for instance to sync up your segments and playlists
to a distant storage and hosting service such as S3.

Liquidsoap also provides `output.harbor.hls` which allows to serve HLS streams directly from
liquidsoap. Their options should be the same as `output.file.hls`, except for harbor-specifc options `port` and `path`. It is
not recommended for listener-facing setup but can be useful to sync up with a caching system such as cloudfront.

## Keyframes and segment length

In codec terminology, a `keyframe` can be understood as a piece of encoded data that contains enough information to start decoding the stream.
Keyframes are very common in video codecs and can exist in audio codecs.

In order to make sure that a HLS playlist can be decoded starting from any segment, liquidsoap tries to split segments on keyframe boundaries. When not possible,
you will see a warning in the logs.

Segment split is forced when reaching the value specified by `EXT-X-TARGETDURATION` to follow the HLS specifications. For metadata and extra tags, segment split will
occur at the next keyframe.

To make sure that all these requirements operate correctly, you should make sure to set a keyframe frequency in your encoder's settings that generates at lease one
keyframe per segment.

For instance, if your segments are at most `2s` long and encoded using `libx264`, you can use the `keyint` and `keyint-min` parameters, which are expressed
in number of frames. If your video frame rate is `25fps` (liquidsoap's default), you should have at least one keyframe every `2 * 25 = 50` frames.

```liquidsoap
%ffmpeg(
  ...,
  %video(codec="libx264",
         x264opts="keyint=50:min-keyint=50")
)
```

## Metadata

HLS outputs supports metadata in two ways:

- Using the `%ffmpeg` encoder, through a `timed_id3` metadata logical stream with the `mpegts` format.
- Through regular ID3 frames, as requested by the [HLS specifications](https://datatracker.ietf.org/doc/html/rfc8216#section-3.4) for `adts`, `mp3`, `ac3` and `eac3` formats with the `%ffmpeg` encoder and also natively using the `%mp3`, `%shine` or `%fdkaac` encoders.
- There is currently no support for in-stream metadata for the `mp4` format.

Metadata parameters are passed through the record methods of the streams' encoders. Here's an example

```{.liquidsoap include="hls-metadata.liq"}

```

Parameters are:

- `id3`: Set to `false` to deactivate metadata on the streams. Defaults to `true`.
- `id3_version`: Set the `id3v2` version used to export metadata
- `replay_id3`: By default, the latest metadata is inserted at the beginning of each segment to make sure new listeners always get the latest metadata. Set to `false` to disable it.

Metadata for these formats are activated by default. If you are experiencing any issues with them, you can disable them by setting `id3` to `false`.
