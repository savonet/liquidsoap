# HLS Output

Starting with liquidsoap `1.4.0`, it is possible to send your streams as [HLS output](https://en.wikipedia.org/wiki/HTTP_Live_Streaming).

The main operator is `output.file.hls`. Here's an example using it, courtesy of [srt2hls](https://github.com/mbugeia/srt2hls):

```liquidsoap
aac_lofi = %ffmpeg(format="mpegts",
                   %audio(
                     codec="aac",
                     channels=2,
                     ar=44100
                   ))

aac_midfi = %ffmpeg(format="mpegts",
                    %audio(
                      codec="aac",
                      channels=2,
                      ar=44100,
                      b="96k"
                    ))

aac_hifi = %ffmpeg(format="mpegts",
                   %audio(
                     codec="aac",
                     channels=2,
                     ar=44100,
                     b="192k"
                   ))

streams = [("aac_lofi",aac_lofi),
           ("aac_midfi", aac_midfi),
           ("aac_hifi", aac_hifi)]

def segment_name(~position,~extname,stream_name) =
  timestamp = int_of_float(gettimeofday())
  duration = 2
  "#{stream_name}_#{duration}_#{timestamp}_#{position}.#{extname}"
end

output.file.hls(playlist="live.m3u8",
                segment_duration=2.0,
                segments=5,
                segments_overhead=5,
                segment_name=segment_name,
                persist_at="/path/to/state.config",
                "/path/to/hls/directory",
                streams,
                source)
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

## Mp4 format

`mp4` container is supported by requires specific parameters. Here's an example that mixes `aac` and `flac` audio, The parameters
required for `mp4` are `movflags` and `frag_duration`.

```liquidsoap
radio = ...

aac_lofi = %ffmpeg(format="mp4",
                   movflags="+dash+skip_sidx+skip_trailer+frag_custom",
                   frag_duration=10,
                   %audio(
                     codec="aac",
                     channels=2,
                     ar=44100,
                     b="192k"
                   ))

flac_hifi = %ffmpeg(format="mp4",
                    movflags="+dash+skip_sidx+skip_trailer+frag_custom",
                    frag_duration=10,
                    strict="-2",
                    %audio(
                      codec="flac",
                      channels=2,
                      ar=44100
                    ))

flac_hires = %ffmpeg(format="mp4",
                     movflags="+dash+skip_sidx+skip_trailer+frag_custom",
                     frag_duration=10,
                     strict="-2",
                     %audio(
                       codec="flac",
                       channels=2,
                       ar=48000
                     ))

streams = [("aac_lofi", aac_lofi),
           ("flac_hifi", flac_hifi),
           ("flac_hires", flac_hires)]


output.file.hls(playlist="live.m3u8",
                "/path/to/directory",
                streams,
                radio)
```
