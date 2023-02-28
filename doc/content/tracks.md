# Tracks

Starting with version `2.2.0`, liquidsoap now support track-level operations, making it possible to manipulate data at the
track level, including demuxing, remuxing, encoding, decoding, applying filters and other operations and more!

## Multitrack sources

Liquidsoap sources can have multiple tracks, for instance an english audio track and a french audio track.
The number of tracks in a source is determined by how you use it.

For instance, if you have a file `movie.mkv` with two audio tracks and one video track,
you can create a source `s` with it using the `single` operator (or `playlist`, `request.dynamic` etc):

```liquidsoap
s = single("/path/to/movie.mkv")
```

By default, `liquidsoap` decodes _only_ the track that you tell it to pick. So,
if you output this source as an output with only one audio track, it will hapily do so:

```
s = single("/path/to/movie.mkv")

# Copy first audio track and video:
output.file(
  %ffmpeg(
    %audio.copy,
    %video.copy
  ),
  "/path/to/copy.mkv",
  s
)
```

Resulting in the following logs:

```sh
[output_file:3] Content type is {audio=ffmpeg.copy,video=ffmpeg.copy}.
[decoder.ffmpeg:3] Requested content-type for "/path/to/movie.mkv": {audio=ffmpeg.copy,video=ffmpeg.copy}
[decoder.ffmpeg:3] FFmpeg recognizes "/path/to/movie.mkv" as video: {codec: h264, 1920x1038, yuv420p}, audio: {codec: aac, 48000Hz, 6 channel(s)}, audio_2: {codec: aac, 48000Hz, 6 channel(s)}
[decoder.ffmpeg:3] Decoded content-type for "/path/to/movie.mkv": {audio=ffmpeg.copy(codec="aac",channel_layout="5.1",sample_format=fltp,sample_rate=48000),video=ffmpeg.copy(codec="h264",width=1920,height=1038,aspect_ratio=1/1,pixel_format=yuv420p)}
```

This shows that the `output.file` source was initialized with expected content-type `{audio=ffmpeg.copy,video=ffmpeg.copy}`, i.e. one audio and one video
track, both copied from the original file. This comes from the `%ffmpeg` encoder definition in our script.

Then, the `single` file was decoded with the same requested content-type and FFmpeg reported all the details about the file's codec, including a second audio track named `audio_2`.

Eventually, we picked up only first `audio` and first `video` track and reported a more detailed content-type now that we know the actual content of each track.

Now, let's say that we want to also keep the second audio track but convert it to stereo and re-encode it into `aac`. We can then do:

```liquidsoap
s = single("/path/to/movie.mkv")

# Copy first audio track and video track
# and re-encode second audio track:
output.file(
  %ffmpeg(
    %audio.copy,
    %audio_2(
      channels=2,
      codec="aac"
    ),
    %video.copy
  ),
  "/path/to/copy.mkv",
  s
)
```

And now we see the following logs:

```sh
[output_file:3] Content type is {audio=ffmpeg.copy,audio_2=pcm(stereo),video=ffmpeg.copy}.
[decoder.ffmpeg:3] Requested content-type for "/path/to/movie.mkv": {audio=ffmpeg.copy,audio_2=pcm(stereo),video=ffmpeg.copy}
[decoder.ffmpeg:3] FFmpeg recognizes "/path/to/movie.mkv" as video: {codec: h264, 1920x1038, yuv420p}, audio: {codec: aac, 48000Hz, 6 channel(s)}, audio_2: {codec: aac, 48000Hz, 6 channel(s)}
[decoder.ffmpeg:3] Decoded content-type for "/path/to/movie.mkv": {audio=ffmpeg.copy(codec="aac",channel_layout="5.1",sample_format=fltp,sample_rate=48000),audio_2=pcm(5.1),video=ffmpeg.copy(codec="h264",width=1920,height=1038,aspect_ratio=1/1,pixel_format=yuv420p)}
```
