# Multitrack support

Starting with version `2.2.0`, liquidsoap now supports track operations, making it possible to manipulate data at the
track level, including demuxing, remuxing, encoding, decoding, applying filters and more!

**Only the FFmpeg decoder and encoder supports multitrack**. This means that you need to have `liquidsoap` compiled
with the FFmpeg support to be able to decode or encode sources with multiple audio/video tracks.

Support for track muxing and demuxing and track-level operators, however, does not require the FFmpeg support but, without it, all decoders and outputs are limited to at most one `audio` and one `video` track.

## Multitrack sources

Liquidsoap sources can have multiple tracks, such as an english language audio track and a french language audio track.
The number of tracks in a source is determined by how you use it.

For instance, if you have a file `movie.mkv` with two audio tracks and one video track,
you can create a source `s` with it using the `single` operator (or `playlist`, `request.dynamic` etc):

```liquidsoap
s = single("/path/to/movie.mkv")
```

By default, `liquidsoap` decodes _only_ the track that you tell it to pick. So,
if you output this source as an output with only one audio track, it will happily do so:

```liquidsoap
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

```
[output_file:3] Content type is {audio=ffmpeg.copy,video=ffmpeg.copy}.
[decoder.ffmpeg:3] Requested content-type for "/path/to/movie.mkv": {audio=ffmpeg.copy,video=ffmpeg.copy}
[decoder.ffmpeg:3] FFmpeg recognizes "/path/to/movie.mkv" as video: {codec: h264, 1920x1038, yuv420p}, audio: {codec: aac, 48000Hz, 6 channel(s)}, audio_2: {codec: aac, 48000Hz, 6 channel(s)}
[decoder.ffmpeg:3] Decoded content-type for "/path/to/movie.mkv": {audio=ffmpeg.copy(codec="aac",channel_layout="5.1",sample_format=fltp,sample_rate=48000),video=ffmpeg.copy(codec="h264",width=1920,height=1038,aspect_ratio=1/1,pixel_format=yuv420p)}
```

This shows that the `output.file` source was initialized with expected content-type `{audio=ffmpeg.copy,video=ffmpeg.copy}`, i.e. one audio and one video
track, both copied from the original file. This comes from the `%ffmpeg` encoder definition in our script.

Then, the `single` file was decoded with the same requested content-type and FFmpeg reported all the details about the file's content, including a second audio track named `audio_2`.

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

```
[output_file:3] Content type is {audio=ffmpeg.copy,audio_2=pcm(stereo),video=ffmpeg.copy}.
[decoder.ffmpeg:3] Requested content-type for "/path/to/movie.mkv": {audio=ffmpeg.copy,audio_2=pcm(stereo),video=ffmpeg.copy}
[decoder.ffmpeg:3] FFmpeg recognizes "/path/to/movie.mkv" as video: {codec: h264, 1920x1038, yuv420p}, audio: {codec: aac, 48000Hz, 6 channel(s)}, audio_2: {codec: aac, 48000Hz, 6 channel(s)}
[decoder.ffmpeg:3] Decoded content-type for "/path/to/movie.mkv": {audio=ffmpeg.copy(codec="aac",channel_layout="5.1",sample_format=fltp,sample_rate=48000),audio_2=pcm(5.1),video=ffmpeg.copy(codec="h264",width=1920,height=1038,aspect_ratio=1/1,pixel_format=yuv420p)}
```

Now, we are actually using both audio tracks from `movie.mkv` and one of them is being converted to stereo audio!

One thing to keep in mind, however, is that **expected content-type drives the input decoder**. Typically, if, instead of a `single`, you use a `playlist`:

```liquidsoap
s = playlist("/path/to/playlist")

# Copy first audio track and video track
# and re-encode second audio track:
output.file(
  fallible=true,
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

Then all the files in the playlist who do not have at least two `audio` tracks and one `video` track will be rejected by the decoder!

Lastly, it is important to keep in mind that **decoders always assume a specific nomenclature for tracks**. The convention when decoding is to name the first audio track `audio`, then `audio_2`, `audio_3` and etc. Likewise for video: `video`, `video_2`, `video_3`.

Typically, the following will not work:

```liquidsoap
s = playlist("/path/to/playlist")

# Copy first audio track and video track
# and re-encode second audio track:
output.file(
  fallible=true,
  %ffmpeg(
    %audio_fr.copy,
    %audio_en(
      channels=2,
      codec="aac"
    ),
    %video.copy
  ),
  "/path/to/copy.mkv",
  s
)
```

This is because the decoder has no way of knowing which of the audio track present in the files in `s` should be matched to `audio_en` and `audio_fr.copy`.

One might think that the order in which they are declared in the encoder could be used but this would also be tricky as the decoder could report tracks in different order when decoding different files from the playlist.

Also, and perhaps more importantly, tracks can be demuxed and remuxed at will, which also makes us loose the notion of track order. Actually, let's talk about demuxing and remuxing next!

## Tracks demuxing and muxing

For any given source, you can extract its tracks using the `source.tracks` operator:

```liquidsoap
s = playlist(...)

let {audio, video, metadata, track_marks} = source.tracks(s)
```

In the above, `audio` and `video` represent, resp., the `audio` and `video` track from the source `s`.

The `metadata` and `track_marks` tracks are special track type that are available in any source and hold, as the name suggests, the source's metadata and track marks. We will see later how this can be used to e.g. drop all tracks from a source (something that used to be done with the
`drop_tracks` operator), or select metadata only from a specific source or track.

Internally, **a track is a source restricted to a single content-type**. This means that:

- When pulling data for a given track, the underlying source is used, potentially also pulling data for its other tracks
- Tracks are subject to the same limitations as sources w.r.t. clocks
- Tracks, like sources, always have a `metadata` and `track_marks` tracks. The `track.metadata` and `track.track_marks` operators can be used to retrieve them.

Tracks can be muxed using the `source` operator. The operator takes a record of tracks and creates a source with them. Tracks can have any name and type except `metadata` and `track_marks` that are reserved for their corresponding track types.

### Add a video track

Here's how to add a video track to a source

```liquidsoap
# A playlist of audio files
s = playlist(...)

# A static image
image = single("/path/to/image.png")

# Get the playlist's audio track, metadata and track marks
let {audio, metadata, track_marks} = source.track(s)

# Get the video track from our static image
let {video} = source.tracks(s)

# Mux the audio tracks with the image
s = source({
  audio=audio,
  video=video,
  metadata=metadata,
  track_marks=track_marks
})
```

The above example was purposely written in a longer form to make it more explicit. However, if you wish to just add/replace a track, you
can also overload the existing tracks from the first source as follows:

```liquidsoap
# A playlist of audio files
s = playlist(...)

# A static image
image = single("/path/to/image.png")

# Mux the audio tracks with the image
s = source(source.tracks(s).{video=source.tracks(image).video})
```

### Add a default video track

You can also check if a source has a certain track and do something accordingly:

```liquidsoap
s = playlist(...)

# A default video source:
image = single("/path/to/image.png")

# Pick `s` video track if it has one, otherwise use the default one:
video = source.tracks(s).video ?? source.tracks(image).video

# Return a source that always has video:
s = source(source.tracks(s).{video=video})
```

Please not, however, that **tracks available in the playlist sources are determined based on the first decoded file**. If the first file in the playlist is audio-only then the playlist content-type is assumed to be audio-only for the whole playlist and the default video is added to _all decoded files_.

To decide on a case-by-case basis, you might need some more advanced coding!

### Merge all tracks

As mentioned before, you can also remove the track marks from a source as follows:

```liquidsoap
s = playlist(...)

# Extract all tracks except track_marks:
let {track_marks=_, ...tracks} = source.tracks(s)

s = source(tracks)
```

## Track-level operators

Some, but not all, operators have been updated to operate at the track level. They are documented under the `Track` section in [the API documentation](reference.html). More operators might be converted in the future (feel free to file a feature request for those!).

For instance, to convert an audio track to mono PCM audio, one can do:

```liquidsoap
mono_track = track.audio.mean(audio_track)
```

Likewise, inline encoders are now available at the track level, for instance:

```liquidsoap
encoded_audio_track = track.ffmpeg.encode.audio(%ffmpeg(%audio(codec="aac")), audio_track)
```

However, remember that tracks have the same limitations w.r.t. clocks that sources have. Here, in particular, `encoded_audio_track` is in a new
clock (due to the fact that ffmpeg inline encoding is not synchronous). Therefore, the following will fail:

```liquidsoap
s = playlist(...)

let {audio, metadata, track_marks} = source.tracks(s)

encoded = source({
  audio       = track.ffmpeg.encode.audio(%ffmpeg(%audio(codec="aac")), audio),
  metadata    = metadata,
  track_marks = track_marks
})
```

This is because `metadata` and `track_marks` are tracks from the underlying `s` source, which belongs to a different clock. In this case, you should
use the track marks and metadata from the encoded track:

```liquidsoap
s = playlist(...)

let {audio, metadata, track_marks} = source.tracks(s)

encoded_audio = track.ffmpeg.encode.audio(%ffmpeg(%audio(codec="aac")), audio)

encoded = source({
  audio       = encoded_audio,
  metadata    = track.metadata(encoded_audio),
  track_marks = track_marks(encoded_audio)
})
```

## Conventions

Now that we have seen how we can create any collection of tracks with any possible name, in order to make things work, we need to assume a couple of conventions.

**For decoders**, the convention, as explained above, is, when decoding files, to name the first audio track `audio`, then `audio_2`, `audio_3` and etc. Likewise for video: `video`, `video_2`, `video_3`.

This is the convention that you should use when demuxing tracks from request-based source:

```liquidsoap
s = playlist(...)

let {audio, audio_2, video, video_2, video_3} = source.tracks(s)
```

**For encoders**, to drive content-type at runtime, since tracks can be remuxed with any arbitrary name, we need to a way to decide what type of content a track contains, being `audio`, `video` or, potentially `midi` and, planned for later, `subtitles`. This is achieved using the following convention, by order of priority:

1. A `copy` track is any track named `%<track_name>.copy`. We do not need to know the track's content in this case.
2. If a track has `audio_content` or `video_content` as parameter (for instance `%foo(audio_content, ...)`) then it is considered, resp., `audio` or `video`.
3. If the track name has `audio` or `video` in it (for instance `%dolby_audio_fr`) then it is considered, resp., `audio` or `video`
4. If the track codec is hardcoded (for instance (`%foo(codec="aac", ...)`) then the codec is used to detect the content.

For instance, imagine that you want to encode a source with a `fr` and `en` audio track and a `director_cut` video track, you can do the following:

```liquidsoap
output.file(
  %ffmpeg(
    %en(codec="aac"),
    %fr(codec="aac"),
    %director_cut(codec="libx264")
  ),
  "/path/to/copy.mkv",
  s
)
```

This works because each of the codec used in these tracks can be mapped to a specific content-type.

Now, imagine that you actually want to use a variable for the codec of the `en` and `director_cut` track. In this case, you can do:

```liquidsoap
output.file(
  %ffmpeg(
    %en(audio_content, codec=audio_codec)
    %fr(codec="aac"),
    %director_cut(video_content, codec=video_codec)
  ),
  "/path/to/copy.mkv",
  s
)
```

This informs `liquidsoap` what type of content these tracks contain.

However, you might also opt for a more explicit track naming scheme. Something like:

```liquidsoap
output.file(
  %ffmpeg(
    %audio_en(codec=audio_codec),
    %audio_fr(codec="aac"),
    %director_cut_video(codec=video_codec)
  ),
  "/path/to/copy.mkv",
  s
)
```

In this case, `liquidsoap` assumes that the track with `audio` in their name are indeed audio track and the same goes for video tracks.

Lastly, these naming conventions have no bearing for the `FFmpeg` encoder. At the FFmpeg encoder level, tracks are identified by an integer and stored
in the order they are declared in the `%ffmpeg` encoder. This means that, once encoded and saved to a file, track names internal to liquidsoap are not saved by the FFmpeg encoder and, instead, when decoding the file, you will get `audio`, `audio_2`, `video` and etc.
