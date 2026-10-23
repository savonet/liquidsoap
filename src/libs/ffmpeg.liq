%ifdef input.ffmpeg
# Read a video4linux2 capture device with FFmpeg. A capture device paces its
# clock by default: a clock holds one sync source at a time, so a camera in the
# same clock as another sync source (a sound card, a second camera, a live
# network input with `self_sync`) raises the clock's sync error. Set
# `self_sync=false` on the camera, or wrap one side in `buffer()` to put it in
# its own clock.
# @category Source / Input / Active
# @param ~id Force the value of the source ID.
# @param ~max_buffer Maximum data buffered, in seconds.
# @param ~self_sync Pace the clock with the device.
# @param ~device V4L2 device to use.
def input.v4l2(
  ~id=null,
  ~max_buffer=0.5,
  ~self_sync=true,
  ~device="/dev/video0"
) =
  (
    input.ffmpeg(
      id=id,
      format="v4l2",
      max_buffer=max_buffer,
      self_sync=self_sync,
      device
    )
  :
    source(video=yuv420p)
  )
end

# Read an RTMP stream with FFmpeg, by default as a server waiting for a
# publisher.
# @category Source / Input / Active
# @param ~id Force the value of the source ID.
# @param ~max_buffer Maximum data buffered, in seconds.
# @param ~listen Wait for a publisher instead of connecting to a server.
# @param url RTMP URL, e.g. `rtmp://0.0.0.0:1935/live/stream`.
def input.rtmp(~id=null, ~max_buffer=5., ~listen=true, url) =
  input.ffmpeg(
    id=id,
    max_buffer=max_buffer,
    format="live_flv",
    self_sync=true,
    int_args=[("listen", listen ? 1 : 0)],
    url
  )
end
%endif

%ifdef track.ffmpeg.raw.encode.audio
# Encode the audio of a source with `track.ffmpeg.encode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param f Encoding format.
# @param s Input source.
def ffmpeg.encode.audio(~id=null("ffmpeg.encode.audio"), f, s) =
  tracks = source.tracks(s)
  audio = track.ffmpeg.encode.audio(f, tracks.audio)
  source(
    id=id,
    {
      track_marks = track.track_marks(audio),
      metadata = track.metadata(audio),
      audio = audio
    }
  )
end

# Encode the video of a source with `track.ffmpeg.encode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param f Encoding format.
# @param s Input source.
def ffmpeg.encode.video(~id=null("ffmpeg.encode.video"), f, s) =
  tracks = source.tracks(s)
  video = track.ffmpeg.encode.video(f, tracks.video)
  source(
    id=id,
    {
      track_marks = track.track_marks(video),
      metadata = track.metadata(video),
      video = video
    }
  )
end

# Encode the audio and video of a source with `track.ffmpeg.encode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param f Encoding format.
# @param s Input source.
def ffmpeg.encode.audio_video(~id=null("ffmpeg.encode.audio_video"), f, s) =
  tracks = source.tracks(s)
  audio = track.ffmpeg.encode.audio(f, tracks.audio)
  video = track.ffmpeg.encode.video(f, tracks.video)
  source(
    id=id,
    {
      track_marks = track.track_marks(audio),
      metadata = track.metadata(audio),
      audio = audio,
      video = video
    }
  )
end

# Encode the audio of a source with `track.ffmpeg.raw.encode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param f Encoding format.
# @param s Input source.
def ffmpeg.raw.encode.audio(~id=null("ffmpeg.raw.encode.audio"), f, s) =
  tracks = source.tracks(s)
  audio = track.ffmpeg.raw.encode.audio(f, tracks.audio)
  source(
    id=id,
    {
      track_marks = track.track_marks(audio),
      metadata = track.metadata(audio),
      audio = audio
    }
  )
end

# Encode the video of a source with `track.ffmpeg.raw.encode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param f Encoding format.
# @param s Input source.
def ffmpeg.raw.encode.video(~id=null("ffmpeg.raw.encode.video"), f, s) =
  tracks = source.tracks(s)
  video = track.ffmpeg.raw.encode.video(f, tracks.video)
  source(
    id=id,
    {
      track_marks = track.track_marks(video),
      metadata = track.metadata(video),
      video = video
    }
  )
end

# Encode the audio and video of a source with `track.ffmpeg.raw.encode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param f Encoding format.
# @param s Input source.
def ffmpeg.raw.encode.audio_video(
  ~id=null("ffmpeg.raw.encode.audio_video"),
  f,
  s
) =
  tracks = source.tracks(s)
  audio = track.ffmpeg.raw.encode.audio(f, tracks.audio)
  video = track.ffmpeg.raw.encode.video(f, tracks.video)
  source(
    id=id,
    {
      track_marks = track.track_marks(audio),
      metadata = track.metadata(audio),
      audio = audio,
      video = video
    }
  )
end
%endif

%ifdef track.ffmpeg.decode.audio
# Decode the audio of a source with `track.ffmpeg.decode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param s Input source.
def ffmpeg.decode.audio(~id=null("ffmpeg.decode.audio"), s) =
  tracks = source.tracks(s)
  audio = track.ffmpeg.decode.audio(tracks.audio)
  source(
    id=id,
    {
      track_marks = track.track_marks(audio),
      metadata = track.metadata(audio),
      audio = audio
    }
  )
end

# Decode the video of a source with `track.ffmpeg.decode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param s Input source.
def ffmpeg.decode.video(~id=null("ffmpeg.decode.video"), s) =
  tracks = source.tracks(s)
  video = track.ffmpeg.decode.video(tracks.video)
  source(
    id=id,
    {
      track_marks = track.track_marks(video),
      metadata = track.metadata(video),
      video = video
    }
  )
end

# Decode the audio and video of a source with `track.ffmpeg.decode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param s Input source.
def ffmpeg.decode.audio_video(~id=null("ffmpeg.decode.audio_video"), s) =
  tracks = source.tracks(s)
  audio = track.ffmpeg.decode.audio(tracks.audio)
  video = track.ffmpeg.decode.video(tracks.video)
  source(
    id=id,
    {
      track_marks = track.track_marks(audio),
      metadata = track.metadata(audio),
      audio = audio,
      video = video
    }
  )
end

# Decode the audio of a source with `track.ffmpeg.raw.decode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param s Input source.
def ffmpeg.raw.decode.audio(~id=null("ffmpeg.raw.decode.audio"), s) =
  tracks = source.tracks(s)
  audio = track.ffmpeg.raw.decode.audio(tracks.audio)
  source(
    id=id,
    {
      track_marks = track.track_marks(audio),
      metadata = track.metadata(audio),
      audio = audio
    }
  )
end

# Decode the video of a source with `track.ffmpeg.raw.decode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param s Input source.
def ffmpeg.raw.decode.video(~id=null("ffmpeg.raw.decode.video"), s) =
  tracks = source.tracks(s)
  video = track.ffmpeg.raw.decode.video(tracks.video)
  source(
    id=id,
    {
      track_marks = track.track_marks(video),
      metadata = track.metadata(video),
      video = video
    }
  )
end

# Decode the audio and video of a source with `track.ffmpeg.raw.decode.*`. The input is read in a child clock: any other output reading the same source runs at the pace this source is read. With an infallible input, a persistent data fault is a hard error: use `mksafe` or `fallback` to keep streaming through it.
# @category Source / Conversion
# @param ~id Force the value of the source ID.
# @param s Input source.
def ffmpeg.raw.decode.audio_video(
  ~id=null("ffmpeg.raw.decode.audio_video"),
  s
) =
  tracks = source.tracks(s)
  audio = track.ffmpeg.raw.decode.audio(tracks.audio)
  video = track.ffmpeg.raw.decode.video(tracks.video)
  source(
    id=id,
    {
      track_marks = track.track_marks(audio),
      metadata = track.metadata(audio),
      audio = audio,
      video = video
    }
  )
end
%endif
