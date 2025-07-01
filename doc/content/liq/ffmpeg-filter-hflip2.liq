def hflip(s) =
  def mkfilter(graph) =
    let {audio = audio_track, video = video_track} = source.tracks(s)

    video_track = ffmpeg.filter.video.input(graph, video_track)
    video_track = ffmpeg.filter.hflip(graph, video_track)

    audio_track = ffmpeg.filter.audio.input(graph, audio_track)
    audio_track = ffmpeg.filter.acopy(graph, audio_track)

    video_track = ffmpeg.filter.video.output(graph, video_track)
    audio_track = ffmpeg.filter.audio.output(graph, audio_track)

    source(
      {
        audio=audio_track,
        video=video_track,
        metadata=track.metadata(audio_track),
        track_marks=track.track_marks(audio_track)
      }
    )
  end

  ffmpeg.filter.create(mkfilter)
end
