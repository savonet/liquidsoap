def flanger_highpass(audio_track) =
  def mkfilter(graph) =
    audio_track = ffmpeg.filter.audio.input(graph, audio_track)
    audio_track = ffmpeg.filter.flanger(graph, audio_track, delay=10.)
    audio_track = ffmpeg.filter.highpass(graph, audio_track, frequency=4000.)
    ffmpeg.filter.audio.output(graph, audio_track)
  end

  ffmpeg.filter.create(mkfilter)
end
