def parallel_flanger_highpass(s) =
  def mkfilter(graph) =
    audio_track = ffmpeg.filter.audio.input(graph, s.tracks.audio)

    let (audio, _) = ffmpeg.filter.asplit(outputs=2, graph, audio_track)

    let [a1, a2] = audio

    a1 = ffmpeg.filter.flanger(graph, a1, delay=10.)
    a2 = ffmpeg.filter.highpass(graph, a2, frequency=4000.)

    # For some reason, we need to enforce the format here.
    a1 =
      ffmpeg.filter.aformat(
        sample_fmts="s16",
        sample_rates="44100",
        channel_layouts="stereo",
        graph,
        a1
      )
    a2 =
      ffmpeg.filter.aformat(
        sample_fmts="s16",
        sample_rates="44100",
        channel_layouts="stereo",
        graph,
        a2
      )

    audio_track = ffmpeg.filter.amerge(inputs=2, graph, [a1, a2], [])

    ffmpeg.filter.audio.output(graph, audio_track)
  end

  ffmpeg.filter.create(mkfilter)
end
