def dynamic_volume(s) =
  def mkfilter(graph) =
    filter = ffmpeg.filter.volume.create(graph)

    def set_volume(v) =
      ignore(filter.process_command("volume", "#{v}"))
    end

    let {audio = audio_track} = source.tracks(s)

    audio_track = ffmpeg.filter.audio.input(graph, audio_track)
    filter.set_input(audio_track)
    audio_track = filter.output
    audio_track = ffmpeg.filter.audio.output(graph, audio_track)

    s =
      source(
        {
          audio=audio_track,
          metadata=track.metadata(audio_track),
          track_marks=track.track_marks(audio_track)
        }
      )

    (s, set_volume)
  end

  ffmpeg.filter.create(mkfilter)
end

s = playlist("my_playlist")

let (s, set_volume) = dynamic_volume(s)

# END
output.dummy(fallible=true, s)
ignore(set_volume)
