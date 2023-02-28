log.level := 4

def f() =
  s = once(single("../media/@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4"))

  let {audio, audio_2, video} = source.tracks(s)

  s = source({
    en = audio,
    audio_fr = audio_2,
    fr = track.audio.mean(audio_2),
    encoded_fr = track.audio.mean(audio_2),
    encoded_director_cut = video,
    director_cut_video = video
  })

  def on_stop() =
    j = process.read("ffprobe -v quiet -print_format json -show_streams ffmpeg-naming-convention.mkv")

    let json.parse ( {streams = [s1, s2, s3, s4, s5, s6]}: {
      streams: [{codec_type: string, channels?: int}]
    }) = j

    test.equals(s1.channels, 2)
    test.equals(s2.channels, 1)
    test.equals(s3.channels, 2)
    test.equals(s4.channels, 1)
    test.equals(s5.codec_type, "video")
    test.equals(s6.codec_type, "video")

    test.pass()
  end

  audio_codec = "aac"
  video_codec = "libx264"

  output.file(
    fallible=true,
    on_stop=on_stop,
    %ffmpeg(
      %en.copy,
      %fr(audio_content,channels=1,codec=audio_codec),
      %audio_fr(channels=2, codec="aac"),
      %encoded_fr(channels=1,codec="aac"),
      %director_cut_video(video_content,codec=video_codec),
      %encoded_director_cut(codec="libx264")
    ),
    "ffmpeg-naming-convention.mkv",
    s
  )
end

test.check(f)
