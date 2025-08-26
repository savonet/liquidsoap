# Make sure tracks are saved in the same order.
def f() =
  s =
    once(
      single(
        "../media/@ffmpeg[format='mp4',@audio[codec='aac',channels=2],@audio_2[codec='aac',channels=1],@video[codec='libx264'],@video_2[codec='libx264']].mp4"
      )
    )

  def on_stop() =
    j =
      process.read(
        "ffprobe -v quiet -print_format json -show_streams \
         ffmpeg-copy-encode.mkv"
      )

    let json.parse ({streams = [s1, s2]} : {streams: [{channels?: int}]}) = j
    if s1?.channels != 2 or s2?.channels != 1 then test.fail() end
    test.pass()
  end

  o =
    output.file(
      fallible=true,
      %ffmpeg(%audio.copy, %audio_2(channels = 1, codec = "aac"), %video.copy),
      "ffmpeg-copy-encode.mkv",
      s
    )
  o.on_stop(synchronous=true, on_stop)
end

test.check(f)
