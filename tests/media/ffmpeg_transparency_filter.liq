log.level.set(4)

background = single("background.jpg")
playlist = single("@mp3[stereo].mp3")
logo = single("logo.png")
out = "transparency.mp4"

def logo_overlay(background, logo) =
  def mkfilter(graph) =
    background  = ffmpeg.filter.video.input(graph, background)
    logo = ffmpeg.filter.video.input(graph, logo)

    v = ffmpeg.filter.overlay(graph, background, logo,
      y="50",
      x="25"
    )

    ffmpeg.filter.video.output(graph, v)
  end

  ffmpeg.filter.create(mkfilter)
end

video = logo_overlay(source.tracks(background).video, source.tracks(logo).video)

s = source({audio = source.tracks(playlist).audio, video=video})

s = max_duration(3., s)

started = ref(false)

def on_track(_) =
  started := true
end

s.on_track(on_track)

#clock.assign_new(id='test_clock',sync='none',[s])

def on_close (_) =
  if not test.done() and !started then
    j = process.read("ffprobe -v quiet -print_format json -show_streams #{process.quote(out)}")

    let json.parse ( parsed : {
      streams: [{
        channel_layout: string?,
        sample_rate: string?,
        sample_fmt: string?,
        codec_name: string?,
        pix_fmt: string?
      }]
    }) = j

    video_stream = list.find((fun (stream) -> null.defined(stream.pix_fmt)), parsed.streams)
    audio_stream = list.find((fun (stream) -> null.defined(stream.sample_rate)), parsed.streams)

    if null.get(video_stream.codec_name) == "h264" and
       null.get(video_stream.pix_fmt) == "yuv420p" and
       null.get(audio_stream.channel_layout) == "stereo" and
       null.get(audio_stream.codec_name) == "aac" and
       null.get(audio_stream.sample_fmt) == "fltp" and
       null.get(audio_stream.sample_rate) == "44100" then
      test.pass()
    else
      test.fail()
    end
  end
end

encoder = %ffmpeg(
  %audio(codec="aac"),
  %video.raw(codec="libx264")
)

output.file(
  encoder,
  on_close=on_close,
  fallible=true,
  out, s)
