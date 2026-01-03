log.level.set(4)
settings.decoder.decoders.set(["ffmpeg"])

fname = argv(default="", 1)
out = {"#{fname}+ffmpeg_filter-#{random.int()}.mp4"}

def f(s) =
  let {audio = a, video = v} = source.tracks(s)

  def mkfilter(graph) =
    a = ffmpeg.filter.audio.input(graph, a)
    v = ffmpeg.filter.video.input(graph, v)

    let {audio = audio_out, video = video_out} =
      ffmpeg.filter.parse(
        graph,
        description="[v_in]hue=s=0[v_out];[a_in]volume=0.5[a_out]",
        inputs={audio=[("a_in", a)], video=[("v_in", v)]},
        outputs={audio=["a_out"], video=["v_out"]}
      )

    a = list.assoc("a_out", audio_out)
    v = list.assoc("v_out", video_out)

    a = ffmpeg.filter.audio.output(graph, a)
    v = ffmpeg.filter.video.output(graph, v)

    source({audio=a, video=v})
  end

  ffmpeg.filter.create(mkfilter)
end

s = single(fname)
s = once(s)
s = f(s)

done = ref(false)

clock.assign_new(id='test_clock', sync='none', [s])

def on_close(filename) =
  if
    not done()
  then
    done := true

    process.run("sync")

    j =
      process.read(
        "ffprobe -v quiet -print_format json -show_streams #{
          process.quote(filename)
        }"
      )

    let json.parse (parsed :
      {
        streams: [
          {
            channel_layout: string?,
            sample_rate: string?,
            sample_fmt: string?,
            codec_name: string?,
            pix_fmt: string?
          }
        ]
      }
    ) = j

    video_stream =
      list.find((fun (stream) -> null.defined(stream.pix_fmt)), parsed.streams)

    audio_stream =
      list.find(
        (fun (stream) -> null.defined(stream.sample_rate)),
        parsed.streams
      )

    if
      null.get(video_stream.codec_name) == "h264"
    and
      null.get(video_stream.pix_fmt) == "yuv420p"
    and
      null.get(audio_stream.channel_layout) == "stereo"
    and
      null.get(audio_stream.codec_name) == "aac"
    and
      null.get(audio_stream.sample_fmt) == "fltp"
    and
      null.get(audio_stream.sample_rate) == "44100"
    then
      test.pass()
    else
      test.fail()
    end
  end
end

output.file(
  fallible=true,
  on_close=on_close,
  %ffmpeg(
    format = "mkv",
    %audio.raw(codec = "aac"),
    %video.raw(codec = "libx264")
  ),
  out,
  s
)
