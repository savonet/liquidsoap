%include "../test.liq"

log.level.set(4)
settings.decoder.decoders.set(["ffmpeg"])

fname = argv(default="", 1)

def f(s) =
  let {audio = a, video = v} = source.tracks(s)

  def mkfilter(graph) =
    a = ffmpeg.filter.audio.input(graph, a)
    v = ffmpeg.filter.video.input(graph, v)

    let {audio = audio_out, video = video_out} =
      ffmpeg.filter.parse(
        graph,
        description="[v_in]hue=s=0[hue];[hue]fps=fps=30[fps];[fps]format[v_out];[a_in]volume=0.5[volume];[volume]aformat[a_out]",
        inputs={audio = [("a_in", a)], video = [("v_in", v)]},
        outputs={audio = ["a_out"], video = ["v_out"]}
      )

    a = list.assoc("a_out", audio_out)
    v = list.assoc("v_out", video_out)

    a = ffmpeg.filter.audio.output(graph, a)
    v = ffmpeg.filter.video.output(graph, v)

    source({audio = a, video = v})
  end

  ffmpeg.filter.create(mkfilter)
end

s = single(fname)
s = once(s)
s = f(s)

clock.assign_new(id='test_clock', sync='none', [s])

def audio_video(filename) =
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
  ) = test.ffprobe(filename)

  video_stream =
    list.find((fun (stream) -> null.defined(stream.pix_fmt)), parsed.streams)

  audio_stream =
    list.find(
      (fun (stream) -> null.defined(stream.sample_rate)),
      parsed.streams
    )

  (video_stream, audio_stream)
end

# Without a channel count the output keeps the input's layout and rate; with
# channels=2 it is stereo.
let (_, input_audio) = audio_video(fname)
todo = ref(2)
failed = ref(false)

def check(~layout, ~sample_rate) =
  checked = ref(false)

  def on_close(filename) =
    if
      not checked()
    then
      checked := true
      let (video_stream, audio_stream) = audio_video(filename)
      if
        not (
          null.get(video_stream.codec_name) == "h264"
          and null.get(video_stream.pix_fmt) == "yuv420p"
          and audio_stream.channel_layout == layout
          and null.get(audio_stream.codec_name) == "aac"
          and null.get(audio_stream.sample_fmt) == "fltp"
          and audio_stream.sample_rate == sample_rate
        )
      then
        failed := true
      end

      todo := todo() - 1
      if todo() == 0 then if failed() then test.fail() else test.pass() end end
    end
  end

  on_close
end

def out_file(name) =
  "#{fname}+#{name}-#{random.int()}.mp4"
end

output.file(
  fallible=true,
  on_close=check(
    layout=input_audio.channel_layout,
    sample_rate=input_audio.sample_rate
  ),
  %ffmpeg(
    format = "mkv",
    %audio.raw(codec = "aac"),
    %video.raw(codec = "libx264")
  ),
  {out_file("implicit")},
  s
)

output.file(
  fallible=true,
  on_close=check(layout="stereo", sample_rate="44100"),
  %ffmpeg(
    format = "mkv",
    %audio.raw(codec = "aac", channels = 2),
    %video.raw(codec = "libx264")
  ),
  {out_file("explicit")},
  s
)
