log.level := 4
settings.decoder.decoders := ["ffmpeg"]

fname = argv(default="", 1)
fname2 = argv(default="", 2)
out = {"#{fname}+ffmpeg_filter_changing_rate-#{random.int()}.mp4"}

test.skip()

def f(s) =
  tracks = source.tracks(s)

  def mkfilter(graph) =
    a = ffmpeg.filter.audio.input(graph, tracks.audio)
    a = ffmpeg.filter.flanger(graph, a, delay=10.)
    a = ffmpeg.filter.highpass(graph, a, frequency=4000.)
    a = ffmpeg.filter.audio.output(graph, a)
    v = ffmpeg.filter.video.input(graph, tracks.video)
    v = ffmpeg.filter.hflip(graph, v)
    v = ffmpeg.filter.video.output(graph, v)
    source({audio=a, video=v, metadata=track.metadata(a)})
  end

  ffmpeg.filter.create(mkfilter)
end

s = single(fname)
s' = single(fname2)
last = single(id="last", fname)
last = metadata.map(insert_missing=true, fun (_) -> [("title", "done")], last)
s = sequence([s, s', s, s', s, last, s, s', s])
s = f(s)
done = ref(false)
passed = ref(false)

#clock.assign_new(id='test_clock', sync='none', [s])
def on_done(filename) =
  if
    done() and not passed()
  then
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
      passed := true
      test.pass()
    else
      test.fail()
    end
  end
end

def reopen_on_metadata(metadata) =
  if
    metadata["title"] == "done"
  then
    begin
      done := true
      true
    end
  else
    false
  end
end

output.file(
  fallible=true,
  reopen_on_metadata=reopen_on_metadata,
  on_close=on_done,
  %ffmpeg(
    format = "mp4",
    %audio.raw(codec = "aac"),
    %video.raw(codec = "libx264")
  ),
  out,
  s
)
