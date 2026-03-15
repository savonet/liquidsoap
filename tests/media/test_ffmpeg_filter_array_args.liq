log.level.set(4)

fname = argv(default="", 1)

s = once(single(fname))

let {audio = a} = source.tracks(s)

# Test aformat filter with array of sample formats
def mkfilter(graph) =
  s = ffmpeg.filter.audio.input(graph, a)

  # Use aformat with an array of sample_fmts
  s = ffmpeg.filter.aformat(graph, s, sample_fmts=["s32", "fltp"])
  ffmpeg.filter.audio.output(graph, s)
end

a = ffmpeg.filter.create(mkfilter)
s = source({audio=a})

clock.assign_new(sync="none", [s])

filename = file.temp("tmp", ".wav")
on_cleanup({file.remove(filename)})

done = ref(false)

def on_close(_) =
  if
    not done()
  then
    done := true

    let json.parse (parsed : {streams: [{sample_fmt: string?}]}) =
      test.ffprobe(filename)

    audio_stream = list.hd(parsed.streams)
    sample_fmt = null.get(default="", audio_stream.sample_fmt)

    # aformat should have selected one of the allowed formats
    if
      list.mem(sample_fmt, ["s32", "fltp"])
    then
      test.pass()
    else
      log.important(
        "Unexpected sample format: #{sample_fmt}"
      )
      test.fail()
    end
  end
end

output.file(
  fallible=true,
  on_close=on_close,
  %ffmpeg(format = "wav", %audio.raw(codec = "aac")),
  filename,
  s
)
