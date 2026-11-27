%include "../test.liq"

# A filter graph built while the application runs: its inputs are outputs, in
# the liquidsoap sense, and must not start a clock of their own before the
# graph claims them.

def filtered(s) =
  def mkfilter(graph) =
    a = ffmpeg.filter.audio.input(graph, source.tracks(s).audio)
    a = ffmpeg.filter.anull(graph, a)
    a = ffmpeg.filter.audio.output(graph, a)
    source({audio = a})
  end
  ffmpeg.filter.create(mkfilter)
end

def build() =
  s = ffmpeg.raw.encode.audio(%ffmpeg(%audio.raw), buffer(sine()))
  s = ffmpeg.raw.decode.audio(filtered(s))
  o = output.dummy(fallible=true, s)
  o.on_start(synchronous=true, test.pass)
end

output.dummy(blank())
thread.run(delay=0.5, build)
thread.run(delay=10., test.fail)
