#!../../src/liquidsoap ../../libs/stdlib.liq

%include "test.liq"

settings.log.level.set(4)

def astats(s) =
  def mkfilter(graph) =
    s = ffmpeg.filter.audio.input(graph, s)
    s = ffmpeg.filter.astats(graph, s, metadata=true, reset=3)
    ffmpeg.filter.audio.output(graph, s)
  end

  ffmpeg.filter.create(mkfilter)
end

s = chop(noise())

def f(_) =
  [("title", "test ffmpeg frame metadata")]
end

s = map_metadata(f, s)

s = ffmpeg.raw.encode.audio(%ffmpeg(%audio.raw), s)

s = astats(s)

s = ffmpeg.raw.decode.audio(s)

passed = ref(false)

def f(m) =
  if m["title"] == "test ffmpeg frame metadata" and list.assoc.mem("lavfi.astats.Overall.RMS_peak", m) then
    passed := true
    test.pass()
  end
end

s.on_metadata(f)

output.dummy(fallible=true, s)

def fail() =
  if not !passed then
    test.fail()
  end
end

thread.run(delay=20., fail)

