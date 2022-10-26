#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

%include "test.liq"

def filter(s, s') =
  def mkfilter_multi(graph) =
    s = ffmpeg.filter.audio.input(graph, pass_metadata=true, s)
    s = ffmpeg.filter.dynaudnorm(graph, s, gausssize=7, correctdc=true, altboundary=true, maxgain=80., b=true, targetrms=1.)
    s = ffmpeg.filter.volume(graph, s, volume="-18dB", precision=2, replaygain=0)
    s = ffmpeg.filter.aexciter(graph, s, freq=6000.)
    s = ffmpeg.filter.volume(graph, s, volume="+20dB", precision=2, replaygain=0)
    s = ffmpeg.filter.aresample(graph, s, sample_rate=192000)
    s = ffmpeg.filter.alimiter(graph, s, limit=0.8, attack=3., release=50., asc=false, asc_level=0., level=true)
    s = ffmpeg.filter.aresample(graph, s, sample_rate=48000)

    s' = ffmpeg.filter.audio.input(graph, s')
    s = ffmpeg.filter.amix(graph, inputs=2, [s, s'], [])

    s = ffmpeg.filter.ametadata(graph, mode=1, key="tag", value="test", s)

    ffmpeg.filter.audio.output(graph, pass_metadata=true, s)
  end
  ffmpeg.filter.create(mkfilter_multi)
end

s = single("annotate:tag=\"s\":files/audio/@mp3(stereo).mp3")

s' = single("annotate:tag=\"s'\":files/audio/@mp3(mono).mp3")

s = filter(s, s')

s.on_metadata(fun (m) -> if m["tag"] == "test" then test.pass() end)

clock.assign_new(sync="none",[s])

enc = %ffmpeg(%audio.raw(codec="aac"))

filename = file.temp("tmp",".aac")

on_shutdown({file.remove(filename)})

output.file(fallible=true, enc, filename, s)
