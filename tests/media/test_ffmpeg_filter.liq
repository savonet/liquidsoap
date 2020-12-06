#!../../src/liquidsoap ../../libs/pervasives.liq
%include "test.liq"

set("log.level",5)

set("decoder.decoders",["FFMPEG"])

fname = argv(default="",1)
out = "files/test.mp4"

if file.exists(out) then
  file.remove(out)
end

def f(s) =
  def mkfilter(graph) =
    a = ffmpeg.filter.audio.input(graph, s)
    a = ffmpeg.filter.flanger(graph, a, delay=10.)
    a = ffmpeg.filter.highpass(graph, a, frequency=4000.)
    a = ffmpeg.filter.audio.output(graph, a)

    v = ffmpeg.filter.video.input(graph, s)
    v = ffmpeg.filter.hflip(graph, v)
    v = ffmpeg.filter.video.output(graph, v)

    mux_audio(id="mux_audio", audio=a, v)
  end

  ffmpeg.filter.create(mkfilter)
end

s = single(fname)

s = sequence([s,s,s,s,s,fail()])

s = f(s)

clock.assign_new(id='test_clock',sync='none',[s])

def on_done () =
  ojson = process.read("ffprobe -v quiet -print_format json -show_streams '#{out}'")

  output_format = of_json(default=[("streams", [[("samplerate", "0")]])], ojson)

  output_streams = list.assoc(default=[], "streams", output_format)

  params = ["channel_layout", "sample_rate",
            "sample_fmt", "codec_name", "pix_fmt"]

  def m(s) =
    def f(e) =
      let (p, _) = e
      list.mem(p, params)
    end
    list.filter(f, s)
  end

  output_streams = list.map(m, output_streams)

  def cmp(c, c') =
    if c < c' then
      -1
    elsif c' < c then
      1
    else
      0
    end
  end

  output_streams = list.map(list.sort(cmp), output_streams)

  def cmd_l(l, l') =
    cmp(list.assoc("codec_name", l), list.assoc("codec_name", l'))
  end

  output_streams = list.sort(cmd_l, output_streams)

  expected = [
    [("channel_layout", "stereo"), ("codec_name", "aac"), ("sample_fmt", "fltp"), ("sample_rate", "44100")],
    [("codec_name", "h264"), ("pix_fmt", "yuv420p")]
  ]

  if output_streams == expected then
    test.pass()
  else
    test.fail()
  end
end

output.file(fallible=true, on_stop=on_done, %ffmpeg(format="mkv",%audio.raw(codec="aac"),%video.raw(codec="libx264")), out, s)
