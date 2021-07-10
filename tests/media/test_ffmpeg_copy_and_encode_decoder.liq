#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq
%include "test.liq"

log.level.set(5)
settings.decoder.decoders.set(["FFMPEG"])

fname = argv(default="",1)
out = "files/test.mp4"

if file.exists(out) then
  file.remove(out)
end

s = single(fname)

s = once(s)

clock.assign_new(sync='none',[s])

def on_done () =
  json = process.read("ffprobe -v quiet -print_format json -show_streams '#{out}'")

  format = of_json(default=[("streams", [[("samplerate", "0")]])], json)

  streams = list.assoc(default=[], "streams", format)

  params = ["channel_layout", "sample_rate",
            "sample_fmt", "codec_name", "pix_fmt"]

  def m(s) =
    def f(e) =
      let (p, _) = e
      list.mem(p, params)
    end
    list.filter(f, s)
  end

  streams = list.map(m, streams)

  expected = [
    [("channel_layout", "stereo"), ("sample_rate", "44100"), ("sample_fmt", "fltp"), ("codec_name", "aac")],
    [("pix_fmt", "yuv420p"), ("codec_name", "h264")]
  ]

  if streams == expected then
    test.pass()
  else
    test.fail()
  end
end

output.file(fallible=true, on_stop=on_done, %ffmpeg(format="mkv",%audio(codec="aac"),%video.copy), out, s)
