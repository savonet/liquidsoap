#!../../src/liquidsoap ../../libs/pervasives.liq
%include "test.liq"

set("log.level",4)

set("decoder.decoders",["FFMPEG"])

fname = argv(default="",1)
out = "files/test.mp4"

if file.exists(out) then
  file.unlink(out)
end

s = single(fname)

s = once(s)

clock.assign_new(sync='none',[s])

def on_done () =
  ijson = process.read("ffprobe -v quiet -print_format json -show_streams '#{fname}'")
  ojson = process.read("ffprobe -v quiet -print_format json -show_streams '#{out}'")

  iformat = of_json(default=[("streams", [[("samplerate", "0")]])], ijson)
  oformat = of_json(default=[("streams", [[("samplerate", "0")]])], ojson)

  istreams = list.assoc(default=[], "streams", iformat)
  ostreams = list.assoc(default=[], "streams", oformat)

  params = ["channel_layout", "sample_rate",
            "sample_fmt", "codec_name", "pix_fmt"]

  def get(codec, l) =
    def f(l) =
      def g(e) =
        snd(e) == codec
      end
      list.exists(g, l)
    end
    list.find(f, l)
  end
  iaudio = get("aac", istreams)
  oaudio = get("aac", ostreams)
  ovideo = get("h264", ostreams)

  def m(s) =
    def f(e) =
      let (p, _) = e
      list.mem(p, params)
    end
    list.filter(f, s)
  end

  streams = [m(ovideo), m(oaudio)]

  expected = [
    [("pix_fmt", "yuv420p"), ("codec_name", "h264")],
    m(iaudio)
  ]

  if streams == expected then
    test.pass()
  else
    test.fail()
  end
end

output.file(fallible=true, on_stop=on_done, %ffmpeg(format="mkv",%audio.copy,%video.raw(codec="libx264")), out, s)
