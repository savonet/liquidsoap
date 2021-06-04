#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq
%include "test.liq"

set("log.level",5)

set("decoder.decoders",["FFMPEG"])

fname = argv(default="",1)
out = "files/test.mp4"

if file.exists(out) then
  file.remove(out)
end

s = single(fname)

s = once(s)

clock.assign_new(sync='none',[s])

def on_done () =
  ijson = process.read("ffprobe -v quiet -print_format json -show_streams '#{fname}'")
  ojson = process.read("ffprobe -v quiet -print_format json -show_streams '#{out}'")

  input_format = of_json(default=[("streams", [[("samplerate", "0")]])], ijson)
  output_format = of_json(default=[("streams", [[("samplerate", "0")]])], ojson)

  input_streams = list.assoc(default=[], "streams", input_format)
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

  input_streams = list.map(m, input_streams)
  output_streams = list.map(m, output_streams)

  if input_streams == output_streams then
    test.pass()
  else
    test.fail()
  end
end

output.file(fallible=true, on_stop=on_done, %ffmpeg(format="mkv",%audio.copy,%video.copy), out, s)
