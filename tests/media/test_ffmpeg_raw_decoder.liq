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
  ojson = get_process_output("ffprobe -v quiet -print_format json -show_streams '#{out}'")

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
