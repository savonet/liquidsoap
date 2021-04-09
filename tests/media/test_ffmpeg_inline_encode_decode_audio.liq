#!../../src/liquidsoap ../../libs/main.liq
%include "../test.liq"

set("log.level",5)

set("decoder.decoders",["FFMPEG"])

out_copy = "copy.mp4"

if file.exists(out_copy) then
  file.remove(out_copy)
end

out_encode = "encode.mp4"
if file.exists(out_encode) then
  file.remove(out_encode)
end

s = once(blank(duration=10.))

clock.assign_new(sync='none',[s])

def on_done () =
  def check(out) =
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
    ]
    
    streams == expected
  end

  if check(out_copy) and check(out_encode) then
    test.pass()
  else
    test.fail()
  end
end

s = ffmpeg.encode.audio(
  %ffmpeg(
    %audio(codec="aac")
  ),
  s
)

output.file(
  fallible=true,
  %ffmpeg(
    %audio.copy,
  ),
  out_copy,
  s
)

s = ffmpeg.decode.audio(s)

output.file(
  fallible=true,
  on_stop=on_done,
  %ffmpeg(
    %audio(codec="aac")
  ),
  out_encode,
  s
)
