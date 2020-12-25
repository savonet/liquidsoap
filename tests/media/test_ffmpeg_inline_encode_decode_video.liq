#!../../src/liquidsoap ../../libs/pervasives.liq
%include "test.liq"

set("log.level",5)

set("decoder.decoders",["FFMPEG"])

out_copy = "files/copy.mp4"

if file.exists(out_copy) then
  file.remove(out_copy)
end

out_encode = "files/encode.mp4"
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
      [("pix_fmt", "yuv420p"), ("codec_name", "h264")]
    ]
    
    streams == expected
  end

  if check(out_copy) and check(out_encode) then
    test.pass()
  else
    test.fail()
  end
end

s = ffmpeg.encode.video(
  %ffmpeg(
    %video(codec="libx264")
  ),
  s
)

output.file(
  fallible=true,
  %ffmpeg(
    %video.copy,
  ),
  out_copy,
  s
)

s = ffmpeg.decode.video(s)

output.file(
  fallible=true,
  on_stop=on_done,
  %ffmpeg(
    %video(codec="libx264")
  ),
  out_encode,
  s
)
