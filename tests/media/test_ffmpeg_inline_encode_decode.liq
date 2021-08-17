#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq
%include "test.liq"

log.level.set(5)
settings.decoder.decoders.set(["FFMPEG"])

out_copy = "files/test_ffmpeg_inline_encode_decode_copy.mp4"

if file.exists(out_copy) then
  file.remove(out_copy)
end

out_encode = "files/test_ffmpeg_inline_encode_decode_encode.mp4"
if file.exists(out_encode) then
  file.remove(out_encode)
end

s = once(blank(duration=10.))

todo = ref(2)

def on_done () =
  def check(out) =
    json = process.read("ffprobe -v quiet -print_format json -show_streams '#{out}'")

    format = json.parse(default=[("streams", [[("samplerate", "0")]])], json)

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
    
    streams == expected
  end

  todo := !todo - 1

  if !todo == 0 then
    if check(out_copy) and check(out_encode) then
      test.pass()
    else
      test.fail()
    end
  end
end

s = ffmpeg.encode.audio_video(
  id="encoder",
  %ffmpeg(
    %video(codec="libx264", b="5m"),
    %audio(codec="aac")
  ),
  s
)

output.file(
  fallible=true,
  on_stop=on_done,
  %ffmpeg(
    %video.copy,
    %audio.copy,
  ),
  out_copy,
  s
)

s = ffmpeg.decode.audio_video(id="decoder", s)

clock.assign_new(sync='none',[s])

output.file(
  fallible=true,
  on_stop=on_done,
  %ffmpeg(
    %video(codec="libx264", b="5m"),
    %audio(codec="aac")
  ),
  out_encode,
  s
)
