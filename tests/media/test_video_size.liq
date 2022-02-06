#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq
%include "test.liq"

log.level.set(5)
audio.samplerate.set(48000)
video.frame.rate.set(45)
settings.decoder.decoders.set(["FFMPEG"])

fname = argv(default="",1)
out = "files/test_videos_size.mp4"

if file.exists(out) then
  file.remove(out)
end

s = (single(fname) : source(video=yuva420p(width=47, height=52)))

s = once(s)

clock.assign_new(sync='none',[s])

def on_done () =
  j = process.read("ffprobe -v quiet -print_format json -show_streams #{out}")

  let json.parse ( parsed: {
    streams: [{
      width: int,
      height: int
    }]
  }) = j

  let [stream] = parsed.streams

  print("Output video size is #{stream.width}x#{stream.height}.")

  if stream.width == 47 and stream.height == 51 then
    test.pass()
  else
    test.fail()
  end
end

output.file(fallible=true, on_stop=on_done, %ffmpeg(format="mp4",%audio.none,%video(codec="libx264")), out, s)
