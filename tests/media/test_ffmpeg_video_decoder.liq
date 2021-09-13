#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq
%include "test.liq"

log.level.set(5)
audio.samplerate.set(48000)
video.frame.rate.set(45)
settings.decoder.decoders.set(["FFMPEG"])

fname = argv(default="",1)
out = "files/test_ffmpeg_video_decoder.mp4"

if file.exists(out) then
  file.remove(out)
end

s = single(fname)

s = once(s)

clock.assign_new(sync='none',[s])

def on_done () =
  j = process.read("ffprobe -v quiet -print_format json -show_streams #{out}")

  string_format = json.parse(default=[("streams", [[("samplerate", "0")]])], j)
  stream = list.hd(default=[], list.assoc(default=[], "streams", string_format))
  framerate = list.assoc(default="0","r_frame_rate",stream)
  codec = list.assoc(default="0","codec_name",stream)

  if framerate == "45/1" and codec == "h264" then
    test.pass()
  else
    test.fail()
  end
end

output.file(fallible=true, on_stop=on_done, %ffmpeg(format="mp4",%audio.none,%video(codec="libx264")), out, s)
