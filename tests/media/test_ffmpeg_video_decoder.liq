#!../../src/liquidsoap ../../libs/pervasives.liq
%include "test.liq"

set("log.level",4)
set("frame.audio.samplerate",48000)
set("frame.video.framerate",45)

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
  json = process.read("ffprobe -v quiet -print_format json -show_streams #{out}")

  string_format = of_json(default=[("streams", [[("samplerate", "0")]])], json)
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
