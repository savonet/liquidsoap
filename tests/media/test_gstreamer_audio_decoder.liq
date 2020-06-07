#!../../src/liquidsoap ../../libs/pervasives.liq
%include "test.liq"

set("log.level",4)
set("frame.audio.samplerate",48000)

set("decoder.decoders",["GSTREAMER"])

fname = argv(default="",1)
out = "files/test.wav"

if file.exists(out) then
  file.unlink(out)
end

s = (single(fname):source(1,0,0))

s = once(s)

clock.assign_new(sync='none',[s])

def on_done () =
  json = get_process_output("ffprobe -v quiet -print_format json -show_streams #{out}")

  int_format = of_json(default=[("streams", [[("channels", 0)]])], json)
  stream = list.hd(default=[], list.assoc(default=[], "streams", int_format))
  channels = list.assoc(default=0,"channels",stream)

  string_format = of_json(default=[("streams", [[("samplerate", "0")]])], json)
  stream = list.hd(default=[], list.assoc(default=[], "streams", string_format))
  samplerate = list.assoc(default="0","sample_rate",stream)

  if channels == 1 and samplerate == "48000" then
    test.pass()
  else
    test.fail()
  end
end

output.file(fallible=true, on_stop=on_done, %wav(mono), out, s)
