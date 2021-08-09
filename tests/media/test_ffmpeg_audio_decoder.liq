#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq
%include "test.liq"

log.level.set(5)
settings.frame.audio.samplerate.set(48000)
settings.decoder.decoders.set(["FFMPEG"])

fname = argv(default="",1)
out = "files/test_ffmpeg_audio_decoder.wav"

if file.exists(out) then
  file.remove(out)
end

s = single(fname)

s = once(s)

clock.assign_new(sync='none',[s])

def on_done () =
  json = process.read("ffprobe -v quiet -print_format json -show_streams #{out}")

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
