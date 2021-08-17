#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq
%include "test.liq"

log.level.set(5)
audio.samplerate.set(48000)
settings.decoder.priorities.ffmpeg.set(-1)

fname = argv(default="",1)
out = "files/test_stereo.wav"

if file.exists(out) then
  file.remove(out)
end

digests = [
  ("files/@wav(mono).wav", "792e343a2772b8c67bc0d03ec60fe734"),
  ("files/@wav(stereo).wav", "792e343a2772b8c67bc0d03ec60fe734")
] 

s = single(fname)

s = once(s)

clock.assign_new(sync='none',[s])

def on_done () =
  recorded_digest = list.assoc(default="", fname, digests)
  pass_digest =
    if recorded_digest != "" then
      recorded_digest == file.digest(out)
    else
      true
    end

  json = process.read("ffprobe -v quiet -print_format json -show_streams #{out}")

  int_format = json.parse(default=[("streams", [[("channels", 0)]])], json)
  stream = list.hd(default=[], list.assoc(default=[], "streams", int_format))
  channels = list.assoc(default=0,"channels",stream)

  string_format = json.parse(default=[("streams", [[("samplerate", "0")]])], json)
  stream = list.hd(default=[], list.assoc(default=[], "streams", string_format))
  samplerate = list.assoc(default="0","sample_rate",stream)

  if pass_digest and channels == 2 and samplerate == "48000" then
    test.pass()
  else
    test.fail()
  end
end

output.file(fallible=true, on_stop=on_done, %wav(stereo), out, s)
