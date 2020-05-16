#!../../src/liquidsoap ../../libs/pervasives.liq
%include "test.liq"

set("log.level",4)
set("frame.audio.samplerate",48000)

file = argv(default="",1)
out = "files/test.wav"

if file.exists(out) then
  file.unlink(out)
end

digests = [
  ("files/@wav(mono).wav", "792e343a2772b8c67bc0d03ec60fe734"),
  ("files/@wav(stereo).wav", "792e343a2772b8c67bc0d03ec60fe734")
] 

s = (single(file):source(2,0,0))

s = once(s)

clock.assign_new(sync='none',[s])

def on_done () =
  recorded_digest = list.assoc(default="", file, digests)
  pass_digest =
    if recorded_digest != "" then
      recorded_digest == file.digest(out)
    else
      true
    end

  json = get_process_output("ffprobe -v quiet -print_format json -show_streams #{out}")

  int_format = of_json(default=[("streams", [[("channels", 0)]])], json)
  stream = list.hd(default=[], list.assoc(default=[], "streams", int_format))
  channels = list.assoc(default=0,"channels",stream)

  string_format = of_json(default=[("streams", [[("samplerate", "0")]])], json)
  stream = list.hd(default=[], list.assoc(default=[], "streams", string_format))
  samplerate = list.assoc(default="0","sample_rate",stream)

  if pass_digest and channels == 2 and samplerate == "48000" then
    test.pass()
  else
    test.fail()
  end
end

output.file(fallible=true, on_stop=on_done, %wav(stereo), out, s)
