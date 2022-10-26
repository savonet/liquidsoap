#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq
%include "test.liq"

log.level.set(5)
audio.samplerate.set(48000)
settings.decoder.priorities.ffmpeg.set(-1)

fname = argv(default="",1)
out = "files/test_mono.wav"

if file.exists(out) then
  file.remove(out)
end

digests = [
  ("files/@wav(mono).wav", "64ffd09001bee5fc98e0dbaf0aff983f"),
  ("files/@wav(stereo).wav", "64ffd09001bee5fc98e0dbaf0aff983f")
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

  j = process.read("ffprobe -v quiet -print_format json -show_streams #{out}")

  let json.parse ( parsed: {
    streams: [{
      channels: int,
      sample_rate: string
    }]
  }) = j

  let [stream] = parsed.streams

  if pass_digest and stream.channels == 1 and stream.sample_rate == "48000" then
    test.pass()
  else
    test.fail()
  end
end

output.file(fallible=true, on_stop=on_done, %wav(mono), out, s)
