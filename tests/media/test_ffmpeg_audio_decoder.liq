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
  j = process.read("ffprobe -v quiet -print_format json -show_streams #{out}")

  let json.parse ( parsed: {
    streams: [{
      channels: int,
      sample_rate: string
    }]
  }) = j

  let [stream] = parsed.streams

  if stream.channels == 1 and stream.sample_rate == "48000" then
    test.pass()
  else
    test.fail()
  end
end

output.file(fallible=true, on_stop=on_done, %wav(mono), out, s)
