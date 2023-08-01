log.level.set(5)
settings.frame.audio.samplerate.set(48000)
settings.decoder.decoders.set(["ffmpeg"])

fname = argv(default="",1)
out = "#{fname}+ffmpeg_audio_decoder.wav"

if file.exists(out) then
  file.remove(out)
end

s = single(fname)

s = sequence([s, s, once(s)])

#clock.assign_new(sync='none',[s])

def on_done () =
  thread.run(delay=0.2, fun () -> begin
    j = process.read("ffprobe -v quiet -print_format json -show_streams #{process.quote(out)}")

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
  end)
end

output.file(fallible=true, on_stop=on_done, %wav(mono), out, s)
