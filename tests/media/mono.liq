log.level.set(5)
audio.samplerate.set(48000)
settings.audio.converter.samplerate.converters.set(["native"])
settings.decoder.priorities.ffmpeg.set(-1)

fname = argv(default="", 1)
out = {"#{fname}+mono-#{random.int()}.wav"}

s = single(fname)
s = once(s)

done = ref(false)

#clock.assign_new(sync='none', [s])
def on_close(filename) =
  if
    not done()
  then
    done := true

    process.run("sync")

    j =
      process.read(
        "ffprobe -v quiet -print_format json -show_streams #{
          process.quote(filename)
        }"
      )

    let json.parse (parsed :
      {streams: [{channels: int, sample_rate: string}]}
    ) = j

    let [stream] = parsed.streams
    if
      stream.channels == 1 and stream.sample_rate == "48000"
    then
      test.pass()
    else
      test.fail()
    end
  end
end

output.file(fallible=true, on_close=on_close, %wav(mono), out, s)
