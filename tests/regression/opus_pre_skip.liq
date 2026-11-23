%include "../test.liq"

# Native %opus declared a fixed 80 ms (3840 samples) of pre-skip, while the
# encoder only delays by its lookahead (about 6.5 ms): decoders honouring the
# pre-skip dropped about 73 ms of audio at the start of every file.

settings.frame.audio.samplerate := 48000
s = once(sine(duration=1.))
clock.assign_new(sync="none", [s])

def on_close(filename) =
  pre_skip =
    string.trim(
      process.read(
        "ffprobe -v error -show_entries stream=initial_padding -of csv=p=0 #{
          process.quote(filename)
        }"
      )
    )

  print(
    "pre-skip: #{pre_skip}"
  )
  pre_skip = int_of_string(default=-1, pre_skip)
  if 0 < pre_skip and pre_skip < 1000 then test.pass() else test.fail() end
end

output.file(
  fallible=true,
  on_close=on_close,
  %opus(samplerate = 48000),
  file.temp(cleanup=true, "opus_pre_skip", ".opus"),
  s
)
