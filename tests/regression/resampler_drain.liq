%include "../test.liq"

# A writer converting the frame's rate drains its converter at the end: half a
# second at 44.1 kHz written at 48 kHz holds 24000 samples.

def on_close(filename) =
  samples =
    int_of_string(
      default=0,
      string.trim(
        process.read(
          "ffmpeg -v quiet -i #{process.quote(filename)} -f s16le -ac 1 - | wc \
           -c"
        )
      )
    ) /
      2
  print(
    "#{samples} samples"
  )
  if samples == 24000 then test.pass() else test.fail() end
end

s = once(sine(duration=0.5))
clock.assign_new(sync="none", [s])
output.file(
  fallible=true,
  on_close=on_close,
  %wav(samplerate = 48000),
  file.temp(cleanup=true, "resampler_drain", ".wav"),
  s
)
