%include "../test.liq"

# A format's default sample rate is the frame setting in force at start-up:
# printing or comparing the format before the script sets it neither fixes it
# nor fails.

wav = %wav
before = string(wav)
same = wav == %wav
settings.frame.audio.samplerate := 48000
after = string(wav)
print(
  "before=#{before} after=#{after} same=#{same}"
)

def on_close(filename) =
  rate =
    string.sub(encoding="ascii", file.contents(filename), start=24, length=4)
  print(
    "header rate=#{string.quote(rate)}"
  )
  if
    string.contains(substring="samplerate=44100", before)
    and string.contains(substring="samplerate=48000", after)
    and same
    and rate == "\x80\xbb\x00\x00"
  then
    test.pass()
  else
    test.fail()
  end
end

s = once(sine(duration=0.2))
clock.assign_new(sync="none", [s])
output.file(
  fallible=true,
  on_close=on_close,
  wav,
  file.temp(cleanup=true, "encoder_deferred_values", ".wav"),
  s
)
