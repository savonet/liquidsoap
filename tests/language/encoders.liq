#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

%include "test.liq"

def f() =
  # Test errors for encoder parameters
  try
    enc = %wav(samplesize=123456)
  catch err do
    if err.kind != "encoder" or err.message != "invalid sample size" then
      test.fail()
    end
  end

  # Test encoded or not streams, see #1915.
  encoder =
    if true then
      (%ffmpeg(format="fdkaac", %audio.copy))
    else
      (%ffmpeg(format="mp3", %audio(codec = "libmp3lame", samplerate=44100,b="320k")))
    end

  test.pass()
end

test.check(f)
