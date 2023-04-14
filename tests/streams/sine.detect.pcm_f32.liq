log.level.set(4)

def f(freq)
  print("Detected sine at #{freq}Hz.")
  test.pass()
  shutdown()
end

s = sine(440.)
s = (s:source(audio=pcm_s16))
s = audio.decode.pcm_s16(s)
s = sine.detect(debug=false, [440.], s, f)
output.dummy(s)
