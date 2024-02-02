log.level.set(4)

keys = "1234567890*#ABCD"
last_key = ref("")
detected = ref("")

def f(k) =
  # Skip test for now.
  # test.skip()
  print(
    "Detected key #{k}"
  )

  if last_key() != k then detected := !detected ^ k end
  last_key := k

  if
    k == "D"
  then
    if !detected == keys then test.pass() else test.fail() end
  end
end

s = amplify(0.5, dtmf(duration=1., keys))
s = audio.encode.pcm_s16(s)
s = add([s, blank()])
s = audio.decode.pcm_s16(s)

# s = add([s, amplify(0.5, noise())]) # Removed to make the test deterministic
s = dtmf.detect(debug=false, s, f)

clock.assign_new(sync="none", [s])

output.dummy(fallible=true, s)
