log.level.set(4)

keys = "1234567890*#ABCD"
detected = ref("")

def f(k)
  # Skip test for now.
  # test.skip()

  print("Detected key #{k}")
  detected := !detected ^ k
  if k == "D" then
    if !detected == keys then
      test.pass()
    else
      test.fail()
    end
    shutdown()
  end
end

s = amplify(0.5, dtmf(duration=1., keys))
# s = add([s, amplify(0.5, noise())]) # Removed to make the test deterministic
s = dtmf.detect(debug=false, s, f)

output.dummy(fallible=true, s)
