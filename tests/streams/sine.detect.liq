log.level.set(4)
def f(freq) =
  print("Detected sine at #{freq}Hz.")
  test.pass()
end

s = sine(440.)
s = sine.detect(debug=false, [440.], s, f)
output.dummy(s)
