#!../../liquidsoap ../test.liq

g = file.replaygain("file1.mp3")
print("Replay gain: #{g} dB")
if null.defined(g) and abs(null.get(g) - 7.37) < 0.05 then
  test.pass()
else
  test.fail()
end
