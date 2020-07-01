#!../../src/liquidsoap ../../libs/pervasives.liq

%include "test.liq"

s =
if file.which(get(default="???","protocol.gtts.path")) == "" and file.which(get(default="???","protocol.text2wave.path")) == "" then
  print("Could not test gtts")
  sine(duration=1.)
else
  single("say:Hello, world!")
end

n = ref(0)
def f(_)
  if !n < 1 then
    n := !n + 1
  else
    test.pass(); shutdown()
  end
end
s = on_track(f, s)

output.dummy(s)
