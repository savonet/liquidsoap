#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

%include "test.liq"

s =
if file.which(get(default="???","protocol.gtts.path")) == null() and file.which(get(default="???","protocol.text2wave.path")) == null() then
  print("Could not test gtts")
  chop(every=1.,sine())
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
s.on_track(f)

output.dummy(s)
