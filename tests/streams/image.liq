#!../../src/liquidsoap ../../libs/stdlib.liq

%include "test.liq"

s = (single("files/file1.png"):source(audio=none,video=canvas))

output.dummy(fallible=true, s)

def on_done () =
  test.pass()
  shutdown()
end

thread.run(delay=3., on_done)
