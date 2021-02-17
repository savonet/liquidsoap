#!../../src/liquidsoap -i ../../libs/pervasives.liq

# Basic radio test

%include "test.liq"

day     = playlist("~/Music")
night   = playlist("~/Music")
jingles = playlist("~/Music")

# Day / night switch
radio = switch([({8h-20h}, day), ({20h-8h}, night)])
# Crossfade
radio = crossfade(fade_out=3., fade_in=3., duration=5., radio)
# Add jingles
radio = random(weights=[1, 4], [jingles, radio])

output.dummy(fallible=true, radio)

def on_done () =
  test.pass()
  shutdown()
end

thread.run(delay=1., on_done)
