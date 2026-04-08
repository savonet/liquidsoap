# Basic radio test
day = playlist("playlist")
night = playlist("playlist")
jingles = playlist("playlist")

# Day / night switch
radio = switch([({8h-20h}, day), ({20h-8h}, night)])

# Crossfade
radio = crossfade(fade_out=3., fade_in=3., duration=5., radio)

# Add jingles
radio = random([jingles.{weight = 1}, radio.{weight = 4}])
output.dummy(fallible=true, radio)

def on_done() =
  test.pass()
end

thread.run(delay=10., on_done)
