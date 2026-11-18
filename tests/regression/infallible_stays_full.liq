%include "../test.liq"

# Operators found infallible at start stay so: over children that end
# tracks inside frames and come and go, every frame is full. The outputs are
# not fallible, so a single short frame fails the clock.

def tracks(duration) =
  source.dynamic(track_sensitive=true, {sine(duration=duration)})
end

# Available on some frames only.
frames = ref(0)
def gate(~period, s) =
  switch([({frames() mod period < period / 2}, s)])
end

fallback_over_gated =
  fallback([gate(period=7, tracks(0.031)), gate(period=4, sine(220.)), sine()])

rotation =
  rotate(
    [gate(period=5, tracks(0.047)), tracks(0.013).{weight = 2}, sine(330.)]
  )

steps = sequence([sine(duration=0.053), tracks(0.017), sine(550.)])
merged =
  rotate.merge([gate(period=3, tracks(0.029)), tracks(0.011), sine(660.)])

fallback_over_gated.on_frame(synchronous=true, fun () -> ref.incr(frames))

# Every operator has to have gone through its share of frames.
frames_wanted = 1000
still_playing = ref(4)

def play(s) =
  played = ref(0)
  def on_frame() =
    ref.incr(played)
    if
      played() == frames_wanted
    then
      still_playing := still_playing() - 1
      if still_playing() == 0 then test.pass() end
    end
  end

  s.on_frame(synchronous=true, on_frame)
  clock.assign_new(sync="none", [s])
  output.dummy(s)
end

list.iter(play, [fallback_over_gated, rotation, steps, merged])
