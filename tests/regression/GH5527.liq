%include "../test.liq"

# The old track ends inside a frame: its tail in that frame must stay faded.
s = sequence([sine(duration=1.013, 440.), once(blank(duration=1.))])
s = fade.out(duration=0.5, type="lin", track_sensitive=true, s)
s = peak(duration=settings.frame.duration(), s)

max_peak = ref(0.)
frames = ref(0)

s.on_frame(
  synchronous=true,
  before=false,
  fun () ->
    begin
      t = source.time(s)
      if
        t > 0.92 and t < 1.5
      then
        frames := frames() + 1
        max_peak := max(max_peak(), s.peak())
      end
    end
)

o = output.dummy(fallible=true, s)
clock.assign_new(sync="none", [o])

o.on_stop(
  synchronous=true,
  fun () ->
    begin
      print(
        "frames checked: #{frames()}, max peak: #{max_peak()}"
      )
      if
        frames() > 0 and max_peak() < 0.25
      then
        test.pass()
      else
        test.fail()
      end
    end
)
