%include "../test.liq"

settings.frame.audio.samplerate := 44100
pending = ref(0)
failed = ref(false)

def check(name, hz, f, low, high) =
  pending := pending() + 1
  s = once(sine(amplitude=0.5, duration=3., hz))
  s = peak(duration=settings.frame.duration(), f(s))
  max_peak = ref(0.)
  frames = ref(0)
  s.on_frame(
    synchronous=true,
    fun () ->
      if
        source.time(s) > 1.5
      then
        ref.incr(frames)
        p = s.peak()
        max_peak := float.is_nan(p) ? infinity : max(max_peak(), p)
      end
  )

  o = output.dummy(fallible=true, s)
  clock.assign_new(sync="none", [o])
  o.on_stop(
    synchronous=true,
    fun () ->
      begin
        print(
          "#{name}: frames: #{frames()}, max peak: #{max_peak()}"
        )
        if
          frames() < 50 or max_peak() < low or max_peak() > high
        then
          failed := true
        end
        pending := pending() - 1
        if
          pending() == 0
        then
          if failed() then test.fail() else test.pass() end
        end
      end
  )
end

def bandpass(s) =
  filter.iir.resonator.bandpass(frequency=1000., s)
end
def bandstop(s) =
  filter.iir.resonator.bandstop(frequency=1000., s)
end
def allpass(s) =
  filter.iir.resonator.allpass(frequency=1000., s)
end

check(
  "bandpass at its frequency",
  1000.,
  bandpass,
  0.49,
  0.51
)
check(
  "bandstop at its frequency",
  1000.,
  bandstop,
  0.,
  0.01
)
check(
  "bandstop off its frequency",
  100.,
  bandstop,
  0.49,
  0.51
)
check("allpass", 300., allpass, 0.49, 0.51)
