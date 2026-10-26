%include "../test.liq"

# At 882-sample frames, a history offset shared by the channels diverges on stereo.
settings.frame.audio.samplerate := 44100
settings.frame.duration := 0.02
pending = ref(2)
failed = ref(false)

def check(frequency) =
  s = once(sine(amplitude=0.5, duration=3., 1000.))
  s = filter.iir.butterworth.high(frequency=frequency, s)
  s = peak(duration=settings.frame.duration(), s)
  max_peak = ref(0.)
  frames = ref(0)
  s.on_frame(
    synchronous=true,
    fun () ->
      begin
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
          "#{frequency} Hz: frames: #{frames()}, max peak: #{max_peak()}"
        )
        if frames() <= 100 or max_peak() >= 0.7 then failed := true end
        pending := pending() - 1
        if
          pending() == 0
        then
          if failed() then test.fail() else test.pass() end
        end
      end
  )
end

check(30.)
check(300.)
