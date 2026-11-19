%include "../test.liq"

# A transition function that raises does not stop the stream: the error is
# logged and the crossing plays its two sources in order, like one whose
# transition has nothing to deliver. No sample is lost and later crossings
# are unaffected.

rate = float(settings.frame.audio.samplerate())

def samples(seconds) =
  int(seconds * rate + 0.5)
end

calls = ref(0)

def transition(ending, starting) =
  ref.incr(calls)
  if
    calls() == 1
  then
    error.raise(
      error.failure,
      "transition failed"
    )
  end
  add(normalize=false, [ending.source, starting.source])
end

s =
  cross(
    duration=1.,
    deduplicate=false,
    transition,
    sequence([sine(duration=3.), sine(duration=3.), sine(duration=3.)])
  )

out = file.temp(cleanup=true, "cross", ".wav")
clock.assign_new(sync="none", [s])
o = output.file(fallible=true, %wav(stereo = true), out, s)
o.on_stop(
  synchronous=true,
  fun () ->
    begin
      test.equal(calls(), 2)

      # 9 s of tracks, one second played together in the second crossing.
      test.equal((file.size(out) - 44) / 4, samples(8.))
      test.pass()
    end
)
