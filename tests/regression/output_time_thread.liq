%include "../test.liq"

# Reading an output's time from another thread used to make its source
# produce and consume data outside the clock's cycle: the output then got a
# partial frame from an infallible source and failed its assertion.

tones =
  [
    once(sine(duration=0.3, 440.)),
    once(sine(duration=0.3, 550.)),
    once(sine(duration=0.3, 660.))
  ]
s = fallback([sequence(tones), blank()])
o = output.file(%wav, file.temp(cleanup=true, "output_time_thread", ".wav"), s)

clock.assign_new(sync="none", [o])

reads = ref(0)
last = ref(0.)
monotonic = ref(true)

def read_time() =
  t = o.time()
  if t < last() then monotonic := false end
  last := t
  reads := reads() + 1
end

thread.run(every=0.01, read_time)

def check() =
  print(
    "reads: #{reads()}, last time: #{last()}"
  )
  if
    reads() > 100 and monotonic() and last() > 5.
  then
    test.pass()
  else
    test.fail()
  end
end

thread.run(delay=3., check)
