# A switch must re-evaluate its selection when it resumes after not being
# streamed: `jingles` becomes available while `programs` is on the air, so
# `autodj` has to hand over to it as soon as it plays again.

def tagged(~every, ~title, s) =
  n = ref(0)
  chop(
    every=every,
    metadata={
      ref.incr(n)
      [("title", "#{title}#{n()}")]
    },
    s
  )
end

# Long tracks, so waiting one out is unmistakable.
music = tagged(every=2., title="music", sine(440.))

jingle_on = ref(false)
jingles =
  switch(
    id="jingles",
    [({jingle_on()}, tagged(every=0.3, title="jingle", sine(660.)))]
  )

autodj = fallback(id="autodj", [jingles, music])

programs_on = ref(false)
programs =
  switch(
    id="programs",
    [({programs_on()}, tagged(every=0.3, title="program", sine(220.)))]
  )

radio = fallback(id="radio", [programs, autodj.{track_sensitive = true}])

resumed_at = ref(-1.)

def check(m) =
  if
    resumed_at() > 0. and string.contains(prefix="jingle", m["title"])
  then
    elapsed = time() - resumed_at()
    resumed_at := -1.
    if
      elapsed < 0.5
    then
      test.pass()
    else
      print(
        "jingle only played #{elapsed}s after resuming"
      )
      test.fail()
    end
  end
end

radio.on_metadata(synchronous=true, check)

output.dummy(fallible=true, radio)

thread.run(delay=1., {programs_on := true})

thread.run(delay=2.5, {jingle_on := true})
thread.run(
  delay=4.,
  {
    programs_on := false
    resumed_at := time()
  }
)
thread.run(delay=10., {test.fail()})
