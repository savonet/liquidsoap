s1 = source.fail(id="fail")

s2 = blank(id="blank", duration=1.)

s2.on_track(synchronous=true, fun (_) -> test.pass())

r = ref(0)

d = source.dynamic({if r() == 1 then s1 elsif r() == 2 then s2 else null end})

output.dummy(mksafe(d))

thread.run(delay=2., {r := 1})

thread.run(delay=3., {r := 2})

thread.run(
  delay=5.,
  {
    test.fail()
    shutdown()
  }
)
