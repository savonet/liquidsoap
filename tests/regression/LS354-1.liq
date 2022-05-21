s = blank(duration=1.)

s.on_track(
     fun(_)-> begin test.pass(); shutdown() end
)

r = ref(false)
d = source.dynamic({ if !r then s else null() end })

output.dummy(mksafe(d))

thread.run(delay=2., {r:=true})
thread.run(delay=4., {test.fail(); shutdown()})
