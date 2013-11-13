s = on_track(
     fun(_)-> begin print("TEST PASSED") shutdown() end,
     blank(duration=1.))

r = ref false
d = source.dynamic({ if !r then [s] else [] end })

output.dummy(mksafe(d))

add_timeout(2.,{r:=true;(-1.)})
add_timeout(4.,{print("TEST FAILED");shutdown();(-1.)})
