%include "test.liq"

s1 = fail()
s2 = on_track(
       fun(_)-> begin test.pass(); shutdown() end,
       blank(duration=1.))

r = ref 0
d = source.dynamic({ if !r==1 then [s1]
                     elsif !r==2 then [s2]
                     else [] end })

output.dummy(mksafe(d))

add_timeout(2.,{r:=1; (-1.)})
add_timeout(3.,{r:=2; (-1.)})
add_timeout(5.,{test.fail(); shutdown(); (-1.)})
