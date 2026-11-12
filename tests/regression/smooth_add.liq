%include "../test.liq"

# `smooth_add` plays silence in place of its special source until that source
# is available, and must then switch to it right away: silence has no end of
# track to wait for.

special_on = ref(false)
special = switch([({special_on()}, sine(220.))])

played = ref(false)
special.on_track(synchronous=true, fun (_) -> played := true)

output.dummy(smooth_add(normal=sine(), special=special))

thread.run(delay=1., {special_on := true})
thread.run(delay=3., {if played() then test.pass() else test.fail() end})
