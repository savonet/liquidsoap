sa = sine()

sb_on = ref(false)
sb = switch([({!sb_on}, once(sine(220.,duration=2.)))])
thread.run(delay=1., {sb_on := true})

output.dummy(smooth_add(p=0.3, normal=sa, special=sb))

thread.run(delay=4., {test.pass(); shutdown()})
