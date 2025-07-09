s = sine()

is_ready = ref(true)
is_done = ref(false)

s = switch([(is_ready, s)])

o = output.dummy(fallible=true, s)
o.on_stop({if not is_done() then test.fail() end})

thread.run(delay=0.1, {is_ready := false})
thread.run(
  delay=1.,
  {
    is_done := true
    test.pass()
  }
)
