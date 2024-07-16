s = sine()

is_ready = ref(true)
is_done = ref(false)

s = switch([(is_ready, s)])

output.dummy(fallible=true, on_stop={if not is_done() then test.fail() end}, s)

thread.run(delay=0.1, {is_ready := false})
thread.run(
  delay=1.,
  {
    is_done := true
    test.pass()
  }
)
