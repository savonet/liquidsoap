test_no_autostart = ref(true)
test_autostart = ref(false)

def on_unwanted_autostart() =
  test_no_autostart := false
end

def on_wanted_autostart() =
  test_autostart := true
end

s = blank()
clock.assign_new(sync="none", [s])

o = output.dummy(id="no_autostart", start=false, s
o.on_start(synchronous=true, on_unwanted_autostart)

o = output.dummy(id="autostart", start=true, s)
o.on_start(synchronous=true, on_wanted_autostart)


def on_done() =
  if
    test_no_autostart() and test_autostart()
  then
    test.pass()
  else
    test.fail()
  end
end

thread.run(delay=1., on_done)
