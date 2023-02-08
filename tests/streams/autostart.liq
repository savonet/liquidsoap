test_no_autostart = ref(true)
test_autostart = ref(false)

def on_unwanted_autostart() =
  test_no_autostart := false
end

def on_wanted_autostart() =
  test_autostart := true
end

s = blank()

clock.assign_new(sync="none",[s])

output.dummy(id="no_autostart", start=false, on_start=on_unwanted_autostart, s)
output.dummy(id="autostart", start=true, on_start=on_wanted_autostart, s)

def on_done() =
  if test_no_autostart() and test_autostart() then
    test.pass()
  else
    test.fail()
  end
end

thread.run(delay=1.,on_done)
