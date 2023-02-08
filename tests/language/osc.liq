#!../../liquidsoap ../test.liq

success = ref(false)

settings.oscnative.port.set(7778)

def f() =
  # Test lo
  success := false
  osc.on_float("/bla", fun(x) -> if x == 42. then print("got float"); success := true end)
  osc.send_float(host="localhost", port=settings.osc.port(), "/bla", 42.)
  sleep(1.)
  if not success() then test.fail() end

  # Test native OSC
  success := false
  osc.native.on_float("/bla", fun(x) -> if x == 42. then print("got float"); success := true end)
  osc.native.send_float(host="localhost", port=settings.oscnative.port(), "/bla", 42.)
  sleep(1.)
  if not success() then test.fail() end

  success := false
  osc.native.on_string("/bla", fun(x) -> if x == "xxx" then print("got string"); success := true end)
  osc.native.send_string(host="localhost", port=settings.oscnative.port(), "/bla", "xxx")
  sleep(1.)
  if not success() then test.fail() end

  test.pass()
end

test.check(f)
