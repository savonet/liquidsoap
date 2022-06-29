#!../../liquidsoap ../test.liq

sucess = ref(false)

settings.oscnative.port.set(7778)

def f() =
  # Test lo
  sucess := false
  osc.on_float("/bla", fun(x) -> if x == 42. then print("got float"); sucess := true end)
  osc.send_float(host="localhost", port=settings.osc.port(), "/bla", 42.)
  sleep(1.)
  if not !sucess then test.fail() end

  # Test native OSC
  sucess := false
  osc.native.on_float("/bla", fun(x) -> if x == 42. then print("got float"); sucess := true end)
  osc.native.send_float(host="localhost", port=settings.oscnative.port(), "/bla", 42.)
  sleep(1.)
  if not !sucess then test.fail() end

  sucess := false
  osc.native.on_string("/bla", fun(x) -> if x == "xxx" then print("got string"); sucess := true end)
  osc.native.send_string(host="localhost", port=settings.oscnative.port(), "/bla", "xxx")
  sleep(1.)
  if not !sucess then test.fail() end

  test.pass()
end

test.check(f)
