#!../../liquidsoap ../test.liq

sucess = ref(false)

def f() =
  sucess := false
  osc.native.on_float("/bla", fun(x) -> if x == 42. then print("got float"); sucess := true end)
  osc.native.send_float(host="localhost", port=7777, "/bla", 42.)
  sleep(1.)
  if not !sucess then test.fail() end

  sucess := false
  osc.native.on_string("/bla", fun(x) -> if x == "xxx" then print("got string"); sucess := true end)
  osc.native.send_string(host="localhost", port=7777, "/bla", "xxx")
  sleep(1.)
  if not !sucess then test.fail() end

  test.pass()
end

test.check(f)
