# Verify that burst=null (no burst buffer) still allows listeners to connect.
def f() =
  o = output.harbor(%mp3, mount="test", port=3472, burst=null, sine())
  o.on_connect(synchronous=false, fun (_) -> test.pass())

  l = input.http("http://localhost:3472/test")
  output.dummy(fallible=true, l)
end

test.check(f)
