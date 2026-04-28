# Two listeners connect simultaneously to a dedicated_encoder=true output.
# Verifies that each gets its own encoder and on_connect fires for both.
def f() =
  count = ref(0)

  o = output.harbor(%mp3, mount="test", port=3471, dedicated_encoder=true, sine())

  def on_connect(_) =
    count := count() + 1
    if count() >= 2 then test.pass() end
  end

  o.on_connect(synchronous=false, on_connect)

  l1 = input.http("http://localhost:3471/test")
  output.dummy(fallible=true, l1)

  l2 = input.http("http://localhost:3471/test")
  output.dummy(fallible=true, l2)
end

test.check(f)
