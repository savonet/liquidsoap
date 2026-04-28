def f() =
  connected = ref(false)

  o = output.harbor(%mp3, mount="test", port=3470, sine())

  def on_connect(_) =
    connected := true
    thread.run(delay=1., fun () -> o.stop())
  end

  o.on_connect(synchronous=false, on_connect)
  o.on_disconnect(synchronous=false, fun (_) -> if connected() then test.pass() end)

  l = input.http("http://localhost:3470/test")
  output.dummy(fallible=true, l)
end

test.check(f)
