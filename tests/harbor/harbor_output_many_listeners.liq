# Twelve listeners spread over three writer tasks all connect, and all are
# disconnected when the output stops.
def f() =
  settings.harbor.output.writers := 3
  connected = ref(0)
  disconnected = ref(0)

  o = output.harbor(%mp3, mount="test", port=3475, sine())

  def on_disconnect(_) =
    disconnected := disconnected() + 1
    if disconnected() == 12 then test.pass() end
  end

  def on_connect(_) =
    connected := connected() + 1
    if connected() == 12 then thread.run(delay=1., {o.stop()}) end
  end

  o.on_connect(synchronous=false, on_connect)
  o.on_disconnect(synchronous=false, on_disconnect)

  for _ = 1 to 12 do
    l = input.http("http://localhost:3475/test")
    output.dummy(fallible=true, l)

  end
end

test.check(f)
