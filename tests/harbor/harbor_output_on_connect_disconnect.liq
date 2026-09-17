def f() =
  connected = ref(null)

  o = output.harbor(%mp3, mount="test", port=3470, sine())

  def on_connect(listener) =
    connected := listener
    thread.run(delay=1., fun () -> o.stop())
  end

  def on_disconnect(listener) =
    let connection = null.get(connected())
    in
    test.equal(listener.id, connection.id)
    test.equal(listener.ip, "127.0.0.1")
    test.equal(listener.uri, "/test")
    test.equal(connection.bytes_sent, 0)
    test.equal(listener.bytes_sent > 0, true)
    test.equal(listener.duration > 0., true)
    test.equal(listener.connected_at, connection.connected_at)
    test.equal(o.listeners(), [])
    test.pass()
  end

  o.on_connect(synchronous=true, on_connect)
  o.on_disconnect(synchronous=true, on_disconnect)

  l = input.http("http://localhost:3470/test")
  output.dummy(fallible=true, l)
end

test.check(f)
