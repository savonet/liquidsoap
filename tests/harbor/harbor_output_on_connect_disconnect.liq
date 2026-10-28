%include "../test.liq"

# The disconnect record matches the connect record, carries the bytes the
# client received, response head included, and the listener is already gone
# from `listeners()` inside a synchronous disconnect handler. A raising handler
# stops neither the other handlers nor the teardown.
port = 18104
o = output.harbor(port=port, mount="cb.mp3", %mp3, sine())

connected_id = ref(-1)
connected_at = ref(0.)
received = ref(0)
others = ref(0)

o.on_connect(synchronous=true, fun (_) -> error.raise(error.failure, "boom"))
o.on_connect(
  synchronous=true,
  fun (l) ->
    begin
      atomic(fun () -> others := others() + 1)
      connected_id := l.id
      connected_at := l.connected_at
      test.equal(l.bytes_sent, 0)
      test.equal(l.uri, "/cb.mp3")
      test.equal(l.ip, "127.0.0.1")
      test.equal(l.protocol, "1.1")
      test.equal(list.assoc("x-test", l.headers), "Value")
    end
)
o.on_disconnect(synchronous=true, fun (_) -> error.raise(error.failure, "boom"))
o.on_disconnect(
  synchronous=true,
  fun (l) ->
    begin
      atomic(fun () -> others := others() + 1)
      test.equal(l.id, connected_id())
      test.equal(l.connected_at, connected_at())
      test.equal(l.bytes_sent, received())
      test.equal(l.bytes_sent > 1000, true)
      test.equal(l.duration > 0., true)
      test.equal(o.listeners(), [])
      test.equal(others(), 2)
      test.pass()
    end
)

def check() =
  sock = socket.unix()
  sock.connect(
    socket.address.internet_address(socket.internet_address("127.0.0.1"), port)
  )
  sock.write(
    "GET /cb.mp3 HTTP/1.1\r\nX-Test: Value\r\n\r\n"
  )
  def rec read() =
    chunk =
      try
        sock.read(timeout=5.)
      catch _ do
        ""
      end
    received := received() + string.length(encoding="ascii", chunk)
    if chunk != "" then read() end
  end
  thread.run(
    delay=2.,
    fun () ->
      begin
        test.equal(list.length(o.listeners()), 1)
        o.stop()
      end
  )
  read()
  sock.close()
end

o.on_start(synchronous=true, fun () -> thread.run(delay=0.5, check))
