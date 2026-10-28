%include "../test.liq"

# An encoder output larger than `buffer`, and larger than the stream store,
# does not disconnect a listener that keeps up.
settings.frame.duration := 0.2
port = 18111
o =
  output.harbor(
    port=port,
    mount="large.wav",
    buffer=1000,
    burst=null,
    %wav(samplerate = 192000, stereo),
    sine()
  )

disconnects = ref(0)
o.on_disconnect(synchronous=true, fun (_) -> disconnects := disconnects() + 1)

def check() =
  sock = socket.unix()
  sock.connect(
    socket.address.internet_address(socket.internet_address("127.0.0.1"), port)
  )
  sock.write(
    "GET /large.wav HTTP/1.0\r\n\r\n"
  )
  start = time()
  total = ref(0)
  def rec read() =
    if
      time() - start < 4.
    then
      chunk =
        try
          sock.read(timeout=5.)
        catch _ do
          ""
        end
      if
        chunk == ""
      then
        test.fail(
          "disconnected after #{total()} bytes"
        )
      end
      total := total() + string.length(encoding="ascii", chunk)
      read()
    end
  end
  read()
  sock.close()
  if
    total() < 2 * 768000
  then
    test.fail(
      "only #{total()} bytes"
    )
  end
  test.equal(disconnects() <= 1, true)
  test.pass()
end

o.on_start(synchronous=true, fun () -> thread.run(delay=1., check))
