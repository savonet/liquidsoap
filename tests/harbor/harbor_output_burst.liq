%include "../test.liq"

# A joiner gets the burst at once, more than one second of a 128 kbps stream;
# with no burst, a joiner still streams.
port = 18109
o = output.harbor(port=port, mount="burst.mp3", %mp3(bitrate = 128), sine())
plain =
  output.harbor(
    port=port,
    mount="plain.mp3",
    burst=null,
    %mp3(bitrate = 128),
    sine()
  )

# Bytes received within `duration` seconds, response head included.
def received(mount, duration) =
  sock = socket.unix()
  sock.connect(
    socket.address.internet_address(socket.internet_address("127.0.0.1"), port)
  )
  sock.write(
    "GET /#{mount} HTTP/1.0\r\n\r\n"
  )
  start = time()
  total = ref(0)
  def rec read() =
    left = duration - (time() - start)
    if
      left > 0.
    then
      chunk =
        try
          sock.read(timeout=left)
        catch _ do
          ""
        end
      total := total() + string.length(encoding="ascii", chunk)
      if chunk != "" then read() end
    end
  end
  read()
  sock.close()
  total()
end

def check() =
  burst = received("burst.mp3", 1.)
  if
    burst < 30000
  then
    test.fail(
      "burst too short: #{burst}"
    )
  end
  streamed = received("plain.mp3", 3.)
  if
    streamed < 16000
  then
    test.fail(
      "no burst stream too short: #{streamed}"
    )
  end
  if
    streamed > 3 * 16000 + 8000
  then
    test.fail(
      "no burst stream got a burst: #{streamed}"
    )
  end
  test.pass()
end

o.on_start(synchronous=true, fun () -> thread.run(delay=4., check))
