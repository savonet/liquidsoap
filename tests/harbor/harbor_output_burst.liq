%include "../test.liq"

# A listener joining a running mount receives the burst on top of the live
# stream: in one second it gets well over the 16 KB/s of 128 kbps mp3.
def f() =
  output.harbor(%mp3, mount="test", port=3476, sine())

  def check() =
    sock = socket.unix()
    sock.connect(
      socket.address.internet_address(
        socket.internet_address("127.0.0.1"),
        3476
      )
    )
    sock.write(
      "GET /test HTTP/1.0\r\n\r\n"
    )
    received = ref(0)
    deadline = time() + 1.
    while received() < 30000 and time() < deadline do
      received := received() + string.length(sock.read(timeout=1.))

    end
    sock.close()
    if
      received() >= 30000
    then
      test.pass()
    else
      test.fail(
        "got #{received()} bytes"
      )
    end
  end

  thread.run(delay=6., check)
end

test.check(f)
