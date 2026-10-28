%include "../test.liq"

# Mounts match literally; on a duplicate mount the most recent output serves
# and each output's removal takes only its own route; an output, an HTTP route
# and an input share one port; a stop followed at once by a start serves
# again; an unsupported version gets 505.
port = 18107

first = output.harbor(port=port, mount="dup.mp3", %mp3, sine())
second =
  output.harbor(port=port, mount="dup.mp3", start=false, %mp3, sine(440.))

served_by = ref("")
first.on_connect(synchronous=true, fun (_) -> served_by := "first")
second.on_connect(synchronous=true, fun (_) -> served_by := "second")

harbor.http.register.simple(
  port=port,
  "/route",
  fun (_) -> http.response(data="route")
)
live = input.harbor(port=port, "live")
output.dummy(fallible=true, live)

solo = output.harbor(port=port + 1, mount="solo.mp3", %mp3, sine())

def request(~port=port, path, ~version="HTTP/1.0") =
  sock = socket.unix()
  sock.connect(
    socket.address.internet_address(socket.internet_address("127.0.0.1"), port)
  )
  sock.write(
    "GET #{path} #{version}\r\n\r\n"
  )
  data = ref("")
  def rec read() =
    chunk =
      try
        sock.read(timeout=5.)
      catch _ do
        ""
      end
    data := data() ^ chunk
    if
      chunk != ""
      and not string.contains(encoding="ascii", substring="\r\n\r\n", data())
    then
      read()
    end
  end
  read()
  sock.close()
  string.sub(encoding="ascii", data(), start=9, length=3)
end

# Waits until `cond` holds, failing after five seconds.
def wait(label, cond) =
  def rec loop(n) =
    if
      not cond()
    then
      if
        n == 0
      then
        test.fail(
          "timeout waiting for #{label}"
        )
      end
      sleep(0.1)
      loop(n - 1)
    end
  end
  loop(50)
end

def served(expected) =
  served_by := ""
  test.equal(request("/dup.mp3"), "200")
  wait("callback", fun () -> served_by() != "")
  test.equal(served_by(), expected)
end

def check() =
  test.equal(request("/dupXmp3"), "404")
  test.equal(request("/dup.mp3x"), "404")
  test.equal(request("/route"), "200")
  test.equal(request("/dup.mp3", version="ICE/1.0"), "505")
  test.equal(request("/dup.mp3", version="HTTP/2.0"), "505")
  served("first")

  second.start()
  wait(
    "second start",
    second.is_started
  )
  sleep(0.5)
  served("second")

  second.stop()
  wait(
    "second stop",
    fun () -> not second.is_started()
  )
  sleep(0.5)
  served("first")

  second.start()
  wait(
    "second restart",
    second.is_started
  )
  sleep(0.5)
  first.stop()
  wait(
    "first stop",
    fun () -> not first.is_started()
  )
  sleep(0.5)
  served("second")
  test.equal(request("/route"), "200")

  second.stop()
  second.start()
  wait(
    "immediate restart",
    second.is_started
  )
  sleep(0.5)
  served("second")

  test.equal(request(port=port + 1, "/solo.mp3"), "200")
  solo.stop()
  solo.start()
  wait(
    "solo restart",
    solo.is_started
  )
  sleep(0.5)
  test.equal(request(port=port + 1, "/solo.mp3"), "200")
  solo.stop()
  wait(
    "solo stop",
    fun () -> not solo.is_started()
  )
  sleep(0.5)
  solo.start()
  wait(
    "solo start after close",
    solo.is_started
  )
  sleep(0.5)
  test.equal(request(port=port + 1, "/solo.mp3"), "200")
  test.pass()
end

first.on_start(synchronous=true, fun () -> thread.run(delay=0.5, check))
