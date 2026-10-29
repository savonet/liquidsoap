%include "../test.liq"

# An ICY source on a port that already serves HTTP opens port + 1 on the
# running port: an ICY client is answered there, and HTTP clients still are.
port = 18133
harbor.http.register.simple(
  port=port,
  method="GET",
  "/ping",
  fun (_) -> http.response(data="pong")
)
s = input.harbor(port=port, icy=true, password="hackme", "/")
output.dummy(fallible=true, s)

def exchange(port, message) =
  sock = socket.unix()
  sock.connect(
    socket.address.internet_address(socket.internet_address("127.0.0.1"), port)
  )
  sock.write(message)
  reply =
    try
      sock.read(timeout=5.)
    catch _ do
      ""
    end
  sock.close()
  reply
end

def check() =
  test.equal(
    string.contains(prefix="OK2", exchange(port + 1, "hackme\r\n\r\n")),
    true
  )
  test.equal(
    string.contains(
      substring=" 200 ",
      exchange(
        port,
        "GET /ping HTTP/1.0\r\n\r\n"
      )
    ),
    true
  )
  test.pass()
end

thread.run(delay=2., check)
