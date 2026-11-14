%include "../test.liq"

# The webcast protocol over websocket: close codes, ping, close echo and
# metadata handling of input.harbor.
port = 18141
s = input.harbor(port=port, user="source", password="hackme", "mount")
output.dummy(fallible=true, s)

def byte(n) =
  string.binary.of_int(pad=1, n)
end

# Unmasked frame with a payload shorter than 126 bytes.
def ws_frame(opcode, payload) =
  byte(128 + opcode) ^ byte(string.bytes.length(payload)) ^ payload
end

def text(payload) =
  ws_frame(1, payload)
end

def connect() =
  sock = socket.unix()
  sock.connect(
    socket.address.internet_address(socket.internet_address("127.0.0.1"), port)
  )
  sock.write(
    "GET /mount HTTP/1.1\r\nHost: localhost\r\nUpgrade: websocket\r\n" ^
      "Connection: Upgrade\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n" ^
      "Sec-WebSocket-Version: 13\r\nSec-WebSocket-Protocol: webcast\r\n\r\n"
  )
  test.equal(
    string.contains(
      substring=" 101 ",
      sock.read(timeout=5.)
    ),
    true
  )
  sock
end

# Reads one server frame: its opcode, and its close code when it is a close
# frame.
def read_frame(sock) =
  data =
    try
      sock.read(timeout=5.)
    catch _ do
      ""
    end
  if
    string.bytes.length(data) < 2
  then
    {opcode = -1, code = -1, payload = ""}
  else
    opcode = string.nth(data, 0) mod 16
    length = string.nth(data, 1) mod 128
    code =
      opcode == 8 and length >= 2
        ? string.nth(data, 2) * 256 + string.nth(data, 3)
        : -1
    {
      opcode = opcode,
      code = code,
      payload = string.sub(encoding="ascii", data, start=2, length=length)
    }
  end
end

hello =
  text(
    '{"type":"hello","data":{"mime":"audio/mpeg","user":"source","password":"hackme"}}'
  )

def expect_close(name, messages, code) =
  sock = connect()
  list.iter(fun (message) -> sock.write(message), messages)
  reply = read_frame(sock)
  sock.close()
  if
    reply.opcode != 8 or reply.code != code
  then
    print(
      "#{name}: expected close #{code}, got opcode #{reply.opcode} code #{
        reply.code
      }"
    )
    test.fail()
  end
end

def check() =
  expect_close(
    "missing user",
    [text('{"type":"hello","data":{"mime":"audio/mpeg","password":"hackme"}}')],
    4401
  )
  expect_close(
    "wrong password",
    [
      text(
        '{"type":"hello","data":{"mime":"audio/mpeg","user":"source","password":"wrong"}}'
      )
    ],
    4401
  )
  expect_close(
    "invalid hello",
    [
      text(
        "not json"
      )
    ],
    4400
  )

  # A ping is answered, and metadata with non-string values or unknown message
  # types keep the connection open.
  live = connect()
  live.write(hello)
  live.write(
    text('{"type":"metadata","data":{"title":"Song","track":3,"live":true}}')
  )
  live.write(text('{"type":"unknown"}'))
  live.write(ws_frame(9, "beat"))
  pong = read_frame(live)
  test.equal(pong.opcode, 10)
  test.equal(pong.payload, "beat")

  # The mount is in use while this source streams.
  expect_close(
    "mount taken",
    [hello],
    4409
  )

  # A malformed message closes the connection.
  live.write(text("{"))
  test.equal(read_frame(live).code, 4400)
  live.close()
  thread.pause(1.)

  expect_close(
    "repeated hello",
    [hello, hello],
    4400
  )
  thread.pause(1.)

  # A client close is echoed.
  expect_close(
    "client close",
    [hello, ws_frame(8, byte(3) ^ byte(232))],
    1000
  )

  test.pass()
end

thread.run(delay=2., check)
