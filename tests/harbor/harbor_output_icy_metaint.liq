%include "../test.liq"

# ICY blocks fall exactly every `metaint` stream bytes: the first one carries
# the title, the next one is empty since the title did not change.
port = 18101
s = insert_metadata(sine())

o = output.harbor(port=port, mount="icy.mp3", metaint=1024, burst=null, %mp3, s)

def check() =
  sock = socket.unix()
  sock.connect(
    socket.address.internet_address(socket.internet_address("127.0.0.1"), port)
  )
  sock.write(
    "GET /icy.mp3 HTTP/1.0\r\nIcy-MetaData: 1\r\n\r\n"
  )
  data = ref("")
  while string.length(encoding="ascii", data()) < 4096 do
    chunk = sock.read(timeout=5.)
    if
      chunk == ""
    then
      test.fail(
        "connection closed early"
      )
    end
    data := data() ^ chunk

  end
  sock.close()
  d = data()
  header_end = string.index(substring="\r\n\r\n", d)
  head =
    string.case(
      lower=true,
      string.sub(encoding="ascii", d, start=0, length=header_end)
    )
  test.equal(
    string.contains(
      encoding="ascii",
      substring="icy-metaint: 1024",
      head
    ),
    true
  )
  body =
    string.sub(
      encoding="ascii",
      d,
      start=header_end + 4,
      length=4096 - header_end - 4
    )
  test.equal(string.nth(body, 1024), 2)
  block = string.sub(encoding="ascii", body, start=1025, length=32)
  test.equal(
    string.contains(
      encoding="ascii",
      substring="StreamTitle='icy test';",
      block
    ),
    true
  )
  test.equal(string.nth(body, 1024 + 33 + 1024), 0)
  test.pass()
end

o.on_start(
  synchronous=true,
  fun () ->
    begin
      s.insert_metadata(
        [
          (
            "title",
            "icy test"
          )
        ]
      )
      thread.run(delay=1., check)
    end
)
