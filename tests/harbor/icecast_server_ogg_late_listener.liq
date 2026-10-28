%include "../test.liq"

# A listener joining an Ogg/Opus mount after its burst no longer holds the
# stream's start gets the stream headers first, and no ICY metadata even when
# it asks for it.
def f() =
  port = 3551
  dir = file.temp_dir("icecast", "ogg")
  config = path.concat(dir, "icecast.xml")
  file.write(
    data="<icecast><authentication><source-password>hackme</source-password></authentication><limits><burst-size>4096</burst-size></limits></icecast>",
    config
  )
  server = icecast.server(port=port, config=config)

  output.icecast(
    %ogg(%opus),
    host="localhost",
    port=port,
    password="hackme",
    mount="radio.opus",
    sine()
  )

  def check() =
    sock = socket.unix()
    sock.connect(
      socket.address.internet_address(
        socket.internet_address("127.0.0.1"),
        port
      )
    )
    sock.write(
      "GET /radio.opus HTTP/1.0\r\nIcy-MetaData: 1\r\n\r\n"
    )
    data = ref("")
    while string.length(encoding="ascii", data()) < 8000 do
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
    headers =
      string.case(
        lower=true,
        string.sub(encoding="ascii", d, start=0, length=header_end)
      )
    test.equal(
      string.contains(encoding="ascii", substring="icy-metaint", headers),
      false
    )
    body = string.sub(encoding="ascii", d, start=header_end + 4, length=64)
    test.equal(string.sub(encoding="ascii", body, start=0, length=4), "OggS")
    test.equal(
      string.contains(encoding="ascii", substring="OpusHead", body),
      true
    )
    test.pass()
  end

  server.on_source_connect(fun (_) -> thread.run(delay=4., check))
end

test.check(f)
