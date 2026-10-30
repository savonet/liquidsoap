%include "../test.liq"

# Listeners of an `icecast.server` mount get the source's stream identity as
# `icy-*` headers and a burst by default, as on Icecast; `<max-listeners>`
# refuses a listener past the limit with 503.
port = 18124

def f() =
  dir = file.temp_dir("icecast", "listeners")
  config = path.concat(dir, "icecast.xml")
  file.write(
    data="<icecast><authentication><source-password>hackme</source-password></authentication><mount><mount-name>/radio.mp3</mount-name><max-listeners>1</max-listeners></mount></icecast>",
    config
  )
  server = icecast.server(port=port, config=config)

  output.icecast(
    %mp3(bitrate = 128),
    host="localhost",
    port=port,
    password="hackme",
    mount="radio.mp3",
    name="Test Radio",
    genre="Jazz",
    sine()
  )

  def connect() =
    sock = socket.unix()
    sock.connect(
      socket.address.internet_address(
        socket.internet_address("127.0.0.1"),
        port
      )
    )
    sock.write(
      "GET /radio.mp3 HTTP/1.0\r\n\r\n"
    )
    sock
  end

  # Bytes received within `duration` seconds.
  def receive(sock, duration) =
    start = time()
    chunks = ref([])
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
        if
          chunk != ""
        then
          chunks := chunk::chunks()
          read()
        end
      end
    end
    read()
    string.concat(list.rev(chunks()))
  end

  def check() =
    first = connect()
    d = receive(first, 1.)
    header_end = string.index(substring="\r\n\r\n", d)
    head =
      string.case(
        lower=true,
        string.sub(encoding="ascii", d, start=0, length=header_end)
      )
    test.equal(
      string.contains(
        encoding="ascii",
        substring="icy-name: test radio",
        head
      ),
      true
    )
    test.equal(
      string.contains(
        encoding="ascii",
        substring="icy-genre: jazz",
        head
      ),
      true
    )
    body = string.length(encoding="ascii", d) - header_end - 4
    if
      body < 30000
    then
      test.fail(
        "no burst: #{body} bytes in one second"
      )
    end

    second = connect()
    refused = receive(second, 2.)
    second.close()
    test.equal(
      string.sub(encoding="ascii", refused, start=0, length=12),
      "HTTP/1.0 503"
    )
    first.close()
    test.pass()
  end

  server.on_source_connect(fun (_) -> thread.run(delay=5., check))
end

test.check(f)
