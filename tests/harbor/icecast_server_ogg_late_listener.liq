%include "../test.liq"

# A listener joining an Ogg/Opus mount once its burst has moved past the
# stream's start gets the stream headers first, and no ICY metadata even when
# it asks for it.
def f() =
  port = 3551
  burst = 4096
  server = icecast.server({port = port, burst_size = burst})

  output.icecast(
    %ogg(%opus),
    host="localhost",
    port=port,
    password="hackme",
    mount="radio.opus",
    sine()
  )

  checks = ref(0)
  def check(label, cond) =
    checks := checks() + 1
    if not cond then test.fail(label) end
  end

  def bytes_read() =
    list.fold(
      fun (n, m) -> m.mount == "/radio.opus" ? m.bytes_read : n,
      0,
      server.stats().mounts
    )
  end

  # The listener joins once the stream's start is out of the burst.
  def wait_past_burst() =
    deadline = time() + 30.
    def rec wait() =
      if
        bytes_read() <= 8 * burst
      then
        if
          time() > deadline
        then
          test.fail(
            "the source never sent past the burst: #{bytes_read()} bytes"
          )
        end
        thread.pause(0.1)
        wait()
      end
    end

    wait()
  end

  def listen() =
    sock = socket.unix()
    try
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
        chunk =
          try
            sock.read(timeout=5.)
          catch err do
            test.fail(
              "reading the stream failed: #{err.message}"
            )
            ""
          end
        if
          chunk == ""
        then
          test.fail(
            "connection closed early"
          )
        end
        data := data() ^ chunk

      end
      data()
    finally
      sock.close()
    end
  end

  def run() =
    wait_past_burst()
    d = listen()
    header_end = string.index(substring="\r\n\r\n", d)
    headers =
      string.case(
        lower=true,
        string.sub(encoding="ascii", d, start=0, length=header_end)
      )
    check(
      "no icy-metaint for an Ogg mount: #{headers}",
      not string.contains(encoding="ascii", substring="icy-metaint", headers)
    )
    body = string.sub(encoding="ascii", d, start=header_end + 4, length=64)
    check(
      "the late listener's stream starts with an Ogg page",
      string.sub(encoding="ascii", body, start=0, length=4) == "OggS"
    )
    check(
      "the late listener gets the Opus header first",
      string.contains(encoding="ascii", substring="OpusHead", body)
    )
    if
      checks() != 3
    then
      test.fail(
        "#{checks()} checks ran, expected 3"
      )
    end
    test.pass()
  end

  server.on_source_connect(fun (_) -> thread.run(run))
end

test.check(f)
