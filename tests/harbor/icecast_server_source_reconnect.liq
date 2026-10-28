%include "../test.liq"

# A source leaves and returns on the same mount: the mount's new output serves,
# and the old output's removal, whenever it lands, leaves the new route in
# place.
port = 18125

def f() =
  dir = file.temp_dir("icecast", "reconnect")
  config = path.concat(dir, "icecast.xml")
  file.write(
    data="<icecast><authentication><source-password>hackme</source-password></authentication></icecast>",
    config
  )
  server = icecast.server(port=port, config=config)

  source =
    output.icecast(
      %mp3,
      host="localhost",
      port=port,
      password="hackme",
      mount="back.mp3",
      sine()
    )

  def status() =
    sock = socket.unix()
    sock.connect(
      socket.address.internet_address(
        socket.internet_address("127.0.0.1"),
        port
      )
    )
    sock.write(
      "GET /back.mp3 HTTP/1.0\r\n\r\n"
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

  def wait_for(label, expected) =
    def rec loop(n) =
      code = status()
      if
        code != expected
      then
        if
          n == 0
        then
          test.fail(
            "#{label}: got #{code}"
          )
        end
        thread.pause(0.5)
        loop(n - 1)
      end
    end
    loop(40)
  end

  connections = ref(0)
  def check() =
    wait_for(
      "first source",
      "200"
    )
    source.stop()
    wait_for(
      "source gone",
      "404"
    )
    source.start()
    wait_for(
      "source back",
      "200"
    )
    for _ = 1 to 6 do
      thread.pause(0.5)
      test.equal(status(), "200")

    end
    source.stop()
    source.start()
    def rec logged_in(n) =
      if
        connections() < 3
      then
        if
          n == 0
        then
          test.fail(
            "source did not log in again"
          )
        end
        thread.pause(0.2)
        logged_in(n - 1)
      end
    end
    logged_in(100)
    wait_for(
      "quick reconnect",
      "200"
    )
    for _ = 1 to 6 do
      thread.pause(0.5)
      test.equal(status(), "200")

    end
    test.equal(connections(), 3)
    test.pass()
  end

  server.on_source_connect(
    fun (_) ->
      begin
        n =
          atomic(
            fun () ->
              begin
                connections := connections() + 1
                connections()
              end
          )
        if n == 1 then thread.run(delay=2., check) end
      end
  )
end

test.check(f)
