%include "../test.liq"

# A process shut down while a listener is connected, with its clock stuck in
# a frame, closes the listener and exits.
port = 18119

def f() =
  dir = file.temp_dir("harbor", "shutdown")
  script = path.concat(dir, "child.liq")
  file.write(
    data="s = sine()
o = output.harbor(port=#{
      port
    }, mount=\"down.mp3\", %mp3, s)
stuck = ref(false)
o.on_connect(synchronous=true, fun (_) -> thread.run(delay=1., fun () -> begin stuck := true; shutdown() end))
s.on_frame(synchronous=true, fun () -> if stuck() then sleep(30.) end)
",
    script
  )
  status = ref(null)
  thread.run(
    fun () ->
      begin
        r =
          process.run(
            timeout=60.,
            "liquidsoap #{process.quote(script)}"
          )
        status := (r.status : string)
      end
  )

  def rec connect(n) =
    sock = socket.unix()
    try
      sock.connect(
        socket.address.internet_address(
          socket.internet_address("127.0.0.1"),
          port
        )
      )
      sock
    catch _ do
      sock.close()
      if
        n == 0
      then
        test.fail(
          "child never listened"
        )
      end
      sleep(0.5)
      connect(n - 1)
    end
  end
  sock = connect(60)
  sock.write(
    "GET /down.mp3 HTTP/1.0\r\n\r\n"
  )
  start = time()
  def rec read() =
    chunk =
      try
        sock.read(timeout=20.)
      catch _ do
        ""
      end
    if chunk != "" then read() end
  end
  read()
  sock.close()
  if
    time() - start > 15.
  then
    test.fail(
      "listener not closed at shutdown"
    )
  end

  def rec wait(n) =
    if
      null.defined(status())
    then
      test.equal(null.get(status()), "exit")
    else
      if
        n == 0
      then
        test.fail(
          "child did not exit"
        )
      end
      sleep(0.5)
      wait(n - 1)
    end
  end
  wait(80)
  test.pass()
end

test.check(f)
