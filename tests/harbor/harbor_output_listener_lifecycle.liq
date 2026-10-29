%include "../test.liq"

# Reconnecting clients over repeated stops and starts: every connect has its
# disconnect, no listener is left after the last stop, and the process's open
# descriptors return to their baseline.
port = 18118
o = output.harbor(port=port, mount="churn.mp3", %mp3, sine())
connects = ref(0)
disconnects = ref(0)
o.on_connect(
  synchronous=true,
  fun (_) -> atomic(fun () -> connects := connects() + 1)
)
o.on_disconnect(
  synchronous=true,
  fun (_) -> atomic(fun () -> disconnects := disconnects() + 1)
)

def descriptors() =
  list.length(file.ls("/proc/self/fd"))
end

def client(n) =
  for _ = 1 to n do
    sock = socket.unix()
    try
      sock.connect(
        socket.address.internet_address(
          socket.internet_address("127.0.0.1"),
          port
        )
      )
      sock.write(
        "GET /churn.mp3 HTTP/1.0\r\n\r\n"
      )
      ignore(
        try
          sock.read(timeout=1.)
        catch _ do
          ""
        end
      )
      thread.pause(random.float(min=0., max=0.3))
    catch _ do
      thread.pause(0.1)
    finally
      sock.close()
    end

  end
end

def wait_for(label, cond) =
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
      thread.pause(0.1)
      loop(n - 1)
    end
  end
  loop(100)
end

def check() =
  if not file.exists("/proc/self/fd") then test.skip() end
  baseline = descriptors()
  done = ref(0)
  for _ = 1 to 4 do
    thread.run(
      fun () ->
        begin
          client(15)
          atomic(fun () -> done := done() + 1)
        end
    )

  end
  for _ = 1 to 5 do
    thread.pause(0.7)
    o.stop()
    wait_for("stop", fun () -> not o.is_started())
    o.start()
    wait_for("start", o.is_started)

  end
  wait_for("clients", fun () -> done() == 4)
  o.stop()
  wait_for(
    "last stop",
    fun () -> not o.is_started()
  )
  wait_for("disconnects", fun () -> connects() == disconnects())
  if
    connects() < 10
  then
    test.fail(
      "only #{connects()} connects"
    )
  end
  test.equal(o.listeners(), [])
  o.start()
  wait_for(
    "final start",
    o.is_started
  )
  thread.pause(1.)
  test.equal(descriptors(), baseline)
  test.pass()
end

o.on_start(
  synchronous=true,
  fun () -> if connects() == 0 then thread.run(delay=0.5, check) end
)
