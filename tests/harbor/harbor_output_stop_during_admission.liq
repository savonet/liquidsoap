%include "../test.liq"

# A stop forced between admission steps, from the user code that runs there:
# during authentication or the admission predicate, the request gets 503 and
# neither callback; during `on_connect`, the listener ends closed with exactly
# one disconnect.
port = 18117
stop_in = ref("")
o = ref(null)

def stop_and_wait(step) =
  if
    stop_in() == step
  then
    out = null.get(o())
    out.stop()
    def rec wait(n) =
      if
        out.is_started()
      then
        if
          n == 0
        then
          test.fail(
            "stop did not happen"
          )
        end
        thread.pause(0.05)
        wait(n - 1)
      end
    end
    wait(100)
  end
end

def auth(_) =
  stop_and_wait("auth")
  true
end

def accept(_) =
  stop_and_wait("accept")
  true
end

out =
  output.harbor(
    port=port,
    mount="race.mp3",
    auth=auth,
    accept=accept,
    %mp3,
    sine()
  )
o := out

connects = ref(0)
disconnects = ref(0)
out.on_connect(
  synchronous=true,
  fun (_) ->
    begin
      atomic(fun () -> connects := connects() + 1)
      stop_and_wait("on_connect")
    end
)
out.on_disconnect(
  synchronous=true,
  fun (_) -> atomic(fun () -> disconnects := disconnects() + 1)
)

def request() =
  sock = socket.unix()
  sock.connect(
    socket.address.internet_address(socket.internet_address("127.0.0.1"), port)
  )
  sock.write(
    "GET /race.mp3?user=u&pass=p HTTP/1.0\r\n\r\n"
  )
  chunks = ref([])
  def rec read() =
    chunk =
      try
        sock.read(timeout=5.)
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
  read()
  sock.close()
  string.concat(list.rev(chunks()))
end

def restart() =
  out.start()
  def rec wait(n) =
    if
      not out.is_started()
    then
      if
        n == 0
      then
        test.fail(
          "restart did not happen"
        )
      end
      thread.pause(0.05)
      wait(n - 1)
    end
  end
  wait(100)
  thread.pause(0.3)
end

def check() =
  stop_in := "auth"
  test.equal(
    string.sub(encoding="ascii", request(), start=0, length=12),
    "HTTP/1.0 503"
  )
  test.equal((connects(), disconnects()), (0, 0))
  restart()

  stop_in := "accept"
  test.equal(
    string.sub(encoding="ascii", request(), start=0, length=12),
    "HTTP/1.0 503"
  )
  test.equal((connects(), disconnects()), (0, 0))
  restart()

  stop_in := "on_connect"
  response = request()
  test.equal(
    string.contains(
      encoding="ascii",
      substring="200 OK",
      response
    ),
    false
  )
  thread.pause(1.)
  test.equal((connects(), disconnects()), (1, 1))
  test.equal(out.listeners(), [])
  test.pass()
end

out.on_start(
  synchronous=true,
  fun () -> if stop_in() == "" then thread.run(delay=0.5, check) end
)
