%include "../test.liq"

# With `keep_listeners`, an output over an unavailable source starts at once
# and stays started through a gap longer than `timeout`: its listeners get no
# byte and stay connected, a joiner gets no burst of the stream before the gap,
# a listener that hangs up is disconnected, and the stream resumes with the
# source. The same gap stops an output without `keep_listeners`.
port = 18161
plain_port = 18162
ready = ref(false)
s = sine()
s = switch([({ready()}, s.{track_sensitive = false})])

kept =
  output.harbor(
    port=port,
    mount="kept.mp3",
    fallible=true,
    keep_listeners=true,
    timeout=1.,
    %mp3,
    s
  )

stops = ref(0)
disconnects = ref(0)
kept.on_stop(synchronous=true, fun () -> atomic(fun () -> stops := stops() + 1))
kept.on_disconnect(
  synchronous=true,
  fun (_) -> atomic(fun () -> disconnects := disconnects() + 1)
)

plain =
  output.harbor(
    port=plain_port,
    mount="plain.mp3",
    fallible=true,
    timeout=1.,
    %mp3,
    s
  )

plain_disconnects = ref(0)
plain.on_disconnect(
  synchronous=true,
  fun (_) -> atomic(fun () -> plain_disconnects := plain_disconnects() + 1)
)

sockets = ref([])

# A listener socket of `mount` on `port`, once its 200 reply head is read, and
# the stream bytes that came with the head.
def listen(port, mount) =
  sock = socket.unix()
  sockets := [...sockets(), sock]
  sock.connect(
    socket.address.internet_address(socket.internet_address("127.0.0.1"), port)
  )
  sock.write(
    "GET /#{mount} HTTP/1.0\r\n\r\n"
  )
  head = ref("")
  deadline = time() + 10.
  def rec read_head() =
    chunk =
      try
        sock.read(timeout=5.)
      catch _ do
        ""
      end
    head := head() ^ chunk
    if
      chunk != ""
      and time() < deadline
      and not string.contains(encoding="ascii", substring="\r\n\r\n", head())
    then
      read_head()
    end
  end
  read_head()
  if
    not string.contains(
      encoding="ascii",
      substring="200 OK",
      head()
    )
  then
    test.fail(
      "#{mount}: no 200 reply: #{head()}"
    )
  end
  let [_, ...body] = string.split(separator="\r\n\r\n", head())
  (sock, string.concat(body))
end

# Bytes `sock` receives within `duration`, and whether it reached end of stream.
def receive(sock, duration) =
  deadline = time() + duration
  data = ref("")
  closed = ref(false)
  def rec read_more() =
    remaining = deadline - time()
    if
      remaining > 0. and not closed()
    then
      try
        chunk = sock.read(timeout=remaining)
        if chunk == "" then closed := true else data := data() ^ chunk end
      catch _ do
        ()
      end
      read_more()
    end
  end
  read_more()
  {data = data(), closed = closed()}
end

# Reads until `sock` stays silent for half a second.
def drain(sock) =
  deadline = time() + 10.
  def rec read_more() =
    if
      receive(sock, 0.5).data != ""
    then
      if
        time() > deadline
      then
        test.fail(
          "stream did not pause"
        )
      end
      read_more()
    end
  end
  read_more()
end

def expect_silent(~received="", name, sock, duration) =
  r = receive(sock, duration)
  r = r.{data = received ^ r.data}
  if
    r.closed
  then
    test.fail(
      "#{name}: disconnected during the gap"
    )
  end
  if
    r.data != ""
  then
    test.fail(
      "#{name}: received stream bytes during the gap"
    )
  end
end

def expect_stream(name, sock) =
  r = receive(sock, 3.)
  if
    r.data == ""
  then
    test.fail(
      "#{name}: no stream byte once the source is available"
    )
  end
end

def wait_for(label, cond) =
  deadline = time() + 10.
  def rec wait() =
    if
      not cond()
    then
      if
        time() > deadline
      then
        test.fail(
          "timeout waiting for #{label}"
        )
      end
      thread.pause(0.1)
      wait()
    end
  end
  wait()
end

def check() =
  try
    wait_for(
      "output start over an unavailable source",
      {kept.is_started()}
    )
    let (first, received) = listen(port, "kept.mp3")
    expect_silent(
      received=received,
      "first listener before any source",
      first,
      1.5
    )

    ready := true
    expect_stream(
      "first listener",
      first
    )
    wait_for(
      "plain output start",
      {plain.is_started()}
    )
    let (plain_listener, _) = listen(plain_port, "plain.mp3")
    expect_stream(
      "plain listener",
      plain_listener
    )

    ready := false
    drain(first)
    wait_for(
      "plain listener disconnect",
      {plain_disconnects() == 1}
    )
    expect_silent(
      "first listener",
      first,
      2.5
    )

    let (joiner, received) = listen(port, "kept.mp3")
    expect_silent(
      received=received,
      "joiner during the gap",
      joiner,
      1.
    )

    let (leaver, _) = listen(port, "kept.mp3")
    wait_for(
      "three listeners",
      {list.length(kept.listeners()) == 3}
    )
    leaver.close()
    wait_for(
      "leaver disconnect",
      {disconnects() == 1}
    )
    if
      list.length(kept.listeners()) != 2
    then
      test.fail(
        "leaver still listed"
      )
    end
    if
      stops() != 0
    then
      test.fail(
        "output stopped during the gap"
      )
    end
    if
      not kept.is_started()
    then
      test.fail(
        "output not started"
      )
    end

    ready := true
    expect_stream(
      "first listener after the gap",
      first
    )
    expect_stream(
      "joiner after the gap",
      joiner
    )

    kept.stop()
    wait_for(
      "disconnects after stop",
      {disconnects() == 3}
    )
    if
      stops() != 1
    then
      test.fail(
        "on_stop fired #{stops()} times"
      )
    end
    test.pass()
  finally
    list.iter(
      fun (sock) ->
        try
          sock.close()
        catch _ do
          ()
        end,
      sockets()
    )
  end
end

thread.run(delay=1., check)
