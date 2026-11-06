%include "../test.liq"

# An admitting decision's headers follow the output's own on the 200 head, in
# order, duplicates included. Every admission ends in exactly one of
# `on_connect` or `on_dropped`: an invalid header or a negative or non-finite
# time limit gives 403, an output stopped during the predicate gives 503, each
# with one `on_dropped`.
# A refusal ignores the admit fields and never calls `on_dropped`.
port = 18160
out = ref(null)
connects = ref([])
dropped = ref([])

def stop_output() =
  o = null.get(out())
  o.stop()
  deadline = time() + 5.
  def rec wait() =
    if
      o.is_started()
    then
      if
        time() > deadline
      then
        test.fail(
          "output did not stop"
        )
      end
      thread.pause(0.05)
      wait()
    end
  end
  wait()
end

def accept(l) =
  agent = list.assoc(default="", "user-agent", l.headers)
  on_dropped = fun () -> atomic(fun () -> dropped := [...dropped(), agent])
  if
    agent == "plain"
  then
    {
      headers =
        [("Access-Control-Allow-Origin", "*"), ("Cache-Control", "private")],
      on_dropped = on_dropped
    }
  elsif
    agent == "bad_header"
  then
    {
      headers =
        [
          (
            "Bad Name",
            "x"
          )
        ],
      on_dropped = on_dropped
    }
  elsif
    agent == "bad_limit"
  then
    {time_limit = -1., on_dropped = on_dropped}
  elsif
    agent == "infinite_limit"
  then
    {time_limit = infinity, on_dropped = on_dropped}
  elsif
    agent == "nan_limit"
  then
    {time_limit = nan, on_dropped = on_dropped}
  elsif
    agent == "refused"
  then
    {
      refuse =
        {status = 403, reason = "Nope", headers = [("X-A", "1"), ("X-A", "1")]},
      headers = [("X-B", "2")],
      preroll = "zz",
      on_dropped = on_dropped
    }
  else
    stop_output()
    {on_dropped = on_dropped}
  end
end

o =
  output.harbor(
    port=port,
    mount="dropped.mp3",
    headers=[("Cache-Control", "no-cache")],
    accept=accept,
    %mp3,
    sine()
  )
out := o
o.on_connect(
  synchronous=true,
  fun (l) ->
    atomic(
      fun () ->
        connects :=
          [...connects(), list.assoc(default="", "user-agent", l.headers)]
    )
)

# The reply head to a request by `agent`.
def fetch_head(agent) =
  sock = socket.unix()
  data = ref("")
  try
    sock.connect(
      socket.address.internet_address(
        socket.internet_address("127.0.0.1"),
        port
      )
    )
    sock.write(
      "GET /dropped.mp3 HTTP/1.1\r\nUser-Agent: #{agent}\r\n\r\n"
    )
    def rec read_more() =
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
        read_more()
      end
    end
    read_more()
  finally
    sock.close()
  end
  data()
end

def starts_with(head, prefix) =
  string.sub(
    encoding="ascii",
    head,
    start=0,
    length=string.length(encoding="ascii", prefix)
  ) ==
    prefix
end

def count(agent, l) =
  list.length(list.filter(fun (a) -> a == agent, l))
end

# Waits for `agent`'s drop count to reach `expected`, then checks it stays.
def check_dropped(agent, expected) =
  deadline = time() + 5.
  def rec wait() =
    if
      count(agent, dropped()) < expected and time() < deadline
    then
      thread.pause(0.05)
      wait()
    end
  end
  wait()
  n = count(agent, dropped())
  if
    n != expected
  then
    test.fail(
      "#{agent}: on_dropped called #{n} times, expected #{expected}"
    )
  end
end

def check_status(agent, head, status) =
  if
    not starts_with(head, status)
  then
    test.fail(
      "#{agent}: expected #{status}, got #{string.quote(head)}"
    )
  end
end

def check() =
  plain = fetch_head("plain")
  check_status(
    "plain",
    plain,
    "HTTP/1.1 200 OK"
  )
  if
    not string.contains(
      encoding="ascii",
      substring="\r\nCache-Control: no-cache\r\nAccess-Control-Allow-Origin: \
       *\r\nCache-Control: private\r\n",
      plain
    )
  then
    test.fail(
      "admit headers missing or out of order: #{string.quote(plain)}"
    )
  end

  check_status(
    "bad_header",
    fetch_head("bad_header"),
    "HTTP/1.1 403"
  )
  check_status(
    "bad_limit",
    fetch_head("bad_limit"),
    "HTTP/1.1 403"
  )
  check_status(
    "infinite_limit",
    fetch_head("infinite_limit"),
    "HTTP/1.1 403"
  )
  check_status(
    "nan_limit",
    fetch_head("nan_limit"),
    "HTTP/1.1 403"
  )

  refused = fetch_head("refused")
  check_status(
    "refused",
    refused,
    "HTTP/1.1 403 Nope"
  )
  if
    not string.contains(
      encoding="ascii",
      substring="X-A: 1\r\nX-A: 1\r\n",
      refused
    )
  then
    test.fail(
      "refusal headers not written as given: #{string.quote(refused)}"
    )
  end
  if
    string.contains(encoding="ascii", substring="X-B", refused)
  then
    test.fail(
      "refusal carries admit headers: #{string.quote(refused)}"
    )
  end

  check_status(
    "race",
    fetch_head("race"),
    "HTTP/1.1 503"
  )

  check_dropped("bad_header", 1)
  check_dropped("bad_limit", 1)
  check_dropped("infinite_limit", 1)
  check_dropped("nan_limit", 1)
  check_dropped("race", 1)
  check_dropped("plain", 0)
  check_dropped("refused", 0)
  if
    connects() != ["plain"]
  then
    test.fail(
      "on_connect fired for #{connects()}, expected only plain"
    )
  end
  test.pass()
end

o.on_start(synchronous=true, fun () -> thread.run(delay=0.5, check))
