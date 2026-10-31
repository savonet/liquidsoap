%include "../test.liq"

# The admission predicate sees the listener record: a refusal is sent as its
# status line and headers and fires neither callback, null admits, a time
# limit disconnects the listener once it has passed. A raising predicate, or
# an invalid refusal, gets 403 Forbidden.
port = 18103
connects = ref([])
disconnects = ref([])
seen = ref([])

def accept(l) =
  atomic(fun () -> seen := [...seen(), l.id])
  if
    l.uri != "/accept.mp3"
  then
    test.fail(
      "wrong uri #{l.uri}"
    )
  end
  ua = list.assoc(default="", "user-agent", l.headers)
  if
    ua == "raise"
  then
    error.raise(
      error.failure,
      "predicate failure"
    )
  end
  if
    ua == "welcome"
  then
    null
  elsif
    ua == "limited"
  then
    {time_limit = 1.}
  elsif
    ua == "redirect"
  then
    {
      refuse =
        {
          status = 302,
          reason = "Found",
          headers = [("Location", "http://elsewhere/")]
        }
    }
  elsif
    ua == "invalid"
  then
    {refuse = {status = 200, reason = "OK"}}
  else
    {refuse = {status = 403, reason = "Forbidden"}}
  end
end

o = output.harbor(port=port, mount="accept.mp3", accept=accept, %mp3, sine())
o.on_connect(
  synchronous=true,
  fun (l) -> atomic(fun () -> connects := [...connects(), l.id])
)
o.on_disconnect(
  synchronous=true,
  fun (l) ->
    atomic(fun () -> disconnects := [...disconnects(), (l.id, l.duration)])
)

# Socket of a request by `agent`, after its reply head.
def open_request(agent) =
  sock = socket.unix()
  sock.connect(
    socket.address.internet_address(socket.internet_address("127.0.0.1"), port)
  )
  sock.write(
    "GET /accept.mp3 HTTP/1.1\r\nUser-Agent: #{agent}\r\n\r\n"
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
  (sock, data())
end

def request(agent) =
  let (sock, head) = open_request(agent)
  sock.close()
  head
end

def check() =
  refused = request("nope")
  test.equal(
    string.sub(encoding="ascii", refused, start=0, length=22),
    "HTTP/1.1 403 Forbidden"
  )
  test.equal(
    string.contains(
      encoding="ascii",
      substring="Content-Length: 0",
      refused
    ),
    true
  )
  redirected = request("redirect")
  test.equal(
    string.sub(encoding="ascii", redirected, start=0, length=18),
    "HTTP/1.1 302 Found"
  )
  test.equal(
    string.contains(
      encoding="ascii",
      substring="\r\nLocation: http://elsewhere/\r\n",
      redirected
    ),
    true
  )
  raised = request("raise")
  test.equal(
    string.sub(encoding="ascii", raised, start=0, length=12),
    "HTTP/1.1 403"
  )
  invalid = request("invalid")
  test.equal(
    string.sub(encoding="ascii", invalid, start=0, length=12),
    "HTTP/1.1 403"
  )
  test.equal(connects(), [])
  admitted = request("welcome")
  test.equal(
    string.sub(encoding="ascii", admitted, start=0, length=15),
    "HTTP/1.1 200 OK"
  )
  test.equal(list.length(seen()), 5)
  test.equal(connects(), [list.nth(seen(), 4)])

  let (limited, head) = open_request("limited")
  try
    if
      string.sub(encoding="ascii", head, start=0, length=15) !=
        "HTTP/1.1 200 OK"
    then
      test.fail(
        "time-limited listener not admitted: #{head}"
      )
    end
    limited_id = list.nth(seen(), 5)
    deadline = time() + 10.
    def rec wait() =
      duration = list.assoc.nullable(limited_id, disconnects())
      if
        null.defined(duration)
      then
        null.get(duration)
      elsif
        time() > deadline
      then
        test.fail(
          "time-limited listener still connected after 10s"
        )
        0.
      else
        thread.pause(0.1)
        wait()
      end
    end
    duration = wait()
    if
      duration < 1.
    then
      test.fail(
        "time-limited listener cut after #{duration}s, before its limit"
      )
    end
  finally
    limited.close()
  end
  test.pass()
end

o.on_start(synchronous=true, fun () -> thread.run(delay=0.5, check))
