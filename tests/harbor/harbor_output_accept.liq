%include "../test.liq"

# The admission predicate sees the listener record: false gets 403 Forbidden
# and fires neither callback, true admits. A raising predicate refuses.
port = 18103
connects = ref([])
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
  ua == "welcome"
end

o = output.harbor(port=port, mount="accept.mp3", accept=accept, %mp3, sine())
o.on_connect(
  synchronous=true,
  fun (l) -> atomic(fun () -> connects := [...connects(), l.id])
)

def request(agent) =
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
  sock.close()
  data()
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
  raised = request("raise")
  test.equal(
    string.sub(encoding="ascii", raised, start=0, length=12),
    "HTTP/1.1 403"
  )
  test.equal(connects(), [])
  admitted = request("welcome")
  test.equal(
    string.sub(encoding="ascii", admitted, start=0, length=15),
    "HTTP/1.1 200 OK"
  )
  test.equal(list.length(seen()), 3)
  test.equal(connects(), [list.nth(seen(), 2)])
  test.pass()
end

o.on_start(synchronous=true, fun () -> thread.run(delay=0.5, check))
