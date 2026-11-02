%include "../test.liq"

# Listener admission of icecast.server (listeners.md): the order of the steps
# (max_clients before the decision, the decision before max_listeners), the
# replies of each outcome with their final header list, limits per
# compatibility, exempt, time limits, prerolls, one release per admission, and
# URL authentication against a backend.
port = 18176
kh_port = 18178
backend_port = 18187
checks = ref(0)
sockets = ref([])

def check(label, cond) =
  checks := checks() + 1
  if not cond then test.fail(label) end
end

def connect(port) =
  sock = socket.unix()
  sockets := [...sockets(), sock]
  sock.connect(
    socket.address.internet_address(socket.internet_address("127.0.0.1"), port)
  )
  sock
end

# The reply to `GET path` on `port`: status, lowercase head, the bytes after
# the head, and the socket, left open. A request that came while the mount
# output was starting is redirected to itself and sent again.
def listen(~port=port, ~headers="", path) =
  def rec attempt() =
    sock = connect(port)
    sock.write(
      "GET #{path} HTTP/1.0\r\n#{headers}\r\n"
    )
    data = ref("")
    deadline = time() + 10.
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
        and time() < deadline
        and not string.contains(encoding="ascii", substring="\r\n\r\n", data())
      then
        read()
      end
    end

    read()
    let (head, rest) =
      string.contains(encoding="ascii", substring="\r\n\r\n", data())
        ? string.split.first(encoding="ascii", separator="\r\n\r\n", data())
        : (data(), "")
    status =
      int_of_string(
        default=0,
        string.sub(encoding="ascii", head, start=9, length=3)
      )
    head = string.case(lower=true, head)
    if
      status == 307
      and string.contains(
        substring="location: #{path}\r",
        head ^ "\r"
      )
    then
      sock.close()
      attempt()
    else
      {status = status, head = head, rest = rest, sock = sock}
    end
  end

  attempt()
end

def request(~port=port, ~headers="", path) =
  r = listen(port=port, headers=headers, path)
  r.sock.close()
  r
end

def has(r, text) =
  string.contains(encoding="ascii", substring=text, r.head)
end

# Bytes `sock` receives within `duration`, and whether it was closed.
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

def wait_for(label, cond) =
  deadline = time() + 20.
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

decisions = ref(0)
releases = ref([])
max_clients = ref(null)

def decide(request) =
  atomic(fun () -> decisions := decisions() + 1)
  query = request.query
  def release(record) =
    atomic(
      fun () ->
        releases :=
          [
            ...releases(),
            {
              query = query,
              duration = record.duration,
              bytes_sent = record.bytes_sent,
              listeners = record.listeners
            }
          ]
    )
  end

  if
    query == "raw401"
  then
    {refuse = {status = 401, reason = "No"}}
  elsif
    query == "redirect"
  then
    {refuse = icecast.server.redirect(302, "http://example.org/x")}
  elsif
    query == "raise"
  then
    error.raise(
      error.not_found,
      "decision failed"
    )
  elsif
    query == "invalid"
  then
    {refuse = {status = 200, reason = "OK"}}
  elsif
    query == "limit0"
  then
    {time_limit = 0., on_release = release}
  elsif
    query == "limit1"
  then
    {time_limit = 1., on_release = release}
  elsif
    query == "preroll"
  then
    {preroll = "PREROLLBYTES", on_release = release}
  elsif
    query == "exempt"
  then
    {exempt = true, on_release = release}
  else
    {on_release = release}
  end
end

backend_requests = ref([])
harbor.http.register(
  port=backend_port,
  method="POST",
  "/auth",
  fun (request, response) ->
    begin
      form = url.split_args(request.body())
      atomic(fun () -> backend_requests := [...backend_requests(), form])
      if
        list.assoc(default="", "action", form) == "listener_remove"
      then
        response.status_code(200)
      elsif
        list.assoc(default="", "user", form) == "good"
      then
        response.header("icecast-auth-user", "1")
      else
        response.status_code(200)
      end
    end
)

cache =
  icecast.server.preroll_cache(
    directory=".",
    preroll_timeout=5.,
    preroll_max_size=1000000,
    preroll_revalidate=3600.
  )
url_auth =
  icecast.server.decision.url_auth(
    cache=cache,
    {
      listener_add = "http://127.0.0.1:#{backend_port}/auth",
      listener_remove = "http://127.0.0.1:#{backend_port}/auth"
    }
  )

def mount(path) =
  if
    path == "/a.mp3"
  then
    {max_listeners = 1, max_listener_duration = 30., listener_decision = decide}
  elsif
    path == "/b.mp3"
  then
    {
      http_headers =
        [{name = "X-Mount", value = "m"}, {name = "X-Server", value = "mount"}],
      listener_decision = fun (_) -> {on_release = fun (_) -> thread.pause(10.)}
    }
  elsif
    path == "/u.mp3"
  then
    {listener_decision = url_auth}
  else
    null
  end
end

server =
  icecast.server(
    {
      port = port,
      max_clients = {max_clients()},
      http_headers =
        [
          {name = "X-Server", value = "s"},
          {name = "X-Status", compute = fun (_, status) -> "#{status}"}
        ],
      mount = mount
    }
  )
kh =
  icecast.server(
    {
      port = kh_port,
      compatibility = "kh",
      mount = fun (_) -> {max_listeners = 0}
    }
  )

disconnects = ref([])
server.on_listener_disconnect(
  synchronous=true,
  fun (l) ->
    atomic(fun () -> disconnects := [...disconnects(), (l.mount, time())])
)

connects = ref([])
server.on_listener_connect(
  synchronous=true,
  fun (l) -> atomic(fun () -> connects := [...connects(), l.mount])
)

def client(~port=port, ~name="", path) =
  output.icecast(
    %mp3,
    port=port,
    password="hackme",
    mount=path,
    name=name,
    sine()
  )
end

a = client("a.mp3")
b = client(name="Bee", "b.mp3")
u = client("u.mp3")
k = client(port=kh_port, "k.mp3")

def listener_count(path) =
  list.fold(
    fun (n, m) -> m.mount == path ? n + list.length(m.listeners) : n,
    0,
    server.stats().mounts
  )
end

def mounts(s) =
  list.map(fun (m) -> m.mount, s.stats().mounts)
end

def releases_of(query) =
  list.filter(fun (r) -> r.query == query, releases())
end

def check_replies() =
  stream = listen("/b.mp3")
  check(
    "stream reply: #{stream.status}",
    stream.status == 200
  )
  check(
    "identity header: #{stream.head}",
    has(
      stream,
      "icy-name: bee"
    )
  )
  check(
    "server header beats the mount's: #{stream.head}",
    has(
      stream,
      "x-server: s\r"
    )
    and not has(
      stream,
      "x-server: mount"
    )
  )
  check(
    "mount header",
    has(
      stream,
      "x-mount: m"
    )
  )
  check(
    "computed header on the stream reply",
    has(
      stream,
      "x-status: 200"
    )
  )
  ignore(receive(stream.sock, 1.5))
  joiner = listen("/b.mp3")
  burst =
    string.length(joiner.rest) + string.length(receive(joiner.sock, 0.3).data)
  check(
    "burst on connect: #{burst}",
    burst > 12000
  )
  joiner.sock.close()
  stream.sock.close()

  raw401 = request("/a.mp3?raw401")
  check(
    "decision 401: #{raw401.status}",
    raw401.status == 401
  )
  check(
    "default challenge on 401: #{raw401.head}",
    has(
      raw401,
      "www-authenticate: basic realm=\"icecast2 server\""
    )
  )
  check(
    "computed header on a refusal",
    has(
      raw401,
      "x-status: 401"
    )
  )
  check(
    "content-length 0",
    has(
      raw401,
      "content-length: 0"
    )
  )
  redirect = request("/a.mp3?redirect")
  check(
    "redirect: #{redirect.head}",
    redirect.status == 302
    and has(
      redirect,
      "location: http://example.org/x"
    )
    and not has(redirect, "www-authenticate")
  )
  raised = request("/a.mp3?raise")
  check(
    "raising decision is 403 with the configured headers: #{raised.head}",
    raised.status == 403
    and has(
      raised,
      "x-server: s"
    )
  )
  invalid = request("/a.mp3?invalid")
  check(
    "invalid record is a bare 403: #{invalid.head}",
    invalid.status == 403 and not has(invalid, "x-server")
  )
  check(
    "no connect event for refusals: #{connects()}",
    not list.mem("/a.mp3", connects())
  )
end

def check_limits() =
  first = listen("/a.mp3")
  check(
    "first listener of /a.mp3",
    first.status == 200
  )
  wait_for(
    "first listener counted",
    {listener_count("/a.mp3") == 1}
  )
  before = decisions()
  full = request("/a.mp3?full")
  check(
    "past max_listeners under icecast-2.5: #{full.status}",
    full.status == 503
  )
  check(
    "past max_listeners reaches the decision",
    decisions() == before + 1
  )
  wait_for(
    "refused admission released",
    {releases_of("full") != []}
  )
  released = list.hd(releases_of("full"))
  check(
    "limit refusal released with duration 0 and listeners without it: #{
      released
    }",
    released.duration == 0.
    and released.bytes_sent == 0
    and released.listeners == 1
  )
  exempt = listen("/a.mp3?exempt")
  check(
    "exempt skips max_listeners: #{exempt.status}",
    exempt.status == 200
  )
  wait_for(
    "exempt counted",
    {listener_count("/a.mp3") == 2}
  )
  max_clients := 2
  before = decisions()
  over = request("/a.mp3?over")
  check(
    "past max_clients: #{over.status}",
    over.status == 503
  )
  check(
    "past max_clients never reaches the decision",
    decisions() == before
  )
  max_clients := null
  exempt.sock.close()
  first.sock.close()
  wait_for(
    "served listeners released",
    {releases_of("") != []}
  )
  served = list.hd(releases_of(""))
  check(
    "served release: #{served}",
    served.duration > 0. and served.bytes_sent > 0 and served.listeners >= 1
  )
  wait_for(
    "listeners gone",
    {listener_count("/a.mp3") == 0}
  )
  wait_for(
    "every admission released",
    {list.length(releases()) >= 3}
  )
  check(
    "one release per admission: #{releases()}",
    list.length(releases()) == 3
  )

  kh_full = request(port=kh_port, "/k.mp3")
  check(
    "past max_listeners under kh: #{kh_full.status}",
    kh_full.status == 403
  )
end

def check_time_limits() =
  zero = listen("/a.mp3?limit0")
  check(
    "time limit 0 disconnects at once: #{zero.status}",
    (zero.status == 200 or zero.status == 0) and receive(zero.sock, 3.).closed
  )
  wait_for(
    "time limit 0 released",
    {releases_of("limit0") != []}
  )
  check(
    "time limit 0 fires a connect event",
    list.mem("/a.mp3", connects())
  )
  started = time()
  one = listen("/a.mp3?limit1")
  closed = receive(one.sock, 6.).closed
  check(
    "decision limit beats the mount's: #{time() - started}",
    closed and time() - started < 5.
  )
  preroll = listen("/a.mp3?preroll")
  check(
    "preroll sent before the stream",
    string.contains(encoding="ascii", prefix="PREROLLBYTES", preroll.rest)
  )
  preroll.sock.close()
end

def check_url_auth() =
  def basic(user) =
    "Authorization: Basic #{string.base64.encode("#{user}:pw")}\r\n"
  end

  good = listen(headers=basic("good"), "/u.mp3")
  check(
    "URL authentication admits: #{good.status}",
    good.status == 200
  )
  bad = request(headers=basic("bad"), "/u.mp3")
  check(
    "URL authentication refuses: #{bad.status}",
    bad.status == 401
  )
  good.sock.close()
  wait_for(
    "listener_remove",
    {
      list.exists(
        fun (r) -> list.assoc(default="", "action", r) == "listener_remove",
        backend_requests()
      )
    }
  )
  adds =
    list.filter(
      fun (r) -> list.assoc(default="", "action", r) == "listener_add",
      backend_requests()
    )
  check(
    "listener_add carries the server and mount: #{adds}",
    list.length(adds) == 2
    and list.for_all(
      fun (r) ->
        list.assoc(default="", "mount", r) == "/u.mp3"
        and list.assoc(default="", "port", r) == "#{port}",
      adds
    )
  )
end

def check_no_mount_headers() =
  none = listen("/nothing.mp3")
  none.sock.close()
  check(
    "a path with no mount answers 404 with the server's headers: #{none.head}",
    none.status == 404
    and string.contains(
      substring="x-status: 404",
      none.head
    )
    and string.contains(
      substring="x-server: s",
      none.head
    )
  )
end

# A slow `on_release` (/b.mp3's takes 10 s) does not delay the disconnect.
def check_release_in_background() =
  listener = listen("/b.mp3")
  wait_for(
    "/b.mp3 listener",
    {listener_count("/b.mp3") == 1}
  )
  closed = time()
  listener.sock.close()
  wait_for(
    "/b.mp3 disconnect event",
    {
      list.exists(
        fun (d) -> fst(d) == "/b.mp3" and snd(d) >= closed,
        disconnects()
      )
    }
  )
  at =
    list.find(fun (d) -> fst(d) == "/b.mp3" and snd(d) >= closed, disconnects())
  check(
    "a slow on_release does not delay the disconnect: #{snd(at) - closed}s",
    snd(at) - closed < 5.
  )
end

def run() =
  try
    wait_for(
      "mounts",
      {list.length(mounts(server)) == 3 and list.mem("/k.mp3", mounts(kh))}
    )
    check_replies()
    check_limits()
    check_time_limits()
    check_url_auth()
    check_release_in_background()
    check_no_mount_headers()
    if
      checks() != 33
    then
      test.fail(
        "#{checks()} checks ran, expected 33"
      )
    end
    test.pass()
  finally
    list.iter(
      fun (s) ->
        try
          s.close()
        catch _ do
          ()
        end,
      sockets()
    )
  end
end

thread.run(delay=1., run)
