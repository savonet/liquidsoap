%include "../test.liq"

# Source clients of icecast.server (sources.md): PUT, SOURCE and Shoutcast
# clients feed mounts; the checks run in order (reserved path, credentials,
# mount taken, max_sources) with the statuses of each `compatibility`; per-mount
# credentials and source_decision; stream identity per feeder; source events
# and mount callbacks; killsource and metadata mount commands; of two source
# clients racing for one mount, exactly one feeds it.
port = 18170
kh_port = 18172
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

# The reply head read from `sock`, lowercased, its status, and the bytes after.
def read_head(sock) =
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
  let [head, ...rest] = string.split(separator="\r\n\r\n", data())
  status =
    int_of_string(
      default=0,
      string.sub(encoding="ascii", head, start=9, length=3)
    )
  {
    status = status,
    head = string.case(lower=true, head),
    rest = string.concat(rest)
  }
end

def exchange(port, text) =
  sock = connect(port)
  sock.write(text)
  reply = read_head(sock)
  sock.close()
  reply
end

def wait_for(label, cond) =
  deadline = time() + 15.
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

def basic(user, password) =
  "Authorization: Basic #{string.base64.encode("#{user}:#{password}")}\r\n"
end

def put(~port=port, ~auth="", ~headers="", path) =
  exchange(
    port,
    "PUT #{path} HTTP/1.1\r\n#{auth}#{headers}\r\n"
  )
end

decisions = ref([])
cb_version = ref("A")
callbacks = ref([])
max_sources = ref(null)
source_timeout = ref(10.)

def add_callback(name) =
  atomic(fun () -> callbacks := [...callbacks(), name])
end

backend_port = 18185
backend = ref([])
def record_backend(path, form) =
  atomic(fun () -> backend := [...backend(), (path, form)])
end
harbor.http.register(
  port=backend_port,
  method="POST",
  "/:action",
  fun (request, response) ->
    begin
      form = url.split_args(request.body())
      action = request.path
      record_backend(action, form)
      if
        list.assoc(default="", "user", form) == "good"
      then
        response.header("icecast-auth-user", "1")
      end
    end
)
backend_url = "http://127.0.0.1:#{backend_port}"
stream_auth =
  icecast.server.source_decision.stream_auth(
    {stream_auth = "#{backend_url}/auth"}
  )
notifications =
  icecast.server.source_callbacks.url(
    {mount_add = "#{backend_url}/add", mount_remove = "#{backend_url}/remove"}
  )

def backend_forms(path) =
  list.map(snd, list.filter(fun (r) -> fst(r) == path, backend()))
end

def mount(path) =
  if
    path == "/sa.mp3"
  then
    {
      source_decision = stream_auth,
      on_connect = notifications.on_connect,
      on_disconnect = notifications.on_disconnect
    }
  elsif
    path == "/per.mp3"
  then
    {source_user = "u", source_password = "mountpw"}
  elsif
    path == "/open.mp3"
  then
    {source_password = ""}
  elsif
    path == "/dec.mp3" or path == "/admin/dec"
  then
    {
      source_decision =
        fun (r) ->
          begin
            atomic(fun () -> decisions := [...decisions(), r])
            r.user == "ok"
          end
    }
  elsif
    path == "/cb.mp3"
  then
    version = cb_version()
    {
      on_connect =
        fun (r) ->
          add_callback(
            "connect #{r.mount}"
          ),
      on_disconnect =
        fun (r) ->
          add_callback(
            "disconnect #{version} #{r.mount} #{r.agent}"
          )
    }
  else
    null
  end
end

server =
  icecast.server(
    {
      port = port,
      source_password = "pw",
      max_sources = {max_sources()},
      source_timeout = {source_timeout()},
      mount = mount
    }
  )
kh =
  icecast.server({port = kh_port, compatibility = "kh", source_password = "pw"})

events = ref([])
def add_event(e) =
  atomic(fun () -> events := [...events(), e])
end
server.on_source_connect(
  synchronous=true,
  fun (r) ->
    add_event(
      "connect #{r.mount} #{r.agent} #{r.server.port}"
    )
)
server.on_source_disconnect(
  synchronous=true,
  fun (r) ->
    add_event(
      "disconnect #{r.mount} #{r.agent} #{r.ip != ""}"
    )
)

def mounts() =
  list.map(fun (m) -> m.mount, server.stats().mounts)
end

def mount_stats(path) =
  list.find(fun (m) -> m.mount == path, server.stats().mounts)
end

mp3 = file.temp("icecast_sources", ".mp3")
encoder = output.file(%mp3, mp3, sine())

# Outputs created by the test thread start when it returns: start them now.
def started(o) =
  try
    clock(o.clock).start(force=false)
  catch _ do
    ()
  end
  o
end

def client(~name="Radio", ~user="source", ~password="pw", path) =
  started(
    output.icecast(
      %mp3,
      port=port,
      user=user,
      password=password,
      mount=path,
      name=name,
      sine()
    )
  )
end

def check_refusals() =
  reserved =
    put(
      auth=basic("source", "pw"),
      headers="Content-Type: audio/mpeg\r\n",
      "/status.json"
    )
  check(
    "reserved /status.json: #{reserved.status}",
    reserved.status == 403
  )
  check(
    "reserved /admin/x",
    put(auth=basic("source", "pw"), "/admin/x").status == 403
  )
  check(
    "reserved /a/health",
    put(auth=basic("source", "pw"), "/a/health").status == 403
  )
  check(
    "reserved /a/manage",
    put(auth=basic("source", "pw"), "/a/manage").status == 403
  )
  check(
    "reserved /admin/dec: no source decision",
    put(auth=basic("ok", "x"), "/admin/dec").status == 403 and decisions() == []
  )
  bad =
    put(
      auth=basic("source", "wrong"),
      headers="Content-Type: audio/mpeg\r\n",
      "/x.mp3"
    )
  check(
    "bad credentials: #{bad.status}",
    bad.status == 401
  )
  check(
    "challenge on 401: #{bad.head}",
    string.contains(
      substring="www-authenticate: basic realm=\"icecast2 server\"",
      bad.head
    )
  )
  check(
    "no credentials for an empty password, no Content-Type: 400",
    put("/open.mp3").status == 400
  )
  check(
    "global password refused on a mount with its own",
    put(
      auth=basic("source", "pw"),
      headers="Content-Type: audio/mpeg\r\n",
      "/per.mp3"
    ).status ==
      401
  )
  refused =
    put(
      auth=basic("no", "x"),
      headers="Content-Type: audio/mpeg\r\nUser-Agent: agent-x\r\nX-Test: 1\r\n",
      "/dec.mp3"
    )
  check(
    "source decision refusal is 401",
    refused.status == 401
  )
  wait_for(
    "source decision call",
    {decisions() != []}
  )
  r = list.hd(decisions())
  check(
    "source decision request: #{r.agent} #{r.server.port} #{r.compatibility}",
    r.agent == "agent-x"
    and list.assoc(default="", "x-test", r.headers) == "1"
    and r.server.port == port
    and r.server.instance != ""
    and r.compatibility == "icecast-2.5"
    and r.address == "127.0.0.1"
    and not r.admin
  )
  check(
    "kh bad credentials",
    put(port=kh_port, auth=basic("source", "wrong"), "/x.mp3").status == 401
  )
end

def check_feeding() =
  per = client(user="u", password="mountpw", name="First", "per.mp3")
  wait_for(
    "per-mount credentials feed /per.mp3",
    {list.mem("/per.mp3", mounts())}
  )
  taken =
    put(
      auth=basic("u", "mountpw"),
      headers="Content-Type: audio/mpeg\r\n",
      "/per.mp3"
    )
  check(
    "mount taken under icecast-2.5: #{taken.status}",
    taken.status == 409
  )
  max_sources := 1
  full =
    put(
      auth=basic("source", "pw"),
      headers="Content-Type: audio/mpeg\r\n",
      "/other.mp3"
    )
  check(
    "max_sources under icecast-2.5: #{full.status}",
    full.status == 503
  )
  max_sources := null

  kh_client =
    started(
      output.icecast(%mp3, port=kh_port, password="pw", mount="k.mp3", sine())
    )
  wait_for(
    "kh mount",
    {list.mem("/k.mp3", list.map(fun (m) -> m.mount, kh.stats().mounts))}
  )
  kh_taken =
    put(
      port=kh_port,
      auth=basic("source", "pw"),
      headers="Content-Type: audio/mpeg\r\n",
      "/k.mp3"
    )
  check(
    "mount taken under kh: #{kh_taken.status}",
    kh_taken.status == 403
    and string.contains(
      substring="mountpoint in use",
      kh_taken.head
    )
  )
  kh_client.shutdown()

  listener =
    exchange(
      port,
      "GET /per.mp3 HTTP/1.0\r\n\r\n"
    )
  check(
    "listener gets the identity: #{listener.head}",
    string.contains(
      substring="icy-name: first",
      listener.head
    )
  )

  unauthorized =
    exchange(
      port,
      "GET /admin/killsource?mount=/per.mp3 HTTP/1.0\r\n\r\n"
    )
  check(
    "killsource without credentials: #{unauthorized.status}",
    unauthorized.status == 401
  )
  unknown =
    exchange(
      port,
      "GET /admin/killsource?mount=/none.mp3 HTTP/1.0\r\n#{
        basic("u", "mountpw")
      }\r\n"
    )
  check(
    "killsource on an unknown mount under icecast-2.5",
    unknown.status == 404
  )
  meta =
    exchange(
      port,
      "GET /admin/metadata?mount=/per.mp3&mode=updinfo&song=Hello HTTP/1.0\r\n#{
        basic("u", "mountpw")
      }\r\n"
    )
  check(
    "metadata update: #{meta.status}",
    meta.status == 200
  )
  wait_for(
    "metadata applied",
    {
      list.assoc(default="", "title", mount_stats("/per.mp3").metadata) ==
        "Hello"
    }
  )
  cgi =
    exchange(
      port,
      "GET /admin.cgi?mount=/per.mp3&pass=mountpw&mode=updinfo&song=Cgi \
       HTTP/1.0\r\n\r\n"
    )
  check(
    "/admin.cgi without user is the mount's source_user: #{cgi.status}",
    cgi.status == 200
  )
  wait_for(
    "/admin.cgi metadata applied",
    {list.assoc(default="", "title", mount_stats("/per.mp3").metadata) == "Cgi"}
  )
  query_login =
    exchange(
      port,
      "GET \
       /admin/metadata?mount=/per.mp3&user=u&pass=mountpw&mode=updinfo&song=Query \
       HTTP/1.0\r\n\r\n"
    )
  check(
    "query credentials outside /admin.cgi are refused: #{query_login.status}",
    query_login.status == 401
  )
  no_mode =
    exchange(
      port,
      "GET /admin/metadata?mount=/per.mp3&song=x HTTP/1.0\r\n#{
        basic("u", "mountpw")
      }\r\n"
    )
  check(
    "metadata without updinfo",
    no_mode.status == 400
  )

  per.shutdown()
  wait_for(
    "/per.mp3 gone",
    {not list.mem("/per.mp3", mounts())}
  )
  second = client(user="u", password="mountpw", name="Second", "per.mp3")
  wait_for(
    "/per.mp3 back",
    {list.mem("/per.mp3", mounts())}
  )
  rejoin =
    exchange(
      port,
      "GET /per.mp3 HTTP/1.0\r\n\r\n"
    )
  check(
    "joiner after a reconnect gets the new identity: #{rejoin.head}",
    string.contains(
      substring="icy-name: second",
      rejoin.head
    )
  )
  kill =
    exchange(
      port,
      "GET /admin/killsource?mount=/per.mp3 HTTP/1.0\r\n#{
        basic("u", "mountpw")
      }\r\n"
    )
  check(
    "killsource: #{kill.status}",
    kill.status == 200
  )
  wait_for(
    "killed source disconnect",
    {
      list.exists(
        fun (e) ->
          string.contains(
            prefix="disconnect /per.mp3",
            e
          ),
        events()
      )
    }
  )
  second.shutdown()
end

def check_callbacks() =
  cb = client("cb.mp3")
  wait_for(
    "cb.mp3 connect callback",
    {
      list.mem(
        "connect /cb.mp3",
        callbacks()
      )
    }
  )
  cb_version := "B"
  cb.shutdown()
  wait_for(
    "cb.mp3 disconnect callback",
    {list.length(callbacks()) == 2}
  )
  last = list.nth(default="", callbacks(), 1)
  check(
    "disconnect callback read at connect: #{last}",
    string.contains(
      prefix="disconnect A /cb.mp3 Liquidsoap",
      last
    )
  )
  connects =
    list.filter(
      fun (e) ->
        string.contains(
          prefix="connect /cb.mp3",
          e
        ),
      events()
    )
  check(
    "one source-connect event with agent and port: #{connects}",
    list.length(connects) == 1
    and string.contains(substring="Liquidsoap", list.hd(connects))
    and string.contains(
      suffix=" #{port}",
      list.hd(connects)
    )
  )
  wait_for(
    "cb.mp3 disconnect event",
    {
      list.exists(
        fun (e) ->
          string.contains(
            prefix="disconnect /cb.mp3",
            e
          ),
        events()
      )
    }
  )
  disconnects =
    list.filter(
      fun (e) ->
        string.contains(
          prefix="disconnect /cb.mp3",
          e
        ),
      events()
    )
  check(
    "one source-disconnect event with agent and ip: #{disconnects}",
    list.length(disconnects) == 1
    and string.contains(
      suffix=" true",
      list.hd(disconnects)
    )
  )
end

# Raw protocols with the encoded bytes of `mp3`: SOURCE on the port and
# Shoutcast on the next one.
def check_protocols() =
  data = file.contents(mp3)
  source = connect(port)
  source.write(
    "SOURCE /raw.mp3 HTTP/1.0\r\n#{basic("source", "pw")}Content-Type: \
     audio/mpeg\r\nice-name: Raw\r\n\r\n"
  )
  reply = read_head(source)
  check(
    "SOURCE is answered 200: #{reply.status}",
    reply.status == 200
  )
  source.write(data)
  wait_for(
    "SOURCE feeds /raw.mp3",
    {list.mem("/raw.mp3", mounts())}
  )
  source.close()
  shout = connect(port + 1)
  shout.write("pw\r\n")
  ok = shout.read(timeout=5.)
  check(
    "Shoutcast OK2: #{ok}",
    string.contains(prefix="OK2", ok)
  )
  shout.write(
    "icy-name: Shout\r\ncontent-type: audio/mpeg\r\n\r\n"
  )
  shout.write(data)
  wait_for(
    "Shoutcast feeds /stream",
    {list.mem("/stream", mounts())}
  )
  shout.close()
end

# Two SOURCE clients sent at once on one mount: one feeds it, the other is
# refused or dropped once admitted.
# Two source clients connect at once, on `paths`; returns their replies and
# sockets once one of them feeds its mount.
def race(paths) =
  data = file.contents(mp3)
  results = ref([])
  def feed(path) =
    sock = connect(port)
    sock.write(
      "SOURCE #{path} HTTP/1.0\r\n#{basic("source", "pw")}Content-Type: \
       audio/mpeg\r\n\r\n"
    )
    reply = read_head(sock)
    if
      reply.status == 200
    then
      try
        sock.write(data)
      catch _ do
        ()
      end
    end
    atomic(
      fun () -> results := [...results(), {status = reply.status, sock = sock}]
    )
  end
  list.iter(fun (path) -> thread.run(fun () -> feed(path)), paths)
  wait_for(
    "both race replies",
    {list.length(results()) == 2}
  )
  wait_for(
    "race mount fed",
    {list.exists(fun (path) -> list.mem(path, mounts()), paths)}
  )
  results()
end

# The server never writes to a source client: a kept connection's read waits
# for its timeout, a dropped one ends at once, closed or reset.
def kept(r) =
  started = time()
  chunk =
    try
      r.sock.read(timeout=3.)
    catch _ do
      ""
    end
  r.status == 200 and (chunk != "" or time() - started > 2.5)
end

def check_race() =
  # Data runs out while the outcome is judged: the kept client must not be
  # dropped for silence meanwhile.
  source_timeout := 60.
  try
    results = race(["/race.mp3", "/race.mp3"])
    statuses = list.map(fun (r) -> r.status, results)
    check(
      "exactly one racing client kept: #{statuses}",
      list.length(list.filter(kept, results)) == 1
    )
    connects =
      list.filter(
        fun (e) ->
          string.contains(
            prefix="connect /race.mp3",
            e
          ),
        events()
      )
    check(
      "exactly one race source-connect event: #{connects}",
      list.length(connects) == 1
    )
    list.iter(fun (r) -> r.sock.close(), results)
  finally
    source_timeout := 10.
  end
end

# `source_timeout` is read when a source client connects: a value lowered after
# startup drops the next silent client within it.
def check_source_timeout() =
  source_timeout := 1.
  sock = connect(port)
  try
    sock.write(
      "SOURCE /quiet.mp3 HTTP/1.0\r\n#{basic("source", "pw")}Content-Type: \
       audio/mpeg\r\n\r\n"
    )
    reply = read_head(sock)
    sock.write(file.contents(mp3))
    wait_for(
      "quiet mount fed",
      {list.mem("/quiet.mp3", mounts())}
    )
    started = time()
    wait_for(
      "silent source client dropped",
      {not list.mem("/quiet.mp3", mounts())}
    )
    check(
      "a lowered source_timeout applies to the next client: #{reply.status} \
       after #{time() - started}s",
      reply.status == 200 and time() - started < 8.
    )
  finally
    source_timeout := 10.
    sock.close()
  end
end

def check_race_max_sources() =
  max_sources := 1
  source_timeout := 60.
  try
    results = race(["/ra.mp3", "/rb.mp3"])
    statuses = list.map(fun (r) -> r.status, results)
    check(
      "max_sources holds for clients connecting at once: #{statuses}",
      list.length(list.filter(kept, results)) == 1
    )
    list.iter(fun (r) -> r.sock.close(), results)
  finally
    max_sources := null
    source_timeout := 10.
  end
end

def check_identity() =
  identity =
    icecast.server.sources.identity(
      [
        ("ice-name", "A"),
        ("icy-name", "B"),
        ("ice-genre", "x\ny"),
        ("icy-br", "128")
      ]
    )
  check(
    "first identity form wins: #{identity}",
    list.assoc(default="", "icy-name", identity) == "A"
  )
  check(
    "identity value with a line break dropped",
    not list.assoc.mem("icy-genre", identity)
  )
  check(
    "icy-br passed",
    list.assoc(default="", "icy-br", identity) == "128"
  )
end

def check_url_services() =
  refused =
    put(
      auth=basic("bad", "x"),
      headers="Content-Type: audio/mpeg\r\nUser-Agent: agent-y\r\n",
      "/sa.mp3"
    )
  check(
    "stream_auth refusal is 401: #{refused.status}",
    refused.status == 401
  )
  form = list.hd(default=[], backend_forms("/auth"))
  check(
    "stream_auth fields: #{form}",
    list.assoc(default="", "action", form) == "stream_auth"
    and list.assoc(default="", "mount", form) == "/sa.mp3"
    and list.assoc(default="", "port", form) == "#{port}"
    and list.assoc(default="", "server-instance", form) != ""
    and list.assoc(default="", "ip", form) == "127.0.0.1"
    and list.assoc(default="", "agent", form) == "agent-y"
    and list.assoc(default="", "user", form) == "bad"
    and list.assoc(default="", "pass", form) == "x"
    and not list.assoc.mem("admin", form)
  )
  sa = client(user="good", password="any", "sa.mp3")
  wait_for(
    "stream_auth admits /sa.mp3",
    {list.mem("/sa.mp3", mounts())}
  )
  wait_for("mount_add", {backend_forms("/add") != []})
  admin =
    exchange(
      port,
      "GET /admin/metadata?mount=/sa.mp3&mode=updinfo&song=x HTTP/1.0\r\n#{
        basic("good", "any")
      }\r\n"
    )
  admin_forms =
    list.filter(
      fun (form) -> list.assoc(default="", "admin", form) == "1",
      backend_forms("/auth")
    )
  check(
    "an admin command's stream_auth carries the requester's address: #{
      admin.status
    } #{admin_forms}",
    admin.status == 200
    and admin_forms != []
    and list.for_all(
      fun (form) -> list.assoc(default="", "ip", form) == "127.0.0.1",
      admin_forms
    )
  )
  sa.shutdown()
  wait_for("mount_remove", {backend_forms("/remove") != []})
  add = list.hd(backend_forms("/add"))
  remove = list.hd(backend_forms("/remove"))
  check(
    "mount_add and mount_remove once each with ip and agent: #{add} #{remove}",
    list.length(backend_forms("/add")) == 1
    and list.length(backend_forms("/remove")) == 1
    and list.assoc(default="", "action", remove) == "mount_remove"
    and list.assoc(default="", "mount", remove) == "/sa.mp3"
    and list.assoc(default="", "ip", remove) == "127.0.0.1"
    and string.contains(
      prefix="Liquidsoap",
      list.assoc(default="", "agent", add)
    )
    and list.assoc(default="", "agent", remove) ==
      list.assoc(default="", "agent", add)
  )
end

def run() =
  try
    wait_for(
      "encoded mp3 bytes",
      {file.exists(mp3) and file.size(mp3) > 40000}
    )
    encoder.shutdown()
    check_identity()
    check_refusals()
    check_feeding()
    check_callbacks()
    check_protocols()
    check_race()
    check_race_max_sources()
    check_source_timeout()
    check_url_services()
    if
      checks() < 40
    then
      test.fail(
        "only #{checks()} checks ran"
      )
    end
    test.pass()
  catch err do
    test.fail(
      "#{err.kind}: #{err.message}"
    )
  finally
    try
      file.remove(mp3)
    catch _ do
      ()
    end
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
