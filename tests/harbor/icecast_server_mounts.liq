%include "../test.liq"

# Mounts of icecast.server (mounts.md): an attached Liquidsoap source feeds a
# mount and keeps its listeners while it has no data; a mount switches to its
# fallback and back without disconnecting a listener (Improvement), follows
# chains and stops at loops; linger keeps listeners through a source blip
# (Improvement), also across new MP3 parameters; a mount with nothing to send
# shuts down; fallback_when_full redirects; a configured mount with no output
# is created by a listener request through its live fallback; dump files.
port = 18174
checks = ref(0)
sockets = ref([])
dump_dir = file.temp_dir("icecast_mounts")

def check(label, cond) =
  checks := checks() + 1
  if not cond then test.fail(label) end
end

def connect() =
  sock = socket.unix()
  sockets := [...sockets(), sock]
  sock.connect(
    socket.address.internet_address(socket.internet_address("127.0.0.1"), port)
  )
  sock
end

def read_head(sock) =
  data = ref("")
  deadline = time() + 10.
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
      and time() < deadline
      and not string.contains(encoding="ascii", substring="\r\n\r\n", data())
    then
      read_more()
    end
  end
  read_more()
  let [head, ...rest] = string.split(separator="\r\n\r\n", data())
  {
    status =
      int_of_string(
        default=0,
        string.sub(encoding="ascii", head, start=9, length=3)
      ),
    head = string.case(lower=true, head),
    rest = string.concat(rest)
  }
end

# A listener socket on `path` and its reply, after the redirect to `path` of
# a request that came while the mount output was starting.
def listen(~follow=true, path) =
  sock = connect()
  sock.write(
    "GET #{path} HTTP/1.0\r\n\r\n"
  )
  reply = read_head(sock)
  if
    follow
    and reply.status == 307
    and string.contains(
      substring="location: #{path}\r",
      reply.head ^ "\r"
    )
  then
    sock.close()
    sock = connect()
    sock.write(
      "GET #{path} HTTP/1.0\r\n\r\n"
    )
    read_head(sock).{sock = sock}
  else
    reply.{sock = sock}
  end
end

def request(path) =
  r = listen(follow=false, path)
  r.sock.close()
  r
end

# Bytes `sock` receives within `duration`, and whether it was closed.
def receive(~until_data=false, sock, duration) =
  deadline = time() + duration
  data = ref("")
  closed = ref(false)
  def rec read_more() =
    remaining = deadline - time()
    if
      remaining > 0. and not closed() and not (until_data and data() != "")
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

# Whether `sock` keeps receiving: bytes within 2 seconds, or the first ones
# within 10 seconds when a new feeder is still connecting.
def streaming(label, sock) =
  r = receive(sock, 2.)
  r = r.data == "" and not r.closed ? receive(until_data=true, sock, 10.) : r
  check(
    "#{label}: still connected",
    not r.closed
  )
  check(
    "#{label}: receives stream bytes",
    r.data != ""
  )
end

def mount(path) =
  if
    path == "/main.mp3"
  then
    {fallback = {mount = "/live.mp3"}}
  elsif
    path == "/linger.mp3"
  then
    {linger = 4.}
  elsif
    path == "/c1.mp3"
  then
    {fallback = {mount = "/c2.mp3"}}
  elsif
    path == "/c2.mp3"
  then
    {fallback = {mount = "/live.mp3"}}
  elsif
    path == "/l1.mp3"
  then
    {fallback = {mount = "/l2.mp3"}}
  elsif
    path == "/l2.mp3"
  then
    {fallback = {mount = "/l1.mp3"}}
  elsif
    path == "/full.mp3"
  then
    {
      max_listeners = 1,
      fallback_when_full = true,
      fallback = {mount = "/live.mp3"}
    }
  elsif
    path == "/virt.mp3"
  then
    {fallback = {mount = "/live.mp3"}}
  elsif
    path == "/skip.mp3"
  then
    {fallback = {mount = "/o.ogg"}}
  elsif
    path == "/o.ogg"
  then
    {linger = 10.}
  elsif
    path == "/dump.mp3"
  then
    {dump_file = "#{dump_dir}/dump-%H%M%S.mp3", dump_file_size_limit = 20000}
  else
    null
  end
end

server = icecast.server({port = port, mount = mount})

connects = ref([])
server.on_source_connect(
  synchronous=true,
  fun (r) -> atomic(fun () -> connects := [...connects(), r.mount])
)
disconnects = ref([])
server.on_source_disconnect(
  synchronous=true,
  fun (r) -> atomic(fun () -> disconnects := [...disconnects(), r.mount])
)

def mounts() =
  list.map(fun (m) -> m.mount, server.stats().mounts)
end

# Outputs created by the test thread start when it returns: start them now.
def started(o) =
  try
    clock(o.clock).start(force=false)
  catch _ do
    ()
  end
  o
end

def client(~bitrate=128, path) =
  started(
    output.icecast(
      %mp3(bitrate = bitrate),
      port=port,
      password="hackme",
      mount=path,
      sine()
    )
  )
end

ready = ref(true)
live =
  ffmpeg.encode.audio(
    %ffmpeg(%audio(codec = "libmp3lame", sample_format = "fltp")),
    switch([({ready()}, sine().{track_sensitive = false})])
  )
mp3_copy = %ffmpeg(format = "mp3", %audio.copy)

def check_attached() =
  server.attach_source(
    format=mp3_copy,
    headers=[("ice-name", "Live")],
    "/live.mp3",
    live
  )
  wait_for(
    "/live.mp3 attached",
    {list.mem("/live.mp3", mounts())}
  )
  check(
    "attach raises a source-connect event",
    list.mem("/live.mp3", connects())
  )
  try
    server.attach_source(format=mp3_copy, "/live.mp3", live)
    check(
      "attach to a fed mount raises",
      false
    )
  catch _ : [error.icecast.server.mount_taken] do
    check(
      "attach to a fed mount raises mount_taken",
      true
    )
  end
  try
    server.attach_source(format=mp3_copy, "/admin/x", live)
    check(
      "attach to a reserved path raises",
      false
    )
  catch _ : [error.icecast.server.reserved_path] do
    check(
      "attach to a reserved path raises reserved_path",
      true
    )
  end
  admin = connect()
  admin.write(
    "GET /admin/metadata?mount=/live.mp3&mode=updinfo&song=Attached \
     HTTP/1.0\r\nAuthorization: Basic #{
      string.base64.encode("source:hackme")
    }\r\n\r\n"
  )
  meta = read_head(admin)
  admin.close()
  check(
    "attached mp3 mount takes a metadata update: #{meta.status}",
    meta.status == 200
  )
  l = listen("/live.mp3")
  check(
    "attached source listener: #{l.status}",
    l.status == 200
    and string.contains(
      substring="icy-name: live",
      l.head
    )
  )
  streaming(
    "attached source listener",
    l.sock
  )
  ready := false
  wait_for(
    "attached source paused",
    {receive(l.sock, 0.6).data == ""}
  )
  r = receive(l.sock, 2.)
  check(
    "idle attached source keeps its listener",
    not r.closed and r.data == ""
  )
  check(
    "idle attached source keeps its mount",
    list.mem("/live.mp3", mounts())
  )
  ready := true
  streaming(
    "attached source back",
    l.sock
  )
  l.sock.close()
end

def check_fallback() =
  c = client("main.mp3")
  wait_for("/main.mp3", {list.mem("/main.mp3", mounts())})
  l = listen("/main.mp3")
  check(
    "main listener",
    l.status == 200
  )
  streaming(
    "main listener on its source",
    l.sock
  )
  c.shutdown()
  streaming(
    "main listener on the fallback",
    l.sock
  )
  check(
    "main mount kept on its fallback",
    list.mem("/main.mp3", mounts())
  )
  c = client("main.mp3")
  wait_for(
    "main source back",
    {list.length(list.filter(fun (m) -> m == "/main.mp3", connects())) == 2}
  )
  streaming(
    "main listener back on its source",
    l.sock
  )
  l.sock.close()
  c
end

def check_chains() =
  c1 = client("c1.mp3")
  wait_for("/c1.mp3", {list.mem("/c1.mp3", mounts())})
  l = listen("/c1.mp3")
  c1.shutdown()
  streaming(
    "chain c1 -> c2 -> live",
    l.sock
  )
  l.sock.close()
  l1 = client("l1.mp3")
  wait_for("/l1.mp3", {list.mem("/l1.mp3", mounts())})
  ll = listen("/l1.mp3")
  l1.shutdown()
  wait_for(
    "loop l1 <-> l2 shuts l1 down",
    {not list.mem("/l1.mp3", mounts())}
  )
  check(
    "loop: listener disconnected",
    receive(ll.sock, 3.).closed
  )
  check(
    "loop: path answers 404",
    request("/l1.mp3").status == 404
  )
end

def check_linger() =
  c = client("linger.mp3")
  wait_for("/linger.mp3", {list.mem("/linger.mp3", mounts())})
  l = listen("/linger.mp3")
  streaming(
    "linger listener",
    l.sock
  )
  c.shutdown()
  wait_for(
    "linger source gone",
    {receive(l.sock, 0.6).data == ""}
  )
  r = receive(l.sock, 1.)
  check(
    "linger keeps the listener",
    not r.closed
  )
  c = client(bitrate=64, "linger.mp3")
  streaming(
    "linger listener with a new source of other MP3 parameters",
    l.sock
  )
  c.shutdown()
  wait_for(
    "linger window over",
    {not list.mem("/linger.mp3", mounts())}
  )
  check(
    "after linger the listener is disconnected",
    receive(l.sock, 3.).closed
  )

  n = client("nolinger.mp3")
  wait_for("/nolinger.mp3", {list.mem("/nolinger.mp3", mounts())})
  nl = listen("/nolinger.mp3")
  n.shutdown()
  wait_for(
    "/nolinger.mp3 gone",
    {not list.mem("/nolinger.mp3", mounts())}
  )
  check(
    "without linger the listener is disconnected",
    receive(nl.sock, 3.).closed
  )
  check(
    "without linger the path answers 404",
    request("/nolinger.mp3").status == 404
  )
end

def check_full_and_virtual() =
  f = client("full.mp3")
  wait_for("/full.mp3", {list.mem("/full.mp3", mounts())})
  first = listen("/full.mp3")
  check(
    "first listener of a full mount",
    first.status == 200
  )
  wait_for(
    "first listener counted",
    {
      list.exists(
        fun (m) -> m.mount == "/full.mp3" and list.length(m.listeners) == 1,
        server.stats().mounts
      )
    }
  )
  second = request("/full.mp3?x=1")
  check(
    "fallback_when_full redirects: #{second.status} #{second.head}",
    second.status == 307
    and string.contains(
      substring="location: /live.mp3?x=1",
      second.head
    )
  )
  first.sock.close()
  f.shutdown()

  check(
    "unconfigured mount without output answers 404",
    request("/nothing.mp3").status == 404
  )
  before = list.length(connects())
  redirect = request("/virt.mp3?a=b")
  check(
    "configured mount with a live fallback is created: #{redirect.status}",
    redirect.status == 307
    and string.contains(
      substring="location: /virt.mp3?a=b",
      redirect.head
    )
  )
  v = listen("/virt.mp3")
  check(
    "listener admitted to the created mount: #{v.status}",
    v.status == 200
  )
  streaming(
    "created mount fed by its fallback",
    v.sock
  )
  check(
    "no source-connect event for a created mount",
    list.length(connects()) == before
  )
  v.sock.close()
end

def check_format() =
  o =
    started(
      output.icecast(
        %ogg(%vorbis),
        port=port,
        password="hackme",
        mount="o.ogg",
        sine()
      )
    )
  wait_for("/o.ogg", {list.mem("/o.ogg", mounts())})
  try
    server.attach_source(format=mp3_copy, "/o.ogg", live)
    check(
      "attach to a fed mount of another format raises",
      false
    )
  catch _ : [error.icecast.server.mount_taken] do
    check(
      "attach to a fed mount of another format raises mount_taken",
      true
    )
  end
  s = client("skip.mp3")
  wait_for("/skip.mp3", {list.mem("/skip.mp3", mounts())})
  sl = listen("/skip.mp3")
  s.shutdown()
  wait_for(
    "fallback of another format skipped",
    {not list.mem("/skip.mp3", mounts())}
  )
  check(
    "skipped fallback: listener disconnected",
    receive(sl.sock, 3.).closed
  )
  o.shutdown()
  wait_for(
    "/o.ogg lingering",
    {list.mem("/o.ogg", disconnects())}
  )
  try
    server.attach_source(format=mp3_copy, "/o.ogg", live)
    check(
      "attach to an output of another format raises",
      false
    )
  catch _ : [error.icecast.server.format_mismatch] do
    check(
      "attach to an output of another format raises format_mismatch",
      true
    )
  end
end

def check_dump() =
  d = client("dump.mp3")
  wait_for("/dump.mp3", {list.mem("/dump.mp3", mounts())})
  l = listen("/dump.mp3")
  wait_for(
    "dump file over its size limit",
    {
      list.exists(
        fun (f) -> file.size("#{dump_dir}/#{f}") > 20000,
        file.ls(dump_dir)
      )
    }
  )
  streaming(
    "dump size limit leaves the stream",
    l.sock
  )
  def dump_size() =
    list.fold(
      fun (n, f) -> n + file.size("#{dump_dir}/#{f}"),
      0,
      file.ls(dump_dir)
    )
  end
  last = ref(dump_size())
  def settled() =
    before = last()
    thread.pause(0.2)
    last := dump_size()
    last() == before
  end
  wait_for(
    "dump size settled",
    settled
  )
  size = last()

  # A non-growth check: the stream goes on, the file must not.
  thread.pause(1.5)
  after = dump_size()
  check(
    "dump stopped at its size limit: #{size} then #{after}",
    size == after and size < 80000
  )
  d.shutdown()
  l.sock.close()
  wait_for(
    "/dump.mp3 gone",
    {not list.mem("/dump.mp3", mounts())}
  )
  first_second = time.string("%H%M%S")
  wait_for(
    "a new second for the next dump name",
    {time.string("%H%M%S") != first_second}
  )
  d = client("dump.mp3")
  wait_for(
    "second dump file",
    {list.length(file.ls(dump_dir)) == 2}
  )
  check(
    "one dump file per feeder connection",
    true
  )
  d.shutdown()
end

def run() =
  try
    check_attached()
    main = check_fallback()
    check_chains()
    check_linger()
    check_full_and_virtual()
    check_format()
    check_dump()
    main.shutdown()
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
