%include "../test.liq"

# Events and statistics of icecast.server (observability.md §1 to §5, §13):
# one connect and one later disconnect per admitted listener and per feeder,
# none for a refused listener; synchronous handlers in order, counted in the
# stats they read; a raising or blocking handler changes nothing; the listener
# record and its hashed address; mount counters across a source reconnect,
# metadata events and a bounded history; server-wide counters.
port = 18182
checks = ref(0)
sockets = ref([])

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

# The status of the reply to `text`, and the socket, left open. A listener
# request that came while its mount output was starting is redirected to
# itself and sent again.
def send(text) =
  def rec attempt() =
    sock = connect()
    sock.write(text)
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
    status =
      int_of_string(
        default=0,
        string.sub(encoding="ascii", data(), start=9, length=3)
      )
    if
      status == 307
    then
      sock.close()
      attempt()
    else
      {status = status, sock = sock}
    end
  end

  attempt()
end

def listen(path) =
  send(
    "GET #{path} HTTP/1.1\r\nUser-Agent: agent-x\r\nReferer: \
     http://r/\r\nX-Forwarded-For: 10.9.8.7\r\n\r\n"
  )
end

# Whether `sock` receives bytes within `duration`.
def receives(sock, duration) =
  try
    sock.read(timeout=duration) != ""
  catch _ do
    false
  end
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

# Source admissions on /bad.mp3, whose connection fails while `bindings_fail`.
bad_attempts = ref(0)
bindings_fail = ref(false)

def mount(path) =
  if
    path == "/e.mp3"
  then
    {linger = 10., max_history = 2}
  elsif
    path == "/bad.mp3"
  then
    {
      source_decision =
        fun (_) ->
          begin
            atomic(fun () -> bad_attempts := bad_attempts() + 1)
            true
          end
    }
  elsif
    path == "/h.mp3"
  then
    {
      hidden = true,
      listener_decision = fun (_) -> {refuse = icecast.server.refusal(403)}
    }
  else
    null
  end
end

log_lines = ref([])
def add(line) =
  atomic(fun () -> log_lines := [...log_lines(), line])
end

def binding(event) =
  {
    event = event,
    synchronous = true,
    handler =
      fun (b) ->
        add(
          "binding #{b.event} #{b.mount} #{b.listeners} #{b.compatibility}"
        )
  }
end

server =
  icecast.server(
    {
      port = port,
      mount = mount,
      event_bindings =
        {
          bindings_fail()
            ? error.raise(
              error.invalid,
              "bindings fail"
            )
            : [binding("listener_connect"), binding("listener_disconnect")]
        }
    }
  )

server.on_listener_connect(
  synchronous=true,
  fun (_) ->
    error.raise(
      error.not_found,
      "a failing handler"
    )
)
server.on_listener_connect(
  synchronous=true,
  fun (l) ->
    begin
      counted =
        list.exists(
          fun (m) -> m.mount == l.mount and list.length(m.listeners) >= 1,
          server.stats().mounts
        )
      add(
        "connect1 #{l.mount} #{l.duration} #{l.bytes_sent} #{counted} #{l.ip}"
      )
    end
)
server.on_listener_connect(
  synchronous=true,
  fun (l) ->
    add(
      "connect2 #{l.mount}"
    )
)
server.on_listener_connect(fun (_) -> thread.pause(5.))
records = ref([])
server.on_listener_disconnect(
  synchronous=true,
  fun (l) ->
    begin
      atomic(fun () -> records := [...records(), l])
      add(
        "disconnect #{l.mount}"
      )
    end
)
sources = ref([])
server.on_source_connect(
  synchronous=true,
  fun (r) ->
    add(
      "source_connect #{r.mount} #{r.agent} #{r.server.port}"
    )
)
server.on_source_disconnect(
  synchronous=true,
  fun (r) ->
    add(
      "source_disconnect #{r.mount} #{r.agent}"
    )
)
metadata_events = ref([])
server.on_metadata(
  synchronous=true,
  fun (m) ->
    atomic(
      fun () ->
        metadata_events :=
          [
            ...metadata_events(),
            {
              mount = m.mount,
              title = list.assoc(default="", "title", m.metadata),
              listeners = m.listeners
            }
          ]
    )
)

def started(o) =
  try
    clock(o.clock).start(force=false)
  catch _ do
    ()
  end
  o
end

def client(path) =
  started(
    output.icecast(%mp3, port=port, password="hackme", mount=path, sine())
  )
end

def lines(prefix) =
  list.filter(fun (l) -> string.contains(prefix=prefix, l), log_lines())
end

def stats_of(path) =
  list.find(fun (m) -> m.mount == path, server.stats().mounts)
end

def mounts() =
  list.map(fun (m) -> m.mount, server.stats().mounts)
end

def metadata(title) =
  r =
    send(
      "GET /admin/metadata?mount=/e.mp3&mode=updinfo&song=#{title} \
       HTTP/1.0\r\nAuthorization: Basic #{
        string.base64.encode("source:hackme")
      }\r\n\r\n"
    )
  r.sock.close()
  r.status
end

def check_listeners() =
  first = listen("/e.mp3")
  check(
    "first listener",
    first.status == 200
  )
  check(
    "a blocking asynchronous handler does not delay streaming",
    receives(first.sock, 2.)
  )
  connects =
    lines(
      "connect1 /e.mp3"
    )
  check(
    "connect event after a raising handler, counted in stats, zero duration and \
     bytes, hashed address: #{connects}",
    list.length(connects) == 1
    and r/^connect1 \/e\.mp3 0\.0* 0 true [0-9a-f]{16}$/.test(list.hd(connects))
  )
  check(
    "synchronous handlers run in registration order: #{log_lines()}",
    list.length(lines("connect")) == 2
    and string.contains(
      prefix="connect1",
      list.nth(default="", lines("connect"), 0)
    )
  )
  second = listen("/e.mp3")
  wait_for(
    "second listener counted",
    {list.length(stats_of("/e.mp3").listeners) == 2}
  )
  first.sock.close()
  wait_for(
    "first disconnect",
    {list.length(lines("disconnect")) == 1}
  )
  r = list.hd(records())
  check(
    "listener record: #{r.user_agent} #{r.referer} #{r.uri} #{r.protocol}",
    r.user_agent == "agent-x"
    and r.referer == "http://r/"
    and r.uri == "/e.mp3"
    and r.protocol == "1.1"
    and r.duration > 0.
    and r.bytes_sent > 0
    and r.connected_at > 0.
  )
  check(
    "the record carries no raw address: #{r.ip}",
    r.ip != "127.0.0.1" and r.ip != "10.9.8.7" and string.length(r.ip) == 16
  )
  s = stats_of("/e.mp3")
  check(
    "totals count the finished session only: #{s.listening_time} #{
      s.bytes_sent
    }",
    s.listening_time == r.duration and s.bytes_sent == r.bytes_sent
  )
  check(
    "peak, connections and current listeners: #{s.peak_listeners} #{
      s.connections
    }",
    s.peak_listeners == 2
    and s.connections == 2
    and list.length(s.listeners) == 1
  )
  hidden =
    send(
      "GET /h.mp3 HTTP/1.0\r\n\r\n"
    )
  hidden.sock.close()
  check(
    "refused listener: #{hidden.status}",
    hidden.status == 403
  )
  check(
    "no events for a refused listener",
    lines(
      "connect1 /h.mp3"
    ) ==
      []
    and lines(
      "disconnect /h.mp3"
    ) ==
      []
  )
  check(
    "a hidden mount is in stats",
    stats_of("/h.mp3").hidden
  )
  second
end

def check_metadata() =
  def update(title) =
    status = metadata(title)
    wait_for(
      "metadata event #{title}: #{metadata_events()}",
      {list.exists(fun (m) -> m.title == title, metadata_events())}
    )
    status
  end

  check(
    "metadata updates",
    update("one") == 200 and update("two") == 200 and update("three") == 200
  )
  titles = list.map(fun (h) -> h.title, stats_of("/e.mp3").history)
  check(
    "history bounded by max_history, newest first: #{titles}",
    titles == ["three", "two"]
  )
  check(
    "metadata event with the mount and its listener count: #{
      metadata_events()
    }",
    list.exists(
      fun (m) ->
        m.mount == "/e.mp3" and m.title == "three" and m.listeners == 1,
      metadata_events()
    )
  )
end

def check_reconnect(source) =
  before = stats_of("/e.mp3")
  source.shutdown()
  wait_for(
    "source disconnect",
    {
      lines(
        "source_disconnect /e.mp3"
      ) !=
        []
    }
  )
  source = client("e.mp3")
  wait_for(
    "source reconnect",
    {
      list.length(
        lines(
          "source_connect /e.mp3"
        )
      ) ==
        2
    }
  )
  after = stats_of("/e.mp3")
  check(
    "counters survive a source reconnect and started changes: #{
      before.connections
    } #{after.connections}",
    after.connections == before.connections
    and after.peak_listeners == before.peak_listeners
    and after.started > before.started
  )
  check(
    "one source event per feeder, with agent and port: #{lines("source_")}",
    list.length(
      lines(
        "source_disconnect /e.mp3"
      )
    ) ==
      1
    and string.contains(
      substring="Liquidsoap",
      list.hd(
        lines(
          "source_connect /e.mp3"
        )
      )
    )
    and string.contains(
      suffix=" #{port}",
      list.hd(
        lines(
          "source_connect /e.mp3"
        )
      )
    )
  )
  source
end

def check_attached() =
  encoded = ffmpeg.encode.audio(%ffmpeg(%audio(codec = "libmp3lame")), sine())
  server.attach_source(
    format=%ffmpeg(format = "mp3", %audio.copy),
    "/att.mp3",
    encoded
  )
  server.detach_source("/att.mp3")
  wait_for(
    "attached source events",
    {
      lines(
        "source_disconnect /att.mp3"
      ) !=
        []
    }
  )
  check(
    "one source event each for a Liquidsoap source, empty agent: #{
      lines(
        "source_connect /att.mp3"
      )
    }",
    lines(
      "source_connect /att.mp3"
    ) ==
      [
        "source_connect /att.mp3  #{port}"
      ]
    and list.length(
      lines(
        "source_disconnect /att.mp3"
      )
    ) ==
      1
  )
end

# A source client whose connection fails while it takes the mount gets
# neither a connect nor a disconnect event, and leaves no feeder.
def check_failed_take() =
  bindings_fail := true
  bad = client("bad.mp3")
  try
    wait_for(
      "two failed /bad.mp3 attempts",
      {bad_attempts() >= 2}
    )
    events =
      list.filter(
        fun (line) ->
          string.contains(
            substring=" /bad.mp3",
            line
          ),
        log_lines()
      )
    check(
      "a failed take raises no source event: #{events} #{mounts()}",
      events == [] and not list.mem("/bad.mp3", mounts())
    )
  finally
    bad.shutdown()
    bindings_fail := false
  end
end

def run() =
  try
    source = client("e.mp3")
    hidden = client("h.mp3")
    wait_for(
      "mounts",
      {list.mem("/e.mp3", mounts()) and list.mem("/h.mp3", mounts())}
    )
    second = check_listeners()
    check_metadata()
    source = check_reconnect(source)
    check_attached()
    check_failed_take()
    second.sock.close()
    wait_for(
      "second disconnect",
      {
        list.length(
          lines(
            "disconnect /e.mp3"
          )
        ) ==
          2
      }
    )
    def bindings() =
      list.filter(
        fun (line) ->
          string.contains(
            substring=" /e.mp3 ",
            line
          ),
        lines("binding")
      )
    end

    wait_for(
      "four bindings",
      {list.length(bindings()) == 4}
    )
    check(
      "bindings receive the mount's count after each event: #{bindings()}",
      bindings() ==
        [
          "binding listener_connect /e.mp3 1 icecast-2.5",
          "binding listener_connect /e.mp3 2 icecast-2.5",
          "binding listener_disconnect /e.mp3 1 icecast-2.5",
          "binding listener_disconnect /e.mp3 0 icecast-2.5"
        ]
    )
    check(
      "exactly one connect and one disconnect per listener",
      list.length(
        lines(
          "connect1 /e.mp3"
        )
      ) ==
        2
    )
    totals = server.stats()
    check(
      "server-wide counters: #{totals.listener_connections} #{
        totals.source_connections
      }",
      totals.listener_connections == 2
      and totals.source_connections == 4
      and totals.server_start > 0.
      and totals.last_reload == totals.server_start
    )
    source.shutdown()
    hidden.shutdown()
    if
      checks() != 21
    then
      test.fail(
        "#{checks()} checks ran, expected 21"
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
