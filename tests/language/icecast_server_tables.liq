%include "../test.liq"

def f() =
  # mount_table: fallback wiring between mounts
  t = icecast.server.mount_table()
  created = ref([])
  def create(mount) =
    fun (_) ->
      begin
        created := [mount, ...created()]
        {mount = mount}
      end
  end
  a = blank(id="a")
  b = blank(id="b")
  b2 = blank(id="b2")

  # /a falls back on /b, which is not there yet
  entry_a = t.connect(mount="/a", fallback_mount="/b", source=a, create("/a"))
  test.equal(created(), ["/a"])
  test.equal(entry_a.data.mount, "/a")
  test.equal(null.defined(entry_a.source()), true)
  test.equal(null.defined(entry_a.fallback()), false)
  test.equal(source.id(null.get(t.source("/a"))), "a")
  test.equal(null.defined(t.source("/b")), false)

  # /b arrives: /a now sees it as fallback
  ignore(t.connect(mount="/b", fallback_mount=null, source=b, create("/b")))
  test.equal(created(), ["/b", "/a"])
  test.equal(source.id(null.get(entry_a.fallback())), "b")

  # /a's source leaves: entry survives on its fallback, nothing shut down
  test.equal(list.map(fst, t.disconnect("/a")), [])
  test.equal(t.has("/a"), true)
  test.equal(null.defined(t.source("/a")), false)

  # /a reconnects: existing entry reused, create not called again
  ignore(t.connect(mount="/a", fallback_mount="/b", source=a, create("/a")))
  test.equal(created(), ["/b", "/a"])
  test.equal(source.id(null.get(t.get("/a").source())), "a")

  # /b leaves: with no fallback of its own it is torn down at once, while /a
  # keeps its output and loses its fallback. A new /b source gets a new entry
  # and /a's fallback follows it.
  test.equal(list.map(fst, t.disconnect("/b")), ["/b"])
  test.equal(t.has("/b"), false)
  test.equal(null.defined(t.get("/a").fallback()), false)
  ignore(t.connect(mount="/b", fallback_mount=null, source=b2, create("/b")))
  test.equal(created(), ["/b", "/b", "/a"])
  test.equal(source.id(null.get(t.get("/a").fallback())), "b2")

  # /a leaves, then /b leaves: both are orphaned and returned together
  test.equal(list.map(fst, t.disconnect("/a")), [])
  orphans = list.map(fst, t.disconnect("/b"))
  test.equal(
    list.sort(fun (x, y) -> if x < y then -1 else 1 end, orphans),
    ["/a", "/b"]
  )
  test.equal(list.map(fst, t.all()), [])

  # A mount without fallback is orphaned as soon as its source leaves
  ignore(t.connect(mount="/c", fallback_mount=null, source=a, create("/c")))
  test.equal(list.map(fst, t.disconnect("/c")), ["/c"])
  test.equal(list.map(fst, t.disconnect("/unknown")), [])

  # listener_counters: peak, connections and finished-session totals
  c = icecast.server.listener_counters()
  c.connect()
  c.connect()
  c.disconnect({duration = 1.5, bytes_sent = 10})
  c.connect()
  c.disconnect({duration = 2., bytes_sent = 5})
  test.equal(
    c.get(),
    {peak = 2, connections = 3, listening_time = 3.5, bytes_sent = 15}
  )

  # ip_hash: 16 hex characters, same value on every run
  hash = icecast.server.ip_hash
  test.equal(
    hash("1.2.3.4"),
    string.sub(string.digest("icecast.server:1.2.3.4"), start=0, length=16)
  )
  test.equal(r/^[0-9a-f]{16}$/.test(hash("1.2.3.4")), true)
  test.equal(hash("1.2.3.4") == hash("1.2.3.5"), false)

  # listener_view: X-Forwarded-For resolved, then hashed; headers dropped
  harbor_listener =
    {
      id = 7,
      ip = "10.0.0.1",
      uri = "/a",
      protocol = "1.1",
      headers = [("x-forwarded-for", "5.5.5.5"), ("user-agent", "VLC/3.0")],
      connected_at = 100.,
      duration = 61.9,
      bytes_sent = 4096
    }
  view =
    icecast.server.listener_view(
      mount="/a",
      x_forwarded_for=null,
      proxy_ips=[],
      ip_hash=fun (ip) -> "hash(#{ip})",
      harbor_listener
    )
  test.equal(view.ip, "hash(5.5.5.5)")
  test.equal(view.user_agent, "VLC/3.0")
  test.equal(view.referer, null)
  test.equal(view.id, 7)
  test.equal(view.mount, "/a")

  # log_escape: icecast's %H rules, UTF-8 kept
  test.equal(
    icecast.server.log_escape(
      "a b\"c\\d\te!`é"
    ),
    "a\\x20b\\x22c\\x5cd\\x09e\\x21\\x60é"
  )
  test.equal(
    icecast.server.log_escape(
      keep_spaces=true,
      "a b\n"
    ),
    "a b\\x0a"
  )

  # access_log_line
  test.equal(
    icecast.server.access_log_line(now=0., view),
    "hash(5.5.5.5) - - [#{icecast.server.log_date(0.)}] \"GET /a HTTP/1.1\" 200 \
     4096 \"-\" \"VLC/3.0\" 61"
  )

  # playlist_log_line
  test.equal(
    icecast.server.playlist_log_line(
      now=0.,
      {mount = "/a", listeners = 3, metadata = [("title", "t")]}
    ),
    "#{icecast.server.log_date(0.)}|/a|3|t"
  )
  test.equal(
    icecast.server.playlist_log_line(
      now=0.,
      {
        mount = "/a",
        listeners = 3,
        metadata = [("artist", "a|b"), ("title", "t\nu")]
      }
    ),
    "#{icecast.server.log_date(0.)}|/a|3|a b - t u"
  )
  test.equal(
    icecast.server.playlist_log_line(
      now=0.,
      {mount = "/a", listeners = 0, metadata = []}
    ),
    null
  )

  # log_writer: rotation to .old, or to a timestamped archive
  dir = file.temp_dir("icecast", "logs")
  log_path = path.concat(dir, "access.log")
  target = ref({path = (log_path : string?), size_limit = 5, archive = false})
  write = icecast.server.log_writer(fun () -> target())
  write(
    "first line"
  )
  write("second")
  test.equal(file.contents(log_path), "second\n")
  test.equal(
    file.contents("#{log_path}.old"),
    "first line\n"
  )
  target := {path = (log_path : string?), size_limit = 5, archive = true}
  write("third")
  test.equal(file.contents(log_path), "third\n")
  test.equal(
    list.exists(fun (f) -> r/^access\.log\.\d{8}_\d{6}$/.test(f), file.ls(dir)),
    true
  )

  # The open descriptor follows a moved file until SIGHUP reopens the path
  target := {path = (log_path : string?), size_limit = 1000, archive = false}
  moved = path.concat(dir, "moved.log")
  file.move(force=true, log_path, moved)
  write("fourth")
  test.equal(file.contents(moved), "third\nfourth\n")
  ignore(
    process.run(
      "kill -HUP #{process.pid()}"
    )
  )
  thread.delay(1.)
  write("fifth")
  test.equal(file.contents(log_path), "fifth\n")

  # A new path applies to the next line, a null one disables the log
  other = path.concat(dir, "other.log")
  target := {path = (other : string?), size_limit = 1000, archive = false}
  write("sixth")
  test.equal(file.contents(other), "sixth\n")
  target := {path = (null : string?), size_limit = 1000, archive = false}
  write("seventh")
  test.equal(file.contents(other), "sixth\n")
  test.equal(file.contents(log_path), "fifth\n")
  file.rmdir(dir)

  # basic_auth
  def request(value) =
    {headers = [("authorization", value)]}
  end
  def check_auth(value) =
    icecast.server.basic_auth(user="admin", password="pw", request(value))
  end
  test.equal(
    check_auth(
      "Basic #{string.base64.encode("admin:pw")}"
    ),
    true
  )
  test.equal(
    check_auth(
      "Basic #{string.base64.encode("admin:nope")}"
    ),
    false
  )
  test.equal(
    check_auth(
      "Basic !!!"
    ),
    false
  )
  test.equal(
    icecast.server.basic_auth(user="admin", password="pw", {headers = []}),
    false
  )

  # x_forwarded_for / listener_ip
  xff = icecast.server.x_forwarded_for
  test.equal(xff({ip = "10.0.0.1", headers = [], proxy_ips = []}), "10.0.0.1")
  test.equal(
    xff(
      {
        ip = "10.0.0.1",
        headers =
          [
            (
              "x-forwarded-for",
              "5.5.5.5, 6.6.6.6"
            )
          ],
        proxy_ips = []
      }
    ),
    "6.6.6.6"
  )
  test.equal(
    xff(
      {
        ip = "10.0.0.1",
        headers =
          [
            (
              "x-forwarded-for",
              "5.5.5.5, 6.6.6.6"
            )
          ],
        proxy_ips = ["6.6.6.6", "10.0.0.1"]
      }
    ),
    "5.5.5.5"
  )
  test.equal(
    xff(
      {
        ip = "10.0.0.1",
        headers = [("x-forwarded-for", "6.6.6.6")],
        proxy_ips = ["6.6.6.6"]
      }
    ),
    "10.0.0.1"
  )
  info =
    {
      ip = "10.0.0.1",
      headers = [("x-forwarded-for", "5.5.5.5")],
      protocol = "http",
      uri = "/a"
    }
  test.equal(
    icecast.server.listener_ip(x_forwarded_for=null, proxy_ips=null, info),
    "10.0.0.1"
  )
  test.equal(
    icecast.server.listener_ip(x_forwarded_for=null, proxy_ips=[], info),
    "5.5.5.5"
  )
  test.equal(
    icecast.server.listener_ip(
      x_forwarded_for=fun (_) -> "7.7.7.7",
      proxy_ips=null,
      info
    ),
    "7.7.7.7"
  )

  # authorize
  def mount_config(uri) =
    if
      uri == "/dj"
    then
      {username = "dj", password = "djpw"}
    else
      {username = null, password = null}
    end
  end
  def check(
    ~taken=false,
    ~active=0,
    ~max_sources=null,
    ~password="pw",
    ~method="SOURCE",
    ~user="source",
    ~provided="pw",
    uri
  ) =
    icecast.server.authorize(
      log_event=fun (~level:_, _, _) -> (),
      password=password,
      max_sources=max_sources,
      mount_config=mount_config,
      is_taken=fun (_) -> taken,
      active_sources=fun () -> active,
      {user = user, password = provided, uri = uri, method = method}
    )
  end
  test.equal(check("/live"), true)
  test.equal(check(method="PUT", "/live"), true)
  test.equal(check(provided="wrong", "/live"), false)
  test.equal(check(user="dj", "/live"), false)
  test.equal(check(user="dj", provided="djpw", "/dj"), true)
  test.equal(check(provided="pw", "/dj"), false)
  test.equal(check(password="", provided="anything", "/live"), true)
  test.equal(check("/admin/metadata"), false)
  test.equal(check("/admin.cgi"), false)
  test.equal(check("/administrative"), true)
  test.equal(check(max_sources=2, active=2, "/live"), false)
  test.equal(check(max_sources=2, active=1, "/live"), true)
  test.equal(check(method="GET", provided="wrong", "/admin/stats"), true)
  test.equal(check(method="GET", taken=true, "/live"), true)
  test.equal(
    try
      check(taken=true, "/live")
    catch err : [error.icecast.server.mount_taken] do
      false
    end,
    false
  )

  test.pass()
end

test.check(f)
