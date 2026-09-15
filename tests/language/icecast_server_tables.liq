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

  # listener_table: one disconnect drops one listener from that connection IP
  l = icecast.server.listener_table()
  l.connect(
    connection_ip="1.1.1.1",
    {ip = "1.1.1.1", protocol = "http", uri = "/a"}
  )
  l.connect(
    connection_ip="1.1.1.1",
    {ip = "1.1.1.1", protocol = "http", uri = "/a"}
  )
  l.connect(
    connection_ip="2.2.2.2",
    {ip = "9.9.9.9", protocol = "http", uri = "/a"}
  )
  test.equal(list.length(l.all()), 3)
  test.equal(l.peak(), 3)
  l.disconnect("1.1.1.1")
  test.equal(list.map(fun (x) -> x.ip, l.all()), ["9.9.9.9", "1.1.1.1"])
  l.disconnect("3.3.3.3")
  test.equal(list.length(l.all()), 2)
  l.disconnect("2.2.2.2")
  l.disconnect("1.1.1.1")
  test.equal(l.all(), [])
  test.equal(l.peak(), 3)

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
      label="test",
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
