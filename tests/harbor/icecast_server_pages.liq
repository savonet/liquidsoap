%include "../test.liq"

# Pages of icecast.server (observability.md §6 to §8, §12): the status pages
# exist exactly while `status_page` is on, behind `status_access`, without
# hidden mounts or addresses, with values escaped; `/status-json.xsl` has
# Icecast's shape; the callbacks replace `/` and `/status.json` only; the admin
# pages need the admin credentials whatever `status_page` says, list hidden
# mounts and hashed addresses; `/admin/listclients` takes the mount's
# credentials; `/health` honours `health_password`; `/metrics` exists exactly
# while `metrics` is on.
port = 18190
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

# The reply to `GET path`, read until the server closes: its status, head
# and body.
def fetch(~headers=[], path) =
  sock = connect()
  extra =
    string.concat(
      list.map(
        fun (h) ->
          "#{fst(h)}: #{snd(h)}\r\n",
        headers
      )
    )
  sock.write(
    "GET #{path} HTTP/1.0\r\nHost: radio.test:#{port}\r\n#{extra}\r\n"
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
    if chunk != "" and time() < deadline then read() end
  end

  read()
  sock.close()
  let (head, body) =
    string.contains(substring="\r\n\r\n", data())
      ? string.split.first(separator="\r\n\r\n", data())
      : (data(), "")
  status =
    int_of_string(
      default=0,
      string.sub(encoding="ascii", head, start=9, length=3)
    )
  {status = status, head = string.case(lower=true, head), body = body}
end

def basic(user, password) =
  [
    (
      "Authorization",
      "Basic #{string.base64.encode("#{user}:#{password}")}"
    )
  ]
end

admin = basic("admin", "apw")

def contains(text, part) =
  string.contains(substring=part, text)
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

# While set, the hidden mount's parameters fail validation.
broken_parameters = ref(false)

# While set, reading `hostname` fails.
broken_hostname = ref(false)

def mount(path) =
  if
    path == "/h.mp3"
  then
    {
      hidden = true,
      health_password = "hh",
      max_listeners = {broken_parameters() ? -1 : null}
    }
  else
    null
  end
end

def status_access(request) =
  not list.exists(
    fun (h) -> string.case(lower=true, fst(h)) == "x-deny",
    request.headers
  )
end

def parameters(
  ~status_page=true,
  ~metrics=true,
  ~all_hidden=false,
  ~status_json=null,
  ~status_html=null
) =
  {
    port = port,
    admin_password = "apw",
    health_password = "hpw",
    status_page = status_page,
    metrics = metrics,
    location =
      "Here & there",
    status_access = status_access,
    hostname =
      {
        broken_hostname() ? error.raise(error.invalid, "hostname") : "localhost"
      },
    status_json = status_json,
    status_html = status_html,
    mount = all_hidden ? fun (_) -> mount("/h.mp3") : mount
  }
end

server = icecast.server(parameters())

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

def mounts() =
  list.map(fun (m) -> m.mount, server.stats().mounts)
end

# A listener of `path`, left connected.
def listen(path) =
  deadline = time() + 20.
  def rec attempt() =
    sock = connect()
    sock.write(
      "GET #{path} HTTP/1.0\r\nUser-Agent: agent-x\r\n\r\n"
    )
    head = sock.read(timeout=5.)
    if
      string.sub(encoding="ascii", head, start=8, length=5) ==
        " 307 "
    then
      sock.close()
      if
        time() > deadline
      then
        test.fail(
          "listener of #{path} still redirected"
        )
      end
      thread.pause(0.1)
      attempt()
    else
      sock
    end
  end

  attempt()
end

def set_title(title) =
  r =
    fetch(
      headers=basic("source", "hackme"),
      "/admin/metadata?mount=/p.mp3&mode=updinfo&artist=A&title=#{
        url.encode(title)
      }"
    )
  wait_for(
    "metadata #{title}",
    {
      list.exists(
        fun (m) -> list.assoc(default="", "title", m.metadata) == title,
        server.stats().mounts
      )
    }
  )
  r.status
end

def check_status_pages() =
  status = fetch("/status.json")
  check(
    "/status.json is served: #{status.status} #{status.body}",
    status.status == 200 and contains(status.head, "application/json")
  )
  let json.parse (parsed :
    {
      mounts: [
        {
          mount: string,
          name: string,
          content_type: string,
          mime_type: string,
          started: float,
          listener_count: int,
          peak_listeners: int,
          current_metadata: [{key: string, value: string}]
        }
      ]
    }
  ) = status.body
  check(
    "/status.json lists the public mount only, with its keys: #{status.body}",
    list.map(fun (m) -> m.mount, parsed.mounts) == ["/p.mp3"]
    and list.hd(parsed.mounts).listener_count == 1
    and list.hd(parsed.mounts).peak_listeners == 1
    and list.hd(parsed.mounts).mime_type == "audio/mpeg"
    and list.exists(
      fun (kv) -> kv.key == "title" and kv.value == "<b>x</b>",
      list.hd(parsed.mounts).current_metadata
    )
  )
  broken_parameters := true
  broken = fetch("/status.json")
  broken_parameters := false
  check(
    "a hidden mount whose parameters fail stays hidden: #{broken.body}",
    broken.status == 200 and not contains(broken.body, "/h.mp3")
  )
  page = fetch("/")
  check(
    "/ is HTML with the mount, metadata as text, no hidden mount: #{page.body}",
    page.status == 200
    and contains(page.body, "/p.mp3")
    and contains(
      page.body,
      "A - &lt;b&gt;x&lt;/b&gt;"
    )
    and not contains(page.body, "<b>x</b>")
    and not contains(page.body, "/h.mp3")
    and contains(page.body, "type=\"audio/mpeg\"")
    and contains(page.body, "setInterval")
  )
  xsl = fetch("/status-json.xsl")
  let json.parse (one :
    {
      icestats: {
        admin: string,
        host: string,
        location: string,
        server_id: string,
        server_start: string,
        server_start_iso8601: string,
        source: {
          listenurl: string,
          listeners: int,
          listener_peak: int,
          server_type: string,
          stream_start_iso8601: string,
          title: string
        }
      }
    }
  ) = xsl.body
  check(
    "/status-json.xsl: one mount is an object, numbers, listenurl from Host: #{
      xsl.body
    }",
    xsl.status == 200
    and one.icestats.location ==
      "Here & there"
    and one.icestats.host == "localhost"
    and one.icestats.source.listenurl == "http://radio.test:#{port}/p.mp3"
    and one.icestats.source.listeners == 1
    and one.icestats.source.title ==
      "A - <b>x</b>"
    and r/^\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d[+-]\d{4}$/.test(
      one.icestats.server_start_iso8601
    )
    and not contains(xsl.body, "/h.mp3")
  )
  denied = fetch(headers=[("X-Deny", "1")], "/status.json")
  check(
    "status_access refusal is 401 with the realm: #{denied.status} #{
      denied.head
    }",
    denied.status == 401
    and contains(
      denied.head,
      "www-authenticate: basic realm=\"icecast2 server\""
    )
    and denied.body == ""
  )
  denied_metrics = fetch(headers=[("X-Deny", "1")], "/metrics")
  check(
    "/metrics follows status_access: #{denied_metrics.status}",
    denied_metrics.status == 401
  )
end

def check_failing_admin_page() =
  broken_hostname := true
  stats =
    try
      fetch(headers=basic("admin", "apw"), "/admin/stats")
    finally
      broken_hostname := false
    end
  check(
    "an admin page that fails to render answers 500: #{stats.status}",
    stats.status == 500
  )
end

def check_admin_pages() =
  check(
    "admin pages refuse without credentials",
    fetch("/admin/stats").status == 401
    and fetch("/admin/listeners.json").status == 401
    and fetch(headers=basic("source", "hackme"), "/admin/listmounts").status ==
      401
  )
  stats = fetch(headers=admin, "/admin/stats")
  check(
    "/admin/stats is Icecast's XML with the hidden mount: #{stats.body}",
    stats.status == 200
    and contains(stats.head, "text/xml")
    and contains(stats.body, "<icestats>")
    and contains(
      stats.body,
      "<source mount=\"/h.mp3\">"
    )
    and contains(
      stats.body,
      "<location>Here &amp; there</location>"
    )
    and contains(stats.body, "<sources>2</sources>")
    and contains(stats.body, "<listeners>1</listeners>")
    and contains(stats.body, "<source_ip>127.0.0.1</source_ip>")
    and contains(stats.body, "<max_listeners>unlimited</max_listeners>")
    and contains(stats.body, "<banned_IPs>0</banned_IPs>")
    and contains(
      stats.body,
      "<title>A - &lt;b&gt;x&lt;/b&gt;</title>"
    )
  )
  listmounts = fetch(headers=admin, "/admin/listmounts")
  check(
    "/admin/listmounts: #{listmounts.body}",
    listmounts.status == 200
    and contains(
      listmounts.body,
      "<source mount=\"/h.mp3\">"
    )
    and contains(listmounts.body, "<Listeners>1</Listeners>")
    and contains(listmounts.body, "<content-type>audio/mpeg</content-type>")
  )
  clients =
    fetch(headers=basic("source", "hackme"), "/admin/listclients?mount=/p.mp3")
  check(
    "/admin/listclients with the mount's credentials, hashed address: #{
      clients.body
    }",
    clients.status == 200
    and contains(clients.body, "<Listeners>1</Listeners>")
    and r/<IP>[0-9a-f]{16}<\/IP>/.test(clients.body)
    and contains(clients.body, "<UserAgent>agent-x</UserAgent>")
  )
  check(
    "/admin/listclients without a mount is 400",
    fetch(headers=admin, "/admin/listclients").status == 400
  )
  listeners = fetch(headers=admin, "/admin/listeners.json")
  let json.parse (parsed :
    {mounts: [{mount: string, connections: int, listeners: [{ip: string}]}]}
  ) = listeners.body
  check(
    "/admin/listeners.json lists hidden mounts and hashed addresses: #{
      listeners.body
    }",
    listeners.status == 200
    and list.mem("/h.mp3", list.map(fun (m) -> m.mount, parsed.mounts))
    and list.exists(
      fun (m) ->
        m.mount == "/p.mp3"
        and list.length(m.listeners) == 1
        and r/^[0-9a-f]{16}$/.test(list.hd(m.listeners).ip),
      parsed.mounts
    )
  )
  html = fetch(headers=admin, "/admin/listeners")
  check(
    "/admin/listeners is HTML",
    html.status == 200 and contains(html.body, "agent-x")
  )
end

# The `total_bytes_read` of /p.mp3 in `/admin/stats`, -1 when absent.
def bytes_read() =
  body = fetch(headers=admin, "/admin/stats").body
  source =
    list.nth(
      default="",
      string.split(
        separator="<source mount=\"/p.mp3\">",
        body
      ),
      1
    )
  match = r/<total_bytes_read>(\d+)<\/total_bytes_read>/.exec(source)
  int_of_string(default=-1, list.assoc(default="", 1, match))
end

def check_xml_values() =
  set_title("a\x01b")
  stats = fetch(headers=admin, "/admin/stats").body
  check(
    "/admin/stats has no XML-forbidden control byte: #{stats}",
    contains(
      stats,
      "<title>A - ab</title>"
    )
    and not r/[\x01]/.test(stats)
  )
  set_title("<b>x</b>")
  first = bytes_read()
  check(
    "total_bytes_read is counted: #{first}",
    first > 0
  )
  wait_for(
    "total_bytes_read grows",
    {bytes_read() > first}
  )
  check(
    "total_bytes_read grows",
    true
  )
end

def check_health_and_metrics() =
  check(
    "/health needs its password, the user name is not checked",
    fetch("/health").status == 401
    and fetch(headers=basic("x", "wrong"), "/health").status == 401
  )
  health = fetch(headers=basic("anyone", "hpw"), "/health")
  let json.parse (parsed :
    {
      last_config_reload: int,
      total_listener_count: int,
      total_source_count: int
    }
  ) = health.body
  check(
    "/health keys, hidden mount excluded: #{health.body}",
    health.status == 200
    and contains(health.body, "\"mounts\":{\"/p.mp3\":{")
    and contains(health.body, "\"content_type\":\"audio/mpeg\"")
    and contains(health.body, "\"codec\":\"MP3\"")
    and not contains(health.body, "/h.mp3")
    and parsed.total_listener_count == 1
    and parsed.total_source_count == 1
    and parsed.last_config_reload > 0
    and contains(
      health.body,
      "\"now_playing\":\"A - <b>x</b>\""
    )
  )
  mount_health = fetch(headers=basic("", "hpw"), "/p.mp3/health")
  hidden_health = fetch(headers=basic("", "hh"), "/h.mp3/health")
  check(
    "/<mount>/health: #{mount_health.body} #{hidden_health.status}",
    mount_health.status == 200
    and contains(mount_health.body, "\"listener_count\":1")
    and fetch("/p.mp3/health").status == 401
  )
  check(
    "/<mount>/health: hidden or unknown mount is 404 before authentication",
    hidden_health.status == 404
    and fetch(headers=basic("", "hpw"), "/h.mp3/health").status == 404
    and fetch("/h.mp3/health").status == 404
    and fetch("/nope.mp3/health").status == 404
  )
  metrics = fetch("/metrics")
  check(
    "/metrics in the Prometheus format, without hidden mounts: #{metrics.body}",
    metrics.status == 200
    and contains(
      metrics.head,
      "text/plain; version=0.0.4"
    )
    and contains(
      metrics.body,
      "# TYPE icecast_server_listeners gauge"
    )
    and contains(
      metrics.body,
      "icecast_server_listeners{mount=\"/p.mp3\"} 1\n"
    )
    and contains(
      metrics.body,
      "icecast_server_source_up{mount=\"/p.mp3\"} 1\n"
    )
    and contains(
      metrics.body,
      "icecast_server_mounts 1\n"
    )
    and not contains(metrics.body, "/h.mp3")
  )
  check(
    "/metrics: times keep every second, three decimals: #{metrics.body}",
    r/^icecast_server_start_time_seconds \d{10}\.\d{3}$/m.test(metrics.body)
  )
end

def check_privacy() =
  bodies =
    list.map(
      fun (path) -> fetch(headers=admin, path).body,
      [
        "/",
        "/status.json",
        "/status-json.xsl",
        "/admin/listmounts",
        "/admin/listeners",
        "/admin/listeners.json",
        "/admin/listclients?mount=/p.mp3",
        "/metrics"
      ]
    )
  check(
    "no raw listener address on a page",
    not list.exists(fun (b) -> contains(b, "127.0.0.1"), bodies)
  )
end

def check_switches() =
  def status_json(stats) =
    "custom #{list.length(stats.mounts)}"
  end

  server.reload(
    parameters(
      status_page=false,
      metrics=false,
      status_json=status_json,
      status_html=fun (_) -> "html"
    )
  )
  check(
    "status_page off: the status pages, /metrics and /health answer 404",
    list.for_all(
      fun (path) -> fetch(headers=basic("", "hpw"), path).status == 404,
      ["/", "/status.json", "/status-json.xsl", "/metrics", "/health"]
    )
  )
  check(
    "status_page off: the admin pages are served",
    fetch(headers=admin, "/admin/stats").status == 200
  )
  server.reload(
    parameters(
      all_hidden=true,
      status_json=status_json,
      status_html=fun (_) -> "html"
    )
  )
  check(
    "callbacks replace / and /status.json only",
    fetch("/status.json").body ==
      "custom 3"
    and fetch("/").body == "html"
    and contains(fetch("/status-json.xsl").body, "\"icestats\"")
  )
  check(
    "/status-json.xsl without a public mount has no source",
    not contains(fetch("/status-json.xsl").body, "\"source\"")
  )
  server.reload(parameters(all_hidden=true))
  check(
    "/ without a public mount says so",
    contains(
      fetch("/").body,
      "No active mounts."
    )
  )
  server.reload(
    parameters(
      status_json=fun (_) ->
        error.raise(
          error.not_found,
          "failing callback"
        )
    )
  )
  check(
    "a failing callback is a 500",
    fetch("/status.json").status == 500
  )
  server.reload(parameters())
  xsl = fetch("/status-json.xsl").body
  let json.parse (several : {icestats: {source: [{listenurl: string}]}}) = xsl
  check(
    "/status-json.xsl lists several mounts as an array, in name order: #{xsl}",
    list.map(fun (s) -> s.listenurl, several.icestats.source) ==
      ["http://radio.test:#{port}/p.mp3", "http://radio.test:#{port}/q.mp3"]
  )
end

def run() =
  try
    p = client("p.mp3")
    h = client("h.mp3")
    wait_for(
      "mounts",
      {list.mem("/p.mp3", mounts()) and list.mem("/h.mp3", mounts())}
    )
    listener = listen("/p.mp3")
    wait_for(
      "listener",
      {
        list.exists(
          fun (m) -> m.mount == "/p.mp3" and list.length(m.listeners) == 1,
          server.stats().mounts
        )
      }
    )
    check(
      "metadata update",
      set_title("<b>x</b>") == 200
    )
    check_status_pages()
    check_admin_pages()
    check_failing_admin_page()
    check_xml_values()
    check_health_and_metrics()
    check_privacy()
    q = client("q.mp3")
    wait_for(
      "second public mount",
      {list.mem("/q.mp3", mounts())}
    )
    check_switches()
    listener.close()
    p.shutdown()
    h.shutdown()
    q.shutdown()
    if
      checks() != 33
    then
      test.fail(
        "#{checks()} checks ran, expected 33"
      )
    end
    test.pass()
  catch err do
    test.fail(
      "uncaught error: #{err}"
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
