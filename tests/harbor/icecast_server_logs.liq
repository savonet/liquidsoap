%include "../test.liq"

# Log files of icecast.server (observability.md §9 to §11, §14): one access
# line per finished admitted session, field by field, escaped per
# `compatibility`; one playlist line per metadata update with text; the error
# log's format, level names and `log_level` filter; rotation to `.old` and to
# archive names, `reopen_logs` and a changed target.
port = 18192
kh_port = 18194
checks = ref(0)
sockets = ref([])
dir = "icecast_server_logs.#{random.int(min=0, max=1000000000)}"
file.mkdir(dir)

def in_dir(name) =
  path.concat(dir, name)
end

def fail(message) =
  file.rmdir(dir)
  test.fail(message)
end

def check(label, cond) =
  checks := checks() + 1
  if not cond then fail(label) end
end

def lines(name) =
  list.filter(
    fun (l) -> l != "",
    string.split(separator="\n", file.contents(in_dir(name)))
  )
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
        fail(
          "timeout waiting for #{label}"
        )
      end
      thread.pause(0.1)
      wait()
    end
  end

  wait()
end

def connect(port) =
  sock = socket.unix()
  sockets := [...sockets(), sock]
  sock.connect(
    socket.address.internet_address(socket.internet_address("127.0.0.1"), port)
  )
  sock
end

# Sends `text` and returns the socket once the reply head is read; a listener
# that came while its mount output was starting is redirected and sent again.
def send(~port, text) =
  deadline = time() + 20.
  def rec attempt() =
    sock = connect(port)
    sock.write(text)
    head = sock.read(timeout=5.)
    if
      string.sub(encoding="ascii", head, start=8, length=5) ==
        " 307 "
    then
      sock.close()
      if
        time() > deadline
      then
        fail(
          "still redirected: #{text}"
        )
      end
      thread.pause(0.1)
      attempt()
    else
      sock.{head = head}
    end
  end

  attempt()
end

def mount(path) =
  if
    path == "/r.mp3"
  then
    {listener_decision = fun (_) -> {refuse = icecast.server.refusal(403)}}
  else
    null
  end
end

def parameters(
  ~log_level=4,
  ~log_size=10240000,
  ~log_archive=false,
  ~playlist_log="playlist.log"
) =
  {
    port = port,
    access_log = in_dir("access.log"),
    playlist_log = in_dir(playlist_log),
    error_log = in_dir("error.log"),
    log_level = log_level,
    log_size = log_size,
    log_archive = log_archive,
    mount = mount
  }
end

server = icecast.server(parameters())
kh =
  icecast.server(
    {
      port = kh_port,
      compatibility = "kh",
      access_log = in_dir("kh.log"),
      mount =
        fun (_) ->
          {
            listener_decision =
              fun (_) ->
                {
                  user =
                    "a b"
                }
          }
    }
  )

records = ref([])
server.on_listener_disconnect(
  synchronous=true,
  fun (r) -> atomic(fun () -> records := [...records(), r])
)
source_connects = ref([])
server.on_source_connect(
  synchronous=true,
  fun (r) ->
    atomic(fun () -> source_connects := [...source_connects(), r.mount])
)

def started(o) =
  try
    clock(o.clock).start(force=false)
  catch _ do
    ()
  end
  o
end

def client(~port=port, path) =
  started(
    output.icecast(%mp3, port=port, password="hackme", mount=path, sine())
  )
end

def mounts(server) =
  list.map(fun (m) -> m.mount, server.stats().mounts)
end

date =
  "\\d\\d/[A-Z][a-z]{2}/\\d{4}:\\d\\d:\\d\\d:\\d\\d [+-]\\d{4}"

# The line with its date replaced by `DATE`, `""` when the date is missing.
def undated(line) =
  parts =
    regexp(
      " \\[#{date}\\] "
    ).split(line)
  list.length(parts) == 2
    ? "#{list.nth(parts, 0)} [DATE] #{list.nth(parts, 1)}"
    : ""
end

def check_access() =
  first =
    send(
      port=port,
      "GET /a.mp3?token=secret HTTP/1.1\r\nUser-Agent: VLC \"x\" \\ \
       é\ttab\r\nReferer: http://r/!\r\n\r\n"
    )
  first.close()
  wait_for(
    "first session ends",
    {list.length(records()) == 1}
  )
  second =
    send(
      port=port,
      "GET /a.mp3 HTTP/1.0\r\n\r\n"
    )
  second.close()
  wait_for(
    "second session ends",
    {list.length(records()) == 2}
  )
  refused =
    send(
      port=port,
      "GET /r.mp3 HTTP/1.0\r\n\r\n"
    )
  refused.close()
  check(
    "refused listener: #{refused.head}",
    string.contains(
      substring=" 403 ",
      refused.head
    )
  )
  wait_for(
    "access lines",
    {list.length(lines("access.log")) >= 2}
  )
  access = lines("access.log")
  check(
    "one access line per finished admitted session: #{access}",
    list.length(access) == 2
  )
  r1 = list.nth(records(), 0)
  r2 = list.nth(records(), 1)
  check(
    "access line, field by field, escaped, query dropped: #{access}",
    undated(list.nth(access, 0)) ==
      "#{r1.ip} - - [DATE] \"GET /a.mp3 HTTP/1.1\" 200 #{r1.bytes_sent} \
       \"http://r/\\x21\" \"VLC \\x22x\\x22 \\x5c é\\x09tab\" #{
        int_of_float(r1.duration)
      }"
  )
  check(
    "absent fields are -, the address is hashed: #{access}",
    undated(list.nth(access, 1)) ==
      "#{r2.ip} - - [DATE] \"GET /a.mp3 HTTP/1.0\" 200 #{r2.bytes_sent} \"-\" \
       \"-\" #{int_of_float(r2.duration)}"
    and r/^[0-9a-f]{16}$/.test(r2.ip)
  )
end

def check_kh_access() =
  agent =
    "K \"q\" #{string.make(char_code=120, 200)}"
  sock =
    send(
      port=kh_port,
      "GET /k.mp3 HTTP/1.0\r\nUser-Agent: #{agent}\r\n\r\n"
    )
  sock.close()
  wait_for(
    "kh access line",
    {file.exists(in_dir("kh.log")) and lines("kh.log") != []}
  )
  line = undated(list.hd(lines("kh.log")))
  check(
    "kh: user agent unescaped and cut to 150 bytes, user percent-encoded: #{
      line
    }",
    r/^[0-9a-f]{16} - a%20b \[DATE\] "GET \/k\.mp3 HTTP\/1\.0" 200 \d+ "-" "/.test(
      line
    )
    and string.contains(
      substring="\"#{string.sub(agent, start=0, length=150)}\" ",
      line
    )
  )
  sock =
    send(
      port=kh_port,
      "GET /k.mp3 HTTP/1.0\r\nUser-Agent: Short UA\r\nReferer: http://r/a \
       b\r\n\r\n"
    )
  sock.close()
  wait_for(
    "second kh access line",
    {list.length(lines("kh.log")) >= 2}
  )
  line = undated(list.nth(lines("kh.log"), 1))
  check(
    "kh: a short referer and user agent are written whole: #{line}",
    string.contains(
      substring="\"http://r/a b\" \"Short UA\" ",
      line
    )
  )
end

# Updates the metadata of /a.mp3 and waits until the mount carries `expect`:
# updates within one frame would otherwise merge.
def metadata(~expect, fields) =
  sock =
    send(
      port=port,
      "GET /admin/metadata?mount=/a.mp3&mode=updinfo&#{fields} \
       HTTP/1.0\r\nAuthorization: Basic #{
        string.base64.encode("source:hackme")
      }\r\n\r\n"
    )
  sock.close()
  wait_for(
    "metadata #{fields}",
    {
      list.exists(
        fun (m) ->
          m.mount == "/a.mp3"
          and list.assoc(default="", fst(expect), m.metadata) == snd(expect),
        server.stats().mounts
      )
    }
  )
end

def playlist_texts(name) =
  list.map(
    fun (l) -> list.nth(default="", string.split(separator="\\|", l), 3),
    lines(name)
  )
end

def wait_playlist(name, text) =
  wait_for(
    "playlist line #{text}",
    {file.exists(in_dir(name)) and list.mem(text, playlist_texts(name))}
  )
end

def check_playlist() =
  listener =
    send(
      port=port,
      "GET /a.mp3 HTTP/1.0\r\n\r\n"
    )
  metadata(expect=("title", "Ti|tle"), "artist=Art&title=Ti%7Ctle")
  metadata(expect=("title", "Solo"), "title=Solo")
  metadata(expect=("artist", "OnlyA"), "artist=OnlyA")
  metadata(expect=("url", "http://x"), "url=http%3A%2F%2Fx")
  metadata(expect=("title", "After"), "title=After")
  wait_playlist("playlist.log", "After")
  check(
    "playlist text: artist - title, | replaced, no line without text: #{
      lines("playlist.log")
    }",
    playlist_texts("playlist.log") ==
      [
        "Art - Ti tle",
        "Solo",
        "OnlyA",
        "After"
      ]
  )
  check(
    "playlist line: date, mount, listener count: #{lines("playlist.log")}",
    regexp("^#{date}\\|/a\\.mp3\\|1\\|Solo$").test(
      list.nth(lines("playlist.log"), 1)
    )
  )
  listener.close()
end

def check_error_log() =
  errors = file.contents(in_dir("error.log"))
  check(
    "error log: source login line: #{errors}",
    r/^\[\d{4}-\d\d-\d\d  \d\d:\d\d:\d\d\] INFO connection-handle\/_handle_source_request Source logging in at mountpoint "\/a\.mp3"$/m.test(
      errors
    )
  )
  check(
    "error log: EROR and WARN level names",
    r/^\[[^\]]+\] EROR /m.test(errors) and r/^\[[^\]]+\] WARN /m.test(errors)
  )
  server.reload(parameters(log_level=3))
  b = client("b.mp3")
  wait_for(
    "b.mp3 connects",
    {list.mem("/b.mp3", source_connects())}
  )
  check(
    "log_level filters the error log",
    not string.contains(
      substring="mountpoint \"/b.mp3\"",
      file.contents(in_dir("error.log"))
    )
  )
  server.reload(parameters())
  b.shutdown()
  wait_for(
    "source exit line",
    {
      string.contains(
        substring="INFO source/source_main Source at \"/b.mp3\" exiting",
        file.contents(in_dir("error.log"))
      )
    }
  )
  check(
    "error log: source exit line",
    true
  )
end

def check_files() =
  server.reload(parameters(log_size=60))
  metadata(expect=("title", "R1"), "title=R1")
  metadata(expect=("title", "R2"), "title=R2")
  metadata(expect=("title", "R3"), "title=R3")
  wait_playlist("playlist.log", "R3")
  old = file.contents(in_dir("playlist.log.old"))
  metadata(expect=("title", "R4"), "title=R4")
  metadata(expect=("title", "R5"), "title=R5")
  wait_playlist("playlist.log", "R5")
  check(
    "rotation past log_size replaces .old: #{old} / #{
      file.contents(in_dir("playlist.log.old"))
    }",
    old != ""
    and file.contents(in_dir("playlist.log.old")) != old
    and file.size(in_dir("playlist.log")) < 120
  )
  server.reload(parameters(log_size=60, log_archive=true))
  metadata(expect=("title", "A1"), "title=A1")
  metadata(expect=("title", "A2"), "title=A2")
  wait_playlist("playlist.log", "A2")
  check(
    "archive names: #{file.ls(dir)}",
    list.exists(
      fun (f) -> r/^playlist\.log\.\d{8}_\d{6}$/.test(f),
      file.ls(dir)
    )
  )
  server.reload(parameters())
  file.move(in_dir("playlist.log"), in_dir("moved.log"))
  server.reopen_logs()
  metadata(expect=("title", "Reopened"), "title=Reopened")
  wait_playlist("playlist.log", "Reopened")
  check(
    "reopen_logs reopens at the configured path",
    not list.mem("Reopened", playlist_texts("moved.log"))
  )
  server.reload(parameters(playlist_log="other.log"))
  metadata(expect=("title", "Elsewhere"), "title=Elsewhere")
  wait_playlist("other.log", "Elsewhere")
  check(
    "a changed target takes the next line",
    not list.mem("Elsewhere", playlist_texts("playlist.log"))
  )
end

def run() =
  try
    a = client("a.mp3")
    refusing = client("r.mp3")
    k = client(port=kh_port, "k.mp3")
    wait_for(
      "mounts",
      {
        list.mem("/a.mp3", mounts(server))
        and list.mem("/r.mp3", mounts(server))
        and list.mem("/k.mp3", mounts(kh))
      }
    )
    check_access()
    check_kh_access()
    check_playlist()
    check_error_log()
    check_files()
    a.shutdown()
    refusing.shutdown()
    k.shutdown()
    if
      checks() != 16
    then
      fail(
        "#{checks()} checks ran, expected 16"
      )
    end
    test.pass()
  catch err do
    fail(
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
    file.rmdir(dir)
  end
end

thread.run(delay=1., run)
