%include "../test.liq"

# Reload of icecast.server (parameters.md §8): an invalid record is rejected
# and changes nothing; a valid one is swapped whole for every use, so requests
# see all old or all new values; startup fields are kept; a mount output keeps
# the values it was created with; a live mount follows its new fallback at
# once and one left with nothing to send is gone when reload returns; nobody is
# disconnected; handlers stay, bindings change; the overlay is cleared and the
# transport reloaded.
port = 18180
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

# The reply head of `text` sent on a new socket, lowercase, with its status
# and the socket, left open. A listener request that came while its mount
# output was starting is redirected to itself and sent again.
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
    head =
      string.case(
        lower=true,
        string.contains(encoding="ascii", substring="\r\n\r\n", data())
          ? fst(
            string.split.first(encoding="ascii", separator="\r\n\r\n", data())
          )
          : (data())
      )
    status =
      int_of_string(
        default=0,
        string.sub(encoding="ascii", head, start=9, length=3)
      )
    if
      status == 307
      and string.contains(
        prefix="GET ",
        text
      )
    then
      sock.close()
      attempt()
    else
      {status = status, head = head, sock = sock}
    end
  end

  attempt()
end

def has(r, text) =
  string.contains(substring=text, r.head)
end

def listen(path) =
  send(
    "GET #{path} HTTP/1.0\r\n\r\n"
  )
end

def request(path) =
  r = listen(path)
  r.sock.close()
  r
end

def put(password, path) =
  r =
    send(
      "PUT #{path} HTTP/1.1\r\nContent-Type: audio/mpeg\r\nAuthorization: Basic #{
        string.base64.encode("source:#{password}")
      }\r\n\r\n"
    )
  r.sock.close()
  r
end

# Whether `sock` is still open and receiving within `duration`.
def streaming(sock, duration) =
  deadline = time() + duration
  def rec read_more(received) =
    remaining = deadline - time()
    if
      remaining <= 0.
    then
      received
    else
      chunk =
        try
          sock.read(timeout=remaining)
        catch _ do
          "timeout"
        end
      if chunk == "" then false else read_more(true) end
    end
  end

  read_more(false)
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

log_path = file.temp("icecast-reload", ".log")
settings.log.file.path := log_path
settings.log.file := true

def logged(text) =
  log.flush()
  list.exists(
    fun (line) -> string.contains(substring=text, line),
    file.lines(log_path)
  )
end

transport_reloads = ref(0)
transport =
  http.transport.unix.{
    reload = fun () -> transport_reloads := transport_reloads() + 1
  }
probes = ref([])
seen = ref([])
binding_calls = ref([])

def parameters(version) =
  def decision(request) =
    atomic(
      fun () ->
        seen :=
          [
            ...seen(),
            {
              version = version,
              hostname = request.server.hostname,
              instance = request.server.instance,
              compatibility = request.compatibility
            }
          ]
    )
    if
      request.query == "probe"
    then
      atomic(
        fun () -> probes := [...probes(), (version, request.server.hostname)]
      )
      {refuse = icecast.server.refusal(403)}
    else
      {}
    end
  end

  def mount(path) =
    if
      path == "/m.mp3" and version == 2
    then
      {fallback = {mount = "/f.mp3"}, listener_decision = decision}
    elsif
      path == "/m.mp3" and version == 9
    then
      {max_history = 0}
    elsif
      path == "/m.mp3"
    then
      {listener_decision = decision}
    else
      null
    end
  end

  {
    port = version == 1 ? port : (port + 1),
    compatibility = version == 1 ? "icecast-2.5" : "kh",
    transport =
      transport.{certificate = version == 1 ? "/one.pem" : "/two.pem"},
    source_password = "pw#{version}",
    hostname = "host#{version}",
    http_headers = [{name = "X-Version", value = "#{version}"}],
    event_bindings =
      [
        {
          event = "listener_connect",
          handler =
            fun (_) ->
              atomic(fun () -> binding_calls := [...binding_calls(), version])
        }
      ],
    mount = mount
  }
end

server = icecast.server(parameters(1))
handler_calls = ref(0)
server.on_listener_connect(
  synchronous=true,
  fun (_) -> handler_calls := handler_calls() + 1
)

def started(o) =
  try
    clock(o.clock).start(force=false)
  catch _ do
    ()
  end
  o
end

def client(password, path) =
  started(
    output.icecast(%mp3, port=port, password=password, mount=path, sine())
  )
end

def mounts() =
  list.map(fun (m) -> m.mount, server.stats().mounts)
end

def fallback_of(path) =
  list.fold(
    fun (found, m) -> m.mount == path ? m.fallback : found,
    null,
    server.stats().mounts
  )
end

def check_rejected(feeder, listener) =
  def rejected(version) =
    try
      server.reload(parameters(version))
      false
    catch _ : [error.icecast.server.invalid_parameters] do
      true
    end
  end

  check(
    "an invalid mount record of a live mount is rejected",
    rejected(9)
  )
  check(
    "after a rejected reload the old password still opens the mount",
    put("pw1", "/m.mp3").status == 409
  )
  check(
    "after a rejected reload the listener is still served",
    streaming(listener.sock, 2.)
  )
  check(
    "after a rejected reload the source is still connected",
    list.mem("/m.mp3", mounts()) and feeder.is_started()
  )
end

def check_swap(listener) =
  def rec probe_loop(until) =
    if
      not until()
    then
      ignore(request("/m.mp3?probe"))
      probe_loop(until)
    end
  end

  # The reload waits for a request decided under the first record.
  reloaded = ref(false)
  def reload_after_first_probe() =
    deadline = time() + 10.
    def rec wait() =
      if
        not list.exists(fun (p) -> fst(p) == 1, probes()) and time() < deadline
      then
        thread.pause(0.05)
        wait()
      end
    end

    wait()
    server.reload(parameters(2))
    reloaded := true
  end

  thread.run(reload_after_first_probe)
  probe_loop({reloaded()})
  ignore(request("/m.mp3?probe"))
  check(
    "requests on both sides of the reload: #{probes()}",
    list.exists(fun (p) -> fst(p) == 1, probes())
    and list.exists(fun (p) -> fst(p) == 2, probes())
  )
  check(
    "no request sees a mixture of the two records: #{probes()}",
    list.for_all(fun (p) -> snd(p) == "host#{fst(p)}", probes())
  )
  check(
    "the transport is reloaded once",
    transport_reloads() == 1
  )
  check(
    "a startup field is kept: the port still serves",
    put("pw2", "/m.mp3").status == 409
  )
  check(
    "startup changes are logged as not applied, the transport's included",
    logged(
      "Parameter port changed"
    )
    and logged(
      "Parameter compatibility changed"
    )
    and logged(
      "Parameter transport changed"
    )
  )
  check(
    "a use field applies at the next use: the old password is refused",
    put("pw1", "/m.mp3").status == 401
  )
  check(
    "the reload disconnects no listener",
    streaming(listener.sock, 2.)
  )
  last = list.nth(default=list.hd(seen()), seen(), list.length(seen()) - 1)
  check(
    "requests keep the started compatibility and instance: #{last}",
    last.version == 2
    and last.compatibility == "icecast-2.5"
    and last.instance == list.hd(seen()).instance
  )
end

def check_after_swap(feeder, listener) =
  old_output = listen("/m.mp3")
  check(
    "a mount output created before the reload keeps its headers",
    has(
      old_output,
      "x-version: 1"
    )
  )
  old_output.sock.close()
  n = client("pw2", "n.mp3")
  wait_for("/n.mp3", {list.mem("/n.mp3", mounts())})
  new_output = listen("/n.mp3")
  check(
    "a mount output created after the reload has the new headers",
    has(
      new_output,
      "x-version: 2"
    )
  )
  check(
    "a handler registered before the reload still fires",
    handler_calls() >= 3
  )
  check(
    "only the new record's bindings fire: #{binding_calls()}",
    list.mem(2, binding_calls())
    and list.length(list.filter(fun (v) -> v == 1, binding_calls())) == 1
  )
  feeder.shutdown()
  check(
    "the mount follows its new fallback at once",
    streaming(listener.sock, 3.) and list.mem("/m.mp3", mounts())
  )
  admin =
    send(
      "GET /admin/fallbacks?mount=/n.mp3&fallback=/f.mp3 \
       HTTP/1.0\r\nAuthorization: Basic #{
        string.base64.encode("source:pw2")
      }\r\n\r\n"
    )
  admin.sock.close()
  check(
    "the fallbacks request sets the overlay: #{admin.status}",
    admin.status == 200 and fallback_of("/n.mp3") == "/f.mp3"
  )
  server.reload(parameters(3))
  check(
    "a mount whose fallback was removed is gone when reload returns",
    not list.mem("/m.mp3", mounts())
  )
  check(
    "a reload clears the overlay",
    fallback_of("/n.mp3") == null
  )
  new_output.sock.close()
  n.shutdown()
end

def run() =
  try
    feeder = client("pw1", "m.mp3")
    fallback = client("pw1", "f.mp3")
    wait_for(
      "mounts",
      {list.mem("/m.mp3", mounts()) and list.mem("/f.mp3", mounts())}
    )
    listener = listen("/m.mp3")
    check(
      "first listener",
      listener.status == 200
    )
    check_rejected(feeder, listener)
    check_swap(listener)
    check_after_swap(feeder, listener)
    fallback.shutdown()
    if
      checks() != 21
    then
      test.fail(
        "#{checks()} checks ran, expected 21"
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
