%include "../test.liq"

# Credentials in the Authorization header or in the query string are admitted;
# missing or wrong ones get 401 with the Basic challenge, the mount-taken error
# gets 403, and a refused request fires neither callback.
port = 18102
connects = ref(0)
disconnects = ref(0)

def auth(r) =
  if
    r.login == "taken"
  then
    error.raise(error.register("error.icecast.server.mount_taken"), "taken")
  end
  r.login == "user" and r.password == "secret"
end

o = output.harbor(port=port, mount="auth.mp3", auth=auth, %mp3, sine())
o.on_connect(synchronous=true, fun (_) -> connects := connects() + 1)
o.on_disconnect(synchronous=true, fun (_) -> disconnects := disconnects() + 1)

# Sends a request and returns the response head, reading at most 2 KiB.
def request(path, headers) =
  sock = socket.unix()
  sock.connect(
    socket.address.internet_address(socket.internet_address("127.0.0.1"), port)
  )
  sock.write(
    "GET #{path} HTTP/1.0\r\n#{headers}\r\n"
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

def status(response) =
  string.sub(encoding="ascii", response, start=0, length=12)
end

def check() =
  test.equal(
    status(request("/auth.mp3", "")),
    "HTTP/1.0 401"
  )
  test.equal(
    string.contains(
      encoding="ascii",
      substring="WWW-Authenticate: Basic",
      request("/auth.mp3", "")
    ),
    true
  )
  wrong = string.base64.encode("user:wrong")
  test.equal(
    status(
      request(
        "/auth.mp3",
        "Authorization: Basic #{wrong}\r\n"
      )
    ),
    "HTTP/1.0 401"
  )
  taken = string.base64.encode("taken:x")
  test.equal(
    status(
      request(
        "/auth.mp3",
        "Authorization: Basic #{taken}\r\n"
      )
    ),
    "HTTP/1.0 403"
  )
  test.equal(connects(), 0)
  test.equal(disconnects(), 0)

  good = string.base64.encode("user:secret")
  test.equal(
    status(
      request(
        "/auth.mp3",
        "Authorization: Basic #{good}\r\n"
      )
    ),
    "HTTP/1.0 200"
  )
  test.equal(
    status(request("/auth.mp3?user=user&pass=secret", "")),
    "HTTP/1.0 200"
  )
  thread.run(
    delay=1.,
    fun () ->
      begin
        test.equal(connects(), 2)
        test.equal(disconnects(), 2)
        test.pass()
      end
  )
end

o.on_start(synchronous=true, fun () -> thread.run(delay=0.5, check))
