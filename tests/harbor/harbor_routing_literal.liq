%include "../test.liq"

# A literal route (an output's mount) beats a regexp route that matches the
# same path, whatever the order they were registered in.
port = 18186
checks = ref(0)

harbor.http.register.simple.regexp(
  port=port,
  r/^\/.*$/,
  fun (_) -> http.response(data="catchall")
)
o = output.harbor(port=port, mount="lit.mp3", %mp3, sine())

def check(label, cond) =
  checks := checks() + 1
  if not cond then test.fail(label) end
end

# The reply to `GET path`, read until it contains `until`.
def request(path, until) =
  sock = socket.unix()
  data = ref("")
  try
    sock.connect(
      socket.address.internet_address(
        socket.internet_address("127.0.0.1"),
        port
      )
    )
    sock.write(
      "GET #{path} HTTP/1.0\r\n\r\n"
    )
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
        and not string.contains(encoding="ascii", substring=until, data())
      then
        read()
      end
    end

    read()
  finally
    sock.close()
  end
  data()
end

def run() =
  deadline = time() + 20.
  def rec wait() =
    if
      not o.is_started()
    then
      if
        time() > deadline
      then
        test.fail(
          "output never started"
        )
      end
      thread.pause(0.1)
      wait()
    end
  end

  wait()
  mount = request("/lit.mp3", "\r\n\r\n")
  check(
    "the mount is served by its output, not the earlier catch-all",
    string.contains(
      encoding="ascii",
      prefix="HTTP/1.0 200",
      mount
    )
    and string.contains(encoding="ascii", substring="audio/mpeg", mount)
  )
  other = request("/other", "catchall")
  check(
    "another path reaches the catch-all: #{other}",
    string.contains(encoding="ascii", substring="catchall", other)
  )
  if
    checks() != 2
  then
    test.fail(
      "only #{checks()} checks ran"
    )
  end
  test.pass()
end

thread.run(delay=1., run)
