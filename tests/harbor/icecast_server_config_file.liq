%include "../test.liq"

# A server run from an Icecast configuration file (icecast-config.md): its
# mounts follow the file, a fallback mount that goes live after the mount
# depending on it is followed, an edited file applies on reload without
# disconnecting anyone, and a broken file is rejected with the previous
# record kept.
port = 18200
checks = ref(0)
sockets = ref([])
directory = file.temp_dir(cleanup=true, "icecast-config-file")
config = path.concat(directory, "icecast.xml")

def check(label, cond) =
  checks := checks() + 1
  if not cond then test.fail(label) end
end

def write_config(password) =
  file.write(
    data="<icecast><listen-socket><port>#{
      port
    }</port></listen-socket><authentication><source-password>#{
      password
    }</source-password></authentication><mount><mount-name>/main.mp3</mount-name><fallback-mount>/backup.mp3</fallback-mount></mount></icecast>",
    config
  )
end

def connect() =
  sock = socket.unix()
  sockets := [...sockets(), sock]
  sock.connect(
    socket.address.internet_address(socket.internet_address("127.0.0.1"), port)
  )
  sock
end

# The status of the reply to `request`.
def status(request) =
  sock = connect()
  sock.write(request)
  head =
    try
      sock.read(timeout=5.)
    catch _ do
      ""
    end
  sock.close()
  int_of_string(
    default=0,
    string.sub(encoding="ascii", head, start=9, length=3)
  )
end

def put(password, path) =
  status(
    "PUT #{path} HTTP/1.1\r\nAuthorization: Basic #{
      string.base64.encode("source:#{password}")
    }\r\nContent-Type: audio/mpeg\r\n\r\n"
  )
end

# A listener on `path`: its socket, and whether it still receives the stream.
def listen(path) =
  sock = connect()
  sock.write(
    "GET #{path} HTTP/1.0\r\n\r\n"
  )
  ignore(sock.read(timeout=5.))
  sock.{
    streaming =
      fun () ->
        try
          sock.read(timeout=3.) != ""
        catch _ do
          false
        end
  }
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

write_config("pw1")
server = icecast.server(icecast.server.config.load(config))

def mounts() =
  list.map(fun (m) -> m.mount, server.stats().mounts)
end

def started(o) =
  try
    clock(o.clock).start(force=false)
  catch _ do
    ()
  end
  o
end

def client(~password, path) =
  started(
    output.icecast(%mp3, port=port, password=password, mount=path, sine())
  )
end

def run() =
  try
    main = client(password="pw1", "main.mp3")
    wait_for(
      "/main.mp3 from the file",
      {list.mem("/main.mp3", mounts())}
    )
    listener = listen("/main.mp3")

    # The fallback goes live after the mount that depends on it.
    backup = client(password="pw1", "backup.mp3")
    wait_for("/backup.mp3", {list.mem("/backup.mp3", mounts())})
    main.shutdown()
    wait_for(
      "/main.mp3 follows its late fallback",
      {
        list.exists(
          fun (m) -> m.mount == "/main.mp3" and m.fallback == "/backup.mp3",
          server.stats().mounts
        )
      }
    )
    check(
      "a listener stays through the switch to a late fallback",
      listener.streaming()
    )

    write_config("pw2")
    server.reload(icecast.server.config.load(config))
    check(
      "an edited file applies on reload: the new password is taken",
      put("pw1", "/other.mp3") == 401 and put("pw2", "/other.mp3") == 200
    )
    check(
      "a reload disconnects nobody",
      listener.streaming() and list.mem("/backup.mp3", mounts())
    )

    file.write(data="<icecast><port>x</port></icecast>", config)
    rejected =
      try
        server.reload(icecast.server.config.load(config))
        false
      catch _ : [error.icecast.server.invalid_parameters] do
        true
      end
    check(
      "a broken file is rejected and the previous record kept",
      rejected and put("pw2", "/third.mp3") == 200
    )
    backup.shutdown()
    if
      checks() != 4
    then
      test.fail(
        "#{checks()} checks ran, expected 4"
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
