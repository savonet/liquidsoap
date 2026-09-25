%include "../test.liq"

# A listener on /a keeps receiving data from /b once /a's source leaves, with
# the fallback added by a reload while both mounts are live.
def f() =
  dir = file.temp_dir("icecast", "fallback")
  config = path.concat(dir, "icecast.xml")

  def write_config(mounts) =
    file.write(
      data="<icecast><authentication><source-password>hackme</source-password></authentication>#{
        mounts
      }</icecast>",
      config
    )
  end

  write_config("")
  server = icecast.server(port=3495, config=config)

  def source_client(mount) =
    output.icecast(
      %mp3,
      host="localhost",
      port=3495,
      password="hackme",
      mount=mount,
      sine()
    )
  end

  def bytes_sent() =
    stats =
      list.assoc(
        default=[],
        "/a",
        list.map(fun ((m, s)) -> (m, s.listeners), server.stats())
      )
    list.fold(fun (total, l) -> total + l.bytes_sent, 0, stats)
  end

  a = source_client("a")
  b = ref(null)

  listener_left = ref(false)
  server.on_listener_disconnect(
    synchronous=true,
    fun (_) -> listener_left := true
  )

  def check_fallback() =
    sent = bytes_sent()
    thread.run(
      delay=2.,
      fun () ->
        begin
          test.equal(listener_left(), false)
          test.equal(bytes_sent() > sent, true)
          server.on_source_disconnect(
            fun (_) -> if server.mounts() == [] then test.pass() end
          )
          null.get(b()).stop()
        end
    )
  end

  def on_listener_connect(_) =
    write_config(
      "<mount><mount-name>/a</mount-name><fallback-mount>/b</fallback-mount></mount>"
    )
    server.reload()
    server.on_source_disconnect(
      fun (s) ->
        if s.mount == "/a" then thread.run(delay=1., check_fallback) end
    )
    a.stop()
  end

  server.on_source_connect(
    fun (s) ->
      if
        s.mount == "/a"
      then
        b := source_client("b")
      else
        server.on_listener_connect(on_listener_connect)
        listener = input.http("http://localhost:3495/a")
        output.dummy(fallible=true, listener)
      end
  )
end

test.check(f)
