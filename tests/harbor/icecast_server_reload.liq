%include "../test.liq"

# A source connects with the first password, then the config is reloaded with
# a second one: new sources need the second password, the connected source
# stays, the admin page appears, the error log moves to its new file, and an
# invalid file is rejected.
def f() =
  dir = file.temp_dir("icecast", "reload")
  config = path.concat(dir, "icecast.xml")

  def write_config(~admin_password="", password) =
    file.write(
      data="<icecast><paths><logdir>#{dir}</logdir></paths><logging><errorlog>#{
        password
      }.log</errorlog></logging><authentication><source-password>#{
        password
      }</source-password>#{admin_password}</authentication></icecast>",
      config
    )
  end

  def logged_in(log, mount) =
    file.exists(path.concat(dir, log))
    and string.contains(
      substring="Source logging in at mountpoint \"#{mount}\"",
      file.contents(path.concat(dir, log))
    )
  end

  write_config("first")
  server = icecast.server(port=3490, config=config)

  clients = ref([])

  # Sources are stopped before passing: shutting down with a source still
  # connected hangs on its harbor feeding thread.
  def source_client(~password, mount) =
    client =
      output.icecast(
        %mp3,
        host="localhost",
        port=3490,
        password=password,
        mount=mount,
        sine()
      )
    clients := [client, ...clients()]
    client
  end

  def admin_status() =
    http.get("http://localhost:3490/admin/listeners.json").status_code
  end

  def check_invalid_config() =
    file.write(data="<icecast>", config)
    raised =
      try
        server.reload()
        false
      catch _ do
        true
      end
    test.equal(raised, true)
    test.equal(
      list.sort(fun (x, y) -> if x < y then -1 else 1 end, server.mounts()),
      ["/a", "/c"]
    )
    test.equal(logged_in("first.log", "/a"), true)
    test.equal(logged_in("second.log", "/c"), true)
    test.equal(logged_in("first.log", "/c"), false)
    server.on_source_disconnect(
      fun (_) -> if server.mounts() == [] then test.pass() end
    )
    list.iter(fun (client) -> client.stop(), clients())
  end

  def reload() =
    test.equal(admin_status(), 404)
    write_config(
      admin_password="<admin-password>apw</admin-password>",
      "second"
    )
    server.reload()
    test.equal(admin_status(), 401)

    rejected = source_client(password="first", "b")
    rejected.on_connect(synchronous=true, fun () -> test.fail())
    rejected.on_error(
      synchronous=true,
      fun (~restart_in, _) ->
        begin
          restart_in(null)
          test.equal(server.mounts(), ["/a"])
          ignore(source_client(password="second", "c"))
        end
    )
  end

  server.on_source_connect(
    fun (c) -> if c.mount == "/a" then reload() else check_invalid_config() end
  )
  ignore(source_client(password="first", "a"))
end

test.check(f)
