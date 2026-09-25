%include "../test.liq"

# A source streams to icecast.server and a listener tunes in: the admin page
# lists the listener under a hashed IP, a metadata update reaches the playlist
# log, the listener's session ends up in the access log once the source leaves,
# and the source's connection is in the error log.
def f() =
  dir = file.temp_dir("icecast", "logs")
  access_log = path.concat(dir, "access.log")
  playlist_log = path.concat(dir, "playlist.log")
  error_log = path.concat(dir, "error.log")
  config = path.concat(dir, "icecast.xml")
  file.write(
    data="<icecast><paths><logdir>#{
      dir
    }</logdir></paths><logging><errorlog>error.log</errorlog></logging></icecast>",
    config
  )

  server =
    icecast.server(
      port=3480,
      password="hackme",
      access_log=access_log,
      playlist_log=playlist_log,
      admin_password="apw",
      config=config
    )

  s = sine()
  source_client =
    output.icecast(
      %mp3,
      host="localhost",
      port=3480,
      password="hackme",
      mount="live",
      s
    )

  def check_admin() =
    test.equal(
      http.get("http://localhost:3480/admin/listeners.json").status_code,
      401
    )
    credentials = string.base64.encode("admin:apw")
    resp =
      http.get(
        headers=[
          (
            "Authorization",
            "Basic #{credentials}"
          )
        ],
        "http://localhost:3480/admin/listeners.json"
      )
    test.equal(resp.status_code, 200)
    let json.parse (admin :
      {mounts: [{mount: string, listeners: [{ip: string}]}]}
    ) = resp
    let [mount] = admin.mounts
    test.equal(mount.mount, "/live")
    let [listener] = mount.listeners
    test.equal(r/^[0-9a-f]{16}$/.test(listener.ip), true)
  end

  def on_listener_connect(_) =
    thread.run(
      delay=2.,
      fun () ->
        begin
          check_admin()
          thread.run(delay=2., fun () -> source_client.stop())
        end
    )
  end

  def check_logs() =
    let [line, ..._] = string.split(separator="\n", file.contents(access_log))
    test.equal(
      r/^[0-9a-f]{16} - - \[[^\]]+\] "GET \/live HTTP\/1\.[01]" 200 [1-9]\d* "-" "[^"]*" \d+$/.test(
        line
      ),
      true
    )
    test.equal(
      r/\|\/live\|\d+\|hello\n/.test(file.contents(playlist_log)),
      true
    )
    test.equal(
      r/^\[\d{4}-\d{2}-\d{2}  \d{2}:\d{2}:\d{2}\] INFO connection-handle\/_handle_source_request Source logging in at mountpoint "\/live"$/m.test(
        file.contents(error_log)
      ),
      true
    )
    test.pass()
  end

  done = ref(false)
  server.on_listener_connect(synchronous=true, on_listener_connect)
  server.on_listener_disconnect(
    synchronous=true,
    fun (_) ->
      if
        not done()
      then
        done := true
        thread.run(delay=0.5, check_logs)
      end
  )

  server.on_source_connect(
    fun (_) -> thread.run(delay=1., {s.insert_metadata([("title", "hello")])})
  )

  thread.run(
    delay=2.,
    fun () ->
      begin
        listener = input.http("http://localhost:3480/live")
        output.dummy(fallible=true, listener)
      end
  )
end

test.check(f)
