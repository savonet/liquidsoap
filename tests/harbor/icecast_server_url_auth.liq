%include "../test.liq"

# A mount with `<authentication type="url">` POSTs each listener request to
# `listener_add` and admits the listener only when the reply carries
# `auth_header`, else refuses with Icecast's 401 challenge; a configured
# `auth_header` disables `x-icecast-auth-result`. A reply's time limit cuts the
# listener, a zero one at once, and each departure is POSTed to
# `listener_remove`. A reload changes the listener limits of a running mount.
port = 18137
auth_port = 18141

def f() =
  posted = ref([])
  removed = ref([])
  backend_authorization = ref("")

  harbor.http.register(
    port=auth_port,
    method="POST",
    "/auth",
    fun (req, response) ->
      begin
        params = url.split_args(req.body())
        if
          list.assoc(default="", "action", params) == "listener_remove"
        then
          atomic(fun () -> removed := [...removed(), params])
        else
          posted := params
          backend_authorization :=
            list.assoc(default="", "authorization", req.headers)
        end
        let (_, args) = url.split(list.assoc(default="", "mount", params))
        token = list.assoc(default="", "token", args)
        if
          token == "ok"
        then
          response.header("Icecast-Auth-User", "1")
        elsif
          token == "new"
        then
          response.header("X-Icecast-Auth-Result", "ok")
        elsif
          token == "timed"
        then
          response.header("Icecast-Auth-User", "1")
          response.header("Icecast-Auth-Timelimit", "1")
        elsif
          token == "zero"
        then
          response.header("Icecast-Auth-User", "1")
          response.header("Icecast-Auth-Timelimit", "0")
        elsif
          token == "alter"
        then
          response.header("Icecast-Auth-User", "1")
          response.header("X-Icecast-Auth-Alter-Action", "redirect_temporary")
          response.header(
            "X-Icecast-Auth-Alter-Argument",
            "http://edge/radio.mp3"
          )
        elsif
          token == "subscribe"
        then
          response.header("Icecast-Auth-User", "0")
          response.header("Icecast-Auth-Redirect", "http://x/subscribe.mp3")
        elsif
          token == "geo"
        then
          response.header("Icecast-Auth-User", "0")
          response.header("Icecast-Resp-Status", "403")
          response.header("Icecast-Resp-Reason", "Geoblocked")
        elsif
          token == "realm"
        then
          response.header("Icecast-Auth-User", "0")
          response.header(
            "Icecast-Www-Authenticate",
            "Basic realm=\"premium\""
          )
        end
      end
  )

  dir = file.temp_dir("icecast", "url_auth")
  config = path.concat(dir, "icecast.xml")
  file.write(
    data="<icecast><hostname>radio.example</hostname><authentication><source-password>hackme</source-password></authentication><mount><mount-name>/radio.mp3</mount-name><authentication \
     type=\"url\"><option name=\"listener_add\" value=\"http://127.0.0.1:#{
      auth_port
    }/auth\"/><option name=\"listener_remove\" value=\"http://127.0.0.1:#{
      auth_port
    }/auth\"/><option name=\"auth_header\" value=\"icecast-auth-user: \
     1\"/></authentication></mount></icecast>",
    config
  )
  server = icecast.server(port=port, config=config)

  output.icecast(
    %mp3(bitrate = 128),
    host="localhost",
    port=port,
    password="hackme",
    mount="radio.mp3",
    sine()
  )

  def connect(uri) =
    sock = socket.unix()
    sock.connect(
      socket.address.internet_address(
        socket.internet_address("127.0.0.1"),
        port
      )
    )
    sock.write(
      "GET #{uri} HTTP/1.0\r\nAuthorization: Basic #{
        string.base64.encode("fred:secret")
      }\r\nUser-Agent: test-agent\r\nReferer: http://player\r\n\r\n"
    )
    sock
  end

  # Reply head of a listener request for `uri`.
  def request(uri) =
    sock = connect(uri)
    reply = ref("")
    try
      reply := sock.read(timeout=5.)
    finally
      sock.close()
    end
    reply()
  end

  # Polls `f` until it returns a value, failing with `what` after 10 seconds.
  def wait_for(what, f) =
    deadline = time() + 10.
    def rec poll() =
      value = f()
      if
        null.defined(value)
      then
        null.get(value)
      else
        if
          time() > deadline
        then
          test.fail(
            "timed out waiting for #{what}"
          )
        end
        thread.pause(0.1)
        poll()
      end
    end
    poll()
  end

  def expect_status(uri, expected) =
    got = string.sub(encoding="ascii", request(uri), start=0, length=12)
    if
      got != expected
    then
      test.fail(
        "#{uri}: expected #{expected}, got #{got}"
      )
    end
  end

  def check() =
    expect_status(
      "/radio.mp3?token=ok&x=%2F",
      "HTTP/1.0 200"
    )
    admitted_client = list.assoc(default="", "client", posted())

    expected =
      [
        ("action", "listener_add"),
        ("server", "radio.example"),
        ("port", string(port)),
        ("mount", "/radio.mp3?token=ok&x=%2F"),
        ("user", "fred"),
        ("pass", "secret"),
        ("ip", "127.0.0.1"),
        ("agent", "test-agent"),
        ("protocol", "http"),
        ("referrer", "http://player"),
        ("listener-count", "0")
      ]
    list.iter(
      fun ((name, value)) ->
        begin
          got = list.assoc(default="<missing>", name, posted())
          if
            got != value
          then
            test.fail(
              "listener_add field #{name}: expected #{value}, got #{got}"
            )
          end
        end,
      expected
    )
    if
      string.length(list.assoc(default="", "server-instance", posted())) != 36
    then
      test.fail(
        "listener_add server-instance is not a UUID: #{
          list.assoc(default="", "server-instance", posted())
        }"
      )
    end
    if
      backend_authorization() !=
        "Basic #{string.base64.encode("fred:secret")}"
    then
      test.fail(
        "listener credentials not forwarded to listener_add: #{
          backend_authorization()
        }"
      )
    end

    expect_status(
      "/radio.mp3?token=new",
      "HTTP/1.0 401"
    )
    expect_status(
      "/radio.mp3?token=bad",
      "HTTP/1.0 401"
    )
    challenge = request("/radio.mp3?token=bad")
    if
      not string.contains(
        encoding="ascii",
        substring="\r\nWWW-Authenticate: Basic realm=\"Icecast2 Server\"\r\n",
        challenge
      )
    then
      test.fail(
        "refusal without Icecast's challenge: #{challenge}"
      )
    end

    def expect_reply(uri, status_line, header) =
      reply = request(uri)
      if
        not string.contains(
          encoding="ascii",
          prefix="#{status_line}\r\n",
          reply
        )
        or not string.contains(
          encoding="ascii",
          substring="\r\n#{header}\r\n",
          reply
        )
      then
        test.fail(
          "#{uri}: expected #{status_line} with #{header}, got: #{reply}"
        )
      end
    end
    expect_reply(
      "/radio.mp3?token=alter",
      "HTTP/1.0 307 Temporary Redirect",
      "Location: http://edge/radio.mp3"
    )
    expect_reply(
      "/radio.mp3?token=subscribe",
      "HTTP/1.0 303 See Other",
      "Location: http://x/subscribe.mp3"
    )
    expect_reply(
      "/radio.mp3?token=geo",
      "HTTP/1.0 403 Geoblocked",
      "Content-Length: 0"
    )
    expect_reply(
      "/radio.mp3?token=realm",
      "HTTP/1.0 401 Authentication Required",
      "WWW-Authenticate: Basic realm=\"premium\""
    )

    removal =
      wait_for(
        "listener_remove of client #{admitted_client}",
        fun () ->
          begin
            matching =
              list.filter(
                fun (params) ->
                  list.assoc(default="", "client", params) == admitted_client,
                removed()
              )
            matching == [] ? null : (list.hd(matching))
          end
      )
    test.equal(
      list.assoc(default="", "mount", removal),
      "/radio.mp3?token=ok&x=%2F"
    )
    test.equal(list.assoc(default="", "user", removal), "fred")
    if
      int_of_string(default=-1, list.assoc(default="", "duration", removal)) < 0
    then
      test.fail(
        "listener_remove duration: #{
          list.assoc(default="", "duration", removal)
        }"
      )
    end

    timed = connect("/radio.mp3?token=timed")
    try
      start = time()
      eof =
        wait_for(
          "the time-limited listener to be disconnected",
          fun () ->
            begin
              chunk =
                try
                  timed.read(timeout=0.5)
                catch _ do
                  "timeout"
                end
              chunk == "" ? time() - start : null
            end
        )
      if
        eof < 1.
      then
        test.fail(
          "time-limited listener cut after #{eof}s, before its limit"
        )
      end
    finally
      timed.close()
    end

    zero = connect("/radio.mp3?token=zero")
    try
      ignore(
        wait_for(
          "the listener with a zero time limit to be disconnected",
          fun () ->
            begin
              chunk =
                try
                  zero.read(timeout=0.5)
                catch _ do
                  "timeout"
                end
              chunk == "" ? true : null
            end
        )
      )
    finally
      zero.close()
    end

    file.write(
      data=r/<\/mount>/.replace(
        fun (_) -> "<max-listeners>0</max-listeners></mount>",
        file.contents(config)
      ),
      config
    )
    server.reload()
    expect_status(
      "/radio.mp3?token=ok",
      "HTTP/1.0 503"
    )
    test.pass()
  end

  server.on_source_connect(fun (_) -> thread.run(delay=5., check))
end

test.check(f)
