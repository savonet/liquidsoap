%include "../test.liq"

# A listener gets the `preroll` URL of its `listener_add` reply, else the
# mount's `<intro>` under `<webroot>`, before the stream. A preroll URL is
# fetched once for many listeners, and one that fails to load admits the
# listener without a preroll. A backend's preroll must be a URL: a local path,
# which could name any file outside `<webroot>`, is ignored. The cache keeps to its byte budget and keeps a
# file whose revalidation fails.
port = 18145
backend_port = 18147
intro = string.make(char_code=73, 2000)
ad = string.make(char_code=65, 3000)

def f() =
  secret_path = ref("")
  ad_fetches = ref(0)

  harbor.http.register(
    port=backend_port,
    method="POST",
    "/auth",
    fun (req, response) ->
      begin
        params = url.split_args(req.body())
        let (_, args) = url.split(list.assoc(default="", "mount", params))
        token = list.assoc(default="", "token", args)
        response.header("Icecast-Auth-User", "1")
        if
          token == "ad"
        then
          response.header("Preroll", "http://127.0.0.1:#{backend_port}/ad.mp3")
        elsif
          token == "local"
        then
          response.header("Preroll", secret_path())
        elsif
          token == "broken"
        then
          response.header(
            "Preroll",
            "http://127.0.0.1:#{backend_port}/none.mp3"
          )
        end
      end
  )

  harbor.http.register(
    port=backend_port,
    method="GET",
    "/ad.mp3",
    fun (_, response) ->
      begin
        atomic(fun () -> ad_fetches := ad_fetches() + 1)
        response.header("ETag", "\"v1\"")
        response.content_type("audio/mpeg")
        response.data(ad)
      end
  )

  harbor.http.register(
    port=backend_port,
    method="GET",
    "/none.mp3",
    fun (_, response) -> response.status_code(404)
  )

  other_fetches = ref(0)
  harbor.http.register(
    port=backend_port,
    method="GET",
    "/other.mp3",
    fun (_, response) ->
      begin
        atomic(fun () -> other_fetches := other_fetches() + 1)
        response.data(ad)
      end
  )

  flaky_down = ref(false)
  harbor.http.register(
    port=backend_port,
    method="GET",
    "/flaky.mp3",
    fun (_, response) ->
      if flaky_down() then response.status_code(502) else response.data(ad) end
  )

  dir = file.temp_dir("icecast", "preroll")
  file.write(data=intro, path.concat(dir, "intro.mp3"))
  secret_dir = file.temp_dir("icecast", "secret")
  file.write(data="SECRET", path.concat(secret_dir, "secret"))
  secret_path := "../#{path.basename(secret_dir)}/secret"
  config = path.concat(dir, "icecast.xml")
  file.write(
    data="<icecast><paths><webroot>#{
      dir
    }</webroot></paths><authentication><source-password>hackme</source-password></authentication><mount><mount-name>/radio.mp3</mount-name><intro>/intro.mp3</intro><authentication \
     type=\"url\"><option name=\"listener_add\" value=\"http://127.0.0.1:#{
      backend_port
    }/auth\"/></authentication></mount></icecast>",
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

  # The first `length` body bytes of a listener request for `uri`.
  def body(uri, length) =
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
        "GET #{uri} HTTP/1.0\r\n\r\n"
      )
      deadline = time() + 10.
      def rec read() =
        chunk =
          try
            sock.read(timeout=5.)
          catch _ do
            ""
          end
        data := data() ^ chunk
        index = string.index(substring="\r\n\r\n", data())
        if
          chunk != ""
          and time() < deadline
          and (index < 0 or string.bytes.length(data()) < index + 4 + length)
        then
          read()
        end
      end
      read()
    finally
      sock.close()
    end
    index = string.index(substring="\r\n\r\n", data())
    if
      index < 0
    then
      test.fail(
        "#{uri}: no response head"
      )
    end
    string.sub(encoding="ascii", data(), start=index + 4, length=length)
  end

  def expect_start(uri, expected, what) =
    length = string.bytes.length(expected)
    if
      body(uri, length) != expected
    then
      test.fail(
        "#{uri}: body does not start with the #{what}"
      )
    end
  end

  def check() =
    expect_start(
      "/radio.mp3?token=plain",
      intro,
      "mount intro"
    )
    expect_start(
      "/radio.mp3?token=ad",
      ad,
      "listener_add preroll"
    )
    expect_start(
      "/radio.mp3?token=ad",
      ad,
      "cached listener_add preroll"
    )
    if
      ad_fetches() != 1
    then
      test.fail(
        "preroll fetched #{ad_fetches()} times for two listeners"
      )
    end
    expect_start(
      "/radio.mp3?token=local",
      intro,
      "mount intro, ignoring a backend's local path"
    )
    data = body("/radio.mp3?token=broken", 2000)
    if
      data == intro
      or data == string.sub(encoding="ascii", ad, start=0, length=2000)
    then
      test.fail(
        "a preroll that failed to load was replaced by another one"
      )
    end

    # A budget below two files drops the least recently used one.
    settings.icecast.server.preroll_cache_size := 4000
    cache = icecast.server.preroll_cache()
    url = fun (name) -> "http://127.0.0.1:#{backend_port}/#{name}.mp3"
    fetches = ad_fetches()
    ignore(cache.get(url("ad")))
    ignore(cache.get(url("other")))
    ignore(cache.get(url("ad")))
    if
      ad_fetches() - fetches != 2 or other_fetches() != 1
    then
      test.fail(
        "budget eviction: #{ad_fetches() - fetches} fetches of the evicted file"
      )
    end
    settings.icecast.server.preroll_cache_size := 268435456

    stale = icecast.server.preroll_cache(revalidate=0.)
    ignore(stale.get(url("flaky")))
    flaky_down := true
    if
      stale.get(url("flaky")) != ad
    then
      test.fail(
        "a failed revalidation dropped the cached preroll"
      )
    end
    test.pass()
  end

  server.on_source_connect(fun (_) -> thread.run(delay=5., check))
end

test.check(f)
