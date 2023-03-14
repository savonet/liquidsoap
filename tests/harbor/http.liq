def f() =
  # Default response
  def handler(req, _) =
   test.equals(req.http_version, "1.1")
   test.equals(req.method, "GET")
   test.equals(req.body(timeout=5.0), "")
   test.equals(req.query, [])
   test.equals(req.path, "/default")
  end

  harbor.http.register("/default", port=3456, handler)
  resp = http.get("http://localhost:3456/default")
  test.equals(resp.status_message, "OK")
  test.equals(resp.status_code, 200)
  test.equals(resp.http_version, "1.1")
  test.equals(resp.headers, [])
  test.equals("#{resp}", "")

  # Endpoint are executed in the order they are declared
  def handler(_, _) =
    test.fail()
  end

  harbor.http.register.regexp(r/default/g, port=3456, handler)
  harbor.http.register("/default", port=3456, handler)
  resp = http.get("http://localhost:3456/default")
  test.equals(resp.status_message, "OK")
  test.equals(resp.status_code, 200)
  test.equals(resp.http_version, "1.1")
  test.equals(resp.headers, [])
  test.equals("#{resp}", "")

  # String response with matches
  def handler(req, _) =
   test.equals(req.http_version, "1.1")
   test.equals(req.method, "GET")
   test.equals(req.body(timeout=5.0), "")
   test.equals(req.query, [("gni", "gno"), ("bla", "blo")])
   test.equals(req.path, "/path/gno/blo")
  end

  harbor.http.register("/path/:gni/:bla", port=3456, handler)
  resp = http.get("http://localhost:3456/path/gno/blo")
  test.equals(resp.status_message, "OK")
  test.equals(resp.status_code, 200)
  test.equals(resp.http_version, "1.1")
  test.equals(resp.headers, [])
  test.equals("#{resp}", "")

  # Full query
  def handler(req, res) =
   test.equals(req.http_version, "1.0")
   test.equals(req.method, "POST")
   test.equals(req.query, [("foo", "with"), ("bla", "in"), ("gnu", "gno"), ("gni", "gno")])
   test.equals(req.headers, [
      ("host", "localhost:3456"),
      ("user-agent", http.user_agent),
      ("accept", "*/*"),
      ("req", "header"),
      ("content-length", "9"),
      ("content-type", "application/x-www-form-urlencoded")
    ])
   test.equals(req.path, "/some/path/with/full/in/it")
   test.equals(req.body(timeout=5.0), "foobarlol")

    res.status_code(201)
    res.status_message("YYR")
    res.http_version("1.0")

    data_count = ref(3)
    def data() =
      if data_count() >= 0 then
        data_count := data_count() - 1
        "gnigno"
      else
        ""
      end
    end

    res.data(data)
    res.headers([("some","value")])
    res.content_type("liquidsoap/test")
  end

  harbor.http.register.regexp(r/(?<foo>[^\/]+)\/full\/(?<bla>[^\/]+)/g, method="POST", port=3456, handler)
  resp = http.post(http_version="1.0", data="foobarlol", headers=[("req","header")], "http://localhost:3456/some/path/with/full/in/it?gni=gno&gnu=gno")

  test.equals(resp.status_message, "YYR")
  test.equals(resp.status_code, 201)
  test.equals(resp.http_version, "1.0")
  test.equals(resp.headers, [
    ("some", "value"),
    ("transfer-encoding", "chunked"),
    ("content-type", "liquidsoap/test"),
  ])
  test.equals("#{resp}", "gnignognignognignognigno")

  # Large non-chunked query
  def handler(req, _) =
   test.equals(req.http_version, "1.1")
   test.equals(req.method, "POST")
   test.equals(req.query, [])
   test.equals(req.headers, [
      ("host", "localhost:3456"),
      ("user-agent", http.user_agent),
      ("accept", "*/*"),
      ("content-length", "10000"),
      ("content-type", "application/x-www-form-urlencoded"),
    ])
   test.equals(req.path, "/large_non_chunked")
   test.equals(string.length(req.body(timeout=5.0)), 10_000)
  end

  harbor.http.register("/large_non_chunked", method="POST", port=3456, handler)

  resp = http.post(data=string.make(10_000), "http://localhost:3456/large_non_chunked")
  test.equals(resp.status_message, "OK")
  test.equals(resp.status_code, 200)
  test.equals(resp.http_version, "1.1")
  test.equals(resp.headers, [])
  test.equals("#{resp}", "")

  # Chunked query
  def handler(req, res) =
   test.equals(req.http_version, "1.1")
   test.equals(req.method, "POST")
   test.equals(req.query, [])
   test.equals(req.headers, [
      ("host", "localhost:3456"),
      ("user-agent", http.user_agent),
      ("accept", "*/*"),
      ("transfer-encoding", "chunked"),
      ("content-type", "application/x-www-form-urlencoded"),
      ("expect", "100-continue")
    ])
   test.equals(req.path, "/chunked")
   test.equals(req.body(timeout=5.0), "foobarlol")
  end

  harbor.http.register("/chunked", method="POST", port=3456, handler)

  data = begin
    is_done = ref(false)
    fun () ->
      if is_done() then "" else
        is_done := true
        "foobarlol"
      end
  end

  resp = http.post(data=data, "http://localhost:3456/chunked")
  test.equals(resp.status_message, "OK")
  test.equals(resp.status_code, 200)
  test.equals(resp.http_version, "1.1")
  test.equals(resp.headers, [])
  test.equals("#{resp}", "")

  # Large chunked query
  def handler(req, res) =
    test.equals(req.http_version, "1.1")
    test.equals(req.method, "POST")
    test.equals(req.query, [])
    test.equals(req.headers, [
      ("host", "localhost:3456"),
      ("user-agent", http.user_agent),
      ("accept", "*/*"),
      ("transfer-encoding", "chunked"),
      ("content-type", "application/x-www-form-urlencoded"),
      ("expect", "100-continue")
    ])
    test.equals(req.path, "/large_chunked")

    def rec f(count) =
      ret = req.data()

      if ret != "" then
        f(count + string.length(ret))
      else
        count
      end
    end

   test.equals(f(0), 10_000_000)
  end

  harbor.http.register("/large_chunked", method="POST", port=3456, handler)

  data = begin
    tmp = string.make(10_000)
    count = ref(10_000_000 / 10_000)
    fun () ->
      if 0 < count() then
        count := count() - 1
        tmp
      else
        ""
      end
  end

  # Custom response
  def handler(req) =
   test.equals(req.http_version, "1.1")
   test.equals(req.method, "GET")
   test.equals(req.data(), "")
   test.equals(req.query, [])
   test.equals(req.path, "/custom")
    req.socket.write("HTTP/1.0 201 YYR\r\nFoo: bar\r\n\r\n")
    req.socket.close()
    null()
  end

  harbor.http.register.simple("/custom", port=3456, handler)
  resp = http.get("http://localhost:3456/custom")

  test.equals(resp.status_message, "YYR")
  test.equals(resp.status_code, 201)
  test.equals(resp.http_version, "1.0")
  test.equals(resp.headers, [("foo","bar")])
  test.equals("#{resp}", "")

  # Cors headers
  harbor.http.middleware.register(harbor.http.middleware.cors(origin="foo.com"))
  resp = http.get("http://localhost:3456/default")
  test.equals(resp.status_message, "OK")
  test.equals(resp.http_version, "1.1")
  test.equals(resp.status_code, 200)
  test.equals(resp.headers, [
    ("access-control-allow-origin", "foo.com"),
    ("vary", "Origin")
  ])
  test.equals("#{resp}", "")

  # transport conflict
  transport = http.transport.ssl(certificate="foo", key="bla")
  try
    harbor.http.register("/default", transport=transport, port=3456, fun (_, _) -> ())
    test.fail()
  catch err : [error.http] do
   test.equals(err.message, "Port is already opened with a different transport")
  end

  # Response ended
  read = ref(fun (~timeout=_) -> error.raise(error.assertion))
  def handler(req, _) =
    read := req.data
  end

  harbor.http.register("/response_ended", port=3456, handler)
  resp = http.get("http://localhost:3456/response_ended")
  test.equals(resp.status_message, "OK")
  test.equals(resp.status_code, 200)

  try
    fn = read()
    fn(timeout=10.)
    test.fail()
  catch err: [error.http] do
   test.equals(err.message, "Response ended!")
  end

  # Register output on the same port
  output.harbor(port=3456, mount="bla.wav", on_start=test.pass, %wav, sine())
end

test.check(f)
