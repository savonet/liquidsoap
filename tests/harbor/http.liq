def f() =
  # Default response
  def handler(req, _) =
    test.equals(req.http_version, "1.1")
    test.equals(req.method, "GET")
    test.equals(req.data, "")
    test.equals(req.query, [])
    test.equals(req.path, "/default")
  end

  harbor.http.register("/default", port=3456, handler)
  resp = http.get("http://localhost:3456/default")
  test.equals(resp.headers, [])
  test.equals(resp.status_message, "OK")
  test.equals(resp.http_version, "1.1")
  test.equals(resp.status_code, 200)
  test.equals("#{resp}", "")

  # Endpoint are executed in the order they are declared
  def handler(_, _) =
    test.fail()
  end

  harbor.http.register.regexp(r/default/g, port=3456, handler)
  harbor.http.register("/default", port=3456, handler)
  resp = http.get("http://localhost:3456/default")
  test.equals(resp.headers, [])
  test.equals(resp.status_message, "OK")
  test.equals(resp.http_version, "1.1")
  test.equals(resp.status_code, 200)
  test.equals("#{resp}", "")

  # String response with matches
  def handler(req, _) =
    test.equals(req.http_version, "1.1")
    test.equals(req.method, "GET")
    test.equals(req.data, "")
    test.equals(req.query, [("gni", "gno"), ("bla", "blo")])
    test.equals(req.path, "/path/gno/blo")
  end

  harbor.http.register("/path/:gni/:bla", port=3456, handler)
  resp = http.get("http://localhost:3456/path/gno/blo")
  test.equals(resp.headers, [])
  test.equals(resp.status_message, "OK")
  test.equals(resp.http_version, "1.1")
  test.equals(resp.status_code, 200)
  test.equals("#{resp}", "")

  # Full query
  def handler(req, res) =
    test.equals(req.http_version, "1.0")
    test.equals(req.method, "POST")
    test.equals(req.data, "foobarlol")
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

    res.status_code(201)
    res.status_message("YYR")
    res.http_version("1.0")

    data_count = ref(3)
    def data() =
      if !data_count >= 0 then
        data_count := !data_count - 1
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
  test.equals(resp.headers, [
    ("some", "value"),
    ("transfer-encoding", "chunked"),
    ("content-type", "liquidsoap/test"),
  ])
  test.equals(resp.status_message, "YYR")
  test.equals(resp.http_version, "1.0")
  test.equals(resp.status_code, 201)
  test.equals("#{resp}", "gnignognignognignognigno")

  # Custom response
  def handler(req) =
    test.equals(req.http_version, "1.1")
    test.equals(req.method, "GET")
    test.equals(req.data, "")
    test.equals(req.query, [])
    test.equals(req.path, "/custom")
    req.socket.write("HTTP/1.0 201 YYR\r\nFoo: bar\r\n\r\n")
    req.socket.close()
    null()
  end

  harbor.http.register.simple("/custom", port=3456, handler)
  resp = http.get("http://localhost:3456/custom")
  test.equals(resp.headers, [("foo","bar")])
  test.equals(resp.status_message, "YYR")
  test.equals(resp.http_version, "1.0")
  test.equals(resp.status_code, 201)
  test.equals("#{resp}", "")

  # Cors headers
  harbor.http.middleware.register(harbor.http.middleware.cors(origin="foo.com"))
  resp = http.get("http://localhost:3456/default")
  test.equals(resp.headers, [
    ("access-control-allow-origin", "foo.com"),
    ("vary", "Origin")
  ])
  test.equals(resp.status_message, "OK")
  test.equals(resp.http_version, "1.1")
  test.equals(resp.status_code, 200)
  test.equals("#{resp}", "")

  # transport conflict
  transport = http.transport.ssl(certificate="foo", key="bla")
  try
    harbor.http.register("/default", transport=transport, port=3456, fun (_, _) -> ())
    test.fail()
  catch err : [error.http] do
    test.equals(error.message(err), "Port is already opened with a different transport")
  end

  # Register output on the same port
  output.harbor(port=3456, mount="bla.wav", on_start=test.pass, %wav, sine())
end

test.check(f)
