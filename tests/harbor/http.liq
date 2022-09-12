def t(v, v') =
  if v != v' then
    print("expected: #{v'}, got: #{v}")
    test.fail()
  end
end

def f() =
  # Default response
  def handler(req, _) =
    t(req.http_version, "1.1")
    t(req.method, "GET")
    t(req.data, "")
    t(req.query, [])
    t(req.path, "/default")
  end

  harbor.http.register("/default", port=3456, handler)
  resp = http.get("http://localhost:3456/default")
  t(resp.headers, [("transfer-encoding", "chunked")])
  t(resp.status_message, "OK")
  t(resp.http_version, "1.1")
  t(resp.status_code, 200)
  t("#{resp}", "")

  # Endpoint are executed in the order they are declared
  def handler(_, _) =
    test.fail()
  end
  harbor.http.register.regexp(r/default/g, port=3456, handler)

  # String response with matches
  def handler(req, _) =
    t(req.http_version, "1.1")
    t(req.method, "GET")
    t(req.data, "")
    t(req.query, [("bla","blo"),("gni","gno")])
    t(req.path, "/path/gno/blo")
  end

  harbor.http.register("/path/:gni/:bla", port=3456, handler)
  resp = http.get("http://localhost:3456/path/gno/blo")
  t(resp.headers, [("transfer-encoding", "chunked")])
  t(resp.status_message, "OK")
  t(resp.http_version, "1.1")
  t(resp.status_code, 200)
  t("#{resp}", "")

  # Full query
  def handler(req, res) =
    t(req.http_version, "1.0")
    t(req.method, "POST")
    t(req.data, "foobarlol")
    t(req.query, [("bla", "in"), ("foo", "with"), ("gnu", "gno"), ("gni", "gno")])
    t(req.headers, [
      ("Host", "localhost:3456"),
      ("User-Agent", http.user_agent),
      ("Accept", "*/*"),
      ("req", "header"),
      ("Content-Length", "9"),
      ("Content-Type", "application/x-www-form-urlencoded")
    ])
    t(req.path, "/some/path/with/full/in/it")

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
  t(resp.headers, [
    ("content-type", "liquidsoap/test"),
    ("some", "value"),
    ("transfer-encoding", "chunked")
  ])
  t(resp.status_message, "YYR")
  t(resp.http_version, "1.0")
  t(resp.status_code, 201)
  t("#{resp}", "gnignognignognignognigno")

  # Custom response
  def handler(req, res) =
    t(req.http_version, "1.1")
    t(req.method, "GET")
    t(req.data, "")
    t(req.query, [])
    t(req.path, "/custom")
    res.custom(true)
    req.socket.write("HTTP/1.0 201 YYR\r\nFoo: bar\r\n")
    req.socket.close()
  end

  harbor.http.register("/custom", port=3456, handler)
  resp = http.get("http://localhost:3456/custom")
  t(resp.headers, [("foo","bar")])
  t(resp.status_message, "YYR")
  t(resp.http_version, "1.0")
  t(resp.status_code, 201)
  t("#{resp}", "")

  test.pass()
end

test.check(f)
