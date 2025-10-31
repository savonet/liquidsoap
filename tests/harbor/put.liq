def f() =
  data =
    http.multipart_form_data(
      boundary="foobar",
      [
        {
          name=
            "the name",
          contents=getter("foobarlol"),
          headers=[("some", "headers")],
          attributes=
            [
              (
                "filename",
                "the filename"
              )
            ]
        },
        {name="namez", contents=getter("gnigno"), headers=[], attributes=[]}
      ]
    )

  test.equal(
    string.getter.flush(data.contents),
    "--foobar\r\nContent-Disposition: form-data; name=\"the name\"; \
     filename=\"the filename\"\r\nsome: \
     headers\r\n\r\nfoobarlol\r\n--foobar\r\nContent-Disposition: form-data; \
     name=\"namez\"\r\n\r\ngnigno\r\n--foobar--\r\n"
  )

  test.equal(data.boundary, "foobar")
  data =
    http.multipart_form_data(
      boundary="foobar",
      [
        {
          name=
            "the name",
          contents=getter("foobarlol"),
          headers=[("some", "headers")],
          attributes=
            [
              (
                "filename",
                "the filename"
              )
            ]
        },
        {name="namez", contents=getter("gnigno"), headers=[], attributes=[]}
      ]
    )

  test.equal(
    string.getter.flush(data.contents),
    "--#{data.boundary}\r\nContent-Disposition: form-data; name=\"the name\"; \
     filename=\"the filename\"\r\nsome: headers\r\n\r\nfoobarlol\r\n--#{
      data.boundary
    }\r\nContent-Disposition: form-data; name=\"namez\"\r\n\r\ngnigno\r\n--#{
      data.boundary
    }--\r\n"
  )

  # Test file upload
  fname = "put.liq"
  range = [...string.char.ascii.alphabet, ...string.char.ascii.number]
  l = list.init(12, fun (_) -> string.char.ascii.random(range))
  boundary = string.concat(l)

  def handler(req, _) =
    test.equal(req.http_version, "1.1")
    test.equal(req.method, "PUT")
    test.equal(req.query, [])
    test.equal(
      req.headers,
      [
        ("host", "localhost:5427"),
        ("user-agent", http.user_agent),
        ("accept", "*/*"),
        ("transfer-encoding", "chunked"),
        (
          "content-type",
          "multipart/form-data; boundary=#{boundary}"
        ),
        ("expect", "100-continue")
      ]
    )

    test.equal(req.path, "/large_non_chunked")
    test.equal(
      req.body(timeout=5.0),
      "--#{boundary}\r\nContent-Disposition: form-data; name=\"file\"; \
       filename=\"#{fname}\"\r\nContent-Type: text/plain\r\n\r\n#{
        file.contents(fname)
      }\r\n--#{boundary}--\r\n"
    )
  end

  harbor.http.register("/large_non_chunked", method="PUT", port=5427, handler)
  resp =
    http.put.file(
      file=fname,
      content_type="text/plain",
      boundary=boundary,
      "http://localhost:5427/large_non_chunked"
    )

  test.equal(resp.status_message, "OK")
  test.equal(resp.status_code, 200)
  test.equal(resp.http_version, "1.1")
  test.equal(resp.headers, [])
  test.equal("#{resp}", "")

  # Test in-memory file upload
  range = [...string.char.ascii.alphabet, ...string.char.ascii.number]
  l = list.init(12, fun (_) -> string.char.ascii.random(range))
  boundary = string.concat(l)

  def handler(req, _) =
    test.equal(req.http_version, "1.1")
    test.equal(req.method, "PUT")
    test.equal(req.query, [])
    test.equal(
      req.headers,
      [
        ("host", "localhost:5427"),
        ("user-agent", http.user_agent),
        ("accept", "*/*"),
        ("transfer-encoding", "chunked"),
        (
          "content-type",
          "multipart/form-data; boundary=#{boundary}"
        ),
        ("expect", "100-continue")
      ]
    )

    test.equal(req.path, "/in_memory_file_upload")
    test.equal(
      req.body(timeout=5.0),
      "--#{boundary}\r\nContent-Disposition: form-data; name=\"file\"; \
       filename=\"foo.txt\"\r\nContent-Type: text/plain\r\n\r\nfoobarlol\r\n--#{
        boundary
      }--\r\n"
    )
  end

  harbor.http.register(
    "/in_memory_file_upload",
    method="PUT",
    port=5427,
    handler
  )

  resp =
    http.put.file(
      filename="foo.txt",
      contents="foobarlol",
      content_type="text/plain",
      boundary=boundary,
      "http://localhost:5427/in_memory_file_upload"
    )

  test.equal(resp.status_message, "OK")
  test.equal(resp.status_code, 200)
  test.equal(resp.http_version, "1.1")
  test.equal(resp.headers, [])
  test.equal("#{resp}", "")

  # Test JSON put
  j = json.object()
  j.add("foo", random.int())
  j.add("contents", file.contents("put.liq"))
  data = json.stringify(j)

  def handler(req, _) =
    test.equal(req.http_version, "1.1")
    test.equal(req.method, "PUT")
    test.equal(req.query, [])
    test.equal(
      req.headers,
      [
        ("host", "localhost:5427"),
        ("user-agent", http.user_agent),
        ("transfer-encoding", "chunked"),
        ("content-type", "application/json"),
        ("accept", "application/json"),
        ("expect", "100-continue")
      ]
    )

    test.equal(req.path, "/json_put")
    test.equal(req.body(timeout=5.0), data)
  end

  harbor.http.register("/json_put", method="PUT", port=5427, handler)
  resp =
    http.put(
      headers=[
        ("Content-Type", "application/json"),
        ("Accept", "application/json")
      ],
      data=data,
      "http://localhost:5427/json_put"
    )

  test.equal(resp.status_message, "OK")
  test.equal(resp.status_code, 200)
  test.equal(resp.http_version, "1.1")
  test.equal(resp.headers, [])
  test.equal("#{resp}", "")
  test.pass()
end

test.check(f)
