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
  fname = "post.liq"
  range = [...string.char.ascii.alphabet, ...string.char.ascii.number]
  l = list.init(12, fun (_) -> string.char.ascii.random(range))
  boundary = string.concat(l)

  def handler(req, _) =
    test.equal(req.http_version, "1.1")
    test.equal(req.method, "POST")
    test.equal(req.query, [])
    test.equal(
      req.headers,
      [
        ("host", "localhost:6543"),
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

    test.equal(req.path, "/file_upload")
    test.equal(
      req.body(timeout=5.0),
      "--#{boundary}\r\nContent-Disposition: form-data; name=\"file\"; \
       filename=\"#{fname}\"\r\nContent-Type: text/plain\r\n\r\n#{
        file.contents(fname)
      }\r\n--#{boundary}--\r\n"
    )
  end

  harbor.http.register("/file_upload", method="POST", port=6543, handler)
  resp =
    http.post.file(
      file=fname,
      content_type="text/plain",
      boundary=boundary,
      "http://localhost:6543/file_upload"
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
    test.equal(req.method, "POST")
    test.equal(req.query, [])
    test.equal(
      req.headers,
      [
        ("host", "localhost:6543"),
        ("user-agent", http.user_agent),
        ("accept", "*/*"),
        (
          "content-type",
          "multipart/form-data; boundary=#{boundary}"
        ),
        ("content-length", "138")
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
    method="POST",
    port=6543,
    handler
  )

  resp =
    http.post.file(
      filename="foo.txt",
      contents="foobarlol",
      content_type="text/plain",
      boundary=boundary,
      "http://localhost:6543/in_memory_file_upload"
    )

  test.equal(resp.status_message, "OK")
  test.equal(resp.status_code, 200)
  test.equal(resp.http_version, "1.1")
  test.equal(resp.headers, [])
  test.equal("#{resp}", "")

  # Test JSON post
  j = json()
  j.add("foo", random.int())
  j.add("contents", file.contents("post.liq"))
  data = json.stringify(j)

  def handler(req, _) =
    test.equal(req.http_version, "1.1")
    test.equal(req.method, "POST")
    test.equal(req.query, [])
    test.equal(
      req.headers,
      [
        ("host", "localhost:6543"),
        ("user-agent", http.user_agent),
        ("content-type", "application/json"),
        ("accept", "application/json"),
        ("content-length", "#{string.length(data)}")
      ]
    )

    test.equal(req.path, "/json_post")
    test.equal(req.body(timeout=5.0), data)
  end

  harbor.http.register("/json_post", method="POST", port=6543, handler)
  resp =
    http.post(
      headers=[
        ("Content-Type", "application/json"),
        ("Accept", "application/json")
      ],
      data=data,
      "http://localhost:6543/json_post"
    )

  test.equal(resp.status_message, "OK")
  test.equal(resp.status_code, 200)
  test.equal(resp.http_version, "1.1")
  test.equal(resp.headers, [])
  test.equal("#{resp}", "")
  test.pass()
end

test.check(f)
