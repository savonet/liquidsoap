def f() =
  data = http.multipart_form_data(boundary="foobar", [{
    name = "the name",
    contents = getter("foobarlol"),
    headers = [("some","headers")],
    attributes = [("filename", "the filename")]
  }, {
    name = "namez",
    contents = getter("gnigno"),
    headers = [],
    attributes = []
  }])

  test.equals(
    string.getter.flush(data.contents),
    "--foobar\r\n\
     Content-Disposition: form-data; name=\"the name\"; filename=\"the filename\"\r\n\
     some: headers\r\n\
     \r\n\
     foobarlol\r\n\
     --foobar\r\n\
     Content-Disposition: form-data; name=\"namez\"\r\n\
     \r\n\
     gnigno\r\n\
     --foobar--\r\n"
  )
  test.equals(data.boundary, "foobar")

  data = http.multipart_form_data(boundary="foobar", [{
    name = "the name",
    contents = getter("foobarlol"),
    headers = [("some","headers")],
    attributes = [("filename", "the filename")]
  }, {
    name = "namez",
    contents = getter("gnigno"),
    headers = [],
    attributes = []
  }])

  test.equals(
    string.getter.flush(data.contents),
    "--#{data.boundary}\r\n\
     Content-Disposition: form-data; name=\"the name\"; filename=\"the filename\"\r\n\
     some: headers\r\n\
     \r\n\
     foobarlol\r\n\
     --#{data.boundary}\r\n\
     Content-Disposition: form-data; name=\"namez\"\r\n\
     \r\n\
     gnigno\r\n\
     --#{data.boundary}--\r\n"
  )

  # Test file upload

  fname = "post.liq"

  range = [...string.char.ascii.alphabet, ...string.char.ascii.number]
  l = list.init(12, fun(_) -> string.char.ascii.random(range))
  boundary = string.concat(l)

  def handler(req, _) =
   test.equals(req.http_version, "1.1")
   test.equals(req.method, "POST")
   test.equals(req.query, [])
   test.equals(req.headers, [
      ("host", "localhost:6543"),
      ("user-agent", http.user_agent),
      ("accept", "*/*"),
      ("transfer-encoding", "chunked"),
      ("content-type", "multipart/form-data; boundary=#{boundary}"),
      ("expect", "100-continue")
    ])
   test.equals(req.path, "/file_upload")
   test.equals(
      req.body(timeout=5.0),
      "--#{boundary}\r\n\
       Content-Disposition: form-data; name=\"file\"; filename=\"#{fname}\"\r\n\
       Content-Type: text/plain\r\n\
       \r\n\
       #{file.contents(fname)}\r\n\
       --#{boundary}--\r\n")
  end

  harbor.http.register("/file_upload", method="POST", port=6543, handler)

  resp = http.post.file(file=fname, content_type="text/plain", boundary=boundary, "http://localhost:6543/file_upload")
  test.equals(resp.status_message, "OK")
  test.equals(resp.status_code, 200)
  test.equals(resp.http_version, "1.1")
  test.equals(resp.headers, [])
  test.equals("#{resp}", "")

  # Test in-memory file upload

  range = [...string.char.ascii.alphabet, ...string.char.ascii.number]
  l = list.init(12, fun(_) -> string.char.ascii.random(range))
  boundary = string.concat(l)

  def handler(req, _) =
   test.equals(req.http_version, "1.1")
   test.equals(req.method, "POST")
   test.equals(req.query, [])
   test.equals(req.headers, [
      ("host", "localhost:6543"),
      ("user-agent", http.user_agent),
      ("accept", "*/*"),
      ("content-type", "multipart/form-data; boundary=#{boundary}"),
      ("content-length", "138")
    ])
   test.equals(req.path, "/in_memory_file_upload")
   test.equals(
      req.body(timeout=5.0),
      "--#{boundary}\r\n\
       Content-Disposition: form-data; name=\"file\"; filename=\"foo.txt\"\r\n\
       Content-Type: text/plain\r\n\
       \r\n\
       foobarlol\r\n\
       --#{boundary}--\r\n")
  end

  harbor.http.register("/in_memory_file_upload", method="POST", port=6543, handler)

  resp = http.post.file(filename="foo.txt", contents="foobarlol", content_type="text/plain", boundary=boundary, "http://localhost:6543/in_memory_file_upload")
  test.equals(resp.status_message, "OK")
  test.equals(resp.status_code, 200)
  test.equals(resp.http_version, "1.1")
  test.equals(resp.headers, [])
  test.equals("#{resp}", "")

  # Test JSON post
  j = json()
  j.add("foo", random.int())
  j.add("contents", file.contents("post.liq"))
  data = json.stringify(j)

  def handler(req, _) =
   test.equals(req.http_version, "1.1")
   test.equals(req.method, "POST")
   test.equals(req.query, [])
   test.equals(req.headers, [
      ("host", "localhost:6543"),
      ("user-agent", http.user_agent),
      ("content-type", "application/json"),
      ("accept", "application/json"),
      ("content-length", "#{string.length(data)}")
    ])
   test.equals(req.path, "/json_post")
   test.equals(
      req.body(timeout=5.0),
      data)
  end

  harbor.http.register("/json_post", method="POST", port=6543, handler)

  resp = http.post(headers=[("Content-Type","application/json"),("Accept","application/json")], data=data, "http://localhost:6543/json_post")
  test.equals(resp.status_message, "OK")
  test.equals(resp.status_code, 200)
  test.equals(resp.http_version, "1.1")
  test.equals(resp.headers, [])
  test.equals("#{resp}", "")

  test.pass()
end

test.check(f)
