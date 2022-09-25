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

  fname = "post.liq"

  range = [...string.char.ascii.alphabet, ...string.char.ascii.number]
  l = list.init(12, fun(_) -> string.char.ascii.random(range))
  boundary = string.concat(l)

  def handler(req, _) =
   test.equals(req.http_version, "1.1")
   test.equals(req.method, "POST")
   test.equals(req.query, [])
   test.equals(req.headers, [
      ("host", "localhost:3456"),
      ("user-agent", http.user_agent),
      ("accept", "*/*"),
      ("transfer-encoding", "chunked"),
      ("content-type", "multipart/form-data; boundary=#{boundary}"),
      ("expect", "100-continue")
    ])
   test.equals(req.path, "/large_non_chunked")
   test.equals(
      harbor.http.request.body(timeout=5.0, req),
      "--#{boundary}\r\n\
       Content-Disposition: form-data; name=\"file\"; filename=\"#{fname}\"\r\n\
       Content-Type: text/plain\r\n\
       \r\n\
       #{file.contents(fname)}\r\n\
       --#{boundary}--\r\n")
  end

  harbor.http.register("/large_non_chunked", method="POST", port=3456, handler)

  resp = http.post.file(file=fname, content_type="text/plain", boundary=boundary, "http://localhost:3456/large_non_chunked")
  test.equals(resp.status_message, "OK")
  test.equals(resp.status_code, 200)
  test.equals(resp.http_version, "1.1")
  test.equals(resp.headers, [])
  test.equals("#{resp}", "")

  test.pass()
end

test.check(f)
