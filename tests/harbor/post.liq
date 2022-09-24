def f() =
  data = http.post.multipart_form_data(boundary="foobar", [{
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

  data = http.post.multipart_form_data(boundary="foobar", [{
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

  test.pass()
end

test.check(f)
