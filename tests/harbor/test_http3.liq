def f() =
  content_type =
    http.headers.content_type(
      [
        (
          "Content-Type",
          "text/html; charset=utf-8"
        )
      ]
    )

  test.equal(content_type, {mime="text/html", args=[("charset", "utf-8")]})
  content_type =
    http.headers.content_type(
      [
        (
          "coNtent-tYpe",
          "multipart/form-data; boundary=something"
        )
      ]
    )

  test.equal(
    content_type,
    {mime="multipart/form-data", args=[("boundary", "something")]}
  )

  extname =
    http.headers.extname(
      [
        (
          "Content-Disposition",
          'attachment; filename="filename.jpg"'
        )
      ]
    )

  test.equal(extname, ".jpg")
  extname =
    http.headers.extname(
      [
        ("Content-type", "audio/mpeg"),
        (
          "Content-Disposition",
          'attachment; filename="filename.jpg"'
        )
      ]
    )

  test.equal(extname, ".jpg")
  settings.http.mime.extnames := [("foo", ".bla")]
  extname = http.headers.extname([("conTent-tyPe", "foo")])
  test.equal(extname, ".bla")
  test.pass()
end

test.check(f)
