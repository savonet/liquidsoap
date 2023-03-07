def f()
  extname = http.headers.extname([
    ("Content-Disposition", 'attachment; filename="filename.jpg"')
  ])
  test.equals(extname, ".jpg")

  extname = http.headers.extname([
    ("Content-type", "audio/mpeg"),
    ("Content-Disposition", 'attachment; filename="filename.jpg"')
  ])
  test.equals(extname, ".jpg")

  settings.http.mime.extnames := [("foo", ".bla")]

  extname = http.headers.extname([
    ("conTent-tyPe", "foo"),
  ])
  test.equals(extname, ".bla")

  test.pass()
end

test.check(f)
