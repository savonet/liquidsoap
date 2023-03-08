def f()
  content_disposition = null.get(http.headers.content_disposition([
    ('Content-Disposition', 'inline')
  ]))
  test.equals(content_disposition.type, "inline")
  test.equals(content_disposition?.filename, null())
  test.equals(content_disposition.args, [])

  content_disposition = null.get(http.headers.content_disposition([
    ('Content-Disposition', 'attachment; filename="filename.jpg"')
  ]))
  test.equals(content_disposition.type, "attachment")
  test.equals(content_disposition?.filename, "filename.jpg")
  test.equals(content_disposition.args, [])

  content_disposition = null.get(http.headers.content_disposition([
    ('Content-Disposition', "attachment; filename*=UTF-8''Na%C3%AFve%20file.txt")
  ]))
  test.equals(content_disposition.type, "attachment")
  test.equals(content_disposition?.filename, "Naïve file.txt")
  test.equals(content_disposition.args, [])

  content_disposition = null.get(http.headers.content_disposition([
    ('Content-Disposition', "attachment; filename=Na%C3%AFve%20file.txt")
  ]))
  test.equals(content_disposition.type, "attachment")
  test.equals(content_disposition?.filename, "Naïve file.txt")
  test.equals(content_disposition.args, [])

  content_disposition = null.get(http.headers.content_disposition([
    ('Content-Disposition', "attachment; filename=Naïve file.txt")
  ]))
  test.equals(content_disposition.type, "attachment")
  test.equals(content_disposition?.filename, "Naïve file.txt")
  test.equals(content_disposition.args, [])

  content_disposition = null.get(http.headers.content_disposition([
    ('coNtent-dispOsition', "attachment; filename*=UTF-8''Na%C3%AFve%20file.txt; name=\"gni%20gno\"; foo=bla")
  ]))
  test.equals(content_disposition.type, "attachment")
  test.equals(content_disposition?.filename, "Naïve file.txt")
  test.equals(content_disposition?.name, "gni gno");
  test.equals(content_disposition.args, [("foo","bla")])

  test.pass()
end

test.check(f)
