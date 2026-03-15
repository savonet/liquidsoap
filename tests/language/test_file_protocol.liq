def rlog(_) =
  ()
end

tests =
  [
    ("file:///path/to/file", "/path/to/file"),
    ("file:/path/to/file", "/path/to/file"),
    (
      "file:/path/to/my%20file",
      "/path/to/my file"
    )
  ]

def f() =
  list.iter(
    fun ((t, t')) -> test.equal(file_protocol(rlog=rlog, maxtime=1., t), t'),
    tests
  )
  test.pass()
end

test.check(f)
