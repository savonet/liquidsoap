def rlog(_) = () end

def f() =
  if file_protocol(rlog=rlog, maxtime=1., "file:///path/to/file") != ["/path/to/file"] then
    test.fail()
  end

  if file_protocol(rlog=rlog, maxtime=1., "file:/path/to/file") != ["/path/to/file"] then
    test.fail()
  end

  if file_protocol(rlog=rlog, maxtime=1., "file:/path/to/my%20file") != ["/path/to/my file"] then
    test.fail()
  end

  test.pass()
end

test.check(f)
