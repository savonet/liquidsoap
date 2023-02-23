def f() =
  let {uri, timeout, cmd, extname} = protocol.process.parse(default_timeout=200., "timeout=123,.foo,bla,gni,gno:uri:uro:ura")
  test.equals(uri, "uri:uro:ura")
  test.equals(timeout, 123.)
  test.equals(cmd, "bla,gni,gno")
  test.equals(extname, ".foo")

  let {uri, timeout, cmd, extname} = protocol.process.parse(default_timeout=200., "timeout=123.123,.foo,bla,gni,gno:uri:uro:ura")
  test.equals(uri, "uri:uro:ura")
  test.equals(timeout, 123.123)
  test.equals(cmd, "bla,gni,gno")
  test.equals(extname, ".foo")

  let {uri, timeout, cmd, extname} = protocol.process.parse(default_timeout=100., "timeout=123,.foo,bla,gni,gno:uri:uro:ura")
  test.equals(uri, "uri:uro:ura")
  test.equals(timeout, 100.)
  test.equals(cmd, "bla,gni,gno")
  test.equals(extname, ".foo")


  let {uri, timeout, cmd, extname} = protocol.process.parse(default_timeout=200., ".foo,bla,gni,gno:uri:uro:ura")
  test.equals(uri, "uri:uro:ura")
  test.equals(timeout, 200.)
  test.equals(cmd, "bla,gni,gno")
  test.equals(extname, ".foo")

  test.pass()
end

test.check(f)
