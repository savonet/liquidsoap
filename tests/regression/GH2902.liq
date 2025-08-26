def f() =
  let {uri, timeout, cmd, extname} =
    protocol.process.parse(
      default_timeout=200.,
      "timeout=123,.foo,bla,gni,gno:uri:uro:ura"
    )

  test.equal(uri, "uri:uro:ura")
  test.equal(timeout, 123.)
  test.equal(cmd, "bla,gni,gno")
  test.equal(extname, ".foo")
  let {uri, timeout, cmd, extname} =
    protocol.process.parse(
      default_timeout=200.,
      "timeout=123.123,.foo,bla,gni,gno:uri:uro:ura"
    )

  test.equal(uri, "uri:uro:ura")
  test.equal(timeout, 123.123)
  test.equal(cmd, "bla,gni,gno")
  test.equal(extname, ".foo")
  let {uri, timeout, cmd, extname} =
    protocol.process.parse(
      default_timeout=100.,
      "timeout=123,.foo,bla,gni,gno:uri:uro:ura"
    )

  test.equal(uri, "uri:uro:ura")
  test.equal(timeout, 100.)
  test.equal(cmd, "bla,gni,gno")
  test.equal(extname, ".foo")
  let {uri, timeout, cmd, extname} =
    protocol.process.parse(default_timeout=200., ".foo,bla,gni,gno:uri:uro:ura")

  test.equal(uri, "uri:uro:ura")
  test.equal(timeout, 200.)
  test.equal(cmd, "bla,gni,gno")
  test.equal(extname, ".foo")
  test.pass()
end

test.check(f)
