# Verify that the auth function receives credentials and can accept connections.
def f() =
  def auth(info) =
    info.login == "valid" and info.password == "secret"
  end

  o = output.harbor(%mp3, mount="test", port=3473, auth=auth, sine())
  o.on_connect(synchronous=false, fun (_) -> test.pass())

  l = input.http("http://valid:secret@localhost:3473/test")
  output.dummy(fallible=true, l)
end

test.check(f)
