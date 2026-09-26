%include "../test.liq"

def raises(kind, f) =
  try
    f()
    false
  catch err do
    err.kind == kind
  end
end

def f() =
  dir = file.temp_dir("gh5454")
  missing = path.concat(dir, "missing")
  server = socket.unix(domain=socket.domain.unix)
  server.bind(socket.address.unix(path.concat(dir, "server.sock")))
  server.listen(1)
  if
    raises("file", fun () -> ignore(file.ls(missing)))
    and raises("socket", fun () -> ignore(socket.unix.client(missing)))
    and raises("socket", fun () -> ignore(server.accept(timeout=0.1)))
    and raises("eval", fun () -> ignore(1 / 0))
    and raises("invalid", fun () -> ignore(string.char(300)))
  then
    test.pass()
  else
    test.fail()
  end
end

test.check(f)
