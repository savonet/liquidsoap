def t(x,y) =
  if x != y then test.fail() end
end

def f() =
  t(process.read("printf toto"), "toto")
  p = process.run("exit 2")
  t(string(p.status), "exit")
  t(p.status.code, 2)
  p = process.run(timeout=0.5, "sleep 1")
  t(string(p.status), "timeout")

  test.pass()
end

test.check(f)
