#!../../liquidsoap ../test.liq

def t(x,y) =
  if x != y then test.fail() end
end

def f() =
  t(process.read("printf toto"), "toto")
  p = process.run("exit 2")
  t(p.status, "exit")
  t(p.status.code, 2)
  p = process.run(timeout=0.5, "sleep 1")
  t(p.status, "timeout")
  t(process.quote.command(stdin="bla", stdout="blo", stderr="blo", "foo", args=["gni", 'gno"gna']), "'foo' 'gni' 'gno\"gna' <'bla' >'blo' 2>&1")

  test.pass()
end

test.check(f)
