%include "../test.liq"

# A failed http download must not leave its temporary file behind.
port = 5668
extname = ".leakcheck5668"

def head(_) =
  http.response(
    status_code=200,
    headers=[
      (
        "Content-Disposition",
        "attachment; filename=\"file#{extname}\""
      )
    ],
    data=""
  )
end

def missing(_) =
  http.response(
    status_code=404,
    data="not here"
  )
end

def slow(_) =
  thread.pause(3.)
  http.response(
    status_code=200,
    data="too late"
  )
end

harbor.http.register.simple("/missing", head, port=port, method="HEAD")
harbor.http.register.simple("/missing", missing, port=port, method="GET")
harbor.http.register.simple("/slow", head, port=port, method="HEAD")
harbor.http.register.simple("/slow", slow, port=port, method="GET")

def leftovers() =
  probe = file.temp("probe", "")
  file.remove(probe)
  list.filter(
    fun (f) -> file.extension(f) == extname,
    file.ls(path.dirname(probe))
  )
end

def fetch(~timeout, name) =
  protocol.http(
    "http",
    rlog=fun (_) -> (),
    maxtime=time() + timeout,
    "//localhost:#{port}/#{name}"
  )
end

def check() =
  before = leftovers()
  test.equal(fetch(timeout=5., "missing"), null)
  test.equal(leftovers(), before)
  test.equal(fetch(timeout=1., "slow"), null)
  test.equal(leftovers(), before)
  test.pass()
end

test.check(check)
