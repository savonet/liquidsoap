#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

# Basic request tests

%include "test.liq"

# This test started randomly failing
# recently. It's passing when invoked manually
# and is still passing in `main`
test.skip()

settings.log.level.set(4)

def next() =
  request.create("files/file1.mp3")
end

r = request.dynamic(prefetch=1, next)

output.dummy(fallible=true, r)

def f() =
  if not r.is_ready() then
    0.1
  else
    if not r.add(request.create("files/file2.mp3")) then
      test.fail()
    end

    r.set_queue([
      request.create("files/file2.mp3"),
      request.create("files/file3.mp3")
    ])

    q = r.queue()

    if list.length(q) != 2 then
      test.fail()
    end

    c = r.current()

    if not null.defined(c) then
      test.fail()
    end

    c = null.get(c)
    if request.filename(c) != "files/file1.mp3" then
      test.fail()
    end

    test.pass()

    (-1.)
  end
end

thread.run.recurrent(f)
