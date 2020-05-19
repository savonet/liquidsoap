#!../src/liquidsoap ../libs/pervasives.liq

%include "test.liq"

def rlog(_) = () end

def f() =
  if file_protocol(rlog=rlog, maxtime=1., "file:///path/to/file") != ["/path/to/file"] then
    test.fail()
  end

  if file_protocol(rlog=rlog, maxtime=1., "file:/path/to/file") != ["/path/to/file"] then
    test.fail()
  end

  test.pass()
end

thread.run(delay=0., f)

output.dummy(blank())
