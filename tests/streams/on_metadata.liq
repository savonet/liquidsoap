#!../../src/liquidsoap ../../libs/stdlib.liq ../../libs/deprecations.liq

%include "test.liq"

log.level.set(4)

def f(_)
  print("Got metadata!")
  test.pass()
  shutdown()
end

s = sine()
s = source.on_metadata(s,f)
s = insert_metadata(s)

thread.run(delay=1., {s.insert_metadata([("test", "bla")])})

output.dummy(s)
