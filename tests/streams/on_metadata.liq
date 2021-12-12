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
# s.on_metadata(f)
s = insert_metadata(s)

def insert()
  print("Inserting metadata")
  s.insert_metadata([("test", "bla")])
end

thread.run(delay=1., insert)

output.dummy(s)
