log.level.set(4)

def f(_)
  print("Got metadata!")
  test.pass()
  shutdown()
end

s = sine()
s = insert_metadata(s)

def insert()
  print("Inserting metadata")
  s.insert_metadata([("test", "bla")])
end

s = source.on_metadata(s,f)

thread.run(delay=1., insert)

output.dummy(s)
