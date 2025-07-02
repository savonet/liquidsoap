log.level.set(4)

def f(_) =
  print(
    "Got metadata!"
  )
  test.pass()
end

s = sine()

def insert() =
  print(
    "Inserting metadata"
  )
  s.insert_metadata([("test", "bla")])
end

s.on_metadata(synchronous=true, f)
thread.run(delay=1., insert)
output.dummy(s)
