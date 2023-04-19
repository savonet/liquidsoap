log.level.set(4)

def f(m)
  print("Got metadata!")
  if m == [("s", "That's me!"), ("test", "bla"), ("s'", "That's me!"), ("test", "blo")] then
    test.pass()
  else
    test.fail()
  end
end

s = sine()
s = insert_metadata(s)

s' = sine()
s' = insert_metadata(s')

def insert()
  print("Inserting metadata")
  s.insert_metadata([("test", "bla"), ("s","That's me!")])
  s'.insert_metadata([("test", "blo"), ("s'","That's me!")])
end

s = source({
  audio = source.tracks(s).audio,
  metadata = track.metadata.merge([
    source.tracks(s).metadata,
    source.tracks(s').metadata
  ])
})

s = source.on_metadata(s,f)

thread.run(delay=1., insert)

output.dummy(s)
