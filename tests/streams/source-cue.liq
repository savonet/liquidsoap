#!../../liquidsoap ../test.liq

cuefile = "/tmp/test.cue"

s = insert_metadata(sine())
f = s.insert_metadata
s = source.cue(s, cuefile, title="tit", performer="perf", file="bla.mp3")

output.dummy(s)

def pass()
  print(file.contents(cuefile))
  test.pass()
end

thread.run(delay=1., {f([("artist", "artist1"), ("title", "title1")])})
thread.run(delay=2., {f([("artist", "artist2"), ("title", "title2")])})
thread.run(delay=3., pass)
