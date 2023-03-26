s = source({
  audio = source.tracks(blank()).audio
})

output.dummy(blank())

thread.run(delay=0.1, test.pass)
