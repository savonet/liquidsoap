settings.log.level.set(4)
output.dummy(fallible = true, sine())
thread.run(delay=3., {
  print("sending kill -s INT #{process.pid()}")
  process.run("kill -s INT #{process.pid()}")
})
