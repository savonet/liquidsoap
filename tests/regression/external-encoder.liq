tmp = file.temp("foo", "flv")
on_cleanup({file.remove(tmp)})
encoder = %external(process = "cat")
o = output.file(
  fallible=true, encoder, "/tmp/cat", sine(duration=1.)
)
o.on_start(test.pass)
