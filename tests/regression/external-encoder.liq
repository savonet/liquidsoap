tmp = file.temp("foo", "flv")
on_cleanup({file.remove(tmp)})

encoder = %external(process="cat")

output.file(
  fallible=true,
  encoder,
  "/tmp/cat",
  on_start=test.pass,
  sine(duration=1.)
)
