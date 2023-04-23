s = sine()

tmp = file.temp("foo", "flv")
on_shutdown({file.remove(tmp)})

encoder = %external(process="cat")

output.file(
  encoder,
  "/tmp/cat",
  on_start=test.pass,
  sine()
)
