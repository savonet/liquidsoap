s = single("./theora-test.mp4")

enc = %theora

tmp = file.temp("foo", "ogg")
on_cleanup({file.remove(tmp)})

output.file(fallible=true, enc, tmp, s)

thread.run(delay=2., test.pass)
