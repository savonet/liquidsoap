s = (single("file1.png"):source(video=canvas))

output.dummy(fallible=true, s)

def on_done () =
  test.pass()
  shutdown()
end

thread.run(delay=3., on_done)
