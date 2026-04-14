#!../../liquidsoap ../test.liq
s = (single("file1.png") : source(video=yuv420p))
output.dummy(fallible=true, s)
thread.run(delay=3., test.pass)
