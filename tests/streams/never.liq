#!../../liquidsoap ../test.liq

# Test none/never annotations in sources, see #3222.

s = (single("file1.png"):source(audio=none,...))
output.dummy(fallible=true, (s:source(video=canvas)))

thread.run(delay=3., test.pass)
