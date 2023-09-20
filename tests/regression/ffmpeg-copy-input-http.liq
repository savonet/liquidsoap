s = input.http("foo")
output.srt(port=10001, %ffmpeg(%audio.copy), fallible=true, s)

def f() =
  test.pass()
end

test.check(f)
