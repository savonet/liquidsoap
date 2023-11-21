#!../../liquidsoap ../test.liq

s_ref = ref((fallback([]) : source))

def next() =
  r = request.create("../streams/file1.png")
  ignore(request.resolve(r, content_type=s_ref()))
  s = request.dynamic(fun () -> null())
  s.set_queue([r])
  s
end

s = (source.dynamic(track_sensitive=true, next) : source(video=canvas))

s_ref := s

output.dummy(fallible=true, s)
thread.run(delay=3., test.pass)
