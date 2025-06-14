#!../../liquidsoap ../test.liq

s_ref = ref((fallback([]) : source))

def next() =
  r = request.create("../streams/file1.png")
  ignore(request.resolve(r))
  s = request.dynamic(id="image-dyn", fun () -> null)
  s.set_queue([r])
  s
end

s =
  (source.dynamic(id="image", track_sensitive=true, next) :
    source(video=canvas)
  )

s_ref := s

output.dummy(fallible=true, s)

s_ref = ref((fallback([]) : source))

def next() =
  r = request.create("../streams/file1.mp3")
  ignore(request.resolve(r))
  s = request.dynamic(id="audio-dyn", fun () -> null)
  s.set_queue([r])
  s
end

s = (source.dynamic(id="audio", track_sensitive=true, next) : source(audio=pcm))

s_ref := s

output.dummy(fallible=true, s)

thread.run(delay=3., test.pass)
