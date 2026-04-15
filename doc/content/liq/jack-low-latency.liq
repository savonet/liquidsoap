%ifndef output.jack
let output.jack = output.dummy
%endif

# BEGIN
video.frame.rate := 0
settings.frame.duration :=
  float_of_int(jack.server.buffer_size()) /
    float_of_int(jack.server.sample_rate())

output.jack(id="playlist", fallible=true, playlist("~/music"))


# END