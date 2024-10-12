s = debug.is_ready(sine(duration=0.2))

s = source.dynamic(track_sensitive=false, {s})

output.dummy(fallible=true, on_stop=test.pass, s)
