s = debug.is_ready(sine(duration=0.2))

s = source.dynamic(track_sensitive=false, {s})

o = output.dummy(fallible=true, s)
o.on_stop(test.pass)
