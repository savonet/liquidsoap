# Simpler version of GH3239 to work on a specific issue with
# source,dynamic

tmp = file.temp("foo", "mp3")
on_cleanup({file.remove(tmp)})

def f() =
  s = once(sine(duration=2.))
  let {audio = a} = source.tracks(s)
  track.audio.amplify(1., a)
end

s = source({audio=f()})

o = output.file(fallible=true, %mp3(mono), tmp, s)
o.on_stop(synchronous=true, test.pass)
