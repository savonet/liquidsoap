a = sine()
v = blank()

s = source(
  source.tracks(a).{
    video = source.tracks(v).video
  }
)

s = source.on_track(s, fun (_) -> test.pass())

tmp = file.temp("foo", "mp3")
on_shutdown({file.remove(tmp)})

output.file(%mp3, tmp, s)
