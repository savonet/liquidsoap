a = sine()
v = blank()

s = source(
  source.tracks(a).{
    video = source.tracks(v).video
  }
)

s = source.on_track(s, fun (_) -> test.pass())

tmp = file.temp("foo", "flv")
on_shutdown({file.remove(tmp)})

output.file(
  %ffmpeg(format="flv",%video.raw(codec="libx264")),
  tmp,
  s
)
