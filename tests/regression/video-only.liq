a = sine()
v = blank()
s = source(source.tracks(a).{video=source.tracks(v).video})
s.on_track(synchronous=true, fun (_) -> test.pass())
tmp = file.temp("foo", "flv")
on_cleanup({file.remove(tmp)})
output.file(%ffmpeg(format = "flv", %video(codec = "libx264")), tmp, s)
