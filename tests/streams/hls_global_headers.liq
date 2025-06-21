tmp_dir = file.temp_dir("tmp")
on_cleanup({file.rmdir(tmp_dir)})

s = video.testsrc.ffmpeg(duration=10.)

s = mksafe(s)

def main_playlist_writer(~extra_tags:_, ~prefix:_, ~version:_, streams) =
  let [{codecs = c1, ..._}, {codecs = c2, ..._}] = streams
  if
    c1 == "avc1.64001f" and c2 == "avc1.64001f"
  then
    test.pass()
  else
    test.fail()
  end
  ""
end

output.file.hls(
  main_playlist_writer=main_playlist_writer,
  segment_duration=2.0,
  tmp_dir,
  [
    ("ts", %ffmpeg(format = "mpegts", %video(codec = "libx264", b = "500k"))),
    ("mp4", %ffmpeg(format = "mp4", %video(codec = "libx264", b = "500k")))
  ],
  s
)
