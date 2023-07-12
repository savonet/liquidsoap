tmp_dir = file.temp_dir("tmp")
on_cleanup({file.rmdir(tmp_dir)})

def f() =
  enc     = %ffmpeg(format="mpegts",%audio(codec="aac"))
  streams = [("enc", enc)]

  output.file.hls(
    fallible=true,
    on_stop=test.pass,
    persist_at="./config",
    path.concat(tmp_dir, "hls"),
    streams,
    once(sine(duration=1.))
  )
end

test.check(f)
