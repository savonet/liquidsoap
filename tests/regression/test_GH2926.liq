tmp_dir = file.temp_dir("tmp")
on_cleanup({file.rmdir(tmp_dir)})

def f() =
  enc = %ffmpeg(format = "mpegts", %audio(codec = "aac"))
  streams = [("enc", enc)]
  o =
    output.file.hls(
      fallible=true,
      persist_at="./config",
      path.concat(tmp_dir, "hls"),
      streams,
      once(sine(duration=1.))
    )
  o.on_stop(synchronous=true, test.pass)
end

test.check(f)
