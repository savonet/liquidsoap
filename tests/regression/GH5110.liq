# Regression test for https://github.com/savonet/liquidsoap/issues/5110
# Crash when multiple AAC encoders share the same source (as in AzuraCast
# "Share Encoders Between Streams").

log.level := 4

def f() =
  s = once(sine(duration=10.))

  aac_48 = ffmpeg.encode.audio(%ffmpeg(%audio(codec = "aac", b = "48k")), s)
  aac_96 = ffmpeg.encode.audio(%ffmpeg(%audio(codec = "aac", b = "96k")), s)
  aac_192 = ffmpeg.encode.audio(%ffmpeg(%audio(codec = "aac", b = "192k")), s)

  clock.assign_new(sync="none", [aac_48, aac_96, aac_192])

  out1 = file.temp(cleanup=true, "liq", ".aac")
  out2 = file.temp(cleanup=true, "liq", ".aac")
  out3 = file.temp(cleanup=true, "liq", ".aac")

  done = ref(0)

  def on_close(_) =
    done := done() + 1
    if done() >= 3 then test.pass() end
  end

  output.file(
    fallible=true,
    on_close=on_close,
    %ffmpeg(format = "adts", %audio.copy),
    out1,
    aac_48
  )
  output.file(
    fallible=true,
    on_close=on_close,
    %ffmpeg(format = "adts", %audio.copy),
    out2,
    aac_96
  )
  output.file(
    fallible=true,
    on_close=on_close,
    %ffmpeg(format = "adts", %audio.copy),
    out3,
    aac_192
  )
end

test.check(f)
