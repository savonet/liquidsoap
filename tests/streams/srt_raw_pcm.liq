log.level := 4
settings.srt.prefer_address := "ipv4"

port = 8004

s =
  input.srt(
    port=port,
    content_type=
      "application/ffmpeg;format=s16le,ch_layout=mono,sample_rate=48000"
  )

s.on_track(fast=true, fun (_) -> test.pass())

output.dummy(fallible=true, s)

enc = %ffmpeg(format = "s16le", %audio(codec = "pcm_s16le", ac = 1, ar = 48000))

output.srt(fallible=true, port=port, enc, sine())
