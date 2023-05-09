log.level := 4

port = random.int(min=8000, max=10000)

s = input.srt(
 port=port,
 content_type = "application/ffmpeg;format=s16le,ch_layout=mono,sample_rate=48000"
)

s = source.on_track(s, fun (_) -> test.pass())

output.dummy(fallible=true, s)

enc = %ffmpeg(
  format="s16le",
  %audio(
    codec="pcm_s16le",
    ac=1,
    ar=48000
  )
)

output.srt(
  fallible=true,
  port=port,
  enc,
  sine()
)
